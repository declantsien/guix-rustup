;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Lars-Dominik Braun <lars@6xq.net>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

;; This is a TOML parser adapted from the ABNF for v1.0.0 from
;; https://github.com/toml-lang/toml/blob/1.0.0/toml.abnf
;; The PEG grammar tries to follow the ABNF as closely as possible with
;; few deviations commented.
;;
;; The semantics are defined in https://toml.io/en/v1.0.0
;; Currently unimplemented:
;; - Array of Tables

(define-module (rustup build toml)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-35)
  #:export (parse-toml parse-toml-file recursive-assoc-ref &file-not-consumed &already-defined))

(define-condition-type &toml-error &error toml-error?)
(define-condition-type &file-not-consumed &toml-error file-not-consumed?)
(define-condition-type &already-defined &toml-error already-defined?)

;; Overall Structure
(define-peg-pattern toml-file body (and expression
                                        (* (and ignore-newline expression))))
(define-peg-pattern expression body (or
                                      (and ws keyval ws (? comment))
                                      (and ws table ws (? comment))
                                      (and ws (? comment))))

;; Whitespace
(define-peg-pattern ws none (* wschar))
(define-peg-pattern wschar body (or " " "\t"))

;; Newline
(define-peg-pattern newline body (or "\n" "\r\n"))
;; This newline’s content is ignored, so we don’t need a bunch of (ignore newline).
(define-peg-pattern ignore-newline none newline)

;; Comment
(define-peg-pattern non-ascii body (or (range #\x80 #\xd7ff)
                                       (range #\xe000 #\x10ffff)))
(define-peg-pattern non-eol body (or "\t" (range #\x20 #\x7f) non-ascii))

(define-peg-pattern comment none (and "#" (* non-eol)))

;; Key-Value pairs
(define-peg-pattern keyval all (and key keyval-sep val))

(define-peg-pattern key body (or dotted-key
                                 simple-key))
(define-peg-pattern simple-key all (or quoted-key
                                       unquoted-key))
(define-peg-pattern unquoted-key body (+ (or (range #\A #\Z)
                                             (range #\a #\z)
                                             (range #\0 #\9)
                                             "-"
                                             "_")))
(define-peg-pattern quoted-key all (or basic-string
                                       literal-string))
(define-peg-pattern dotted-key body (and simple-key
                                         (+ (and dot-sep simple-key))))
(define-peg-pattern dot-sep none (and ws "." ws))
(define-peg-pattern keyval-sep none (and ws "=" ws))

(define-peg-pattern val body (or string
                                 boolean
                                 array
                                 inline-table
                                 date-time
                                 float
                                 integer))

;; String
(define-peg-pattern string all (or ml-basic-string
                                   basic-string
                                   ml-literal-string
                                   literal-string))

;; Basic String
(define-peg-pattern basic-string body (and (ignore "\"")
                                           (* basic-char)
                                           (ignore "\"")))
(define-peg-pattern basic-char body (or basic-unescaped escaped))
(define-peg-pattern basic-unescaped body (or wschar
                                             "\x21"
                                             (range #\x23 #\x5B)
                                             (range #\x5D #\x7E)
                                             non-ascii))
(define-peg-pattern escaped all (and
                                 (ignore "\\")
                                 (or "\"" "\\" "b" "f" "n" "r" "t"
                                     (and (ignore "u")
                                          HEXDIG HEXDIG HEXDIG HEXDIG)
                                     (and (ignore "U")
                                          HEXDIG HEXDIG HEXDIG HEXDIG
                                          HEXDIG HEXDIG HEXDIG HEXDIG))))

;; Multiline Basic String
(define-peg-pattern ml-basic-string body (and
                                           ml-basic-string-delim
                                          (? ignore-newline)
                                          ml-basic-body
                                          ml-basic-string-delim))
(define-peg-pattern ml-basic-string-delim none "\"\"\"")
(define-peg-pattern ml-basic-body body (and
                                         (* mlb-content)
                                         (* (and mlb-quotes (+ mlb-content)))
                                         (? mlb-quotes-final)))

(define-peg-pattern mlb-content body (or mlb-char newline mlb-escaped-nl))
(define-peg-pattern mlb-char body (or mlb-unescaped escaped))
(define-peg-pattern mlb-quotes body (or "\"\"" "\""))
;; We need to convince the parser to backtrack here, thus the additional followed-by rule.
(define-peg-pattern mlb-quotes-final body (or (and "\"\"" (followed-by
                                                           ml-basic-string-delim))
                                              (and "\"" (followed-by
                                                         ml-basic-string-delim))))
(define-peg-pattern mlb-unescaped body (or wschar
                                           "\x21"
                                           (range #\x23 #\x5B)
                                           (range #\x5D #\x7E)
                                           non-ascii))
;; Escaped newlines and following whitespace are removed from the output.
(define-peg-pattern mlb-escaped-nl none (and "\\" ws newline
                                             (* (or wschar newline))))

;; Literal String
(define-peg-pattern literal-string body (and (ignore "'")
                                             (* literal-char)
                                             (ignore "'")))
(define-peg-pattern literal-char body (or "\x09"
                                          (range #\x20 #\x26)
                                          (range #\x28 #\x7E)
                                          non-ascii))

;; Multiline Literal String
(define-peg-pattern ml-literal-string body (and
                                            ml-literal-string-delim
                                            (? ignore-newline)
                                            ml-literal-body
                                            ml-literal-string-delim))
(define-peg-pattern ml-literal-string-delim none "'''")
(define-peg-pattern ml-literal-body body (and
                                          (* mll-content)
                                          (* (and mll-quotes (+ mll-content)))
                                          (? mll-quotes-final)))

(define-peg-pattern mll-content body (or mll-char newline))
(define-peg-pattern mll-char body (or "\x09"
                                      (range #\x20 #\x26)
                                      (range #\x28 #\x7E)
                                      non-ascii))
(define-peg-pattern mll-quotes body (or "''" "'"))
;; We need to convince the parser to backtrack here, thus the additional followed-by rule.
(define-peg-pattern mll-quotes-final body (or (and "''" (followed-by
                                                         ml-literal-string-delim))
                                              (and "'" (followed-by
                                                        ml-literal-string-delim))))

;; Integer
(define-peg-pattern integer all (or hex-int oct-int bin-int dec-int))

(define-peg-pattern digit1-9 body (range #\1 #\9))
(define-peg-pattern digit0-7 body (range #\0 #\7))
(define-peg-pattern digit0-1 body (range #\0 #\1))
(define-peg-pattern DIGIT body (range #\0 #\9))
(define-peg-pattern HEXDIG body (or DIGIT
                                    (range #\a #\f)
                                    (range #\A #\F)))

(define-peg-pattern dec-int all (and (? (or "-" "+")) unsigned-dec-int))
(define-peg-pattern unsigned-dec-int body (or (and digit1-9 (+ (or DIGIT (and (ignore "_") DIGIT))))
                                              DIGIT))

(define-peg-pattern hex-int all (and (ignore "0x")
                                     HEXDIG
                                     (* (or HEXDIG (and (ignore "_") HEXDIG)))))
(define-peg-pattern oct-int all (and (ignore "0o")
                                     digit0-7
                                     (* (or digit0-7 (and (ignore "_") digit0-7)))))
(define-peg-pattern bin-int all (and (ignore "0b")
                                     digit0-1
                                     (* (or digit0-1 (and (ignore "_") digit0-1)))))

;; Float
(define-peg-pattern float all (or
                                (and float-int-part (or exp (and frac (? exp))))
                                special-float))
(define-peg-pattern float-int-part body dec-int)
(define-peg-pattern frac body (and "." zero-prefixable-int))
(define-peg-pattern zero-prefixable-int body (and DIGIT (* (or DIGIT (and (ignore "_") DIGIT)))))

(define-peg-pattern exp body (and (or "e" "E") float-exp-part))
(define-peg-pattern float-exp-part body (and (? (or "-" "+")) zero-prefixable-int))
(define-peg-pattern special-float body (and (? (or "-" "+")) (or "inf" "nan")))

;; Boolean
(define-peg-pattern boolean all (or "true" "false"))

;; Date and Time (as defined in RFC 3339)

(define-peg-pattern date-time body (or offset-date-time
                                       local-date-time
                                       local-date
                                       local-time))

(define-peg-pattern date-fullyear all (and DIGIT DIGIT DIGIT DIGIT))
(define-peg-pattern date-month all (and DIGIT DIGIT))  ; 01-12
(define-peg-pattern date-mday all (and DIGIT DIGIT))  ; 01-28, 01-29, 01-30, 01-31 based on month/year
(define-peg-pattern time-delim none (or "T" "t" " ")) ; T, t, or space
(define-peg-pattern time-hour all (and DIGIT DIGIT))  ; 00-23
(define-peg-pattern time-minute all (and DIGIT DIGIT))  ; 00-59
(define-peg-pattern time-second all (and DIGIT DIGIT))  ; 00-58, 00-59, 00-60 based on leap second rules
(define-peg-pattern time-secfrac all (and (ignore ".") (+ DIGIT)))
(define-peg-pattern time-numoffset body (and (or "+" "-")
                                             time-hour
                                             (ignore ":")
                                             time-minute))
(define-peg-pattern time-offset all (or "Z" time-numoffset))

(define-peg-pattern partial-time body (and time-hour
                                           (ignore ":")
                                           time-minute
                                           (ignore ":")
                                           time-second
                                           (? time-secfrac)))
(define-peg-pattern full-date body (and date-fullyear
                                        (ignore "-")
                                        date-month
                                        (ignore "-")
                                        date-mday))
(define-peg-pattern full-time body (and partial-time time-offset))

;; Offset Date-Time
(define-peg-pattern offset-date-time all (and full-date time-delim full-time))

;; Local Date-Time
(define-peg-pattern local-date-time all (and full-date time-delim partial-time))

;; Local Date
(define-peg-pattern local-date all full-date)

;; Local Time
(define-peg-pattern local-time all partial-time)

;; Array
(define-peg-pattern array all (and (ignore "[")
                                   (? array-values)
                                   (ignore ws-comment-newline)
                                   (ignore "]")))

(define-peg-pattern array-values body (or
                                       (and ws-comment-newline
                                            val
                                            ws-comment-newline
                                            (ignore ",")
                                            array-values)
                                       (and ws-comment-newline
                                            val
                                            ws-comment-newline
                                            (ignore (? ",")))))

(define-peg-pattern ws-comment-newline none (* (or wschar (and (? comment) ignore-newline))))

;; Table
(define-peg-pattern table all (or array-table
                                  std-table))

;; Standard Table
(define-peg-pattern std-table all (and (ignore "[") ws key ws (ignore "]")))
(define-peg-pattern array-table all (and (ignore "[[") ws key ws (ignore "]]")))

;; Inline Table
(define-peg-pattern inline-table all (and (ignore "{")
                                          (* ws)
                                          (? inline-table-keyvals)
                                          (* ws)
                                          (ignore "}")))
(define-peg-pattern inline-table-sep none (and ws "," ws))
(define-peg-pattern inline-table-keyvals body (and keyval
                                                   (? (and inline-table-sep inline-table-keyvals))))


;; Parsing

(define (recursive-acons key value alist)
  "Add a VALUE to ALIST of alists descending into keys according to the
list in KEY. For instance of KEY is (a b) this would create
alist[a][b] = value."
  (match key
    (((? string? key))
     (if (assoc-ref alist key)
       (raise (condition (&already-defined)))
       (alist-cons key value alist)))
    ((elem rest ...) (match (assoc-ref alist elem)
                       (#f
                         (acons elem (recursive-acons rest value '()) alist))
                       (old-value
                         (acons elem (recursive-acons rest value old-value) (alist-delete elem alist)))))
    (() alist)))

(define (recursive-assoc-ref alist key)
  "Retrieve a value from ALIST of alists, descending into each value of
the list KEY. For instance a KEY (a b) would retrieve alist[a][b]."
  (match key
    (((? string? key)) (assoc-ref alist key))
    ((elem rest ...) (recursive-assoc-ref (assoc-ref alist elem) rest))))

(define (eval-toml-file parse-tree)
  "Convert toml parse tree to alist."

  (define (assoc-ref->number alist key)
    (and=> (and=> (assq-ref alist key) car) string->number))

  (define (eval-date rest)
    (let ((args (keyword-flatten '(date-fullyear
                                   date-month
                                   date-mday
                                   time-hour
                                   time-minute
                                   time-second
                                   time-secfrac
                                   time-offset)
                                 rest)))
      (make-date
       (assoc-ref->number args 'time-secfrac)
       (assoc-ref->number args 'time-second)
       (assoc-ref->number args 'time-minute)
       (assoc-ref->number args 'time-hour)
       (assoc-ref->number args 'date-mday)
       (assoc-ref->number args 'date-month)
       (assoc-ref->number args 'date-fullyear)
       (match (assq-ref args 'time-offset)
         (("Z") 0)
         ((sign ('time-hour hour) ('time-minute minute))
          (* (+
               (* (string->number (string-append sign hour)) 60)
               (string->number minute)) 60))
         (#f #f)))))

  (define (eval-value value)
    "Evaluate right-hand-side of 'keyval token (i.e., a value)."
    (match value
      (('boolean "true")
       #t)
      (('boolean "false")
       #f)
      (('integer ('dec-int int))
       (string->number int 10))
      (('integer ('hex-int int))
       (string->number int 16))
      (('integer ('oct-int int))
       (string->number int 8))
      (('integer ('bin-int int))
       (string->number int 2))
      (('float ('dec-int int) b)
       (string->number (string-append int b) 10))
      (('float other)
       (match other
         ("inf" +inf.0)
         ("+inf" +inf.0)
         ("-inf" -inf.0)
         ("nan" +nan.0)
         ("+nan" +nan.0)
         ("-nan" -nan.0)))
      (('offset-date-time rest ...)
       (eval-date rest))
      (('local-date-time rest ...)
       (eval-date rest))
      (('local-date rest ...)
       (eval-date rest))
      (('local-time rest ...)
       (eval-date rest))
      (('string str ...)
       (apply string-append
              (map (match-lambda
                    (('escaped "\"") "\"")
                    (('escaped "\\") "\\")
                    (('escaped "b") "\b")
                    (('escaped "t") "\t")
                    (('escaped "n") "\n")
                    (('escaped (? (lambda (x) (>= (string-length x) 4)) u))
                     (list->string (list (integer->char (string->number u 16)))))
                    ((? string? s) s))
                   (keyword-flatten '(escaped) str))))
      ('string "")
      (('array tails ...)
       (map eval-value (keyword-flatten '(boolean integer float string array
                                          inline-table offset-date-time
                                          local-date-time local-date
                                          local-time)
                                        tails)))
      ('array (list))
      (('inline-table tails ...)
       (eval (keyword-flatten '(keyval) tails) '() '()))))

  (define (ensure-list value)
    (if (list? value)
        value
        (list value)))

  (define (simple-key->list keys)
     (map
      (match-lambda
        (('simple-key 'quoted-key) "")
        (('simple-key ('quoted-key k)) k)
        (('simple-key (? string? k)) k)
        (other (raise-exception `(invalid-simple-key ,other))))
      (keyword-flatten '(simple-key) keys)))

  (define (skip-keyval tails)
    "Skip key-value pairs in tails until the next table."
    (match tails
      ((('keyval key val) tails ...)
       (skip-keyval tails))
      (('keyval keyval)
       '())
      (other other)))

  (define (eval parse-tree current-table result)
    "Evaluate toml file body."

    (match parse-tree
      ((('table ('std-table names ...)) tails ...)
       (eval tails (simple-key->list names) result))
      ((('table ('array-table names ...)) tails ...)
       ;; Not implemented.
       (eval (skip-keyval tails) '() result))
      ((('keyval key val) tails ...)
       (recursive-acons
        (append current-table (ensure-list (simple-key->list key)))
        (eval-value val)
        (eval tails current-table result)))
      (('keyval key val)
       (recursive-acons
        (append current-table (ensure-list (simple-key->list key)))
        (eval-value val)
        result))
      (()
       '())))

  (eval parse-tree '() '()))

(define (parse-toml str)
  "Parse and evaluate toml document from string STR."

  (let* ((match (match-pattern toml-file str))
         (end (peg:end match))
         (tree (peg:tree match))
         (flat-tree (keyword-flatten '(table keyval) tree)))
    (if (eq? end (string-length str))
      (eval-toml-file flat-tree)
      (raise (condition (&file-not-consumed))))))

(define (parse-toml-file file)
  "Parse and evaluate toml document from file FILE."

  (parse-toml (call-with-input-file file get-string-all)))
