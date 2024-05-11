(define-module (rustup build utils)
  #:use-module (srfi srfi-34)
  #:use-module (guix http-client)
  #:use-module (guix base16)
  #:use-module (guix base32)
  #:use-module (gcrypt hash)
  #:use-module (web uri)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:export (http-fetch/guarded
            channel-str-normalize
            base32-from-sha256))

(define* (http-fetch/guarded url #:optional (hash #f))
  ;; Handle 404
  (guard (c ((http-get-error? c)
             (if (= 404 (http-get-error-code c)) ;"Not Modified"
                 #f
                 (raise c)))
	    (#t c))
    (let* ((port  (http-fetch/cached (string->uri url)
				     #:ttl (* 6 3600)))
	   (content (get-string-all port))
           (checksum (if hash (bytevector->base16-string (sha256 (string->utf8 content))) #f)))
      (close-port port)
      (when (and hash (not (string= hash checksum)))
        (error (format #t "~%!Error: sha256 mismatch~%Expecting: ~a~%Actual:    ~a~%~%" hash checksum)))
      content)))

(define* (base32-from-sha256 sha256)
  (bytevector->nix-base32-string (base16-string->bytevector sha256)))

;; https://github.com/rust-lang/rustup/blob/54dd3d00fd20e64dc522517a0d7be4285570a2f1/src/dist/dist.rs#L204
(define* (channel-str-normalize channel-str)
  (match channel-str
    ("1.0" "1.0.0")
    ("1.1" "1.1.0")
    ("1.2" "1.2.0")
    ("1.3" "1.3.0")
    ("1.4" "1.4.0")
    ("1.5" "1.5.0")
    ("1.6" "1.6.0")
    ("1.7" "1.7.0")
    ("1.8" "1.8.0")
    (_
     channel-str)))
