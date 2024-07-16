(use-modules ((rustup build manifest)))

(define %root
  (dirname (canonicalize-path (dirname (or (current-filename) "./convert.scm")))))

(define %manifests-dir (string-append %root "/guix/rustup/manifests"))

(define (uri-path url)
  (substring url (+ (string-contains url "dist/") 5)))

(define version "1.1.0")
(for-each
 (lambda (version)
   (call-with-input-file (format #f "/home/declan/backlog/manifests/~a" version)
  (lambda (port)
    (let* ((data (compact-manifest version (read port)))
           (date (cadr data))
           (date-name (format #f "stable-~a" date)))
      (if date
          (call-with-output-file (format #f "~a/~a" %manifests-dir date-name)
            (lambda (port)
              (write data port)
              (call-with-port
                  (open %manifests-dir O_RDONLY)
                (lambda (port)
                  (when (file-exists? (format #f "~a/~a" %manifests-dir version))
                    (delete-file-at port version))
                  (symlinkat port date-name version)
                  ;; (readlink version)
                        ))
              ))
          (begin
            (call-with-output-file (format #f "~a/~a" %manifests-dir version)
              (lambda (port)
                (write data port)))))
      ))))
 '("1.1.0"
"1.2.0"
"1.3.0"
;; "1.4.0"
;; "1.5.0"
;; "1.6.0"
;; "1.7.0"
;; "1.8.0"
;; "1.9.0"
;; "1.10.0"
;; "1.11.0"
;; "1.12.0"
;; "1.12.1"
;; "1.13.0"
;; "1.14.0"
;; "1.15.0"
;; "1.15.1"
;; "1.16.0"
;; "1.17.0"
;; "1.18.0"
;; "1.19.0"
;; "1.20.0"
;; "1.21.0"
;; "1.22.0"
;; "1.22.1"
;; "1.23.0"
;; "1.24.0"
;; "1.24.1"
;; "1.25.0"
;; "1.26.0"
;; "1.26.1"
;; "1.26.2"
;; "1.27.0"
;; "1.27.1"
;; "1.27.2"
;; "1.28.0"
;; "1.29.0"
;; "1.29.1"
;; "1.29.2"
;; "1.30.0"
;; "1.30.1"
;; "1.31.0"
;; "1.31.1"
;; "1.32.0"
;; "1.33.0"
;; "1.34.0"
;; "1.34.1"
;; "1.34.2"
;; "1.35.0"
;; "1.36.0"
;; "1.37.0"
;; "1.38.0"
;; "1.39.0"
;; "1.40.0"
;; "1.41.0"
;; "1.41.1"
;; "1.42.0"
;; "1.43.0"
;; "1.43.1"
;; "1.44.0"
;; "1.44.1"
;; "1.45.0"
;; "1.45.1"
;; "1.45.2"
;; "1.46.0"
;; "1.47.0"
"1.48.0"
))
          

;; (for-each
;;           (lambda (channel)
;;             (let* ((date-name (format #f "~a-~a" channel (symbol->string date)))
;;                    (variable (module-variable module channel)))
;;               (when variable
;;                 (let* ((data (variable-ref variable))
;;                        (version (car data)))
;;                   (call-with-output-file (format #f "~a/~a" %manifests-dir date-name)
;;                     (lambda (port)
;;                       (write `(,version
;;                                ,(symbol->string date)
;;                                ,@(cdr data)) port)))
;;                   (unless (eq? channel 'nightly)
;;                     (call-with-port
;;                         (open %manifests-dir O_RDONLY)
;;                       (lambda (port)
;;                         (when (file-exists? (format #f "~a/~a" %manifests-dir version))
;;                           (delete-file-at port version))
;;                         (symlinkat port date-name version)
;;                         ;; (readlink version)
;;                         )))))))
;;           '("1.1.0"
;; "1.2.0"
;; "1.3.0"
;; "1.4.0"
;; "1.5.0"
;; "1.6.0"
;; "1.7.0"
;; "1.8.0"
;; "1.9.0"
;; "1.10.0"
;; "1.11.0"
;; "1.12.0"
;; "1.12.1"
;; "1.13.0"
;; "1.14.0"
;; "1.15.0"
;; "1.15.1"
;; "1.16.0"
;; "1.17.0"
;; "1.18.0"
;; "1.19.0"
;; "1.20.0"
;; "1.21.0"
;; "1.22.0"
;; "1.22.1"
;; "1.23.0"
;; "1.24.0"
;; "1.24.1"
;; "1.25.0"
;; "1.26.0"
;; "1.26.1"
;; "1.26.2"
;; "1.27.0"
;; "1.27.1"
;; "1.27.2"
;; "1.28.0"
;; "1.29.0"
;; "1.29.1"
;; "1.29.2"
;; "1.30.0"
;; "1.30.1"
;; "1.31.0"
;; "1.31.1"
;; "1.32.0"
;; "1.33.0"
;; "1.34.0"
;; "1.34.1"
;; "1.34.2"
;; "1.35.0"
;; "1.36.0"
;; "1.37.0"
;; "1.38.0"
;; "1.39.0"
;; "1.40.0"
;; "1.41.0"
;; "1.41.1"
;; "1.42.0"
;; "1.43.0"
;; "1.43.1"
;; "1.44.0"
;; "1.44.1"
;; "1.45.0"
;; "1.45.1"
;; "1.45.2"
;; "1.46.0"
;; "1.47.0"))

