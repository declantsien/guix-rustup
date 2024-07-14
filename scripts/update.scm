(define-module (scripts update)
  #:use-module (guix i18n)
  #:use-module (rustup build manifest)
  #:use-module (guix ui))

;; Usage:
;; guile update.scm stable
;; guile update.scm 1.79.0

(define %root
  (dirname (canonicalize-path (dirname (or (current-filename) "./update.scm")))))

(define %manifests-dir (string-append %root "/guix/rustup/manifests/"))

(define (channel version)
  (cond ((string-contains version "beta")
                        "beta")
                       ((string-contains version "nightly")
                        "nightly")
                       (else
                        "stable")))

(define (update arg0 . args)
  (when (nil? args)
    (error (G_ "No channel specified '~a'...~%") `(,arg0 ,@args)))
  (let* ((channel-str (car args))
         (no-update-default-channel (if (nil? (cdr args))
                                        #f
                                        (cadr args)))
         (data (download-and-compact-manifest channel-str))
         (version (car data))
         (date (cadr data))
         (channel (channel version))
         (filename (format #f "~a~a-~a" %manifests-dir channel date)))
    (call-with-output-file filename
      (lambda (port)
        (write data port)))
    (unless (string= channel "nightly")
      (call-with-port
          (open %manifests-dir O_RDONLY)
        (lambda (port)
          (when (file-exists? (format #f "~a/~a" %manifests-dir version))
            (delete-file-at port version))
          (symlinkat port (basename filename) version)
          ;; (readlink version)
          )))
    (unless no-update-default-channel
      (call-with-port
          (open %manifests-dir O_RDONLY)
        (lambda (port)
          (when (file-exists? (format #f "~a/~a" %manifests-dir channel))
            (delete-file-at port channel))
          (symlinkat port (basename filename) channel)
          ;; (readlink version)
          )))
    ;; (info (G_ "no update default channel '~a'...~%") no-update-default-channel)
    ;; (info (G_ "version '~a'...~%") version)
    ;; (info (G_ "date '~a'...~%") date)
    ;; (info (G_ "filename '~a'...~%") filename)
    )
  )

(apply update (command-line))
