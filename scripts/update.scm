(define-module (scripts update)
  #:use-module (guix i18n)
  #:use-module (rustup build manifest)
  #:use-module (guix ui))

;; Requirements:
;; guix shell guile guile-gcrypt guile-gnutls
;; Usage:
;; guile update.scm stable
;; guile update.scm 1.79.0

(define %root
  (dirname (canonicalize-path (dirname (or (current-filename) "./update.scm")))))

(define %manifests-dir (string-append %root "/guix/rustup/manifests/"))

(define (update arg0 . args)
  (when (nil? args)
    (error (G_ "No channel specified '~a'...~%") `(,arg0 ,@args)))
  (let* ((channel-str (car args))
         (no-update-default-channel (if (nil? (cdr args))
                                        #f
                                        (cadr args)))
         (data (download-and-compact-manifest channel-str)))
    (write-manifest data %manifests-dir no-update-default-channel)))

(apply update (command-line))
