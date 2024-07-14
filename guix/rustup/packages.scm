(define-module (rustup packages)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix diagnostics)
  #:use-module (ice-9 match)
  #:export (search-manifest))

(define %distro-root-directory
  ;; Absolute file name of the module hierarchy.  Since (gnu packages …) might
  ;; live in a directory different from (guix), try to get the best match.
  (letrec-syntax ((dirname* (syntax-rules ()
                              ((_ file)
                               (dirname file))
                              ((_ file head tail ...)
                               (dirname (dirname* file tail ...)))))
                  (try      (syntax-rules ()
                              ((_ (file things ...) rest ...)
                               (match (search-path %load-path file)
                                 (#f
                                  (try rest ...))
                                 (absolute
                                  (dirname* absolute things ...))))
                              ((_)
                               #f))))
    (try ("rustup/packages/rust.scm" rustup/ packages/)
         ("rustup/packages.scm"      rustup/)
         ("rustup.scm"))))

(define %manifest-path
  ;; Define it after '%package-module-path' so that '%load-path' contains user
  ;; directories, allowing patches in $GUIX_PACKAGE_PATH to be found.
  (make-parameter
   (map (lambda (directory)
          (if (string=? directory %distro-root-directory)
              (string-append directory "/rustup/manifests")
              directory))
        %load-path)))

(define* (search-manifest file-name)
  "Search the manifest FILE-NAME.  Raise an error if not found."
  (search-path (%manifest-path) file-name))
