(define-module (rustup packages)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix diagnostics)
  #:use-module ((guix build utils) #:select (mkdir-p))
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (search-manifest
            manifests-directory-cache-directory))

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

(define %manifests-directory
  ;; Define it after '%package-module-path' so that '%load-path' contains user
  ;; directories, allowing patches in $GUIX_PACKAGE_PATH to be found.
  (make-parameter
   (map (lambda (directory)
          (if (string=? directory %distro-root-directory)
              (string-append directory "/rustup/manifests")
              directory))
        %load-path)))

;; copied from (guix utils)
(define* (xdg-directory variable suffix #:key (ensure? #t))
  "Return the name of the XDG directory that matches VARIABLE and SUFFIX,
after making sure that it exists if ENSURE? is true.  VARIABLE is an
environment variable name like \"XDG_CONFIG_HOME\"; SUFFIX is a suffix like
\"/.config\".  Honor the XDG specs,
<http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html>."
  (let ((dir (and=> (or (getenv variable)
                        (and=> (or (getenv "HOME")
                                   (passwd:dir (getpwuid (getuid))))
                               (cut string-append <> suffix)))
                    (cut string-append <> "/guix-rustup"))))
    (when ensure?
      (mkdir-p dir))
    dir))

(define cache-directory
  (cut xdg-directory "XDG_CACHE_HOME" "/.cache" <...>))

(define* (manifests-directory-cache-directory #:key (ensure? #t))
  (let ((dir (string-append (cache-directory) "/manifests/")))
    (when ensure?
      (mkdir-p dir))
    dir))

(define* (search-manifest file-name)
  "Search the manifest FILE-NAME.  Raise an error if not found."
  (or (search-path (%manifests-directory) file-name)
      (search-path (list (manifests-directory-cache-directory)) file-name)))
