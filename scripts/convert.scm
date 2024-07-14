(define %root
  (dirname (canonicalize-path (dirname (or (current-filename) "./convert.scm")))))

(define %manifests-dir (string-append %root "/guix/rustup/manifests"))

(map (lambda (date)
       (let* ((module (resolve-module `(rustup dist ,date)))
              )
         (for-each
          (lambda (channel)
            (let* ((date-name (format #f "~a-~a" channel (symbol->string date)))
                   (variable (module-variable module channel)))
              (when variable
                (let* ((data (variable-ref variable))
                       (version (car data)))
                  (call-with-output-file (format #f "~a/~a" %manifests-dir date-name)
                    (lambda (port)
                      (write `(,version
                               ,(symbol->string date)
                               ,@(cdr data)) port)))
                  (unless (eq? channel 'nightly)
                    (call-with-port
                        (open %manifests-dir O_RDONLY)
                      (lambda (port)
                        (when (file-exists? (format #f "~a/~a" %manifests-dir version))
                          (delete-file-at port version))
                        (symlinkat port date-name version)
                        ;; (readlink version)
                        )))))))
          '(stable beta nightly))))
     '(2020-11-19
       2020-12-31
       2021-02-11
       2021-03-25
       2021-05-06
       2021-05-10
       2021-06-17
       2021-07-29
       2021-09-09
       2021-10-21
       2021-11-01
       2021-12-02
       2022-01-13
       2022-01-20
       2022-02-24
       2022-04-07
       2022-05-19
       2022-06-30
       2022-07-19
       2022-08-11
       2022-09-22
       2022-11-03
       2022-12-15
       2023-01-10
       2023-01-26
       2023-02-09
       2023-03-09
       2023-03-23
       2023-03-28
       2023-04-20
       2023-06-01
       2023-07-13
       2023-08-03
       2023-08-24
       2023-09-19
       2023-10-05
       2023-11-16
       2023-12-07
       2023-12-28
       2024-02-08
       2024-03-21
       2024-03-28
       2024-04-09
       2024-05-02
       ))
