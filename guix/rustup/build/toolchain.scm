;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Declan Tsien <declantsien@riseup.net>
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

(define-module (rustup build toolchain)
  #:use-module (rnrs enums)
  #:use-module (guix http-client)
  #:use-module (web uri)
  #:use-module (guix utils)
  #:use-module (guix records)
  #:use-module (guix i18n)
  #:use-module ((guix build utils) #:select (find-files symbolic-link?))
  #:use-module ((gnu home services utils) #:select (list->human-readable-list))
  #:use-module ((guix diagnostics) #:select
                (formatted-message warning info report-error info leave))
  ;; #:use-module (guix build utils)
  #:use-module (ice-9 textual-ports)
  #:use-module (rustup build utils)
  #:use-module (rustup build toml)
  #:use-module (rustup build manifest)
  #:use-module (ice-9 binary-ports)
  #:use-module (json)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-26)  ;; cut
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages compression)
  #:use-module (guix download)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:export (rustup
            toolchain->new
            toolchain->from-file
            toolchain->source
            rust-bin-from-toolchain
            rust-bin-from-toolchain-file
            rust-bin-from-rustc-rev))

(define %toolchain-profiles (make-enumeration
                            '(minimal
                              default
                              complete)))

(define (toolchain-profile? profile)
  (unless (enum-set-member?
           (string->symbol (format #f "~a" profile))
           %toolchain-profiles)
    (raise (formatted-message
            (G_ "Toolchain profile must be one of ~a, was given: ~a")
            (list->human-readable-list (enum-set->list %toolchain-profiles))
            profile))))

(define %minimal-profile '(cargo rust-mingw rust-std rustc))
(define %default-profile '(cargo clippy rust-docs rust-mingw rust-std rustc rustfmt))

;; Copied from (gnu packages rust)
(define* (nix-system->gnu-triplet-for-rust
          #:optional (system (%current-system)))
  (match system
    ("x86_64-linux"   "x86_64-unknown-linux-gnu")
    ("i686-linux"     "i686-unknown-linux-gnu")
    ("armhf-linux"    "armv7-unknown-linux-gnueabihf")
    ("aarch64-linux"  "aarch64-unknown-linux-gnu")
    ("mips64el-linux" "mips64el-unknown-linux-gnuabi64")
    ("riscv64-linux"  "riscv64gc-unknown-linux-gnu")
    (_                (nix-system->gnu-triplet system))))

(define name-ends-with-preview? (cut string-suffix? "/" <>))

(define-record-type* <target>
  target make-target
  target?
  (uri           target->uri
                 (default #f))
  (hash          target->hash
                 (default #f))
  (available?    target->available?
                 (default #f)))

(define-record-type* <toolchain>
  toolchain make-toolchain
  toolchain?
  (channel           toolchain->channel
                     (default "stable"))
  (components        toolchain->components
                     (default (list )))
  (targets           toolchain->targets
		     (default (list )))
  (profile           toolchain->profile
                     (default 'default))
  (manifest           toolchain->manifest
                      (default #f)))

(define* (toolchain->new channel-str
			 #:key
			 (components (list ))
			 (targets (list ))
			 (profile 'default))
  (define _ (channel-str? channel-str))
  (toolchain-profile? profile)
  (unless (list? components)
    (raise (formatted-message
            (G_ "components should be a list, was given: ~a")
            components)))
  (for-each %toolchain-components? components)
  (unless (list? targets)
    (raise (formatted-message
            (G_ "targets should be a list, was given: ~a")
            targets)))
  (for-each %rustc-target-triplets? targets)
  (define alias (find (lambda (alias)
                        (string= (channel-str-normalize channel-str) (symbol->string (car alias))))
                      (module-ref (resolve-module '(rustup dist)) 'aliases)))
  (define aliased-str (cond (alias
                             (symbol->string (cdr alias)))
                            (else
                             channel-str)))
  (define channel (channel->from-str
                   aliased-str))
  (define _channel-name (channel->name channel))
  (define _channel-date (channel->date channel))
  (define _var (if _channel-date
                   (module-variable
                    (resolve-module `(rustup dist ,(string->symbol _channel-date)))
                    (string->symbol (channel-str-normalize _channel-name)))
                   #f))
  (define _data (cond (_var
                       (variable-ref _var))
                      (else
                       (download-and-compact-manifest channel-str))))

  (unless _data
    (raise (formatted-message
            (G_ "Toolchain not avaiable, was given: ~a")
            channel-str)))

  (define version (cond ((equal? _channel-name "nightly")
                         (format #f "~a-~a" (car _data) _channel-date))
                        (else
                         (car _data))))
  (define hashed-binaries (cdr _data))
  (define available-components (map car hashed-binaries))
  (define (component-available? component)
    (unless (member (%toolchain-components->position component) available-components)
      (raise (formatted-message
              (G_ "Channel ~a available components ~a was given: ~a ~a")
              channel-str
              (map %toolchain-components->get available-components)
              component))))
  (for-each component-available? components)

  (define rust-std-component-index (%toolchain-components->position 'rust-std))
  (define available-cross-targets
    (let ((tmp (find (lambda (binary)
                       (equal? (car binary) rust-std-component-index))
                     hashed-binaries)))
      (if tmp (cdr tmp) '())))
  (define available-cross-triplets (map car available-cross-targets))
  (define (cross-target-available? triplet)
    (unless (member (%rustc-target-triplets->position triplet) available-cross-triplets)
      (raise (formatted-message
              (G_ "Channel ~a available cross targets ~a was given: ~a ~a")
              channel-str
              (map %rustc-target-triplets->get available-cross-triplets)
              triplet))))
  (for-each cross-target-available? targets)

  (define profile-components
    (case (string->symbol (format #f "~a" profile))
      ((default)
       (map %toolchain-components->position %default-profile))
      ((minimal)
       (map %toolchain-components->position %minimal-profile))
      ((complete)
       available-components)))

  (define* aggregated-components
    (delete-duplicates
     (append profile-components
             (map %toolchain-components->position components))))

  (define host-triplet
    (%rustc-target-triplets->position (nix-system->gnu-triplet-for-rust)))

  (define sources
    (filter-map
     (lambda (binary)
       (let ((component-name-index (car binary))
             (targets (cdr binary))
             (host-target
              (find (lambda (target)
                      (member
                       (car target)
                       (list (%rustc-target-triplets->position 'all)
                             host-triplet))) (cdr binary))))

         (and (member (car binary) aggregated-components)
              host-target
              `(,component-name-index ,@host-target))))
     hashed-binaries))

  (define cross-sources (filter
           (lambda (target)
             (member (car target)
                     (map %rustc-target-triplets->position targets)))
           available-cross-targets))

  (define all-sources (delete-duplicates
   (append
    sources
    (map (lambda (source)
           `(,rust-std-component-index ,@source))
         cross-sources))))

  (define hashed-binary-urls (map (lambda (source)
         (apply hashed-binary-url
                (append source
                        (case (length source)
                           ((5)
                            '())
                           ((4)
                            '(#f))
                           (else
                            (error "Unexpected hashed binary size")))
                        `(
                          ,(cond ((member _channel-name '("beta" "nightly"))
                                  _channel-name)
                                 (else
                                  version))
                          ,_channel-date ,%rustup-dist-root))))
       all-sources))

  `(,version . ,hashed-binary-urls))

(define* (parse-rust-toolchain-file file)
  (let* ((content (call-with-input-file file get-string-all))
	 (toml (catch #t
                 (lambda () (parse-toml content))
                 (lambda _ #f))))
    (if toml
	(let* ((channel (recursive-assoc-ref
			 toml
			 `("toolchain" "channel")))
	       (components (recursive-assoc-ref
			    toml
			    `("toolchain" "components")))
	       (targets (recursive-assoc-ref
			 toml
			 `("toolchain" "targets")))
	       (profile (recursive-assoc-ref
			 toml
			 `("toolchain" "profile"))))
	  (list channel
                #:components components
                #:targets targets
                #:profile profile))
	(list (string-trim-right content #\newline)))))

(define* (toolchain->version t)
  (let ((manifest (toolchain->manifest t)))
    (if manifest
        (car (string-split
	      (recursive-assoc-ref
	       manifest
	       '("pkg" "rust" "version"))
	      #\ ))
        (toolchain->channel t))))

(define* (rustup #:optional (channel-or-toolchain-file #f)
		 #:key
		 (components (list ))
		 (targets (list ))
		 (profile 'default))
  (let ((args (match (and channel-or-toolchain-file (basename channel-or-toolchain-file))
                ((or "rust-toolchain" "rust-toolchain.toml" #f)
                 (args-from-default-toolchain-file
                  channel-or-toolchain-file
                  #:components components
                  #:targets targets
                  #:profile profile))
                (_ (list channel-or-toolchain-file
                         #:components components
                         #:targets targets
                         #:profile profile)))))
    (make-rust-toolchain (apply toolchain->new args))))

(define* (make-rust-toolchain aggregated)
  (let ((version (car aggregated))
	(sources (cdr aggregated)))
    (package
      (name "rust-toolchain")
      (version version)
      (source #f)
      (build-system binary-build-system)
      (inputs
       (append
	(list curl llvm-13 openssl-1.1 zlib)
	(filter-map
	 (lambda (source)
	   (and source
		(origin
		  (method url-fetch)
		  (uri (car source))
		  (sha256
		   (base32 (cdr source))))))
	 sources)))
      (native-inputs (list `(,gcc "lib")))
      (arguments
       (list
	#:strip-binaries? #t
	#:validate-runpath? #f
	#:phases
	#~(modify-phases %standard-phases
	    (replace 'unpack
              (lambda* (#:key inputs #:allow-other-keys)
		(for-each
		 (lambda (input)
		   (let* ((name (car input)))
		     (when (equal? name "_")
		       (let ((source (cdr input)))
			 ;; (format #t "source : ~a ...~%" source)
			 (invoke "tar" "-xvf" source)))
		     ))
                 inputs)
		))
	    (delete 'patchelf)
	    (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((target (assoc-ref outputs "out")))
		  (setenv "CFG_DISABLE_LDCONFIG" "true")
		  (for-each (lambda (installer)
			      (invoke "sed" "-i" "/install_uninstaller \"/d" installer)
			      (invoke "sh" installer
				      (string-append "--destdir=" target) "--prefix=/"))
                            (find-files "." "install.sh"))
		  )))
            (add-after 'install 'patchelf
              (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)

		(let* ((ld.so (string-append
                               (assoc-ref inputs "libc")
                               #$((@@ (gnu packages bootstrap) glibc-dynamic-linker)))))

                  (for-each
                   (lambda (output)
                     (let* ((name (car output))
                            (out (assoc-ref outputs name))
                            (libdir (string-append out "/lib"))
                            (rpath (string-join
                                    (append
                                     (list "$ORIGIN" libdir)

                                     (if (equal? "out" name)
					 (list)
					 (list
                                          (string-append (assoc-ref outputs "out") "/lib")))

                                     (map
                                      (lambda (input)
					(string-append (cdr input) "/lib"))
                                      inputs))
                                    ":")))

                       (define (patch-elf file)
			 (format #t "Patching ~a ...~%" file)
			 (unless (string-contains file ".so")
                           (invoke "patchelf" "--set-interpreter" ld.so file))
			 (invoke "patchelf" "--set-rpath" rpath file))

                       (for-each (lambda (file)
                                   (when (elf-file? file)
                                     (patch-elf file)))
				 (find-files out (lambda (file stat)
                                                   (elf-file? file))))))
                   outputs))))
	    (add-after 'install 'wrap-rustc
              (lambda* (#:key inputs outputs #:allow-other-keys)
		(let ((out (assoc-ref outputs "out"))
                      (libc (assoc-ref inputs "libc"))
                      (ld-wrapper (assoc-ref inputs "ld-wrapper")))
                  ;; Let gcc find ld and libc startup files.
                  (wrap-program (string-append out "/bin/rustc")
                    `("PATH" ":" prefix (,(string-append ld-wrapper "/bin")))
                    `("LIBRARY_PATH" ":"
                      suffix (,(string-append libc "/lib"))))))))))
      (synopsis "Compiler for the Rust programming language")
      (description "Rust is a systems programming language that provides memory
safety and thread safety guarantees.")
      (home-page "https://www.rust-lang.org")
      (license (list license:asl2.0 license:expat)))
    ))

;; Copied (guix scripts shell)
(define (find-file-in-parent-directories candidates)
  "Find one of CANDIDATES in the current directory or one of its ancestors."
  (define start (getcwd))
  (define device (stat:dev (stat start)))

  (let loop ((directory start))
    (let ((stat (stat directory)))
      (and (= (stat:uid stat) (getuid))
           (= (stat:dev stat) device)
           (or (any (lambda (candidate)
                      (let ((candidate (string-append directory "/" candidate)))
                        (and (file-exists? candidate) candidate)))
                    candidates)
               (and (not (string=? directory "/"))
                    (loop (dirname directory)))))))) ;lexical ".." resolution

(define* (args-from-default-toolchain-file
          #:optional (file #f)
          #:key
	  (components (list ))
	  (targets (list ))
	  (profile 'default))
  (let ((file (or file (find-file-in-parent-directories '("rust-toolchain.toml" "rust-toolchain")))))
    (match file
      (#f
       (warning (G_ "no toolchain specified; using stable channel~%"))
       (list "stable"
             #:components components
             #:targets targets
             #:profile profile))
      (file
       (begin
         (info (G_ "Loading Rust toolchain from '~a'...~%") file)
         (parse-rust-toolchain-file file))))))

(define* (select-latest-nightly-with file)
  ;; Select the latest nightly toolchain which have specific components or profile available.This helps nightly users in case of latest nightly may not contains all components they want.
  ;; `select-latest-nightly-with (toolchain: toolchain.default)` selects the latest nightly toolchain with all `default` components (rustc, cargo, rustfmt, ...) available.
  (error "TODO select-latest-nightly-with"))

(define* (rust-from-rustc-rev file)
  ;; # Custom toolchain from a specific rustc git revision.
  ;; # This does almost the same thing as `rustup-toolchain-install-master`. (https://crates.io/crates/rustup-toolchain-install-master)
  ;; # Parameter `components` should be an attrset with component name as key and its SRI hash as value.
  (error "TODO rust-from-rustc-rev"))
