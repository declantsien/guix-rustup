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

(define-module (rustup build manifest)
  #:use-module (rustup packages)
  #:use-module (rnrs enums)
  #:use-module (guix records)
  #:use-module (guix base32)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (guix i18n)
  #:use-module (guix download)
  #:use-module (guix store)
  #:use-module (guix build utils)
  #:use-module (guix derivations)
  #:use-module ((gnu home services utils) #:select (list->human-readable-list))
  #:use-module ((guix diagnostics) #:select
                (formatted-message warning info report-error info leave))
  #:use-module (rustup build toml)
  #:use-module (rustup build utils)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (map-triplet
            channel-str-match
            channel->from-str
            channel->name
            channel->date
            channel-str?
            download-manifest
            compact-manifest
            download-and-compact-manifest
            write-manifest
            %rustc-target-triplets?
            %toolchain-components
            %toolchain-components?
            compact-toolchain-manifest
            %rustup-dist-root
            hashed-binary-url
            %toolchain-components->position
            %toolchain-components->get
            hashed-binary->name
            %rustc-target-triplets->get
            %rustc-target-triplets->position))

(define %staging? (getenv "RUSTUP_STAGED_MANIFEST"))

;; https://github.com/rust-lang/rustup/blob/54dd3d00fd20e64dc522517a0d7be4285570a2f1/src/dist/dist.rs#L176
;; PCRE non-capturing notation not working in guile ...(?:...)...
(define %channel-rx
  (make-regexp "^(nightly|beta|stable|[0-9]{1}\\.[0-9]{1,3}(\\.[0-9]{1,2})?)(-([0-9]{4}-[0-9]{2}-[0-9]{2}))?(-(.+))?$" regexp/extended))

(define %default-rustup-dist-server
  "https://static.rust-lang.org")

(define %rustup-dist-server
  (or (getenv "RUSTUP_DIST_SERVER") %default-rustup-dist-server))

(define %default-rustup-dist-root
  (format #f "~a/dist" %default-rustup-dist-server ))

(define %rustup-dist-root
  (format #f "~a/dist" %rustup-dist-server ))

(define %compression-kind
  (make-enumeration
   ;; the order here mean we prefer zstd to xz to gzip
   '(zstd xz gzip)))

(define url-fetch*
  (store-lower url-fetch))

(define* (%compression-kind->position kind)
  ((enum-set-indexer %compression-kind) kind))

;; https://static.rust-lang.org/dist/2024-01-25/cargo-nightly-armv7-unknown-linux-gnueabihf.tar.gz

(define* (%compression-kind->get index)
  (find (lambda (kind)
          (equal? (%compression-kind->position kind) index))
        (enum-set->list %compression-kind)))

(define (%compression-kind->key-prefix k)
  (unless (symbol? k)
    (error (format #f "Expecting symbol for compression-kind ~a~%" k)))
  (unless (enum-set-member? k %compression-kind)
    (error "Invalid compression kind :" k))
  (case k
    ((gzip)
     "")
    ((xz)
     "xz_")
    ((zstd)
     "zst_")))

(define (%compression-kind->file-suffix kind)
  (unless (enum-set-member? kind %compression-kind)
    (error "Invalid compression kind :" kind))
  (case kind
    ((gzip)
     ".gz")
    ((xz)
     ".xz")
    ((zstd)
     ".zst")))

;; Warning: When adding a new rustc target, try to append them to
;; the end of the list.
(define %rustc-target-triplets
  (make-enumeration
   '(all
     aarch64-apple-darwin
     aarch64-apple-ios
     aarch64-apple-ios-macabi
     aarch64-apple-ios-sim
     aarch64-apple-tvos
     aarch64-apple-tvos-sim
     aarch64-apple-visionos
     aarch64-apple-visionos-sim
     aarch64-apple-watchos
     aarch64-apple-watchos-sim
     aarch64-fuchsia
     aarch64-kmc-solid_asp3
     aarch64-linux-android
     aarch64-nintendo-switch-freestanding
     aarch64-pc-windows-gnullvm
     aarch64-pc-windows-msvc
     aarch64-unknown-freebsd
     aarch64-unknown-fuchsia
     aarch64-unknown-hermit
     aarch64-unknown-illumos
     aarch64-unknown-linux-gnu
     aarch64-unknown-linux-gnu_ilp32
     aarch64-unknown-linux-musl
     aarch64-unknown-linux-ohos
     aarch64-unknown-netbsd
     aarch64-unknown-none
     aarch64-unknown-none-softfloat
     aarch64-unknown-nto-qnx710
     aarch64-unknown-openbsd
     aarch64-unknown-redox
     aarch64-unknown-teeos
     aarch64-unknown-uefi
     aarch64-uwp-windows-msvc
     aarch64-wrs-vxworks
     aarch64_be-unknown-linux-gnu
     aarch64_be-unknown-linux-gnu_ilp32
     aarch64_be-unknown-netbsd
     arm-linux-androideabi
     arm-unknown-linux-gnueabi
     arm-unknown-linux-gnueabihf
     arm-unknown-linux-musleabi
     arm-unknown-linux-musleabihf
     arm64_32-apple-watchos
     arm64e-apple-darwin
     arm64e-apple-ios
     arm64ec-pc-windows-msvc
     armeb-unknown-linux-gnueabi
     armebv7r-none-eabi
     armebv7r-none-eabihf
     armv4t-none-eabi
     armv4t-unknown-linux-gnueabi
     armv5te-none-eabi
     armv5te-unknown-linux-gnueabi
     armv5te-unknown-linux-musleabi
     armv5te-unknown-linux-uclibceabi
     armv6-unknown-freebsd
     armv6-unknown-netbsd-eabihf
     armv6k-nintendo-3ds
     armv7-linux-androideabi
     armv7-sony-vita-newlibeabihf
     armv7-unknown-freebsd
     armv7-unknown-linux-gnueabi
     armv7-unknown-linux-gnueabihf
     armv7-unknown-linux-musleabi
     armv7-unknown-linux-musleabihf
     armv7-unknown-linux-ohos
     armv7-unknown-linux-uclibceabi
     armv7-unknown-linux-uclibceabihf
     armv7-unknown-netbsd-eabihf
     armv7-wrs-vxworks-eabihf
     armv7a-kmc-solid_asp3-eabi
     armv7a-kmc-solid_asp3-eabihf
     armv7a-none-eabi
     armv7a-none-eabihf
     armv7k-apple-watchos
     armv7r-none-eabi
     armv7r-none-eabihf
     armv7s-apple-ios
     armv8r-none-eabihf
     avr-unknown-gnu-atmega328
     bpfeb-unknown-none
     bpfel-unknown-none
     csky-unknown-linux-gnuabiv2
     csky-unknown-linux-gnuabiv2hf
     hexagon-unknown-linux-musl
     hexagon-unknown-none-elf
     i386-apple-ios
     i586-pc-nto-qnx700
     i586-pc-windows-msvc
     i586-unknown-linux-gnu
     i586-unknown-linux-musl
     i586-unknown-netbsd
     i686-apple-darwin
     i686-linux-android
     i686-pc-windows-gnu
     i686-pc-windows-gnullvm
     i686-pc-windows-msvc
     i686-unknown-freebsd
     i686-unknown-haiku
     i686-unknown-hurd-gnu
     i686-unknown-linux-gnu
     i686-unknown-linux-musl
     i686-unknown-netbsd
     i686-unknown-openbsd
     i686-unknown-uefi
     i686-uwp-windows-gnu
     i686-uwp-windows-msvc
     i686-win7-windows-msvc
     i686-wrs-vxworks
     loongarch64-unknown-linux-gnu
     loongarch64-unknown-linux-musl
     loongarch64-unknown-none
     loongarch64-unknown-none-softfloat
     m68k-unknown-linux-gnu
     mips-unknown-linux-gnu
     mips-unknown-linux-musl
     mips-unknown-linux-uclibc
     mips64-openwrt-linux-musl
     mips64-unknown-linux-gnuabi64
     mips64-unknown-linux-muslabi64
     mips64el-unknown-linux-gnuabi64
     mips64el-unknown-linux-muslabi64
     mipsel-sony-psp
     mipsel-sony-psx
     mipsel-unknown-linux-gnu
     mipsel-unknown-linux-musl
     mipsel-unknown-linux-uclibc
     mipsel-unknown-netbsd
     mipsel-unknown-none
     mipsisa32r6-unknown-linux-gnu
     mipsisa32r6el-unknown-linux-gnu
     mipsisa64r6-unknown-linux-gnuabi64
     mipsisa64r6el-unknown-linux-gnuabi64
     msp430-none-elf
     nvptx64-nvidia-cuda
     powerpc-unknown-freebsd
     powerpc-unknown-linux-gnu
     powerpc-unknown-linux-gnuspe
     powerpc-unknown-linux-musl
     powerpc-unknown-netbsd
     powerpc-unknown-openbsd
     powerpc-wrs-vxworks
     powerpc-wrs-vxworks-spe
     powerpc64-ibm-aix
     powerpc64-unknown-freebsd
     powerpc64-unknown-linux-gnu
     powerpc64-unknown-linux-musl
     powerpc64-unknown-openbsd
     powerpc64-wrs-vxworks
     powerpc64le-unknown-freebsd
     powerpc64le-unknown-linux-gnu
     powerpc64le-unknown-linux-musl
     riscv32gc-unknown-linux-gnu
     riscv32gc-unknown-linux-musl
     riscv32i-unknown-none-elf
     riscv32im-risc0-zkvm-elf
     riscv32im-unknown-none-elf
     riscv32ima-unknown-none-elf
     riscv32imac-esp-espidf
     riscv32imac-unknown-none-elf
     riscv32imac-unknown-xous-elf
     riscv32imafc-esp-espidf
     riscv32imafc-unknown-none-elf
     riscv32imc-esp-espidf
     riscv32imc-unknown-none-elf
     riscv64-linux-android
     riscv64gc-unknown-freebsd
     riscv64gc-unknown-fuchsia
     riscv64gc-unknown-hermit
     riscv64gc-unknown-linux-gnu
     riscv64gc-unknown-linux-musl
     riscv64gc-unknown-netbsd
     riscv64gc-unknown-none-elf
     riscv64gc-unknown-openbsd
     riscv64imac-unknown-none-elf
     s390x-unknown-linux-gnu
     s390x-unknown-linux-musl
     sparc-unknown-linux-gnu
     sparc-unknown-none-elf
     sparc64-unknown-linux-gnu
     sparc64-unknown-netbsd
     sparc64-unknown-openbsd
     sparcv9-sun-solaris
     thumbv4t-none-eabi
     thumbv5te-none-eabi
     thumbv6m-none-eabi
     thumbv7a-pc-windows-msvc
     thumbv7a-uwp-windows-msvc
     thumbv7em-none-eabi
     thumbv7em-none-eabihf
     thumbv7m-none-eabi
     thumbv7neon-linux-androideabi
     thumbv7neon-unknown-linux-gnueabihf
     thumbv7neon-unknown-linux-musleabihf
     thumbv8m.base-none-eabi
     thumbv8m.main-none-eabi
     thumbv8m.main-none-eabihf
     wasm32-unknown-emscripten
     wasm32-unknown-unknown
     wasm32-wasi
     wasm32-wasip1
     wasm32-wasip1-threads
     wasm32-wasip2
     wasm64-unknown-unknown
     x86_64-apple-darwin
     x86_64-apple-ios
     x86_64-apple-ios-macabi
     x86_64-apple-tvos
     x86_64-apple-watchos-sim
     x86_64-fortanix-unknown-sgx
     x86_64-fuchsia
     x86_64-linux-android
     x86_64-pc-nto-qnx710
     x86_64-pc-solaris
     x86_64-pc-windows-gnu
     x86_64-pc-windows-gnullvm
     x86_64-pc-windows-msvc
     x86_64-unikraft-linux-musl
     x86_64-unknown-dragonfly
     x86_64-unknown-freebsd
     x86_64-unknown-fuchsia
     x86_64-unknown-haiku
     x86_64-unknown-hermit
     x86_64-unknown-illumos
     x86_64-unknown-l4re-uclibc
     x86_64-unknown-linux-gnu
     x86_64-unknown-linux-gnux32
     x86_64-unknown-linux-musl
     x86_64-unknown-linux-ohos
     x86_64-unknown-netbsd
     x86_64-unknown-none
     x86_64-unknown-openbsd
     x86_64-unknown-redox
     x86_64-unknown-uefi
     x86_64-uwp-windows-gnu
     x86_64-uwp-windows-msvc
     x86_64-win7-windows-msvc
     x86_64-wrs-vxworks
     x86_64h-apple-darwin

     wasm32-wasi-preview1-threads
     aarch64-unknown-cloudabi
     armv7-apple-ios
     armv7-unknown-cloudabi-eabihf
     asmjs-unknown-emscripten
     i686-unknown-cloudabi
     x86_64-rumprun-netbsd
     x86_64-sun-solaris
     x86_64-unknown-cloudabi
     i686-unknown-redox
     wasm32-unknown-wasi
     arm64e-apple-tvos
     wasm32v1-none
     m68k-unknown-none-elf
     mips-mti-none-elf
     mipsel-mti-none-elf
     amdgcn-amd-amdhsa
     i586-unknown-redox
     loongarch32-unknown-none
     loongarch32-unknown-none-softfloat
     aarch64-unknown-managarm-mlibc
     riscv64gc-unknown-managarm-mlibc
     x86_64-unknown-managarm-mlibc
     riscv64a23-unknown-linux-gnu
     thumbv7a-none-eabi
     thumbv7a-none-eabihf
     thumbv7r-none-eabi
     thumbv7r-none-eabihf
     thumbv8r-none-eabihf
     x86_64-unknown-linux-gnuasan
     s390x-unknown-none-softfloat
     )))

(define (%rustc-target-triplets? triplet)
  (unless (enum-set-member?
           (string->symbol (format #f "~a" triplet))
           %rustc-target-triplets)
    (raise (formatted-message
            (G_ "Toolchain triplet must be one of ~a, was given: ~a")
            (list->human-readable-list (enum-set->list %rustc-target-triplets))
            triplet))))

(define* (%rustc-target-triplets->position key)
  (let ((key (cond ((equal? key "*") 'all)
                   ((string? key) (string->symbol key))
                   (#t key))))
    ((enum-set-indexer %rustc-target-triplets) key)))

(define* (%rustc-target-triplets->get index)
  (find (lambda (target)
          (equal? (%rustc-target-triplets->position target) index))
        (enum-set->list %rustc-target-triplets)))

;; When adding a new components, suffix `-preview` should be removed
;; Refer to `%toolchain-components->position`
(define %toolchain-components
  (make-enumeration
   '(cargo
     clippy
     llvm-bitcode-linker
     llvm-tools
     miri
     reproducible-artifacts
     rls
     rust
     rust-analysis
     rust-analyzer
     rust-docs
     rust-docs-json
     rust-mingw
     rust-src
     rust-std
     rustc
     rustc-codegen-cranelift
     rustc-dev
     rustc-docs
     rustfmt

     lldb
     gcc-x86_64-unknown-linux-gnu
     rustc-codegen-gcc
     )))

(define (%toolchain-components? component)
  (unless (enum-set-member?
           (string->symbol (format #f "~a" component))
           %toolchain-components)
    (raise (formatted-message
            (G_ "Toolchain component must be one of ~a, was given: ~a")
            (list->human-readable-list (enum-set->list %toolchain-components))
            component))))

(define* (%toolchain-components->position key)
  (let ((key (if (string? key)
                 (begin
                   (string->symbol (string-replace-substring key "-preview" "")))
                 key)))
    ((enum-set-indexer %toolchain-components) key)))

(define* (%toolchain-components->get index)
  (find (lambda (type)
          (equal? (%toolchain-components->position type) index))
        (enum-set->list %toolchain-components)))

(define-record-type* <channel>
  channel make-channel
  channel?
  (name          channel->name
                 (default #f))
  (date          channel->date
                 (default #f))
  (host          channel->host
                 (default #f)))

(define* (channel-str-match str)
  (regexp-exec %channel-rx (channel-str-normalize str)))

(define (channel-str? channel-str)
  (let ((match (channel-str-match (format #f "~a" channel-str))))
    (if (not match)
        (raise (formatted-message
                (G_ "Failed to match toolchain channel spec ~a, was given: ~a")
                "https://rust-lang.github.io/rustup/concepts/toolchains.html#toolchain-specification"
                channel-str))
        match)))

(define* (channel->from-str str)
  (define match (channel-str? str))
  ;; https://rust-lang.github.io/rustup/concepts/toolchains.html#toolchain-specification
  ;; stable|beta|nightly|<major.minor>|<major.minor.patch>
  (define channel (match:substring match 1))
  ;; YYYY-MM-DD
  (define date (match:substring match 4))
  ;; <target-triple>
  (define host (match:substring match 6))
  (make-channel channel date host))

(define* (download-and-compact-manifest str)
  (let ((manifest (download-manifest str)))
    (if manifest
        (let ((compacted (compact-manifest str manifest)))
          (write-manifest compacted (manifests-directory-cache-directory))
          compacted)
        #f)))

(define* (download-manifest str)
  (define c (channel->from-str str))
  (define channel (channel->name c))
  (define date (channel->date c))
  (define url-v1
    (cond
     ((and (not date) (not %staging?))
      (format #f "~a/channel-rust-~a" %rustup-dist-root channel))
     ((and date (not %staging?))
      (format #f "~a/~a/channel-rust-~a" %rustup-dist-root date channel))
     ((and (not date) %staging?)
      (format #f "~a/staging/channel-rust-~a" %rustup-dist-root channel))
     ((and date %staging?)
      (error "not a real-world case"))))
  (define url
    (format #f "~a.toml" url-v1))
  (define sha256-url (format #f "~a.sha256" url))

  (info (G_ "Downloading channel manifest checksum from '~a'...~%") sha256-url)
  (define channel-hash-file-hash
    (let* ((content (http-fetch/guarded sha256-url)))
      (if content
          (car (string-split
  	        content
	        #\ ))
          (begin
            (format #t "Failed to download manifest sha256 ~a ~a~%" sha256-url str)
            #f))))
  (info (G_ "Downloading channel manifest from '~a'...~%") url)
  (let* ((content (http-fetch/guarded url channel-hash-file-hash)))
    (if content
        (parse-toml content)
        (begin
          (format #t "Failed to download manifest toml ~a ~a~%" url str)
          #f))))

(define* (compact-manifest str #:optional manifest)
  (define c (channel->from-str str))
  (define channel (channel->name c))
  (define date (or (channel->date c) (recursive-assoc-ref
			 manifest
			 `("date"))))
  (info (G_ "Aggregating Rust toolchain component sources...~%"))
  (define* (compact-manifest data)
    (define toolchain-version
      (car (string-split
  	    (or (recursive-assoc-ref data '("pkg" "rust" "version"))
                (recursive-assoc-ref data '("pkg" "rustc" "version")))
	    #\ )))
    (define components (recursive-assoc-ref data `("pkg")))
    (define* (component-hashed-binaries component)
      (define component-name (car component))
      (define component-name-index (%toolchain-components->position component-name))
      (define* (target-hashed-binary target)
        (define* (target-prop key)
          (recursive-assoc-ref
           target
           `(,key)))
        (define compression-kind
          (find (lambda (k)
                  (let* ((prefix (%compression-kind->key-prefix k))
                         (value (target-prop (format #f "~ahash" prefix))))
                    value))
                (enum-set->list %compression-kind)))
        (define uri (and compression-kind
                         (target-prop
                          (format #f "~aurl"
                                  (%compression-kind->key-prefix compression-kind)))))
        (define hash (and compression-kind
                          (target-prop
                           (format #f "~ahash"
                                   (%compression-kind->key-prefix compression-kind)))))

        ;; rustup for rust-docs rust-docs-json use different triplet for url
        ;; eg: use rust-docs-beta-i686-apple-darwin.tar.xz for x86_64-apple-darwin
        (define uri-triplet-index (and uri (strip-uri-target-triplet-index uri)))
        (define triplet-index (%rustc-target-triplets->position (car target)))

        (unless triplet-index
          (error "Invalid triplet :" (car target)))
        (if hash
            (validate-url-pattern
             component-name-index
             triplet-index
             (%compression-kind->position compression-kind)
             (base32-from-sha256 hash)
             uri-triplet-index
             (cond ((member channel '("beta" "nightly"))
                    channel)
                   (else
                    toolchain-version))
             date
             uri)
            '()))

      (if (not component-name-index)
          (error "Failed to find component:" component-name)
          `(,component-name-index
            ,@(filter (negate null?)
                      (map (lambda (target)
                             (target-hashed-binary target))
                           (recursive-assoc-ref component '("target")))))))

    `(,toolchain-version
      ,date
      ,@(map
         component-hashed-binaries
         components)))

  (let* ((manifest (or manifest (download-manifest str))))
    (compact-manifest manifest)))

(define* (validate-url-pattern component-name-index triplet-index compression-kind-index hash uri-triplet-index channel-name date url)
  (define (uri-path url)
    (if (string=? url "")
        #f
        (if (string-contains url "cargo-dist/")
            (substring url (string-contains url "cargo-dist/"))
            (substring url (string-contains url "dist/")))))

  (let* ((constructed (car (hashed-binary-url component-name-index triplet-index compression-kind-index hash uri-triplet-index channel-name date %default-rustup-dist-root)))
         (matches (string= constructed url)))
    (if (not matches)
        ;;   (error (format #f "failed to constructed url: ~%provided   :~a~%constructed:~a~%~a~%" url constructed `(
        ;;                                                                                                          ,component-name-index
        ;;                                                                                                          ,compression-kind-index
        ;;                                                                                                          ,hash)))
        `(
          ,triplet-index
          ,compression-kind-index
          ,hash
          ,(uri-path url))
        `(
          ,triplet-index
          ,compression-kind-index
          ,hash
          ,@(if (not (equal? uri-triplet-index triplet-index))
                `(,uri-triplet-index) '())))))

(define* (strip-uri-target-triplet-index url)
  (let ((triplet (find (lambda (triplet)
                         (string-contains url (format #f "-~a." triplet)))
                       (enum-set->list %rustc-target-triplets))))
    (and triplet (%rustc-target-triplets->position triplet))))

;; uri-triplet-index url from rustup dist server sometimes using same url for different targets
;; for url from old rust version which can't be constructed using this procedure. we just save the
;; the url segment after 'dist/' in the same slot as uri-triplet-index but as string
(define* (hashed-binary-url component-name-index triplet-index compression-kind-index hash uri-triplet-index channel-name date dist-root)
  (if (string? uri-triplet-index)
      `(,(format #f "~a/~a" %rustup-dist-server uri-triplet-index) . ,hash)
      (let* ((name (%toolchain-components->get component-name-index))
         (suffix (%compression-kind->file-suffix
                  (%compression-kind->get
                   compression-kind-index)))
         (target (%rustc-target-triplets->get (or uri-triplet-index triplet-index)))
         (url (if (equal? target 'all)
                  (format #f "~a/~a/~a-~a.tar~a" dist-root date name channel-name suffix)
                  (format #f "~a/~a/~a-~a-~a.tar~a" dist-root date name channel-name target suffix)))

         )
    `(,url . ,hash))))

(define* (write-manifest data manifests-dir
                         #:optional (no-update-default-channel #t))
  (define (channel version)
    (cond ((string-contains version "beta")
           "beta")
          ((string-contains version "nightly")
           "nightly")
          (else
           "stable")))

  (let* ((version (car data))
         (date (cadr data))
         (channel (channel version))
         (filename (format #f "~a~a-~a" manifests-dir channel date)))
    (call-with-output-file filename
      (lambda (port)
        (write data port)))
    (unless (string= channel "nightly")
      (call-with-port
          (open manifests-dir O_RDONLY)
        (lambda (port)
          (when (file-exists? (format #f "~a/~a" manifests-dir version))
            (delete-file-at port version))
          (symlinkat port (basename filename) version)
          ;; (readlink version)
          )))
    (unless no-update-default-channel
      (call-with-port
          (open manifests-dir O_RDONLY)
        (lambda (port)
          (when (file-exists? (format #f "~a/~a" manifests-dir channel))
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
