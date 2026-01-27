(define-module (rustup packages rust)
  #:use-module (guix packages)
  #:use-module (rustup build toolchain))

;; Note: Rust versions 1.1.0 through 1.47.0 have been removed because their
;; manifests use stale triplet indices that don't match the current
;; %rustc-target-triplets enumeration.

(define-public rust-toolchain-1.48.0
  (rustup "1.48.0"))

(define-public rust-bin-1.48.0
  (deprecated-package "rust-bin" rust-toolchain-1.48.0))

(define-public rust-toolchain-1.49.0
  (rustup "1.49.0"))

(define-public rust-bin-1.49.0
  (deprecated-package "rust-bin" rust-toolchain-1.49.0))

(define-public rust-toolchain-1.50.0
  (rustup "1.50.0"))

(define-public rust-bin-1.50.0
  (deprecated-package "rust-bin" rust-toolchain-1.50.0))

(define-public rust-toolchain-1.51.0
  (rustup "1.51.0"))

(define-public rust-bin-1.51.0
  (deprecated-package "rust-bin" rust-toolchain-1.51.0))

(define-public rust-toolchain-1.52.0
  (rustup "1.52.0"))

(define-public rust-bin-1.52.0
  (deprecated-package "rust-bin" rust-toolchain-1.52.0))

(define-public rust-toolchain-1.52.1
  (rustup "1.52.1"))

(define-public rust-bin-1.52.1
  (deprecated-package "rust-bin" rust-toolchain-1.52.1))

(define-public rust-toolchain-1.53.0
  (rustup "1.53.0"))

(define-public rust-bin-1.53.0
  (deprecated-package "rust-bin" rust-toolchain-1.53.0))

(define-public rust-toolchain-1.54.0
  (rustup "1.54.0"))

(define-public rust-bin-1.54.0
  (deprecated-package "rust-bin" rust-toolchain-1.54.0))

(define-public rust-toolchain-1.55.0
  (rustup "1.55.0"))

(define-public rust-bin-1.55.0
  (deprecated-package "rust-bin" rust-toolchain-1.55.0))

(define-public rust-toolchain-1.56.0
  (rustup "1.56.0"))

(define-public rust-bin-1.56.0
  (deprecated-package "rust-bin" rust-toolchain-1.56.0))

(define-public rust-toolchain-1.56.1
  (rustup "1.56.1"))

(define-public rust-bin-1.56.1
  (deprecated-package "rust-bin" rust-toolchain-1.56.1))

(define-public rust-toolchain-1.57.0
  (rustup "1.57.0"))

(define-public rust-bin-1.57.0
  (deprecated-package "rust-bin" rust-toolchain-1.57.0))

(define-public rust-toolchain-1.58.0
  (rustup "1.58.0"))

(define-public rust-bin-1.58.0
  (deprecated-package "rust-bin" rust-toolchain-1.58.0))

(define-public rust-toolchain-1.58.1
  (rustup "1.58.1"))

(define-public rust-bin-1.58.1
  (deprecated-package "rust-bin" rust-toolchain-1.58.1))

(define-public rust-toolchain-1.59.0
  (rustup "1.59.0"))

(define-public rust-bin-1.59.0
  (deprecated-package "rust-bin" rust-toolchain-1.59.0))

(define-public rust-toolchain-1.60.0
  (rustup "1.60.0"))

(define-public rust-bin-1.60.0
  (deprecated-package "rust-bin" rust-toolchain-1.60.0))

(define-public rust-toolchain-1.61.0
  (rustup "1.61.0"))

(define-public rust-bin-1.61.0
  (deprecated-package "rust-bin" rust-toolchain-1.61.0))

(define-public rust-toolchain-1.62.0
  (rustup "1.62.0"))

(define-public rust-bin-1.62.0
  (deprecated-package "rust-bin" rust-toolchain-1.62.0))

(define-public rust-toolchain-1.62.1
  (rustup "1.62.1"))

(define-public rust-bin-1.62.1
  (deprecated-package "rust-bin" rust-toolchain-1.62.1))

(define-public rust-toolchain-1.63.0
  (rustup "1.63.0"))

(define-public rust-bin-1.63.0
  (deprecated-package "rust-bin" rust-toolchain-1.63.0))

(define-public rust-toolchain-1.64.0
  (rustup "1.64.0"))

(define-public rust-bin-1.64.0
  (deprecated-package "rust-bin" rust-toolchain-1.64.0))

(define-public rust-toolchain-1.65.0
  (rustup "1.65.0"))

(define-public rust-bin-1.65.0
  (deprecated-package "rust-bin" rust-toolchain-1.65.0))

(define-public rust-toolchain-1.66.0
  (rustup "1.66.0"))

(define-public rust-bin-1.66.0
  (deprecated-package "rust-bin" rust-toolchain-1.66.0))

(define-public rust-toolchain-1.66.1
  (rustup "1.66.1"))

(define-public rust-bin-1.66.1
  (deprecated-package "rust-bin" rust-toolchain-1.66.1))

(define-public rust-toolchain-1.67.0
  (rustup "1.67.0"))

(define-public rust-bin-1.67.0
  (deprecated-package "rust-bin" rust-toolchain-1.67.0))

(define-public rust-toolchain-1.67.1
  (rustup "1.67.1"))

(define-public rust-bin-1.67.1
  (deprecated-package "rust-bin" rust-toolchain-1.67.1))

(define-public rust-toolchain-1.68.0
  (rustup "1.68.0"))

(define-public rust-bin-1.68.0
  (deprecated-package "rust-bin" rust-toolchain-1.68.0))

(define-public rust-toolchain-1.68.1
  (rustup "1.68.1"))

(define-public rust-bin-1.68.1
  (deprecated-package "rust-bin" rust-toolchain-1.68.1))

(define-public rust-toolchain-1.68.2
  (rustup "1.68.2"))

(define-public rust-bin-1.68.2
  (deprecated-package "rust-bin" rust-toolchain-1.68.2))

(define-public rust-toolchain-1.69.0
  (rustup "1.69.0"))

(define-public rust-bin-1.69.0
  (deprecated-package "rust-bin" rust-toolchain-1.69.0))

(define-public rust-toolchain-1.70.0
  (rustup "1.70.0"))

(define-public rust-bin-1.70.0
  (deprecated-package "rust-bin" rust-toolchain-1.70.0))

(define-public rust-toolchain-1.71.0
  (rustup "1.71.0"))

(define-public rust-bin-1.71.0
  (deprecated-package "rust-bin" rust-toolchain-1.71.0))

(define-public rust-toolchain-1.71.1
  (rustup "1.71.1"))

(define-public rust-bin-1.71.1
  (deprecated-package "rust-bin" rust-toolchain-1.71.1))

(define-public rust-toolchain-1.72.0
  (rustup "1.72.0"))

(define-public rust-bin-1.72.0
  (deprecated-package "rust-bin" rust-toolchain-1.72.0))

(define-public rust-toolchain-1.72.1
  (rustup "1.72.1"))

(define-public rust-bin-1.72.1
  (deprecated-package "rust-bin" rust-toolchain-1.72.1))

(define-public rust-toolchain-1.73.0
  (rustup "1.73.0"))

(define-public rust-bin-1.73.0
  (deprecated-package "rust-bin" rust-toolchain-1.73.0))

(define-public rust-toolchain-1.74.0
  (rustup "1.74.0"))

(define-public rust-bin-1.74.0
  (deprecated-package "rust-bin" rust-toolchain-1.74.0))

(define-public rust-toolchain-1.74.1
  (rustup "1.74.1"))

(define-public rust-bin-1.74.1
  (deprecated-package "rust-bin" rust-toolchain-1.74.1))

(define-public rust-toolchain-1.75.0
  (rustup "1.75.0"))

(define-public rust-bin-1.75.0
  (deprecated-package "rust-bin" rust-toolchain-1.75.0))

(define-public rust-toolchain-1.76.0
  (rustup "1.76.0"))

(define-public rust-bin-1.76.0
  (deprecated-package "rust-bin" rust-toolchain-1.76.0))

(define-public rust-toolchain-1.77.0
  (rustup "1.77.0"))

(define-public rust-bin-1.77.0
  (deprecated-package "rust-bin" rust-toolchain-1.77.0))

(define-public rust-toolchain-1.77.1
  (rustup "1.77.1"))

(define-public rust-bin-1.77.1
  (deprecated-package "rust-bin" rust-toolchain-1.77.1))

(define-public rust-toolchain-1.77.2
  (rustup "1.77.2"))

(define-public rust-bin-1.77.2
  (deprecated-package "rust-bin" rust-toolchain-1.77.2))

(define-public rust-toolchain-1.78.0
  (rustup "1.78.0"))

(define-public rust-bin-1.78.0
  (deprecated-package "rust-bin" rust-toolchain-1.78.0))

(define-public rust-toolchain-1.79.0
  (rustup "1.79.0"))

(define-public rust-toolchain-1.80.0
  (rustup "1.80.0"))

(define-public rust-toolchain-1.81.0
  (rustup "1.81.0"))

(define-public rust-toolchain-1.82.0
  (rustup "1.82.0"))

(define-public rust-toolchain-1.83.0
  (rustup "1.83.0"))

(define-public rust-toolchain-1.84.0
  (rustup "1.84.0"))

(define-public rust-toolchain-1.85.0
  (rustup "1.85.0"))

(define-public rust-toolchain-1.85.1
  (rustup "1.85.1"))

(define-public rust-toolchain-1.86.0
  (rustup "1.86.0"))

(define-public rust-toolchain-1.87.0
  (rustup "1.87.0"))

(define-public rust-toolchain-1.88.0
  (rustup "1.88.0"))

(define-public rust-toolchain-1.89.0
  (rustup "1.89.0"))

(define-public rust-toolchain-1.90.0
  (rustup "1.90.0"))

(define-public rust-toolchain-1.91.0
  (rustup "1.91.0"))

(define-public rust-toolchain-1.92.0
  (rustup "1.92.0"))

(define-public rust-toolchain-1.93.0
  (rustup "1.93.0"))

(define-public rust-toolchain rust-toolchain-1.93.0)

(define-public rust-bin
  (deprecated-package "rust-bin" rust-toolchain))
