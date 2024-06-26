(define-module (rustup dist))

(define channels
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
    2024-05-02))

(define aliases
  '((1.48.0 . stable-2020-11-19)
    (1.49.0 . stable-2020-12-31)
    (1.50.0 . stable-2021-02-11)
    (1.51.0 . stable-2021-03-25)
    (1.52.0 . stable-2021-05-06)
    (1.52.1 . stable-2021-05-10)
    (1.53.0 . stable-2021-06-17)
    (1.54.0 . stable-2021-07-29)
    (1.55.0 . stable-2021-09-09)
    (1.56.0 . stable-2021-10-21)
    (1.56.1 . stable-2021-11-01)
    (1.57.0 . stable-2021-12-02)
    (1.58.0 . stable-2022-01-13)
    (1.58.1 . stable-2022-01-20)
    (1.59.0 . stable-2022-02-24)
    (1.60.0 . stable-2022-04-07)
    (1.61.0 . stable-2022-05-19)
    (1.62.0 . stable-2022-06-30)
    (1.62.1 . stable-2022-07-19)
    (1.63.0 . stable-2022-08-11)
    (1.64.0 . stable-2022-09-22)
    (1.65.0 . stable-2022-11-03)
    (1.66.0 . stable-2022-12-15)
    (1.66.1 . stable-2023-01-10)
    (1.67.0 . stable-2023-01-26)
    (1.67.1 . stable-2023-02-09)
    (1.68.0 . stable-2023-03-09)
    (1.68.1 . stable-2023-03-23)
    (1.68.2 . stable-2023-03-28)
    (1.69.0 . stable-2023-04-20)
    (1.70.0 . stable-2023-06-01)
    (1.71.0 . stable-2023-07-13)
    (1.71.1 . stable-2023-08-03)
    (1.72.0 . stable-2023-08-24)
    (1.72.1 . stable-2023-09-19)
    (1.73.0 . stable-2023-10-05)
    (1.74.0 . stable-2023-11-16)
    (1.74.1 . stable-2023-12-07)
    (1.75.0 . stable-2023-12-28)
    (1.76.0 . stable-2024-02-08)
    (1.77.0 . stable-2024-03-21)
    (1.77.1 . stable-2024-03-28)
    (1.77.2 . stable-2024-04-09)
    (1.78.0 . stable-2024-05-02)
    (stable . stable-2024-05-02)))
