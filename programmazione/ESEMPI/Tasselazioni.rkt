#lang racket

;; Tassellazioni

(define tass-qr ; tasselazioni-quadrato_rettangolo
  (lambda (n)   ; n intero positivo
    (cond ((= n 1) 1)
          ((= n 2) 2)
          (else
           (+ (tass-qr (- n 2)) (tass-qr (- n 1)))
           )
      )
   )
)

(define tass-rb ; tasselazioni-rosso_blu
  (lambda (n)   ; n intero positivo
    (cond ((= n 1) 2)
          ((= n 2) 3)
          (else
           (+ (tass-rb (- n 2)) (tass-rb (- n 1)))
           )
      )
   )
)

(tass-qr 10)
(tass-rb 12)