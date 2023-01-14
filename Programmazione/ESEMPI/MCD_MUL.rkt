;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es18112022) (read-case-sensitive #t) (teachpacks ((lib "drawing.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawing.ss" "installed-teachpacks")) #f)))
;; moltiplicazione

(define mul ;; val intero positivo
  (lambda (m n) ; m e n interi non negativi
    (cond ((= n 0)
           0)
          ((even? n)                     ;;even? per vedere se è pari
           (mul (* 2 m) (quotient n 2)))
          (else
            (+ m(mul (* 2 m) (quotient n 2))))

    )))

;; massimo comun divisore

(define mcd ;; val intero positivo
  (lambda (x y) ;; x e y interi positivi
    (cond ((= x y)
           x)
          ((< x y)
           (mcd x (- y x)))
          (else
           (mcd (- x y) y)))
    ))
        
(define mul-2
  (lambda (m n)
    (mul-tr m n 0)
    ))

(define mul-tr
  (lambda (m n p)
    (cond ((= n 0)
           p)
          ((even? n)
           (mul-tr (* 2 m) (quotient n 2) p))
          (else
           (mul-tr (* 2 m) (quotient n 2) (+ m p))))
    ))

;;ricorsive di coda: l'ultima condizione possibile quella mul-tr è una ric. di coda



(mcd 60 18)