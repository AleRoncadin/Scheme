;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es14112022) (read-case-sensitive #t) (teachpacks ((lib "drawing.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawing.ss" "installed-teachpacks")) #f)))
;; problema di fibonacci
; t = 0 : 1 coppia fertile
; t = 1 : 1 coppia ertile + 1 coppia cucciola
; t = 2 : 2 coppie fertili + 1 coppia cucciola

; t   : f coppie fertili + c coppie cucciole
; t+1 : f coppie fertili + f coppie cucciole

; f(t+1) = f(t) + c(t)
; c(t+1) = f(t)

(define coppie-cuccioli ; val intero
    (lambda (t) ; t intero non negativo
      (if (= t 0)
      (coppie-fertili (- t 1))
      )
      ))

(define coppie-fertili ; val intero
  (lambda (t) ; t intero non negativo
    (if (= t 0)
    (+ (coppie-fertili (- t 1)) (coppie-cuccioli (- t 1)))
    )
     ))

(define coppie
  (lambda (t)
    (+ (coppie-fertili t) (coppie-cuccioli t))
     ))

;; Test di primilarità

(define primo? ; val booleano
  (lambda (n) ; n > = 2 intero
    (primo? (- n 2))
    
    ))

(define ha-divisori-in? ; val booleano
  (lambda (n a b) ; n >= 2 intero, [a, b]: intervallo di interi
    (cond ((> a b)
        false)
          ((= (remainder n a) 0) ; a divide n
        true)
        (else
         (ha-divisori-in? n (+ a 1) b))
     )
    ))

(define lista-primi ; val: lista di primi in [k, n]
  (lambda (k n) ; k, n: intero
    (cond (> k n)
          null)
         ((primo? k)
          (cons k (lista-primi (+ k 1) n)))
          (else
           (lista-primi (+ k 1) n))
    ))