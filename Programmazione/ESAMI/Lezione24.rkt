;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lezione24) (read-case-sensitive #t) (teachpacks ((lib "drawing.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawing.ss" "installed-teachpacks")) #f)))
; es. 7 date due liste ordinate di numeri interi positivi, restituire lista ordinata degli elementi comuni
; confontare gli elementi

(define shared
  (lambda (u v)
    (cond ((or (null? u) (null? v))
           null)
          ((= (car u) (car v))
           (cons (car u) (shared (cdr u) (cdr v)))) ; cons inserisce elementi nella lista
          ((< (car u) (car v))
           (shared (cdr u) v))
          (else
           (shared u (cdr v)))
     )))





; es. 2

(define offset (char->integer #\0))

(define last-digit
  (lambda (base) (integer->char (+ (- base 1) offset)) ))

(define next-digit
  (lambda (dgt) (string (integer->char (+ (char->integer dgt) 1))) ))

(define increment
  (lambda (num base) ; 2 <= base <= 10
    (let ((digits (string-length num)))
      (if (= digits 0)
          "1"
          (let ((dgt (string-ref num (- digits 1))))
            (if (char=? dgt (last-digit base))
                (string-append (increment (substring num 0 (- digits 1)) base)
                 "0")
                (string-append (substring num 0 (- digits 1)) (next-digit dgt) )
             ))
))))





; es. 4