;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab9) (read-case-sensitive #t) (teachpacks ((lib "drawing.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawing.ss" "installed-teachpacks")) #f)))
(define enc
  (lambda (msg rule) ; msg: stringa composta dai caratteri dell'alfabeto latino che; rule: è rappresentata da una procedura, come rot-3...
    (if (string=? msg "")
        ""
        (string-append 
         (string (rule (string-ref msg 0)))
         (enc (substring msg 1) rule)
         ))
    ))

(define msg-latino "ABCDEFGHILMNOPQRSTVX")
(define array-latino (list #\A #\B #\C #\D #\E #\F #\G #\H #\I #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\V #\X))

(define rot-3 ; val lettera maiuscola. La procedura manda avanti di 3 lettere
  (lambda (c)
    (let (
          (k (+ (char->integer array-latino) 3))
          )
      (if (<= k pos-X )
          (integer->char k)
          (integer->char (- k 20))
          ))
    ))

(define rot-3
  (lambda (c)
    (let (k (
    ))

(define msg-normale "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

; (string-ref "Apple" 0) data la posizione, ritorna il carattere presente in quella posizione, in questo caso #\A

; (list-ref array-latino 19) list-ref: data la posizione, ritorna il carattere

(define pos-X 19)
(define pos-A 0)