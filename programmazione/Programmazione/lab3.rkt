;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab3) (read-case-sensitive #t) (teachpacks ((lib "drawing.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawing.ss" "installed-teachpacks")) #f)))
;; (bin-rep->number "-101010101")

(define bin-rep->number
  (lambda (numero)
    ()
    (string-append (negativo_positivo? numero) "")
    ))

(define negativo_positivo?
(lambda (numero)
 (let ((segno (substring numero 0 1))) ;;(segno (substring numero 0 1))
    (if (equal? segno "-")
        "-"
        "+"))
  ))

(define
(lambda ()



 ))