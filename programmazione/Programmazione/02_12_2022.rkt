;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 02_12_2022) (read-case-sensitive #t) (teachpacks ((lib "drawing.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawing.ss" "installed-teachpacks")) #f)))
;; argomenti procedurali

(define enc
  (lambda (msg rule) ; msg: stringa composta dai caratteri dell'alfabeto inglese maiuscolo , rule: è rappresentata da una procedura
    (if (string=? msg "")
        ""
        (string-append 
         (string (rule (string-ref msg 0)))
         (enc (substring msg 1) rule)
         ))
    ))

;; (enc "ALEAIACTAEST" (lambda (c) c)) ritorna lo stesso carattere in questo modo

;; (enc "ALEAIACTAEST" char-downcase) mette in minuscolo
;; (enc (enc "ALEAIACTAEST" char-downcase) char-upcase) rimane uguale

;; Regola di Cesare


