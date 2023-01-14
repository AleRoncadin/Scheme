;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab_24-11) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define belong?  ; val: booleano
  (lambda (k ls) ; ls: lista; k: intero
    (cond
      ((null? ls) #false)
      ((= (car ls) k) #true)
      (else (belong? k (cdr ls)))
      )
    ))

; se il carattere non appartiene alla lista, la procedura ritorna la lunghezza della lista
(define position  ; val: intero positivo
  (lambda (k ls)  ; ls: lista; k: intero
    (if (or (null? ls) (= (car ls) k))
      0
      (+ (position k (cdr ls)) 1)
      )
    ))

(define sorted-ins  ; val: lista
  (lambda (k ls)    ; k: intero; ls: lista
    (cond
      ((belong? k ls) ls)
      ((or (null? ls) (< k (car ls))) (cons k ls))
      (else (cons (car ls) (sorted-ins k (cdr ls))))
      )
    ))

(define sorted-list
  (lambda (ls)
    (if (null? ls)
        null
        (sorted-ins (car ls) (sorted-list (cdr ls)))
        )
    ))

(sorted-list '(35 8 41 24 7))

