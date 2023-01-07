;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname esercizi_ricorsione) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
;; Funzione ricorsiva che calcola fattoriale

(define factorial
(lambda (n)
(if (= n 1)
1
(* n (factorial (- n 1))))
))

;; Funzione ricorsiva che calcola potenza

(define power
  (lambda (x y)
    (cond ((= y 0)
           1)
          ((= y 1)
           x)
          (else
           (* x (power x (- y 1)))))
    ))

;; Funzione per il complemento a 1

(define complemento-a-uno          ; val: stringa
            (lambda (seq)                    ; seq: stringa di 0/1
              (if (> (string-length seq) 1)
                  (string-append
                   (complemento (substring seq 0 1))
                   (complemento-a-uno (substring seq 1))
                   )
                  (complemento seq)
                  )
              ))

;; Funzione per invertire 0 con 1 e viceversa

(define complemento
  (lambda (seq)
    (if (string=? seq "0")
        "1"
        "0"
       )
    ))