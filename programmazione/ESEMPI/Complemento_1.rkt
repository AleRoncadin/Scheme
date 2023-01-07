;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname esempio_ricorsione) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Complemento a uno

; seq: stringa di 0/1

; seq --> (substring seq 0 1), (substring seq 1)


(define compl-a-uno   ; val: stringa di 0/1
(lambda (seq)         ; seq: stringa di 0/1
  (if (> (string-length seq) 1)
  (string-append
    (bit-compl (substring seq 0 1))
    (compl-a-uno (substring seq 1))
    )
   (bit-compl seq)    ; se c'è un solo bit
   )
))

(define bit-compl     ; val: "0", "1"
  (lambda (bit)       ; bit: "0", "1"
    (if (string=? bit "0")
        "1"
        "0")
    ))

(define compl1     ; val: stringa di 0/1
  (lambda (seq)    ; seq: stringa di 0/1 non vuota

   (if (>= (string-length seq) 2)
       (let ((k (quotient (string-length seq) 2))) ; associa a k quell'espressione
       (string-append
        (compl1 (substring seq 0 k))
        (compl1 (substring seq k))
       ))
       ; se scrivo k qui non la conosce perché è al di fuori del let
       (bit-compl seq)
   )
 ))