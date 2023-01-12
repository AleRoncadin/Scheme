;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |4 Febbraio 2020|) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
; Es. 2

(define standard-form           ; val: lista
  (lambda (x)
    (if (null? x)
        null                    ; se la lista è vuota, stampa una lista vuota
        (map cambia x)          ; applico la funzione cambia a tutti gli elementi della lista
        )
    ))

; map applica la funzione cambia ad ogni elemento della lista

(define cambia            ; converte il primo carattere in uppercase
  (lambda (x)
    (string-append (string-upcase (substring x 0 1)) (substring x 1))
    ))


; Es. 3

(define btr-val ; val: intero
 (lambda (btr) ; btr: stringa di – / . / +
    (let ((k (string-length btr))
            )
        (if (= k 0)
            0
            (let ((p (substring btr 0 (- k 1)))
                    (t (string-ref btr (- k 1)))
                    )
                (+ (* 3 (btr-val p)) (btd-val t))
                )))
))

(define btd-val
 (lambda (t)
    (cond ((char=? t #\-) -1)
        ((char=? t #\.) 0)
        ((char=? t #\+) +1)
    )
))

(btr-val "--+.-")


 (define btr-val-tr ; val: intero
    (lambda (btr) ; btr: stringa di – / . / +
    (btr-val-rec btr 0)
 ))

 (define btr-val-rec
    (lambda (btr x)
        (let ((k (string-length btr))
            )
            (if (= k 0)
                x
            (let ((q (substring btr 1))
                (t (string-ref btr 0))
                )
            (btr-val-rec q (+ x (* (btd-val t) (expt 3 (- k 1)))))
            )))
))

(btr-val-tr "--+.-")
