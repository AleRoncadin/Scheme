#lang racket

(define latino "ABCDEFGHILMNOPQRSTVX")

(define char->pos  ; val: intero
  (lambda (c a)    ; c: carattere, a: stringa
    (if (char=? (string-ref a 0) c)
      0
      (+ (char->pos c (substring a 1)) 1)
      )   
    ))

(define remove-spaces  ; val: stringa
  (lambda (msg)        ; msg: stringa
    (cond
      ((= (string-length msg) 0) "")
      ((string=? (substring msg 0 1) " ") (remove-spaces (substring msg 1)))
      (else (string-append (substring msg 0 1) (remove-spaces (substring msg 1))))
      )
    ))


(define enc           ; val: stringa
  (lambda (msg rule)  ; msg: stringa [A, B, ..., Z], rule: procedura [char -> char]
    (let ((k (remove-spaces msg)))
      (if (string=? k "")
          ""
          (string-append
           (string (rule (string-ref k 0)))
           (enc (substring k 1) rule)
          ))
      )
    ))

(define alfa-rot   ; val: lettera maiuscola
  (lambda (R A)    ; R: intero, A: stringa (alfabeto)  
    (lambda (c)    ; c: lettera maiuscola
      (let (
            (k (+ (char->pos c A) R))
            )
        (if (<= k (- (string-length A) 1))
            (string-ref A k)
            (string-ref A (- k (string-length A)))
        )
      )
    )   
  )
)

(enc "ALEA IACTA EST IVLIVS CAESAR DIXIT" (alfa-rot 3 latino))