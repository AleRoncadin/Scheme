#lang racket

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

(define rot-3 ; val lettera maiuscola
  (lambda (c) ; c è un carattere lettera maiuscola
    (let (
          (k (+ (char->integer c) 3))
          )
      (if (<= k pos-Z )
          (integer->char k)
          (integer->char (- k 26))
          ))
    ))

(define pos-Z (char->integer #\Z))
(define pos-A (char->integer #\A))

; (enc "ALEAIACTAEST" rot-3)

;;Valori procedurali

; Ci sono 2 lambda uno dentro l'altra
(define rot   ;val lettera maiuscola
  (lambda (R) ;R è un intero
    
  (lambda (c) ; c è un carattere lettera maiuscola
    (let (
          (k (+ (char->integer c) R))
          )
      (if (<= k pos-Z )
          (integer->char k)
          (integer->char (- k 26))
          ))
    )))

; (enc "ALEAIACTAEST" (rot 3))


(define dec        ;val procedura
  (lambda (rule)   ;rule procedura
    (let (
          (R (- (char->integer (rule #\A)) pos-A))
          )
      (rot (- 26 R))
      )
    ))

; (enc "IULIUSCAESAR" (rot 3))

(define inv       ; val procedura
  (lambda (rule)  ; rule procedura
    (lambda (c)   ; c carattere
      (find pos-A c rule)
      )
    ))

(define find           ; val carattere
  (lambda (x c rule)   ; x intero (posizione), c carattere, rule procedura [char -> char]
    (if (char=? (rule (integer->char x)) c)
        (integer->char x)
        (find (+ x 1) c rule)
        )
      ))

; (enc "ABC" rot-3)
; (enc "DEF" (inv rot-3))