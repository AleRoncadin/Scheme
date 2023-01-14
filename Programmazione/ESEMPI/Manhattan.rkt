


#lang racket

;; Tipico esercizio di ricorsione ad albero

;; Percorsi di Manhattan

;        A     A'
;          .---.---.---.---.---.---.---.
;          |   |   |   |   |   |   |   |
;       A" .---.---.---.---.---.---.---.
;          |   |   |   |   |   |   |   |
;          .---.---.---.---.---.---.---.
;          |   |   |   |   |   |   |   |
;          .---.---.---.---.---.---.---.
;          |   |   |   |   |   |   |   |
;          .---.---.---.---.---.---.---.
;          |   |   |   |   |   |   |   |
;          .---.---.---.---.---.---.---.
;                                        B

; IN QUANTI MODI POSSO SPOSTARMI DAL PUNTO A AL PUNTO B (non devo tornare indietro)?

(define paths        ; ci aspettiamo un intero positivo
  (lambda (down right)      ; i e j sono interi non negativi
    (if (or (= down 0) (= right 0))
        1
        (+ (paths down (- right 1)) (paths (- down 1) right))
     )
   )
 )

(paths 5 7)