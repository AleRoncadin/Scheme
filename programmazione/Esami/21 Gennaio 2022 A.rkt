#lang racket

; ESERCIZIO 1

(define cyclic-pattern
    (lambda (str k)
        (cond ((or (string=? str "") (= k 0))
                "")
                ((= (string-length str) k)
                str)
                ((< (string-length str) k)
                "")
                (else
                (uguali str k))
        )


))

(define uguali
    (lambda (str k)
    (cond ((string=? str "")
            "")
          ((string=? (substring str k (+ k k)) (substring str 0 k))
                (substring str 0 k)
                (uguali (substring str k (+ k k))))
            (else
            "")
    )        
))

;(cyclic-pattern "" 3)
;(cyclic-pattern "abc" 3)
;(cyclic-pattern "abcabcab" 3) 
;(cyclic-pattern "abcabcabc" 3)
;(cyclic-pattern "abc" 4)

; ESERCIZIO 2

(define tess-1x-2
    (lambda (n)
      (cond ((= n 0)
                0)
                ((= n 1)
                1)
                ((= n 2)
                1)
                ((> n 2)
                (+ (tess-1x-2 (- n 1)) (tess-1x-2 (- n 2))))
      )  
))

(tess-1x-2 5)