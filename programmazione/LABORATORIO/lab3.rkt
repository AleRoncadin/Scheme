#lang racket

(define calcolaIntera
(lambda (n)
  (if (= (string-length n) 0)
    0
    (+ (* (string->number (substring n 0 1)) 
          (expt 2 (- (string-length n) 1)))
       (calcolaIntera (substring n 1 (string-length n)))  
    )
  )
))

(define parteIntera
(lambda (n)
  (if (= (string-length n) 0)
    ""
    (if (string=? (substring n 0 1) ".")
      ""
      (string-append (substring n 0 1) (parteIntera (substring n 1)))
    )
  )
))

(define calcolaDecimale
(lambda (s)
  (if (= (string-length s) 0)
    0
    (+ (* (string->number (substring s (-(string-length s) 1))) 
          (expt 2 (- 0 (string-length s))))
       (calcolaDecimale
          (substring s 0 (- (string-length s) 1) ))
    )
  )
))

(define parteDecimale
(lambda (n s)
  (if (= (string-length n) (string-length s))        ; se le due stringhe NON hanno lunghezza uguale
        "0"
        (substring n 
                (+ (string-length (parteIntera n)) 1) ; somma 1 altrimenti viene preso pure il punto
        )
  )
))

(define bin-rep->number
(lambda (n)
  (cond ((string=? (substring n 0 1) "-")
          (string-append "-" 
            (number->string (exact->inexact
              (+ (calcolaIntera (parteIntera (substring n 1)))
                 (calcolaDecimale (parteDecimale (substring n 1) (parteIntera (substring n 1))))
              )
            ))
          )
        )
        ((string=? (substring n 0 1) "+")
          (string-append "+" 
            (number->string (exact->inexact
              (+ (calcolaIntera (parteIntera (substring n 1)))
                 (calcolaDecimale (parteDecimale (substring n 1) (parteIntera (substring n 1))))
              )
            ))
          )
        )
        (else
          (exact->inexact
            (+ (calcolaIntera (parteIntera (substring n 1)))
                (calcolaDecimale (parteDecimale (substring n 1) (parteIntera (substring n 1))))
            )
          )
        )
  )

))

; la primitiva exact->inexact esprime il risultato in decimale al posto della frazione
; se non la metti, ad esempio 0.25 te lo scrive come 1/4

(bin-rep->number "-11.101")
(bin-rep->number "+1101")
(bin-rep->number "0")
(bin-rep->number "10110.011")
(bin-rep->number "-0.1101001")