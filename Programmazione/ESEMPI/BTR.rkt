#lang racket

(define btr-val ; val intero
    (lambda (btr) ; stringa di -/./+
        (let (
            (k (- (string-length btr) 1))
            )
            (if (= k 0)
                (btd-val btr)
                (+ (* 3 (btr-val (substring btr 0 k)))
                    (btd-val (substring btr k)))
            )    
        )
    )
)

(define btd-val ; val -1, 0, +1
    (lambda (btd) ; btd "-" "." "+"
        (cond ((string=? btd "-")
                -1)
                ((string=? btd ".")
                0)  
                ((string=? btd "+")
                +1) 
        )
    )
)

(define btr-rep ; dato un intero ritorno una stringa di -/./+
    (lambda (n)
        (let (
                (q (quotient n 3))
                (r (remainder n 3))
            )
            (cond ((<= (abs n) 1)
                    (btd-rep n))
                    ((= r -2)
                        ())
                    ((= r 2)
                        ())
                    (else
                        (string-append
                            (btr-rep q) (btd-rep r)
                        )
                    )
            )

        )
    )
)