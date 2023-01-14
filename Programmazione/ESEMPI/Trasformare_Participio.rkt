#lang racket

(define participioInfinito
    (lambda (verbo)         ;val stringa
        (cond 
            ((string=? (substring verbo (- (string-length verbo) 3)) "are")
                (string-append (substring verbo 0 (- (string-length verbo) 3)) "ato"))
            
            ((string=? (substring verbo (- (string-length verbo) 3)) "ere")
                (string-append (substring verbo 0 (- (string-length verbo) 3)) "uto"))

            (else
                (string-append (substring verbo 0 (- (string-length verbo) 3)) "ito"))
            
        )
    )
)