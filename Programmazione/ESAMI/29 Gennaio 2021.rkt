#lang racket

; ES. 1 B

(define pair
(lambda (x y)
    (cond ((char=? x y)
            (cons x x))
        ((char<? x y)
            (cons x y))
        ((char>? x y)
        (cons y x))
    )
))

(define pair-list
(lambda (x y)
    (if (or (null? x) (null? y))
        '()
        (map pair x y)
    )
))

(pair #\q #\n)
(pair #\c #\x)
(pair #\t #\t)
(pair-list '(#\q #\c #\t) '(#\n #\x #\t))
(pair-list '(#\1 #\3 #\5) '(#\4 #\2 #\0)) 

; --------------------------------------------------------------------------------

; ES. 2 B

 (define lcs-align ; val: coppia intero/stringa
 (lambda (u v) ; u, v: stringhe
    (let ((m (string-length u)) (n (string-length v))
        )
        (cond ((= n 0) (list 0 ""))
                ((= m 0)
                (let ((w (lcs-align u (substring v 1))))
                    (list 0 (string-append "_" (cadr w)))
                ))
                ((char=? (string-ref u 0) (string-ref v 0))
                (let ((dx )
                    )
                (list (+ (car dx) 1) (string-append (substring v 0 1) (cadr dx))
                    )))
                (else
                    (let ((du (lcs-align (substring u 1) v))
                        (dv )
                        )
                    (if (< (car du) (car dv))
                        (list (car dv) (string-append "_" (cadr dv)))

                        )))
 ))))