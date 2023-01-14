#lang racket

(define unknown
    (lambda (x)
        (if (= x 0)
            0
            (+ (unknown (- x 1)) (odd x))
        )
    )
)

(define odd
    (lambda (i)
        (if (= i 1)
            1
            (+ (odd (- i 1)) 2)
        )
    )
)