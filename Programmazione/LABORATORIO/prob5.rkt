#lang scheme

(provide paths3d)

(define (paths2d i j)
; returns: int
; i j: int > 0
    (if (or (= i 0) (= j 0))
        1
        (+ (paths2d i (- j 1)) (paths2d (- i 1) j))
    )
)

(define (paths3d x y z)
    (cond 
        ((= x 0) (paths2d y z))
        ((= y 0) (paths2d x z))
        ((= z 0) (paths2d x y))
        (else (+ (paths3d (- x 1) y z) (+ (paths3d x (- y 1) z) (paths3d x y (- z 1)))))
    )
)