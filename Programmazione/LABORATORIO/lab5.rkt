#lang racket

(define paths2d
    (lambda (right down)
        (if (or (= right 0) (= down 0))
            1
            (+ (paths2d (- right 1) down) (paths2d right (- down 1)))
        )
    )
)

(define paths3d
    (lambda (x y z)
        (cond ((= x 0)
                    (paths2d y z))
               ((= y 0)
                    (paths2d x z))
                ((= z 0)
                    (paths2d x y)) 
                (else
                    (+ (paths3d (- x 1) y z) (paths3d x (- y 1) z) (paths3d x y (- z 1))))
        )
    )
)

(paths3d 1 3 4)
(paths3d 0 0 7)
(paths3d 2 0 2)
(paths3d 1 1 1)
(paths3d 1 1 5)
(paths3d 2 3 1)
(paths3d 2 3 3)