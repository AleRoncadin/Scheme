(define belong?
  (lambda (x lista)
    (if (equal? (null? lista) false)
        (if (= (car lista) x )
            true
            (belong? x (cdr lista))
         )

    false
   )     
))


(define position
  (lambda (x lista)
(let ((lun (- (length lista) 1)))

    (if (equal? (null? lista) false)
       (if (= (car (reverse lista)) x )
            lun
            (position x (reverse(cdr (reverse lista))))
         )

    0
   ) 
   
)


))


(define sorted-ins
  (lambda (x l)
    (if (null? l)
        (cons x null)
        (if (< x (car l))
            (cons x l)
            (cons (car l) (sorted-ins x (cdr l)))
            )
        )
    ))

(define sorted-list
  (lambda (l)
    (cond ((null? l) l)
          (else (
                 sorted-ins (car l) (sorted-list (cdr l))
                )
                )
          )
    ))
 



(sorted-list '(8 5 3 1))
