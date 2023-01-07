;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname AllinareSequenze) (read-case-sensitive #t) (teachpacks ((lib "drawing.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawing.ss" "installed-teachpacks")) #f)))
(define llcs
  (lambda (u v)
    (cond ((or (string=? u "") (string=? u ""))
           0)
          ((char=? (string-ref u 0) (string-ref v 0))
           (+ 1 (llcs (substring u 1) (substring v 1))))
          (else
           (max (llcs u (substring v 1))
                (llcs (substring u 1) v)
                ))
     )
    ))

(define lcs
  (lambda (u v)
    (cond ((or (string=? u "") (string=? u ""))
           "")
          ((char=? (string-ref u 0) (string-ref v 0))
           (string-append
            (substring u 0 1)
            (lcs (substring u 1) (substring v 1))
            ))
          (else
           (longer (lcs u (substring v 1))
                (lcs (substring u 1) v)
                ))
     )
    ))

(define longer
  (lambda (u v)
    (let ((m (string-length u))
          (n (string-length v))
          )
      (cond ((< m n)
             v)
            ((> m n)
             u)
            ((= (random 2) 0)
             v)
            (else
             u)
       ))
))