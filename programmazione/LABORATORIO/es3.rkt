;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ConvertitoreBinarioEinAltreBasiFunzionante) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))

(define intera
  (lambda (n)
   (if (>= (string-length n) 1) 
    (+ (* (string->number(substring n 0 1)) (expt 2 (-(string-length n) 1))) (intera(substring n 1 (string-length n) )) )

    0)
    ))

(define decimale
  (lambda (s)
    
  (if (>= (string-length s) 1)
     
    (+ (* (string->number(substring s (-(string-length s) 1))) (expt 2 (- 0 (string-length s)))) (decimale (substring s 0 (- (string-length s) 1) )) )
    0)
    ))

(define bin-rep->number 
  (lambda (n)
    (cond ((string=? (substring n 0 1) "-")
         (string-append "-" (number->string (exact->inexact (+ (intera (interaSenzaPunto (substring n 1 (string-length n) ))) (decimale (decimaleSenzaPunto (substring n 1 (string-length n) ) (interaSenzaPunto (substring n 1 (string-length n) )))))))))
          ((string=? (substring n 0 1) "+")
          (string-append "+" (number->string (exact->inexact (+ (intera (interaSenzaPunto (substring n 1 (string-length n) ))) (decimale (decimaleSenzaPunto (substring n 1 (string-length n) ) (interaSenzaPunto (substring n 1 (string-length n) )))))))))
          (else
          (+ (intera (interaSenzaPunto n)) (decimale (decimaleSenzaPunto n (interaSenzaPunto n )))))
        )  
    ;(if (string=? (substring n 0 1) "-") (string-append "-" (number->string (exact->inexact (+ (intera (interaSenzaPunto (substring n 1 (string-length n) ))) (decimale (decimaleSenzaPunto (substring n 1 (string-length n) ) (interaSenzaPunto (substring n 1 (string-length n) ))))))) )
        ;(+ (intera (interaSenzaPunto n)) (decimale (decimaleSenzaPunto n (interaSenzaPunto n )))))
        
    )
    
)



(define Cifra
  (lambda (b c)

    (if (>= (string-length b) 1)
       
    (if (string=? c (substring b (-(string-length b) 1))) (- (string-length b ) 1)
       (Cifra (substring b 0 (- (string-length b) 1)) c)

    )

    "")
  
))

(define interaSenzaPunto
 (lambda (n)
   (if (>= (string-length n) 1)
       
    (if (not (string=? (substring n 0 1) ".")) (string-append (substring n 0 1) (interaSenzaPunto(substring n 1 (string-length n) ))) "")

    "")
    ))

(define decimaleSenzaPunto ;n è la stringa di cifre intera, s è la parte intera della stringa
 (lambda (n s)
   (if (equal? (length? n s) false)
        (substring n (+ (string-length ( interaSenzaPunto n)) 1) (string-length n))
        "0"
    )
   
    ))

(define length?
   (lambda (s s1)

     (if (= (string-length s) (string-length s1)) true false)     
     
))


(define interaConBase 
  (lambda (n b)  ; n parte intera della stringa del numero, b base della stringa
   (let ((base b)
         (Nbase (string-length b))
        )
        
   (if (>= (string-length n) 1)
       (+ (* (Cifra base (substring n 0 1)) (expt Nbase (-(string-length n) 1))) (interaConBase (substring n 1 (string-length n)) base))
       0
    )
    )
    ))


(define decimaleConBase
  (lambda (n b) ; n parte decimale della stringa del numero, b base della stringa
   (let ((base b)
         (Nbase (string-length b))
         (lung (string-length n))
        )
        
   (if (and (>= (string-length n) 1) (and (not (string=? n "0")) (>= lung 1)))
    (+ (* (Cifra base (substring n (-(string-length n) 1))) (expt Nbase (- 0 (string-length n)))) (decimaleConBase (substring n 0 (-(string-length n) 1)) base))
    0)
    )
 ))



(define rep->number
  (lambda (b n)

       (cond ((string=? (substring n 0 1) "-")
         (string-append "-" (number->string (exact->inexact (+ (interaConBase (interaSenzaPunto (substring n 1 (string-length n))) b) (decimaleConBase(decimaleSenzaPunto (substring n 1 (string-length n)) (interaSenzaPunto (substring n 1 (string-length n) ))) b))))))
          ((string=? (substring n 0 1) "+")
          (string-append "+" (number->string (exact->inexact (+ (interaConBase (interaSenzaPunto (substring n 1 (string-length n))) b) (decimaleConBase(decimaleSenzaPunto (substring n 1 (string-length n)) (interaSenzaPunto (substring n 1 (string-length n) ))) b))))))
          (else
          (string-append "" (number->string (exact->inexact  (+ (interaConBase (interaSenzaPunto n) b) (decimaleConBase (decimaleSenzaPunto n (interaSenzaPunto n )) b))))))
        )  
    )
)

(bin-rep->number "+1101")
(bin-rep->number "0")
(bin-rep->number "10110.011")
(bin-rep->number "-0.1101001")
(rep->number "zu" "-uuzz")
(rep->number "0123" "+21.1")
(rep->number "0123456789ABCDEF" "0.A")
(rep->number "0123456789ABCDEF" "1CF.0")
(rep->number "zu" "-uuzz.zuu")
(rep->number "01234" "-10.02")


