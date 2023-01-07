;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab1) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define frase
(lambda (sogg verbo compl)
 (string-append (articolo sogg) " " (coniuga verbo sogg) " " (articolo compl))
  ))

(define articolo
(lambda (nome)
  (let ((n (substring nome (- (string-length nome) 1))))
  (cond ((string=? n "o")
         (string-append "il " nome)
         )
        ((string=? n "a")
         (string-append "la " nome)
         )
        ((string=? n "e")
         (string-append "le " nome)
         )
        ((string=? n "i")
         (string-append "i " nome)
         )
    ))
  ))

(define coniuga
(lambda (verbo nome)
  (let ((v (substring verbo (- (string-length verbo) 3))))
 (cond ((string=? v "are")
        (if (equal? (nome_singolare? nome) true)
        (string-append (substring verbo 0 (- (string-length verbo) 3)) "a")
        (string-append (substring verbo 0 (- (string-length verbo) 3)) "ano")
        ))
       ((string=? v "ere")
        (if (equal? (nome_singolare? nome) true)
        (string-append (substring verbo 0 (- (string-length verbo) 3)) "e")
        (string-append (substring verbo 0 (- (string-length verbo) 3)) "ono")
        ))
       ((string=? v "ire")
        (if (equal? (nome_singolare? nome) true)
        (string-append (substring verbo 0 (- (string-length verbo) 3)) "e")
        (string-append (substring verbo 0 (- (string-length verbo) 3)) "ono")
        ))
   ))
  ))



(define nome_singolare?
(lambda (nome)
  (let ((n (substring nome (- (string-length nome) 1))))
    (if (or (string=? n "o") (string=? n "a"))
     true
     false)
    )
))