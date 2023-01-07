;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Es1FunzionanteConBool) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define articolo
  (lambda (nome)
     (let ((lunghezzaStringa (- (string-length nome) 1)))
         (cond ((string=? (substring nome lunghezzaStringa) "o")
          "il")
          ((string=? (substring nome lunghezzaStringa) "a")
          "la")
          ((string=? (substring nome lunghezzaStringa) "e")
          "le")
          ((string=? (substring nome lunghezzaStringa) "i")
          "i")
          )
      )
   )
 )

 (define verbo
  (lambda (v nome)
    (let((pos (- (string-length v) 3)))
    (cond ((string=? (substring v pos) "are")
            (if (equal? (Singolare? nome) true)
              (string-append(substring v 0 pos) "a")
              (string-append(substring v 0 pos) "ano")
           ))
          ((string=? (substring v pos) "ere")
          (if (equal? (Singolare? nome) true)
              (string-append(substring v 0 pos) "e")
              (string-append(substring v 0 pos) "ono")
           ))
          ((string=? (substring v pos) "ire")
              (if (equal? (Singolare? nome) true)
              (string-append(substring v 0 pos) "e")
              (string-append(substring v 0 pos) "ono")
           ))
    )
   )
  )
 )    
    
 
 (define frase
  (lambda (n v c)
    (string-append (articolo n) " " n " " (verbo  v n ) " " (articolo c) " " c)
   )
 )

(define Singolare?
 (lambda (nome)
   (let ((lunghezzaStringa (- (string-length nome) 1)))
      (if (or (string=? (substring nome lunghezzaStringa) "o") (string=? (substring nome lunghezzaStringa) "a")) true false)    
   )
   
 
 )
)