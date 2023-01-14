;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab_25-10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ultima-lettera
  (lambda (stringa)
    (substring stringa (- (string-length stringa) 1))
    ))
  
(define plurale?  ; booleano
  (lambda (stringa)       ; stringa: soggetto / complemento oggetto
    (let (
          (k (ultima-lettera stringa))
          )
    (if
       (or (string=? k "i") (string=? k "e"))
       #true
       #false
       )
     )
    ))
;;usare carattere invece di sottostringa

(define maschile?  ; booleano
  (lambda (stringa)       ; stringa
    (let (
          (k (ultima-lettera stringa))
          )
    (if
       (or (string=? k "i") (string=? k "o"))
       #true
       #false
       )
     )
    ))

(define articolo-m
  (lambda (parola)
    (if (plurale? parola)
        "i"
        "il"
     )
    ))

(define articolo-f
  (lambda (parola)
    (if (plurale? parola)
        "le"
        "la"
     )
    ))

(define articolo
  (lambda (parola)
    (if (maschile? parola)
     (articolo-m parola)
     (articolo-f parola)
     )
    ))

(define declinazione-verbo-singolare
  (lambda (prefisso coniug)
   (if (string=? coniug "are")
       (string-append prefisso "a")
       (string-append prefisso "e")
       )
    ))

(define declinazione-verbo-plurale
  (lambda (prefisso coniug)
   (if (string=? coniug "are")
       (string-append prefisso "ano")
       (string-append prefisso "ono")
       )
    ))

(define declinazione-verbo  ; stringa: verbo coniugato
  (lambda (verbo soggetto)  ; verbo: stringa, verbo regolare all'infinito
    (let (
          (coniug (substring verbo (- (string-length verbo) 3)))      ; restituisce "are" "ere" "ire"
          (prefisso (substring verbo 0 (- (string-length verbo) 3)))  ; restisuisce verbo senza le ultime tre lettere
          )
      (if (plurale? soggetto)
          (declinazione-verbo-plurale prefisso coniug)
          (declinazione-verbo-singolare prefisso coniug)
          )
      )
    ))


(define frase
  (lambda (soggetto verbo complemento-oggetto)
    (string-append
     (articolo soggetto)
     " "
     soggetto
     " "
     (declinazione-verbo verbo soggetto)
     " "
     (articolo complemento-oggetto)
     " "
     complemento-oggetto
     )
    ))