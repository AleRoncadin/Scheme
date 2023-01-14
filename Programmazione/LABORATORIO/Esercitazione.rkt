;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname esercitazione) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))

; ---------------------------------------------------------------------------------------------

; ESERCIZIO 1
(define match
 (lambda (u v)
   (if (or (string=? u "") (string=? v ""))
       ""
       (let ( (uh (string-ref u 0)) (vh (string-ref v 0))
              (s (match (substring u 1) (substring v 1)))
              )
         (if (char=? uh vh)
             (string-append (string uh) s)
             (string-append "*" s)
             ))
       )))

(match "astrazione" "estremi")

; ---------------------------------------------------------------------------------------------

; ESERCIZIO 2

(define offset (char->integer #\0))

(define last-digit
 (lambda (base) 
  (integer->char (+ (- base 1) offset)) 
 ))

(define next-digit
 (lambda (dgt)
 (string (integer->char (+ (char->integer dgt) 1))) ))

(define increment
 (lambda (num base) ; 2 <= base <= 10
  (let ((digits (string-length num)))
    (if (= digits 0)
      "1"
    (let ((dgt (string-ref num (- digits 1))))
    (if (char=? dgt (last-digit base))
    (string-append (increment (substring num 0 ( - digits 1)) base)
    "0")
    (string-append (substring num 0 (- digits 1)) (next-digit dgt))
  ))
))))

(increment "1011" 2)

; ---------------------------------------------------------------------------------------------

; ESERCIZIO 3
(define lcs ; valore: lista di terne
 (lambda (u v) ; u, v: stringhe
   (lcs-rec 1 u 1 v)
  ))

(define lcs-rec
 (lambda (i u j v)
   (cond ((or (string=? u "") (string=? v ""))
          null)
         ((char=? (string-ref u 0) (string-ref v 0))
          (cons (list i j (substring u 0 1))
           (lcs-rec (+ i 1) (substring u 1) (+ j 1) (substring v 1) )))
         (else
          (better (lcs-rec i u (+ j 1) (substring v 1))
                  (lcs-rec (+ i 1) (substring u 1) j v))
         ))))

(define better
  (lambda (x y)
    (if (< (length x) (length y)) y x)
    ))

(lcs "pino" "pino")
(lcs "pelo" "peso")
(lcs "ala" "palato")
(lcs "arto" "atrio")

; ---------------------------------------------------------------------------------------------

; ESERCIZIO 4
(define cyclic-string             ; val: stringa
  (lambda (pattern length)        ; pattern: stringa, length: intero non negativo
    (cond ((or (string=? pattern "") (= length 0))
              "")
          ((> (string-length pattern) length)
           (substring pattern 0 length))
          ((= (string-length pattern) length)
          pattern)
          (else
           (string-append pattern (cyclic-string pattern (- length (string-length pattern))))))
    ))

(cyclic-string "abcd" 0)
(cyclic-string "abcd" 1)
(cyclic-string "abcd" 2)
(cyclic-string "abcd" 4)
(cyclic-string "abcd" 5)
(cyclic-string "abcd" 11)

; ---------------------------------------------------------------------------------------------

; ESERCIZIO 5
(define av
  (lambda (l)                   ; l: lista non vuota di numeri uguali a (-1,0,1)
    (cond ((null? l)            ; se la lista è vuota allora stampa una lista vuota
           null)
          ((null? (cdr l))      ; controlla se vi è un solo elemento rimasto
           null)                ; se c'è un solo elemento rimasto non posso fare la somma con il successivo
          ((< (+ (car l) (cadr l)) 0)
              (cons -1 (av (cdr l))))
          ((= (+ (car l) (cadr l)) 0)
              (cons 0 (av (cdr l))))
          (else
              (cons 1 (av (cdr l)))))
    ))

(av '(0 0 -1 -1 1 0 0 1 0))

; ---------------------------------------------------------------------------------------------

; ESERCIZIO 6

(define r-val
(lambda (s)
  (if (string=? s "")
    0
    (if (char=? (string-ref s 0) #\.)
      (+ (* (string->number (substring s (- (string-length s) 1))) (expt 2 (- 0 (- (string-length s) 1)))) (r-val (substring s 1 (- (string-length s) 1) )) )
      (+ (* (string->number (substring s (- (string-length s) 1))) (expt 2 (- 0 (string-length s)))) (r-val (substring s 0 (- (string-length s) 1) )) )
    )
  )
))

(r-val ".1010")

; ---------------------------------------------------------------------------------------------

; ESERCIZIO 7
(define shared
  (lambda (u v)

    (cond ((or (null? u) (null? v))
        '())
        ((> (car u) (car v))(shared  u (cdr v)))
        ((> (car v) (car u))(shared  (cdr u) v))
        (( = (car v) (car u)) (cons (car v) (shared (cdr u) (cdr v))))
     )
))


(shared '(1 3 5 6 7 8 9 10) '(0 1 2 3 4 5 7 9))

; ---------------------------------------------------------------------------------------------

; ESERCIZIO 8

(define contaUno                  ; conta quanti "1" sono presenti nella stringa
  (lambda (stringa)
  (cond ((string=? stringa "")
           0)                         ; dato che 1 è dispari non viene inserito
           ((char=? (string-ref stringa 0) #\1)
            (+ 1 (contaUno (substring stringa 1))))
            (else
             (+ 0 (contaUno (substring stringa 1)))))
  ))

(define parity-check-failures        ; val: lista di interi positivi
  (lambda (lista)                    ; lista di stringhe
    (check-list (map contaUno lista) 0)
    ; passa una lista contente il numero di "1" per ogni elemento della lista originale
    ; e il contatore inizializzato a 0
    ))

(define check-list
  (lambda (l n)

    (if (>= n (length l)) ;Se il numero contenuto nella lista ottenuta dal map è dispari,
                           ;aggiungo la posizione dell'elemento alla lista e incremento il contatore,
                           ;se no incremento solo il contatore
        '()
        (if (even? (list-ref l n))
            (check-list l (+ n 1))
            (cons n (check-list l (+ n 1)))
        )
     )

))

(parity-check-failures '("0110" "1101" "0000" "1011"))

; ---------------------------------------------------------------------------------------------

; ESERCIZIO 9

(define closest-pair
  (lambda (l)
    (if (null? l)                         ; stampa lista vuota se la lista è nulla
      '()
      (if (null? (cdr (cdr l)))           ; se ci sono solo 2 elementi stampo solo quelli
        (list (car l) (cadr l))
        (mins (list (car l) (cadr l)) (closest-pair (cdr l)))
      )
    )
))

(define mins
; faccio la sottrazione tra il secondo della lista e il primo della lista sia di a che di b
; poi verifico quale dei due è minore e a seconda del risultato stampo a se è minore, altrimenti b
  (lambda (a b)
  (if (< (- (cadr a) (car a)) (- (cadr b) (car b)))
      a
      b)
  )
 )

(closest-pair '(0.1 0.15 0.5 0.6 0.8 1.1))