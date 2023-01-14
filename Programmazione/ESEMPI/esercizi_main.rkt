;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname esercizi_main) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Percorsi di Manhattan

(define paths
  (lambda (i j)
    (if (or (= i 0) (= j 0))
        1
        (+ (paths i (- j 1)) (paths (- i 1) j))
        )
    ))


;; Numeri di Stirling del II tipo

(define st
  (lambda (n k)
          (if (or (= k 1) (= k n))
              1
              (+ (st (- n 1) (- k 1)) (* k (st (- n 1) k)))
          )
  ))


;; LCS

(define llcs
  (lambda (u v)
    (cond ((or (string=? u "") (string=? v ""))
           0)
          ((char=? (string-ref u 0) (string-ref v 0))
           (+ 1 (llcs (substring u 1) (substring v 1))))
          (else
           (max (llcs u (substring v 1))
                (llcs (substring u 1) v)
                )
           )
          )))

(llcs "AGACTGAACATAC" "GATCCGACTAC")

(define lcs
  (lambda (u v)
    (cond ((or (string=? u "") (string=? v ""))
           "")
          ((char=? (string-ref u 0) (string-ref v 0))
           (string-append
           (substring u 0 1)
           (lcs (substring u 1) (substring v 1))
           ))
          (else
           (longer
            (lcs u (substring v 1))
            (lcs (substring u 1) v)
            )))
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
            )
          )))

(lcs "AGACTGAACATAC" "GATCCGACTAC")


;; List-ref: Data una lista e la posizione, stampa l'elemento della lista presente in quella posizione

(define lista-ref                         ; ritorna l'elemento della lista
  (lambda (ls k)                          ; ls: lista, k: intero
    (if (= k 0)
        (car ls)
        (lista-ref (cdr ls) (- k 1))
        )
    ))


;; Append

(define giustapposizione
  (lambda (ls1 ls2)    ;; ls1 ls2: liste
    (cond ((null? ls1)
           ls2)
          (else
           (cons (car ls1) (giustapposizione (cdr ls1) ls2))
           ))
    ))

(giustapposizione '(1 2 3 4) '(0 0 0 0 0 0 0))


;; Reverse

(define rovescio
  (lambda (ls)
    (rovescio-rec ls null)
    ))

(define rovescio-rec
  (lambda (ls rv)
    (if (null? ls)                          ; se la lista è nulla, quindi è finita, allora stampa quella rovesciata
        rv
        (rovescio-rec (cdr ls) (cons (car ls) rv))
        )
    ))

(rovescio '(1 2 3 4))


;; Test di primalità
(define primo?
  (lambda (n)
    (not (ha-divisori-in? n 2 (- n 1)))
    ))

(define ha-divisori-in?
  (lambda (n a b)
    (cond ((> a b)
           false)
          ((= (remainder n a) 0)
           true)
          (else (ha-divisori-in? n (+ a 1) b))
          )
    ))

(define lista-primi
  (lambda (k n)
    (cond ((> k n)
           null)
          ((primo? k)
           (cons k (lista-primi (+ k 1) n)))
          (else
           (lista-primi (+ k 1) n))
          )
    ))

(primo? 6)


;; Moltiplicazione

(define mul
  (lambda (m n)
    (cond ((= n 0)
           0)
          ((even? n)
           (mul (* 2 m) (quotient n 2)))
          (else
           (+ m (mul (* 2 m) (quotient n 2))))
          )
    ))


;; Massimo Comun Divisore MCD

(define mcd
  (lambda (x y)
    (cond ((= x y)
           x)
          ((< x y)
           (mcd x (- y x)))
          (else
           (mcd (- x y) y))
          )
    ))


;; Elevamento a Potenza

(define power
  (lambda (x y)
    (if (= y 0)
        1
        (* x (power x (- y 1)))
        )
    ))


;; UFO = Unidentified Flying prOcedure

(define ufo
  (lambda (x)
    (cond ((= x 1)
           1)
          ((even? x)
           (- (* 2 (ufo (quotient x 2))) 1))
          (else
           (+ (* 2 (ufo (quotient x 2))) 1))
          )
    ))

(ufo 6)
(ufo 15)

;; Regola di Cesare

(define rot-3
  (lambda (c)
    (let (
          (k (+ (char->integer c) 3))
          )
      (if (<= k pos-Z)
          (integer->char k)
          (integer->char (- k 26)))
      ))
  )

(define pos-Z (char->integer #\Z))