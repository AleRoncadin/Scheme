;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lezione21) (read-case-sensitive #t) (teachpacks ((lib "drawing.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawing.ss" "installed-teachpacks")) #f)))
; 1 Prova di accertamento 29 Gennaio 2021 B

(define pair            ; val lista di caratteri
  (lambda (x y)         ; x e y sono caratteri
    (if (char<? y x)    ; char<? vede se il carattere viene prima (se è minore)
        (list y x)
        (list x y)
     )
    ))

(define pair-list                  ; val liste di coppie
  (lambda (u v)                    ; u e v sono liste di caratteri
    (if (null? u)                  ; se la lista è vuota (uso null? per vederlo)
        null                       ; null stampa la lista vuota
        (cons                      ; individua il primo elemento della lista che viene restituito come valore dalla procedura
           (pair (car u) (car v))
           (pair-list (cdr u) (cdr v))
           ))
    ))

; la funzione scritta sopra può essere riportata come segue:

(define pair-list2      ; val liste di coppie
  (lambda (u v)        ; u e v sono liste di caratteri
    (map pair u v)     ; map applica la funzione ad ogni elemento della lista
    ))

; Es 2 Prova A del 29/01/2021

(define lcs-align
  (lambda (u v)
  (let ((m (string-length u)) (n (string-length v))) ; m: lunghezza di u, n: lunghezza di v
    (cond ((or (= m 0) (= n 0))
           (list (string->list u) (string->list v))) ; converte le stringhe in liste
          ((char=? (string-ref u 0) (string-ref v 0)) ;string-ref ritorna il carattere nella posizione che vuoi di quella stringa
           (lcs-align (substring u 1) (substring v 1)))
           (else
            (let ((du (lcs-align (substring u 1) v))
             (dv (lcs-align (substring v 1) u))
                 )
             (if (> (+ (length (car du)) (length (cadr du)))
                    (+ (length (car dv)) (length (cadr dv)))
                  )
              (list (car dv) (cons (string-ref v 0) (cadr dv)))
              (list (cons (string-ref u 0) (car du)) (cadr du))
              )))
           ))))

; cons prende due argomenti e ritorna una lista di due elementi
; car seleziona il primo elemento nella lista
; cdr seleziona dal secondo all'ultimo
; cadr seleziona il secondo elemento

; Es 3 29/01/2021

; 3.1

; La proprietà generale è:
; per ogni intero n > 0 e per ogni coppia di interi m,k > 0 con m<=n:
;         (f m k) --> (m+k-1)^2 - (k-1)^2

; CASI BASE:

; 1) Da dimostrare che per ogni coppia di interi m,k > 0 con m <= 1:
;         (f m k) --> (m+k-1)^2 - (k-1)^2

; 2) per ogni k > 0:
;         (f 1 k) --> (1+k-1)^2 - (k-1)^2


; IPOTESI INDUTTIVA:

; 1) considerato intero n > 0, per ogni coppia di interi m,k > 0 con m<=n:
;         (f m k) --> (m+k-1)^2 - (k-1)^2


; PASSO INDUTTIVO:

; 1) per n considerato sopra, per ogni coppia di interi m,k > 0 con m<=n:
;         (f m k) --> (m+k-1)^2 - (k-1)^2

; 2) per n considerato sopra, per ogni k > 0:
;         (f m k) --> (n+1+k-1)^2 - (k-1)^2


; 3.2

; (sq n) --> (f n 1) --> (n+1-1)^2 - (1-1)^2 = n^2

; Es 4

(define parity-check? ; val: booleano
  (lambda (words) ; words: lista non vuota di stringhe di 0/1 della stessa lunghezza
    (rec-check? words 0 (string-length (car words)))
  ))

(define rec-check?
 (lambda (words k n)
  (if (< k n)
   (let ((kths (map (bit k) words))) ; kths: lista dei valori dei bit in posizione k nelle parole di words
    (if (even? (count-ones kths))
     (rec-check? words (+ k 1) n)
     false
     ))
     true
     )))

(define bit
  (lambda (k)
   (lambda (w) (if (char=? (string-ref w k) #\0) 0 1))
   ))

  ; w sta per la stringa di words
  ; con string-ref prendo il carattere di w in posizione k
  ; se il carattere è uguale a 0 allora ritorno 0
  ; altrimenti 1

(define count-ones                   ; conto il numero di 1 presenti
  (lambda (cs)
    (if (null? cs)                   ; nella lista nulla ci sono 0 uni
       0
        (+ (car cs) (count-ones (cdr cs)))  ; sommo il primo con gli ultimi
        )))

















   


