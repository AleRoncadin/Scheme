#lang racket

(define maiuscolo
  (lambda (str)

    (string-append (string(char-upcase(string-ref str 0))) (substring str 1))

))


(define standard-form
  (lambda (list)

    (map maiuscolo list)

))



(define minuscolo
  (lambda (str)

    (string-append (string(char-downcase(string-ref str 0))) (substring str 1))

))


(define lower-first
  (lambda (list)

    (map minuscolo list)

))



(define cyclic-numberkkkk
  (lambda (s k)
    (let ((n (string-length s)))

      (cond ((= n k) 1)
            ((< n k) 0)
            ((= n 0) 0)
            (else

             
            (let ((p (cyclic-pattern (substring s k)k)) )
               (if (string=? (substring s 0 k) p)
                   p
                   ""
                )
               )
              
             )
       )
      )
    )           
)



(define cyclic-pattern
  (lambda (s k) ;s:string, k: intero positivo

    (let ((lung (string-length s)))

      (cond ((< lung k) "")
            ((= lung k) s)
            (else
             (let ((p (cyclic-pattern (substring s k)k)) )
               (if (string=? (substring s 0 k) p)
                   p
                   ""
                )
               )
             )
            )
            
            

      )
))


(define c
  (lambda (s k) ;s:string, k: intero positivo

    (let ((lung (string-length s)))

      (cond ((< lung k) 0)
            ((= lung k) 1)
            (else
             (let ((p (cyclic-pattern (substring s k)k))
                   
                   )
               (if (string=? (substring s 0 k) p)
                   (+ 1 1)
                   0
                )
               )
             )
            )
            
            

      )
))

;(cyclic-pattern "abab" 2)

;(cyclic-number "abcabcabc" 2)
;(c "abab" 2)

;Completa la procedura match che, date due stringhe di lettere u e v, restituisce la stringa delle corrispondenze w così
;definita: w ha la lunghezza della stringa più corta (fra u e v); se in una certa posizione u e v contengono la stessa lettera,
;allora anche w contiene quella lettera nella posizione corrispondente; se invece u e v contengono lettere diverse, w
;contiene il simbolo “asterisco” nella posizione corrispondente. Per esempio, il valore dell’espressione Scheme
;(match "astrazione" "estremi") è rappresentato dalla stringa delle corrispondenze "*str**i".

(define match
 (lambda (u v)
 (if (or (string=? u "") (string=? v ""))
 ""
 (let ( (uh (string-ref u 0)) (vh (string-ref v 0))  
 (s (match (substring u 1) (substring v 1)) )
 )
 (if (char=? uh vh)
 (string-append (string uh)s)
 (string-append "*" s)
 ))
 )))







;10. Programmazione in Scheme
;Scrivi un programma in Scheme basato sulla procedura sorted-char-list che, data una stringa, restituisce la lista
;dei caratteri che vi compaiono, ordinata in ordine alfabetico e senza ripetizioni.

(define sorted-char-list
  (lambda (string)
    (sort (char-list string null) char<?)
   )
 )

(define char-list
  (lambda (string save)
    (if (string=? string "")
        save
     (if (check (string-ref string 0) save)
         (char-list (substring string 1) save)
          (char-list (substring string 1) (cons (string-ref string 0) save))
      )
    )
   )
 )


(define check
  (lambda (char list)
    (if (null? list)
        #false
        (if (char=? char (car list))
            #true
            (check char (cdr list))
         )
     )
  )
)
      
(sorted-char-list "cab")

;4. Definizione di procedure in Scheme
;Definisci formalmente una procedura cyclic-string in Scheme che, dati come argomenti una stringa pattern e un
;numero naturale length, assuma come valore la stringa di lunghezza length risultante dalla ripetizione ciclica di pattern,
;eventualmente troncata a destra. Per esempio, nel caso dell’espressione (cyclic-string "abcd" n) il risultato
;della valutazione per n = 0, 1, 2, 4, 5, 11 deve essere, rispettivamente: "", "a", "ab", "abcd", "abcda",
;"abcdabcdabc".

(define cyclic-string
  (lambda (s k)
    (let(
         (lung (string-length s))
         )
      (cond ((= k 0) "")
            ((= k 1) (string (string-ref s 0)))
            ((= k lung) s)
            ((< k lung) (substring s 0 k))
            ((> k lung) (string-append s (cyclic-string s (- k lung))))
      )
   )
  )
  )
            
             
;(cyclic-string "abcd" 5)



;6. Definizione di procedure in Scheme
;Valori numerici nell’intervallo [0,1) possono essere rappresentati in forma binaria da una stringa di cifre "0" e "1"
;precedute dal carattere "." (punto), dove i singoli bit sono pesati da potenze negative di due. Per esempio, le stringhe
;".1" e ".011" corrispondono ai numeri 0.5 e 0.375, rispettivamente, nella consueta notazione in base dieci.
;Definisci una procedura r-val in Scheme per determinare il valore numerico di stringhe del tipo descritto sopra
;(punto seguito da una o più cifre binarie).

(define decimal
  (lambda (s)
    (let ((l (string-length s))
          )
      (if (<= (string-length s) 0)
          0
          (if (char=? (string-ref s 0) #\.)
           
              (+ (* (string->number (substring s (-(string-length s) 1))) (expt 2 (- 0 (- (string-length s) 1)))) (decimal (substring s 1 (- (string-length s) 1) )) )
              (+ (* (string->number (substring s (-(string-length s) 1))) (expt 2 (- 0 (string-length s)))) (decimal (substring s 0 (- (string-length s) 1) )) )
          )
      )
    )
 )
)




;2. Completa il programma increment, che calcola l’incremento di un numero naturale rappresentato come stringa di
;cifre in una base compresa fra 2 e 10. Gli argomenti sono num, la stringa numerica, e base, di tipo intero; il valore
;restituito è una stringa numerica. Per esempio, il valore dell’espressione (increment "1011" 2) è "1100", dove
;le stringhe rappresentano rispettivamente 11 e 12 in base 2.

(define offset (char->integer #\0))

(define last-digit
 (lambda (base) (integer->char (+ (- base 1) offset)) ))

(define next-digit
 (lambda (dgt) (string (integer->char (+ (char->integer dgt) 1))) ))

(define increment
 (lambda (num base) ; 2 <= base <= 10
 (let ((digits (string-length num)))
 (if (= digits 0)
 "1"
 (let ((dgt (string-ref num (- digits 1) )))
 (if (char=? dgt (last-digit base))
 (string-append (increment (substring num 0 ( - digits 1)) base)
 "0")
 (string-append (substring num 0 (- digits 1)) (next-digit dgt   )
 )))
 ))))



;7. Programmazione in Scheme
;Definisci una procedura shared in Scheme che, date due liste u, v (strettamente) ordinate di numeri interi positivi,
;restituisca la lista ordinata degli elementi comuni a u e v. Per esempio:


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



;5. Definizione di procedure in Scheme
;Definisci una procedura av in Scheme che, data una lista non vuota (x1 x2 ... xn) i cui n elementi xi appartengono
;all’insieme {–1, 0, 1}, restituisca la lista (y1 y2 ... yn–1) di n–1 elementi dello stesso insieme tale che yi = –1 se
;xi + xi+1 < 0, yi = 0 se xi + xi+1 = 0 e yi = 1 se xi + xi+1 > 0. Per esempio:
;(av '(0 0 -1 -1 1 0 0 1 0)) → (0 -1 -1 0 1 0 1 1)

(define av
  (lambda (l)
    (if (<= (length (cdr l)) 0)
        '()
    (cond ((null? l) '())
          ((= (+ (car l) (car(cdr l))) 0) (cons 0 (av (cdr l))))
          ((= (+ (car l) (car(cdr l))) 1) (cons 1 (av (cdr l))))
          ((< (+ (car l) (car(cdr l))) 0) (cons -1 (av (cdr l))))
     )
    )
          


))

;(av '(0 0 -1 -1 1 0 0 1 0)) 




;Una parola binaria, cioè una stringa composta esclusivamente dai simboli 0 e 1, supera il controllo di parità se il
;numero di occorrenze di 1 è pari. Data una lista di parole binarie, la procedura parity-check-failures restituisce
;la lista delle posizioni delle parole che non superano il controllo di parità.

(define parity-check-failures
  (lambda (l)

    (check-list (map find_one l) 0)  ;Procedura che passa la lista contenente il numero di uno
                                     ;per elemento della lista e il contatore a 0

 )
)



 (define find_one
   (lambda (str)

     (if (= (string-length str) 0)
         0
         (if (char=? (string-ref str 0) #\0)  ;Procedura che conta il numero di uno
                                               ;presenti nella stringa

          (+ 0 (find_one (substring str 1)))
          (+ 1 (find_one (substring str 1)))
         )
    )
          
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


(parity-check-failures '("0111" "1001" "0000" "1010"))
(parity-check-failures '("0110" "1101" "0000" "1011"))
(parity-check-failures '("0111" "1011" "0100" "1110"))
(parity-check-failures '("0110" "1001" "0000" "1010")) 






(define lcs ; valore: lista di terne
 (lambda (u v) ; u, v: stringhe
 (lcs-rec 1 u 1 v)
 ))
(define lcs-rec
 (lambda (i u j v)
 (cond ((or (string=? u "") (string=? v ""))
  null
 )
 ((char=? (string-ref u 0) (string-ref v 0))
 (cons (list i  j (substring u 0 1))(lcs-rec (+ i 1) (substring u 1) (+ j 1) (substring v 1))))
 (else
 (better (lcs-rec i u (+ j 1) (substring v 1)) (lcs-rec (+ i 1) (substring u 1) j v) 
 ))
 )))
(define better
 (lambda (x y)
 (if (< (length x) (length y)) y x)
 ))

(lcs "pino" "pino")
(lcs "pelo" "peso")


(define closest-pair
  (lambda (l)
  (if (null? (cdr (cdr l) ) )
      (list (car l) (car (cdr l) ))
      (mins (list (car l) (car (cdr l) )) (closest-pair (cdr l)) )
      )


  )
 )

(define mins
  (lambda (a b)
  (if (< (- (car (cdr a)) (car a)) (- (car (cdr b)) (car b)))
      a
      b)
  )
 )










;Hanoi
(define hanoi-cont ; val: lista di coppie
  (lambda (n s d t k h v a) ; n intero, s, d, t: posizioni
    (if (= n 1)
        (if (> v k)
            0
            (cond ((equal? s h) -1)
                  ((equal? d h) +1)
                  (else 0))
            )
        (let (
              (m1 (hanoi-cont (- n 1) s t d k h (- v a) (quotient a 2) ))
              (m2 (hanoi-cont (- n 1) t d s k h (+ v a) (quotient a 2) ))
              )
          (+ m1 (+

            (if (> v k)
            0
            (cond ((equal? s h) -1)
                  ((equal? d h) +1)
                  (else 0))
            )
            
            m2))
          
          )
        )
    )
  )


(define (hanoi-list n k h)
  (let ((m (expt 2 (- n 1))))
    (if (equal? h 1)
      (+ (hanoi-cont n 1 2 3 k h m (quotient m 2)) n)
      (hanoi-cont n 1 2 3 k h m (quotient m 2)))
    ))


(define (hanoi-disks n k)
  (list
   (list 1 (hanoi-list n k 1))
   (list 2 (hanoi-list n k 2))
   (list 3 (hanoi-list n k 3))
   )
  )