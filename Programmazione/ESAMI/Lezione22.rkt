;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lezione22) (read-case-sensitive #t) (teachpacks ((lib "drawing.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawing.ss" "installed-teachpacks")) #f)))
; 4 febbreio 2020 A

; Es 1

(define f ; val: intero
 (lambda (x y u v) ; x, y ≥ 0 interi; u, v interi
   (cond ((and (= u 0) (= v 0)) 0)
     ((= x 0) (if (= u 0) 0 1))
     ((= y 0) (if (= v 0) 0 1))
     (else (+ (f (- x 1) y (- u 1) v) (f x (- y 1) u (- v 1))))
 )))

; 1) (f 0 5 0 3) → 0
; 2) (f 5 0 3 -1) → 1
; 3) (f 5 1 3 0) → (+ (f 4 1 2 0) (f 5 0 3 -1))
;                → (+ (f 4 1 2 0) 1)
;                → (2 + 1) = 3

; 3.1) (f 4 1 2 0) → (+ (f 3 1 1 0) (f 4 0 2 -1))
;                  → (+ (f 3 1 1 0) 1)
;                  → (1 + 1) = 2

; 3.2) (f 3 1 1 0) → (+ (f 2 1 0 0) (f 3 0 1 -1)) = 1

; il penultimo non serve calcolarlo tutto perché noto che ha scambiato i numeri
; con l'esercizio sopra: se scambio x con y e/o u con v ottengo lo stesso valore

; Es 2

(define first-downcase      ; val: stringa
  (lambda (w)               ; w: stringa
    (string-append
    (string (char-downcase (string-ref w 0)))
    (substring w 1))
    ))

(define lower-first          ; val: liste di stringhe
  (lambda (u)                ; u: lista di stringhe
    (map first-downcase u)
    ))

(define btr-val-tr          ; val: intero
  (lambda (btr)             ; btr: stringa di
    (btr-val-rec btr 0)
     ))

(define btd-val
 (lambda (t)
  (cond ((char=? t #\-) -1)
        ((char=? t #\.) 0)
        ((char=? t #\+) +1)
   )
 ))

(define btr-val-rec        ; val: intero
  (lambda (btr n)          ;
    (let ((k (string-length btr)))
      (if (= k 0)
          n
          (let ((q (substring btr 1))
            (t (string-ref btr 0))
             )
            (btr-val-rec q (+ (* 3 n) (btd-val t)))
            )))
    ))

; Es 4 versione B

; Per ogni s in [0,0] . (f s 1 0 1) --> 0-s
; (f 0 1 0 1) --> 0-0 = 0

; Considero n >= 0 : Per ogni s in [0,n] . (f s 1 n 1) --> n-s

; Per n considerato : Per ogni s in [0,n+1] . (f s 1 n+1 1) --> n+1-s








; Es 5 04-02-2020

(define combinations           ; val: lista di stringhe
  (lambda (k n)                ; k,n: interi non negativi
    (if (= n 0)
        (list "")
        (let ((u (if (= k 0)
              null
              (combinations (- k 1) (- n 1))
              ))
           (v (combinations k (- n 1)))
           )
         (append
          (map (lambda (s) (string-append "1" s)) u)
          (map (lambda (s) (string-append "0" s)) v)
          )))
     ))




