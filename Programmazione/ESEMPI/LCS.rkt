;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname LCS) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))


;; Sottosequenza comune piu' lunga -- campione
;; Longest Common Subsequence (LCS)

(define llcs     ; valore: intero
  (lambda (u v)  ; u, v: stringhe
    (cond ((or (string=? u "") (string=? v ""))
           0)
          ((char=? (string-ref u 0) (string-ref v 0))
           (+ 1 (llcs (substring u 1) (substring v 1))))
          (else
           (max (llcs (substring u 1) v) (llcs u (substring v 1))))
          )))

;(llcs "micio" "macio")

;(llcs "gatto" "gattino")

; ----------------------------------------------------------------------------------------------

(define lcs      ; valore: stringa
  (lambda (u v)  ; u, v: stringhe
    (cond ((or (string=? u "") (string=? v ""))
           "")
          ((char=? (string-ref u 0) (string-ref v 0))
           (string-append (substring u 0 1)
                          (lcs (substring u 1) (substring v 1))))
          (else
           (longer (lcs (substring u 1) v) (lcs u (substring v 1))))
          )))

(define longer  ; valore: stringa
  (lambda (u v)  ; u, v: stringhe
    (if (< (string-length u) (string-length v))
        v
        u
        )))

; altro tipo di longer:
(define longer2
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

(lcs "gatto" "gattino")

; ----------------------------------------------------------------------------------------------

(define lcss     ; valore: stringa
  (lambda (u v)  ; u, v: stringhe
    (cond ((or (string=? u "") (string=? v ""))
           (list ""))
          ((char=? (string-ref u 0) (string-ref v 0))
           (prefix-all (substring u 0 1)
                       (lcss (substring u 1) (substring v 1))))
          (else
           (let ((m (llcs (substring u 1) v))
                 (n (llcs u (substring v 1)))
                 )
             (cond ((< m n)
                    (lcss u (substring v 1)))
                   ((> m n)
                    (lcss (substring u 1) v))
                   (else
                    (merge (lcss (substring u 1) v) (lcss u (substring v 1))))
                   )))
          )))

(define prefix-all     ; val: lista di stringhe
  (lambda (pre words)  ; pre: stringa, words: lista di stringhe
    (if (null? words)
        null
        (cons (string-append pre (car words))
              (prefix-all pre (cdr words)))
        )))

(define merge          ; val: lista di stringhe
  (lambda (ws1 ws2)    ; ws1, ws21: liste di stringhe
    (if (null? ws1)
        ws2
        (merge (cdr ws1)
               (ins (car ws1) ws2))
        )))

(define ins            ; val: lista di stringhe
  (lambda (w words)    ; w: stringa, words: lista di stringhe
    (cond ((null? words)
           (list w))
          ((string=? w (car words))
           words)
          (else
           (cons (car words) (ins w (cdr words))))
          )))

(lcss "alfa" "alfabeto")