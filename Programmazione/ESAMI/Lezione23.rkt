;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lezione23) (read-case-sensitive #t) (teachpacks ((lib "drawing.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawing.ss" "installed-teachpacks")) #f)))
; 03-02-17

; Es. 1

; (f '(5)) --> 1

; Es. 2

(define mh ; val: intero
 (lambda (i j) ; i, j: interi non negativi
   (if (or (= i 0) (= j 0))
    1 ; dato che i e j sono zero io non cambierò mai posizione
    (+ (md (- i 1) j) (mr i (- j 1)))
    )))

(define md ; spostamento precedente “in giù”
 (lambda (i j)
  (if (or (= i 0) (< j 2) )
   1
   (+ (md (- i 1) j ) (mr i (- j 2)))
  )))

(define mr ; spostamento precedente “a destra”
 (lambda (i j)
  (if (or (< i 2) (= j 0))
     1
    (+ (md (- i 2) j) (mr i (- j 1)) )
 )))