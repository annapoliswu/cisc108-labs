;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fa5-reduce) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A [ListOf X] is
;;  -- empty
;;  -- (cons X [ListOf X])
#|TEMPLATE
(define (loX-fun aloX)
  (cond 
    [(empty? aloX) ...]
    [(cons? aloX)
     ... (first aloX) ;an X 
     ... (loX-fun (rest aloX))
    ]))
|# 

;; sum : [ListOf Number] --> Number
(define (sum aloX)
  (cond 
    [(empty? aloX) 0]
    [(cons? aloX)
     (+ (first aloX) ;an X 
        (sum (rest aloX)))
     ]))

;; product : [ListOf Number] --> Number
(define (product aloX)
  (cond 
    [(empty? aloX) 1]
    [(cons? aloX) 
     (* (first aloX) ;an X 
        (product (rest aloX)))
     ]))

;; numreduce : (Numbner Number--> Number) [ListOf Number] --> Number
(define (numreduce COMBINER BASE alon)
  (cond 
    [(empty? aloX) BASE]
    [(cons? aloX) 
     (COMBINER (first aloX) ;an X 
        (numreduce (rest aloX)))
     ]))

;; TEST ABSTRACTION BY REWRITING!!
(define (sum.v1 alox) (numreduce + 0 alox))
(define (product.v1 alox) (numreduce * 1 alox))

(check-expect (concat '()) "")
(check-expect (concat '("hello" "world")) "helloworld")

;; concat : [ListOf String] --> String
(define (concat los)
    (cond 
    [(empty? los) ""]
    [(cons? los) 
     (string-append (first los) ;an X 
         (concat (rest los)))]))


;; reduce : (X Y --> Y) Y [Listof X] --> Y
(define (reduce COMBINER BASE aloX)
  (cond 
    [(empty? aloX) BASE]
    [(cons? aloX) 
     (COMBINER (first aloX) ;an X 
               (reduce COMBINER BASE (rest aloX)))
     ]))









