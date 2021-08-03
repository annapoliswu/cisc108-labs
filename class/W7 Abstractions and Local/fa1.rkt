;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname fa1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A [ListOf Number] is
;; -- empty
;; -- (cons Number [ListOf Number])
#|TEMPLATE
(define (lon-fun alon)
  (cond [(empty? alon) ...]
        [(cons? alon) 
         ... (first alon) ;a Number
         ... (lon-fun (rest alon))   ]))
|#  

;; filter-10 : [ListOf Number]  --> [ListOf Number]
;; returns a list of all the numbers strictly larger than 10
(check-expect (filter-10 empty) empty)
(check-expect (filter-10 '(1 5 10 20 200 1000)) '(20 200 1000))
(define (filter-10 alon)
  (cond [(empty? alon) empty]
        [(cons? alon) 
         (cond [(> (first alon) 10)
                (cons (first alon) (filter-10 (rest alon)))]
               [else (filter-10 (rest alon))])]))

;; filter-100 : [ListOf Number]  --> [ListOf Number]
(check-expect (filter-100 empty) empty)
(check-expect (filter-100 '(1 5 10 20 200 1000)) '(200 1000))
(define (filter-100 alon)
  (cond [(empty? alon) empty]
        [(cons? alon) 
         (cond [(> (first alon) 100)
                (cons (first alon) (filter-100 (rest alon)))]
               [else (filter-100 (rest alon))])]))

;; filter-bigger : [ListOf Number] Number --> [ListOf Number]
;; returns a list of all the numbers strictly larger than the given number
(check-expect (filter-bigger empty 5) empty)
(check-expect (filter-bigger '(1 5 10 20 200 1000) 10) '(20 200 1000))
(check-expect (filter-bigger '(1 5 10 20 200 1000) 100) '(200 1000))

(define (filter-bigger alon n)
  (cond [(empty? alon) empty]
        [(cons? alon) 
         (cond [(> (first alon) n)
                (cons (first alon) (filter-bigger (rest alon) n))]
               [else (filter-bigger (rest alon) n)])]))


; AFTER ABSTRACTING, TEST ABSTRACTION BY REWRITING PREVIOUS DEFINITIONS
(check-expect (filter-10.v2 empty) empty)
(check-expect (filter-10.v2 '(1 5 10 20 200 1000)) '(20 200 1000))
(check-expect (filter-100.v2 empty) empty)
(check-expect (filter-100.v2 '(1 5 10 20 200 1000)) '(200 1000))

(define (filter-10.v2 alon)
   (filter-bigger alon 10))
