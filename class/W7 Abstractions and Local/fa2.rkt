;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname fa2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define X (list 5 6 8 96 4 3 2 5 6 7 3 2 3 5 6 7 4 3 2 4 6 7  9 6 3 2 2 4))

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

;; A [ListOf String] is
;; -- empty
;; -- (cons String [ListOf String])
#|TEMPLATE
(define (los-fun alos)
  (cond [(empty? alos) ...]
        [(cons? alos) 
         ... (first alos) ;a String
         ... (los-fun (rest alos))   ]))
|#

;; Parametric Data Definition, for any Type X
;; a [ListOf X]
;; -- empty
;; -- (cons X [ListOf X])


;; filter-bigger : [ListOf Number] Number --> [ListOf Number]
(check-expect (filter-bigger empty 5) empty)
(check-expect (filter-bigger '(1 5 10 20 200 1000) 10) '(20 200 1000))
(check-expect (filter-bigger '(1 5 10 20 200 1000) 100) '(200 1000))
(define (filter-bigger alon n)
  (cond [(empty? alon) empty]
        [(cons? alon) 
         (cond [(> (first alon) n)
                (cons (first alon) (filter-bigger (rest alon) n))]
               [else (filter-bigger (rest alon) n)])]))

;; filter-smaller : [ListOf Number] Number --> [ListOf Number]
(check-expect (filter-smaller empty 5) empty)
(check-expect (filter-smaller '(1 5 10 20 200 1000) 10) '(1 5))
(check-expect (filter-smaller '(1 5 10 20 200 1000) 100) '(1 5 10 20))
(define (filter-smaller alon n)
  (cond [(empty? alon) empty]
        [(cons? alon) 
         (cond [(< (first alon) n)
                (cons (first alon) (filter-smaller (rest alon) n))]
               [else (filter-smaller (rest alon) n)])]))

;; filter-by-rel : (Number Number -> Boolean) LON Number --> LON
(define (filter-by-rel OPERATOR alon n)
  (cond [(empty? alon) empty]
        [(cons? alon) 
         (cond [(OPERATOR (first alon) n)
                (cons (first alon) (filter-by-rel OPERATOR (rest alon) n))]
               [else (filter-by-rel OPERATOR (rest alon) n)])]))
;; TEST OUR NEW ABSTRACTION

(check-expect (filter-by-rel < X 10 ) (filter-smaller  X 10))

(define (filter-smaller.v2 alon n) (filter-by-rel < alon n))
(define (filter-bigger.v2 alon n) (filter-by-rel > alon n))
(define (filter-equal alon n) (filter-by-rel = alon n))
(define (filter-GT-10 alon) (filter-by-rel > alon 10))


               



; next step abstract entire cond condition


  
;; filter-even : [ListOf Number] --> [ListOf Number]
(define (filter-even alon)
  (cond [(empty? alon) empty] ;heeere
        [(cons? alon) 
         (cond [(even? (first alon))
                (cons (first alon) (filter-even (rest alon)))]
               [else (filter-even (rest alon))])]))


;; myfilter : (Number -> Boolean) List-of-NUmber --> List-of-Number ;note: not the real sig
;; really
;; filter : (X -> Boolean) [ListOf X] --> [ListOf X]

;applying a predicate
(define (myfilter PRED alon)
  (cond [(empty? alon) empty]
        [(cons? alon) 
         (cond [(PRED (first alon))
                (cons (first alon) (myfilter PRED (rest alon)))]
               [else (myfilter PRED (rest alon))])]))

;; TEST BY REWRTING
(define (filter-even.v2 alon) (myfilter even? alon))


(check-expect (filter-10.v3 '(1 5 10 20 200 1000)) '(20 200 1000))

(define (gtten x)( > x 10))
(define (filter-10.v3 alon)(myfilter gtten alon) ;Must make predicate function
     )


;; string-contains-x? : String --> Boolean
(define (string-contains-x? s) (string-contains? "x" s))

;; string-filter-x: [Listof String] --> [Listof String]
(check-expect (string-filter-x '("bill" "sally" "bobx" "tix")) '("bobx" "tix"))
(define (string-filter-x alon)
  (cond [(empty? alon) empty]
        [(cons? alon) 
         (cond [(string-contains-x? (first alon))
                (cons (first alon) (string-filter-x (rest alon)))]
               [else (string-filter-x (rest alon))])]))

(define (string-filter-x.v2 alos) (myfilter string-contains-x? alos))
(check-expect (string-filter-x.v2 '("bill" "sally" "bobx" "tix")) '("bobx" "tix"))









 ;GT10: Number --> Boolean
(define (gt10 x) (> x 100))
(define (filter-GT-10.v2 alon) (filter gt10 alon))

 

(define (gt-x-n x n) (> x n))

;; filter-bigger-than : Number [ListOf Number] --> [ListOf Number]
;; Consumes
;;  Number n,
;;  [ListOf Number] lon
;; Produces new list with only values from lon strictly bigger than n


;; filter : (X -> Boolean) [ListOf X] --> [ListOf X]
#;
(define (myfilter PRED alon)
  (cond [(empty? alon) empty]
        [(cons? alon) 
         (cond [(PRED (first alon))
                (cons (first alon) (myfilter PRED (rest alon)))]
               [else (myfilter PRED (rest alon))])]))

  
  
  
  
  















