;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname qsort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define BIGLIST (build-list 10000 (lambda (x) (random 10000))))


;; Insertion Sort: falls directly out of the design recipe
;; isort: (listof X) --> (listof X)
;; consume an unsorted list of X, and produce
;; a sorted list, ordered by < [can be abstracted]

(define (isort alox)
  (cond [(empty? alox) empty]
        [(cons? alox)
         (insert (first alox)
                 (isort (rest alox)))]))


(define (insert x alox)
  (cond [(empty? alox) (cons x empty)]
        [(cons? alox)
         (cond [(< x (first alox))
                (cons x alox)]
               [else
                (cons (first alox)
                      (insert x (rest alox)))])]))

(check-expect (isort '(10 9 8 7 5 6 4 3 2 1)) '(1 2 3 4 5 6 7 8 9 10))
;(isort <= BIGLIST)



;; qsort : [ListOf Number] --> [ListOf Number]
;; produces same list, but sorted small to large

(define (qsort alon)
  (cond [(empty? alon) '()]
        [else
         (local [(define pivot (first alon))
                 (define smaller (filter (λ (x) (<= x pivot)) (rest alon)))
                 (define larger  (filter (λ (x) (> x pivot)) (rest alon)))]
           (append (qsort smaller)
                   (list pivot)
                   (qsort larger)))]))

;^Splits list in 2 based on values. Sorts then appends.

; Testing (time (begin (qsort BIGLIST) true))

;search algorithm by spliting in half over /over. Generative tree recursion

; < or <=
; alon or (rest alon)


;(check-expect (qsort '(10 9 8 7 6 5 4 3 2 1)) '(1 2 3 4 5 6 7 8 9 10))
;(qsort BIGLIST)



;; ABSTRACT POLYMORPHIC VERSION
;; qs : (X X --> boolean) [ListOf X] --> [ListOf X]
;; produces same list with elements ordered by op (op=true first)
(define (qs op alon)
  (cond [(empty? alon) empty]
        [(cons? alon)
         (local [(define pivot (first alon))
                 (define smaller (filter (λ (x) (op x pivot))       (rest alon)))
                 (define larger  (filter (λ (x) (not (op x pivot))) (rest alon)))]
           (append (qs op smaller)
                   (list pivot)
                   (qs op larger)))]))


(check-expect (qs < (list 10 9 8 7 6 5 4 3 2 1)) '(1 2 3 4 5 6 7 8 9 10))
(check-expect (qs > (list 1 2 3 4 5 6 7 8 9 10)) '(10 9 8 7 6 5 4 3 2 1))
(check-expect (qs string<? '("bill" "bob" "adam" "mary" "kim" "sally" "fred"))
              '("adam" "bill" "bob" "fred" "kim" "mary" "sally"))
(define-struct emp (name salary))
;; an emp is (make-emp string number)
(define MYCO (list (make-emp "bill" 100)
                   (make-emp "bob" 400)
                   (make-emp "adam" 100)
                   (make-emp "mary" 100)
                   (make-emp "kim" 900)
                   (make-emp "sally" 1800)))
;sort by salary?