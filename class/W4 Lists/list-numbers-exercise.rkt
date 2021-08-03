;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname list-numbers-exercise) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; a list of numbers (LoN) is
; - empty
; - (cons number LoN)
(define LON1 (cons 1 (cons 2 (cons 5 (cons 0 (cons 8 empty))))))
(define LON2 (cons -2 (cons -1 (cons 0 (cons 3 (cons 5 empty))))))
(define LON3 (cons 2 empty))
#| TEMPLATE
(define (LoN-fn aLoN)
  (cond [(empty? aLoN) ...]
        [(cons? aLoN) ... (first aLoN)
                      ... (LoN-fn (rest aLoN))]))
|#

; sum: LoN -> number
; Produces the sum of the list of numbers
(define (sum aLoN)
  (cond [(empty? aLoN) 0]
        [(cons? aLoN) (+ (first aLoN)
                         (sum (rest aLoN)))]))

; TODO: write check-expects for sum








; count: LoN -> number
; Produces the count of how many numbers are in the list of numbers

(define (count aLoN)
  (cond [(empty? aLoN) 0 ]
        [(cons? aLoN) (+ 1 (count (rest aLoN))) ]
        ))

; TODO: write check-expects for count


; TODO: write implementation for count







#|
; count-positive: LoN -> number
; Produces the count of how many numbers are positive (0 is considered
; positive) in the list of numbers
(check-expect (count-positive empty) 0)
(check-expect (count-positive LON1) 5)
(check-expect (count-positive LON2) 3)
(check-expect (count-positive LON3) 1)

; TODO: write implementation for count-positive

|#


(define (count-positive aLoN)
  (cond [(empty? aLoN) 0]
        [(cons? aLoN) (+ 1 (if (>= (first aLoN) 0) 1 0)  
                     (count-positive(rest aLoN)))]
                      ))

















; broken for now, we will fix it!

; a non-empty-LoN is
; - (cons number empty)
; - (cons number non-empty-LoN)

; average: non-empty-LoN -> number
; Produces the average of a list of numbers
;(check-expect (average empty) ?) ;; BAD!

;(check-expect (average empty) 0)
(define (average aLoN)
  (/ (sum aLoN) (count aLoN)))









; increasing?: LoN -> boolean
; Produces true if all of the numbers are in increasing order.
; Numbers are in increasing order if for every consecutive two
; numbers A and B: 
; A <= B
(check-expect (increasing? empty) true)
(check-expect (increasing? LON1) false)
(check-expect (increasing? LON2) true)
(check-expect (increasing? LON3) true)

; TODO: write implementation for increasing? (we will do this together)





(define (increasing? alon)
  (cond [(empty? alon) true] ;can just get rid of and say not handling empty lists.
        [(empty? (rest alon)) true] ;tests if list contains 1 number
        [(cons? alon)(and (<= (first alon) (first (rest alon)) ) ;next part of list
                      (increasing? (rest alon)))] ))

         