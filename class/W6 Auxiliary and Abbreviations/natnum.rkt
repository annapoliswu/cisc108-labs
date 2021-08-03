;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname natnum) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; a NatNum is either
;; -- 0
;; -- (add1 NatNum)  same as (+ 1 NatNum)

;; EXAMPLE
(define FOUR (add1 (add1 (add1 (add1 0))))) ;aka the Number 4 :-)

;; TEMPLATE FOR NATURAL NUMBER RECURSION
#;
(define (fn-on-nn nn)
  (cond [(zero? nn) ...]
        [else
         ...nn
         ...(fn-on-nn (sub1 nn))
         ]))


;; gen-2s : NatNum --> ListOfNumber
;; Consumes:
;;  NatNum n: the number of 2's to put in the list
;; Produces: a list with n copies of the number 2.

(define (gen-2s nn)
  (cond [(zero? nn) empty]
        [else (cons 2 (gen-2s (sub1 nn)))
         ]))
(check-expect (gen-2s 0) empty)
(check-expect (gen-2s 5) (list 2 2 2 2 2))


;; gen-strings : NatNum String --> String
;; Consumes
;;  NatNum n: length of the resulting list
;;  String str: a string to use for each value in the resulting list
;; Purpose: a string of copies of s that is exactly n elements long

(define (gen-strings nn str)
  (cond [(zero? nn) ""]
        [else
         (string-append str(gen-strings (sub1 nn) str))
         ]))


(check-expect (gen-strings 0 "snake") "" )
(check-expect (gen-strings 1 "snake") "snake")
(check-expect (gen-strings 5 "hi") "hihihihihi")


;; gen-random-number-list : NatNum NatNum --> ListOfNumber
;; Consumes:
;;  NatNum n:   the length of the resulting list
;;  NatNum max: the biggest random number to place on the list
;; Produces: a list with n random numbers in range [0,max)


(define (gen-random-number-list nn maxn)
  (cond [(zero? nn) empty]
        [else
            (cons (random maxn)
            (gen-random-number-list (sub1 nn) maxn)
            )
         ]))



     
