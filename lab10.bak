;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname lab10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Lab 10

;Names:
;Zihan (Anna) Wu
;Debra Lymon

(require 2htdp/image)
(require 2htdp/universe)


;iter-expt : NatNum NatNum -> NatNum
;consumes: NatNum b, NatNum n
;produces : b to the nth power 

(define (iter-expt b n)
  (local
    [ ; ie-aux: NatNum Number -> Number
     (define (ie-aux counter product)
             (cond [(= counter 0) product]
                   [else (ie-aux (sub1 counter) (* b product))]
                   ))
     ]
    (ie-aux n 1)))

(check-expect (iter-expt 2 3 ) 8)
(check-expect (iter-expt 2 0 ) 1)
(check-expect (iter-expt 10 4 ) 10000)

#|
Notice anything different???
This is called tail recursion. Enter as a comment in your main file an explanation of what you
observed and how it differs from the linear recursive solution. 

In stepper, the iter-expt function does not pile up stacks like the previous slow-expt or fast-expt.
It can evaluate the product right away, and then call recursively. The linear recursive functions
have to recursively call and then store them / wait until the end when they can evaluate everything.
Like slow-expt builds up (*b (slow-expt b (slow-expt b (slow expt... )))) and can not evaluate
until it get to the last/most inwards slow-expt. 


|#




