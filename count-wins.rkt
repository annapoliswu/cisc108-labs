;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname count-wins) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define (swim x) (* 1 x))

(define RUN 2)

(define (fun n)
 (local [(define (bat n) (swim (+ n RUN)))]
 (local [(define RUN 4)
 (define (bat n) (swim (+ n RUN)))]
 (map (lambda (x) (bat (swim x))) (list n)))))

(define (sgroup xs)
(cond [(empty? xs) empty]
      [(empty? (rest xs)) (list (list 1 (first xs)))]
      [else (if (equal? (first xs) (second (first (sgroup (rest xs)))))
                (cons (list (add1 (first (first (sgroup (rest xs)))))
                            (second (first (sgroup (rest xs)))))
                      (rest (sgroup (rest xs))))
                (cons (list 1 (first xs))
                      (sgroup (rest xs))))]))

(define (group xs)
(cond [(empty? xs) empty]
      [(empty? (rest xs)) (list (list 1 (first xs)))]
    
      [else
(local ((define result (group (rest xs))))
       (if (equal? (first xs) (second (first result)))
                (cons (list (add1 (first (first result)))
                            (second (first result)))
                      (rest result))
                (cons (list 1 (first xs))
                     result)))]))


(define list100 (build-list 100 add1))

(define-struct gnode (me? take1 take2 take3))
;; FIELDS
;; me? [Boolean]: true if it's my turn, false otherwise
;; take1 [NGST] : the tree that results if 1 stick is taken
;; take2 [NGST] : the tree that results if 2 sticks are taken
;; take3 [NGST] : the tree that results if 3 sticks are taken
;; An Outcome is either:
;; -- "WIN" [winning node]
;; -- "LOSE" [losing node]
;; A NimGameSearchTree [NGST] is either:
;; -- false [impossible state, like picking up 3 sticks from a stack of 1]
;; -- Outcome
;; -- (make-gnode Boolean NGST NGST NGST) a regular Game Node
(make-gnode true false false false)

(define TREE1 (make-gnode true (make-gnode true false false false)
                               (make-gnode true "WIN" false (make-gnode true false false "WIN"))
                               (make-gnode true false "WIN" "LOSE")))


#;(define (count-solutions aJRGT)
    (cond [(false? aJRGT) 0]
          [(node? aJRGT)
              (+ (if (solved?(node-state aJRGT)) 1 0)
              (if (false?(node-up aJRGT)) 0 (count-solutions(node-up aJRGT)))   
              (if (false?(node-down aJRGT)) 0 (count-solutions(node-down aJRGT)))
              (if (false?(node-left aJRGT)) 0 (count-solutions(node-left aJRGT)))
              (if (false?(node-right aJRGT)) 0 (count-solutions(node-right aJRGT)))
        )]))



(define (count-wins aNGST)
  ( cond [(false? aNGST) 0]
         [(string? aNGST) 
            (if (string=? aNGST "WIN") 1 0)]
         [(gnode? aNGST)
            (+
               (count-wins (gnode-take1 aNGST))
               (count-wins (gnode-take2 aNGST))
               (count-wins (gnode-take3 aNGST))
           )]
  
         ))
        

   

