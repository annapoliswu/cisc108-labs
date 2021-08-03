;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname isort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A List-of-Numbers [LON] is either
;; -- empty, or
;; -- (cons Number LON)
#|TEMPLATE
(define (lon-fun alon)
  (cond [(empty? alon) ...]
        [(cons? alon) 
         ... (first alon) ;a Number
         ... (lon-fun (rest alon))
         ]))
|#


;insertion sort most efficient when already sorted/ close to

;; isort : LON --> LON
;; computes the list with all the elements
;; of the input BUT in increasing numerical order

(define (isort alon)
  (cond [(empty? alon) empty]
        [(cons? alon) 
         (insert (first alon)(isort (rest alon)))         
         ]))

;processing right to left

(check-expect (isort '()) empty)
(check-expect (isort '(1 2 3)) '(1 2 3))
(check-expect (isort '(3 2 1)) '(1 2 3))
(check-expect (isort '(2 3 1)) '(1 2 3))

;compare 
;consumes : Number -> boolean

;; insert : Number LON --> LON
;; Consumes
;;   Number n, the number to insert
;;   LON alon, a SORTED list of numbers
;; Produces a sorted list of numbers

(define (insert n alon)
  (cond [(empty? alon) (cons n empty) ]
        [(cons? alon)
         (cond[( < n (first alon)) (cons n alon)  ]
              [else (cons (first alon) (insert n (rest alon)))]
         )
]))


(check-expect (insert 6 '()) '(6))
(check-expect (insert 6 '(7)) '(6 7))
(check-expect (insert 6 '(5)) '(5 6))
(check-expect (insert 6 '(1 9)) '(1 6 9))

;Generative recursion: list sort w/ tree


;quick sort

#;(define (qsort alon)
  (cond [(empty? alon) '()]
        [else
  (local [(define pivot (first alon))
                   (define smaller ())])]))
;^Basically define in local a bigger and smaller list, then sort that and append. 



         