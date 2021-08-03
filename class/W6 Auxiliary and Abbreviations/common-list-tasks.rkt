;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname common-list-tasks) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; list abbreviations
;; list

;; a list-of-numbers [LON] is either
;; -- empty
;; -- (cons number LON)
#|
(define (lon-fun alon)
  (cond [(empty? alon) ...]
        [(cons? alon) 
         ... (first alon) ;number
         ... (lon-fun (rest alon))
         ]))
|#


;; simple list: reducing/summarize/fold a list to a single value
;; sum: LON --> number
(check-expect (sum empty) 0)
(check-expect (sum (list 1 2 3 4 5)) 15)
(define (sum alon)
  (cond [(empty? alon) 0]
        [else (+ (first alon)
                 (sum (rest alon)))]))

;; functions that create new lists
;; simple list: filtering out some parts of a list
;; only-even: LON --> LON
(check-expect (only-even empty) empty)
(check-expect (only-even (list 5 7 9 13)) empty)
(check-expect (only-even (list 1 2 3 4 5)) (list 2 4))
(check-expect (only-even (list 2 3 4 5)) (list 2 4))
(check-expect (only-even (list 1 2 3 4 5)) (cons 2 (cons 4 empty)))

(define (only-even alon)
  (cond [(empty? alon) empty]
        [(cons? alon) 
         (cond [(= (modulo (first alon) 2) 0)
                (cons (first alon) (only-even (rest alon)))] 
               [else (only-even (rest alon))]
         )]))


;; simple list: mapping one list into a different list
;; to-strings: LON --> LOS
(check-expect (to-strings empty) empty)
(check-expect (to-strings (list 1 2 3)) (list "1" "2" "3"))
(define (to-strings alon)
  (cond [(empty? alon) empty]
        [(cons? alon) 
        (cons (number->string (first alon)) (to-strings (rest alon)))
      
         ]))

