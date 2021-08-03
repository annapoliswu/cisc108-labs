;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname fa3-map) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A [ListOf Number] is
;; -- empty
;; -- (cons Number [ListOf Number])
#|TEMPLATE
(define (lon-fun alon)
  (cond [(empty? alon) ...]
        [(cons? alon) 
         ... (first alon) ;a Number
         ... (lon-fun (rest alon))  ]))
|#  
(define X (list 3 4 87 6  23 7 8 0 3 45 67 2 24 5 7 5))

;; map-square : [ListOf Number] --> [ListOf Number]
;; consume a list of numbers and produces a list of the squares of each 
(define (map-square alon)
  (cond [(empty? alon) empty]
        [(cons? alon) 
         (cons (sqr (first alon)) ;a Number
               (map-square (rest alon)))]))

;; map-sin: [ListOf Number] --> [ListOf Number]
;; consume a list of numbers and produce a list of the sines of each
(define (map-sin alon)
  (cond [(empty? alon) empty]
        [(cons? alon) 
         (cons (sin (first alon)) ;a Number
               (map-sin (rest alon)))]))

;; map1 : 

;;test by rewriting
;(define (map-square.v2 alon) 
;(define (map-sin.v2 alon) 

(define (map-1 fun alon )
  (cond [(empty? alon) empty]
        [(cons? alon) 
         (cons (fun (first alon)) ;a Number
               (map-1 fun (rest alon)))]))

(define (map-square.v2 alon) (map-1 sqr alon))

(check-expect (map-square.v2 X) (map-square X))

;^built innnnnn
; (map sqr LIST) takes sqr and applies to entire list





(define-struct snake (name length color))
;; make-snake: String Number ColorString --> Snake
;; field interpretation: 
;;  name (String): the snake's name, 
;;  length (Number): length in inches, 
;;  color (ColorString): the predominant color of the snake,
;;                      using 2htdp/image color names
(define S1 (make-snake "Bill" 20 "green")) ; example
(define S2 (make-snake "Kathy" 15 "pink")) ; example
(define S3 (make-snake "Henri" 30 "black")) ; example
#| TEMPLATE
(define (snake-fun asnake)
  ... (snake-name asnake)   ;string
  ... (snake-length asnake) ;number
  ... (snake-color asnake) );color-string
|#

;; a [ListOf Snake] is either
;; -- empty
;; -- (cons Snake [ListOf Snake])
#| TEMPLATE
(define (los-fun alos)
  (cond [(empty? alos) ...]
        [(cons? alos) 
         ... (snake-fun (first alos)) ;fn on a Snake
         ... (los-fun (rest alos))    ]))
|#
(define LOS1 (list S1 S2 S3 (make-snake "Susan" 5 "white")))

;; list-names : [ListOf Snake] --> [ListOf String]
;; generate a list of all the snake names
(define (list-names alos)
  (cond [(empty? alos) empty]
        [(cons? alos) 
         (cons (snake-name (first alos)) ;a Snake
               (list-names (rest alos)))]))
;; list-lengths : [ListOf Snake] --> [ListOf Number]
;; generate list of all snake lengths
#;(define (list-lengths alos)
  (cond [(empty? alos) empty]
        [(cons? alos) 
         (cons (snake-length (first alos)) ;a Snake
               (list-list-lengthsnames (rest alos)))]))


(define (list-names.v2 alos)   (map snake-name alos))
;list-lengths.v2





