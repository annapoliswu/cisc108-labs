;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname animal-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ITEMIZATIONS OF MULTIPLE TYPES
;; simple itemization example

;; A ColorString is one of
;; -- "red",
;; -- "blue", or
;; any other string explicitly listed in the
;; Color Database for the Racket Graphics Toolkit.

(define-struct snake (name length color))
;; make-snake: String Number ColorString --> Snake
;; field interpretation: 
;; name (String): the snake's name, 
;; length (Number):length in inches, 
;; color (String): the predominant color of the snake.
(define BILL (make-snake "Bill" 20 "green")) ; example
(define PAM (make-snake "Pam" 15 "white")) ; example
(define FRED (make-snake "Fred" 10 "black")) ; example
#| TEMPLATE
(define (snake-fun asnake)
  ... (snake-name asnake)   ;String
  ... (snake-length asnake) ;Number
  ... (snake-color asnake)  ;ColorString
  )
|#

;; snake-cage-fits? : Snake Number --> Boolean
;; check if length of snake <= cage size in inches
(check-expect (snake-cage-fits? BILL 10) false)
(check-expect (snake-cage-fits? BILL 20) true)

(define (snake-cage-fits? asnake size)
  (cond [(<= (snake-length asnake) size) true]
        [else false]))

;; a List-of-Snakes [LOSnake] is either
;; -- empty
;; -- (cons Snake LOSnake)
#| TEMPLATE
(define (losnake-fun alosnake)
  (cond [(empty? alosnake) ...]
        [(cons? alosnake) 
         ... (snake-fun (first alosnake)) ;a fn on a Snake
         ... (losnake-fun (rest alosnake)) ;recursive call
         ]))
|#
(define LOSNAKES1 (cons BILL (cons PAM (cons FRED empty))))

;; color-filter : LOSnake ColorString --> LOSnake
;; get only the matching color snakes
(check-expect (color-filter empty "green") empty)
(check-expect (color-filter LOSNAKES1 "green") (cons BILL empty))
(check-expect (color-filter LOSNAKES1 "purple") empty)

(define (color-filter alosnake acs)
  (cond [(empty? alosnake) empty]
        [(cons? alosnake) 
         (cond [(string=? acs 
                          (snake-color (first alosnake))) ;if same color
                (cons (first alosnake)
                      (color-filter (rest alosnake) acs))]
               [else ;throw it away
                (color-filter (rest alosnake) acs)])]))

;; names : LOSnake --> LOS
;; give back a list of the snake names
(check-expect (names empty) empty)
(check-expect (names LOSNAKES1) (cons "Bill" (cons "Pam" (cons "Fred" empty))))

(define (names alosnake)
  (cond [(empty? alosnake) empty]
        [(cons? alosnake) 
         (cons (snake-name (first alosnake)) ;a fn on a Snake
               (names (rest alosnake)))])) ;recursive call



;;=================================
(define-struct dillo (length flat?))
;; make-dillo: Number Boolean --> Dillo
;; field interpretation: 
;; length (Number): length in inches,
;; flat? (Boolean): true if armadillo is crushed flat.
(define d1 (make-dillo 6 true)) ;example
(define d2 (make-dillo 8 false))
#| TEMPLATE
(define (dillo-fun adillo)
  ... (dillo-length adillo) ;Number
  ... (dillo-flat? adillo)  ;Boolean
  )
|#

;; dillo-cage-fits? : Dillo Number --> Boolean
;; check if length of armadillo <= cage size in inches
(check-expect (dillo-cage-fits? d1 10) true)
(check-expect (dillo-cage-fits? d1 2) false)

(define (dillo-cage-fits? adillo size)
  (cond [(<= (dillo-length adillo) size) true]
        [else false]))


;;=================================
(define-struct tiger (name length sells))
;; make-tiger: String Number String --> Tiger
;; field interpretation: 
;; name (String): the tiger's name, 
;; length (Number): length in inches, 
;; sells (String): what product the tiger sells
(define t1 (make-tiger "tony" 35 "cereal"))
#| TEMPLATE
(define (tiger-fun atiger)
  ... (tiger-name atiger)   ;String
  ... (tiger-length atiger) ;Number
  ... (tiger-sells atiger)  ;String
  )
|#

;; tiger-cage-fits? : Tiger Number --> Boolean
;; check if length of tiger <= cage size in inches
(check-expect (tiger-cage-fits? t1 10) false)
(check-expect (tiger-cage-fits? t1 50) true)

(define (tiger-cage-fits? atiger size)
  (cond [(<= (tiger-length atiger) size) true]
        [else false]))


;;=================================
;; An Animal is
;; -- a Tiger,
;; -- a Dillo, or
;; -- a Snake,
;; -- a Number (representing a Tribble circumference in inches)

#|TEMPLATE
(define (animal-fun anani)
  (cond [(tiger? anani) (tiger-fun anani)]
        [(dillo? anani) (dillo-fun anani)]
        [(snake? anani) (snake-fun anani)]
        [(number? anani) (number-fun anani)])) 
|#

;; cage-fits?: Animal Number --> Boolean
;; anani (Animal): the animal we are testing
;; size (Number) : the size of the cage

(check-expect (cage-fits? 7 35) true)

(define (cage-fits? anani size)
  (cond [(tiger? anani) (tiger-cage-fits? anani size)]
        [(dillo? anani) (dillo-cage-fits? anani size)]
        [(snake? anani) (snake-cage-fits? anani size)]
        [(number? anani) (<= anani size)]))


(check-expect (cage-fits? t1 10) false)
(check-expect (cage-fits? t1 50) true)
(check-expect (cage-fits? BILL 10) false)
(check-expect (cage-fits? BILL 20) true)
(check-expect (cage-fits? d1 10) true)
(check-expect (cage-fits? d1 2) false)

;; animal-length: Animal --> Number
;; anani (animal): the animal whose length we are finding
(check-expect (animal-length BILL) 20)
(check-expect (animal-length d1) 6)
(check-expect (animal-length t1) 35)
(check-expect (animal-length 9) 9)

(define (animal-length anani)
  (cond [(tiger? anani) (tiger-length anani)]
        [(dillo? anani) (dillo-length anani)]
        [(snake? anani) (snake-length anani)]
        [(number? anani) anani]))


;; a List-of-Animals [LOA] is either
;; -- empty
;; -- (cons Animal LOA)
#| TEMPLATE
(define (loa-fun aloa)

|#
(define ANI-LIST-1 (cons BILL (cons d1 (cons t1 (cons 10 empty)))))
;; sum-lengths: LOA --> number
(check-expect (sum-lengths ANI-LIST-1) 71)
(check-expect (sum-lengths empty) 0)

(define (sum-lengths aLoA)
  (cond [(empty? aLoA)0]
        [(cons? aLoA)(+(animal-length (first aLoA))
                       (sum-lengths (rest aLoA)))]))

