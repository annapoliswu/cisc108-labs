;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname store) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; simple itemization example
;; A Color is one of
;; -- "red",
;; -- "blue", or
;; -- any other string explicitly listed in the
;;    Color Database for the Racket Graphics Toolkit.

(define-struct snake (name length color))
;; make-snake: string number Color --> Snake
;; Interpretation: 
;;   name (string) is the snake's name, 
;;   length (number) is the length in inches, 
;;   color (Color) is the predominant color of the snake.
(define S1 (make-snake "bill" 20 "green")) ; example
;; CONSTRUCTOR
;;   make-snake: string number Color --> Snake
;; SELECTORS
;;   snake-name: Snake --> string
;;   snake-length: Snake --> number
;;   snake-color: Snake --> Color
;; PREDICATE
;;   snake?: ANY --> boolean
#| TEMPLATE
(define (snake-fun as)
 ... (snake-name as) ;string
 ... (snake-length as) ; number
 ... (snake-color as) ;Color string
 )
|#

;; cage-fits? : Snake number --> boolean
;; does the given snake fit the cage. cage must be strictly larger!
;; PARAMETERS:
;;  as (Snake): the snake to test
;;  size (number): the size of the cage, positive number in inches
(check-expect (cage-fits? S1 21) true)
(check-expect (cage-fits? S1 19) false)
(check-expect (cage-fits? S1 20) false)
(define (cage-fits? as size)
  (< (snake-length as) size))

(define PRICE/INCH 5) ;$5 per inch
;; price-snake: Snake --> number
;; find price at PRICE/INCH value of given snake
;; PARAMETERS
;;  as (Snake): the snake to price
(check-expect (price-snake S1) 100)
(define (price-snake as)
  (* (snake-length as) PRICE/INCH))

;; snake-color=?: Snake Snake --> boolean
;; determine if two snakes are the same color
;; PARAMETERS
;;  as1 (Snake): the first snake
;;  as2 (Snake): the second snake
(check-expect (snake-color=? S1 S1) true)
(check-expect (snake-color=? S1 (make-snake "bill" 20 "red")) false)
(define (snake-color=? as1 as2)
  (string=? (snake-color as1) 
            (snake-color as2)))

;; paint-snake: Snake Color --> Snake
;; paint a given snake a new color
;; PARAMETERS
;;  as (Snake): a snake to paint
;;  c (Color): a color to paint the snake (may be the same)
(check-expect (paint-snake S1 "blue")
              (make-snake "bill" 20 "blue"))
(define (paint-snake as c)
  (make-snake (snake-name as) ;string
              (snake-length as) ; number
              c) ;Color string
  )

;; EMBEDDED STRUCTURES

(define-struct store (name snake1 snake2))
;; make-store: String Snake Snake --> Store
;; INTERPRETATION:
;;  name (string) is the name of the store
;;  snake1 (Snake) is the snake in cage 1
;;  snake2 (Snake) is the snake in cage 2.
;; [CONSTRUCTOR]
;; make-store: String Snake Snake --> Store
;; [SELECTORS]
;;   store-snake1: Store -> Snake
;;   store-snake2: Store -> Snake
;; [TYPE PREDICATE]
;; store?: any -> boolean
(define STORE1 
  (make-store "Jim's Snake Emporium"
              S1
              (make-snake "sally" 15 "white")))
#|
(define (store-fun astore)
  ... (store-name astore) ;a string
  ... (snake-fun (store-snake1 astore)) ; fn on a Snake
  ... (snake-fun (store-snake2 astore)) ; fn on a Snake
  )
|#

;; stock-value: store --> number
;; compute the value of all the snakes in the store
;; at the current PRICE/INCH
;; PARAMETERS
;;  astore (Store): a store to compute total snake value on
;(check-expect (stock-value STORE1) 175)


















;; paint-all: store Color --> store
;; Someone has gotten careless and painted the whole store,
;; including the snakes. Consumes a store and a Color and produces
;; a store where all of the snakes are now of that Color.


























(require 2htdp/image)
;; store-sign: Store --> image
;; creates a sign to hang over the store with the
;; name of the store, and the names of our current snakes.
(check-expect (store-sign STORE1)
              (above (text "Jim's Snake Emporium" 50 "black")
                     (text "bill and sally" 40 "blue")))

(define (store-sign astore)
  (above (text (store-name astore) 50 "black")
         (text (string-append (snake-name (store-snake1 astore))
                              " and "
                              (snake-name (store-snake2 astore)))
               40
               "blue")))

;; Test yourself: can you change the sign so that it displays 
;; the price of each snake along with the name?



























;; ITEMIZED STRUCTURES

(define-struct bird (wingspan color carnivore?))
;; make-bird: number Color boolean --> bird
;; interp. A bird has a wingspan in number of inches,
;;  color, and carnivore? is true if the bird eats meat

;; Write a function, bird-price, that computes the price
;; for a bird.  Your function must use at least two of the bird's properties.








;; rarity-rating is one of:
;; -- "common"
;; -- "special"
;; -- "rare"
;; -- "ooak"
(define-struct fish (fins color rarity))
;; make-fish: number Color rarity-rating
;; interp. A fish has a number of fins, a color,
;;  and a rarity rating.


;; Write a function, fish-price, that computes the price
;; for a bird.  Your function must use at least two of the fish's properties.



;; a pet is one of:
;; -- snake
;; -- bird
;; -- fish





;; pet-price: pet --> number
;; Consumes a pet and produces the price for the pet
;; -- we will complete this together



;; fix the earlier definition of store to contain two pets

;; change the stock-value to use our new pet-price function

;; write three functions, snake-noise, bird-noise, and fish-noise
;; that all produce strings

;; write a new version of the store-sign that uses a pet-noise
;; function to display the name of the store and the sounds that
;; can be heard from outside the store