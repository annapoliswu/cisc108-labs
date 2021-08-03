;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snake-tiger-dillo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; simple itemization example
;; A Color is one of
;; -- "red",
;; -- "blue", or
;; -- any other string explicitly listed in the
;;    Color Database for the Racket Graphics Toolkit.

;;----------------------------------------------
;; SNAKES
;;----------------------------------------------

(define-struct snake [name length color])
;; a Snake is a (make-snake String PosNum Color)
;; Interpretation: 
;;   String name:   is the snake's name, 
;;   PosNum length: is the length in inches, 
;;   Color  color:  is the predominant color of the snake.
(define S1 (make-snake "bill" 20 "green")) ; example
;; CONSTRUCTOR
;;   make-snake: String PosNum Color --> Snake
;; SELECTORS
;;   snake-name: Snake --> String
;;   snake-length: Snake --> PosNum
;;   snake-color: Snake --> Color
;; PREDICATE
;;   snake?: ANY --> Boolean
#| TEMPLATE
(define (snake-fun s)
 ... (snake-name s) ;String
 ... (snake-length s) ; PosNum
 ... (snake-color s) ) ;Color String
|#

;; snake-cage-fits? : Snake PosNum --> Boolean
;; does the given snake fit the cage. cage must be strictly larger!
;; PARAMETERS:
;;  Snake  s:   the snake to test
;;  PosNum size: the size of the cage, positive number in inches
(check-expect (snake-cage-fits? S1 21) true)
(check-expect (snake-cage-fits? S1 19) false)
(check-expect (snake-cage-fits? S1 20) false)
(define (snake-cage-fits? s size)
  (< (snake-length s) size))

(define SNAKE-PRICE/INCH 5) ;Snakes are $5 per inch
;; snake-price: Snake --> PosNum
;; find price at SNAKE-PRICE/INCH value of given snake
;; PARAMETERS
;;  Snake s: the snake to find the price of
(check-expect (snake-price S1) 100)
(define (snake-price s)
  (* (snake-length s) SNAKE-PRICE/INCH))

;; snake-color=?: Snake Snake --> boolean
;; determine if two snakes are the same color
;; PARAMETERS
;;  Snake s1: the first snake
;;  Snake s2: the second snake
(check-expect (snake-color=? S1 S1) true)
(check-expect (snake-color=? S1 (make-snake "bill" 20 "red")) false)
(define (snake-color=? s1 s2)
 (string=? (snake-color s1) 
           (snake-color s2)))

;; paint-snake: Snake Color --> Snake
;; paint a given snake a new color
;; PARAMETERS
;;  Snake s: a snake to paint
;;  Color c: a color to paint the snake (may be the same)
(check-expect (paint-snake S1 "blue")
              (make-snake "bill" 20 "blue"))
(define (paint-snake s c)
 (make-snake (snake-name s) ;string
             (snake-length s) ; number
             c) ;Color string
 )
  
;;----------------------------------------------
;; TIGERS
;;----------------------------------------------

(define-struct tiger [name length product price])
;; a Tiger is a (make-tiger String PosNum String PosNum)
;; INTERPRETATION....
;;  String name:    name of the tiger
;;  PosNum length:  length of tiger in inches
;;  String product: what this tiger likes to sell
;;  PosNum price:   is the price of the *tiger* in dollar
(define TONY (make-tiger "Tony" 72 "cereal" 1000000000))
(define EXXON (make-tiger "Esso" 70 "gasoline" 7689203))
;;TIGER TEMPLATE
#;
(define (tiger-fun t)
  ... (tiger-name t) ;String
  ... (tiger-length t) ;PosNum
  ... (tiger-product t) ;String
  ... (tiger-price t) ) ;PosNum



;; tiger-cage-fits?: Tiger PosNum --> Boolean
;; does the given tiger fit the cage. cage must be strictly larger!
;; PARAMETERS:
;;  Tiger  t:    the tiger to test
;;  PosNum size: the size of the cage, positive number in inches
(check-expect (tiger-cage-fits? TONY 73) #true)
(check-expect (tiger-cage-fits? TONY 11) #false)
(define (tiger-cage-fits? t size)
  (< (tiger-length t) size))


;;----------------------------------------------
;; ARMADILLOS
;;----------------------------------------------
;; length, have they been run over yet?
(define-struct dillo [length flat?])
;; a Dillo is a (make-dillo Number Boolean)
;; INTERPRETATION
;;  Number length: length of armadillo in inches
;;  Boolean flat?: has this armadillo been run over yet? True if flattened.
;; TEMPLATE
#;
(define (dillo-fun d)
  ... (dillo-length d) ;Number
  ... (dillo-flat? d) ) ;Boolean

(define D1-LIVE (make-dillo 10 #false))
(define D2-DEAD (make-dillo 15 #true))

;; dillo-cage-fits?: Dillo PosNum --> Boolean
;; does the given armadillo fit the cage. cage must be strictly larger!
;; PARAMETERS:
;;  Dillo  d:    the armadillo to test
;;  PosNum size: the size of the cage, positive number in inches
(check-expect (dillo-cage-fits? D1-LIVE 11) #true)
(check-expect (dillo-cage-fits? D2-DEAD 11) #false)
(define (dillo-cage-fits? d size)
  (< (dillo-length d) size))


(define LIVE-DILLO-PRICE 7) ; $7 for a live armadillo, any size.
;; dillo-price: Dillo --> Number
;; computes the price of an armadillo.
;; Live ones are LIVE-DILLO-PRICE, dead ones are worthless.
;; PARAMETERS
;;  Dillo d: an armadillo to price
(check-expect (dillo-price D1-LIVE) LIVE-DILLO-PRICE)
(check-expect (dillo-price D2-DEAD) 0)
(define (dillo-price d)
  (cond 
    [(dillo-flat? d) 0]
    [else LIVE-DILLO-PRICE]))











;;----------------------------------------------
;; ANIMALS: Itemization of Structs / Complex Itemization
;;----------------------------------------------

;; an Animal is either
;; -- a Snake, or
;; -- a Tiger, or
;; -- a Dillo, or
;; -- a Tribble (which is a Posnum, representing it's size in inches)
#| Animal TEMPLATE
(define (animal-fun a)
  (cond [(snake? a) (snake-fun a)] ;call a snake fn
        [(tiger? a) (tiger-fun a)] ;call a tiger fn
        [(dillo? a) (dillo-fun a)] ;call an armadillo fn
        [(number? a) ...a ])) ;a Tribble is a Number (atomic/primitive)
|#


;; cage-fits? : Animal Number --> Boolean
;; Test if a given animal fits a given cage size
;; PARAMETERS
;;  Animal a:     an animal to test
;;  PosNum size:  a cage size in inches
(check-expect (cage-fits? S1 21) #true)
(check-expect (cage-fits? S1 19) #false)
(check-expect (cage-fits? S1 20) #false)
(check-expect (cage-fits? TONY 73) #true)
(check-expect (cage-fits? TONY 11) #false)
(check-expect (cage-fits? D1-LIVE 11) #true)
(check-expect (cage-fits? D2-DEAD 11) #false)
(check-expect (cage-fits?  14 10) #false)
(check-expect (cage-fits?  14 20) #true)

(define (cage-fits? a size)
  (cond [(snake? a) (snake-cage-fits? a size)] ;call a snake fn
        [(tiger? a) (tiger-cage-fits? a size)] ;call a tiger fn
        [(dillo? a) (dillo-cage-fits? a size)] ;call an armadillo fn
        [(number? a) (< a size) ])) ;a Tribble is a Number (atomic/primitive)




;; animal-price : Animal --> Number
;; computes the price of any animal.
;; PARAMETERS
;;  Animal a: an animal to find the price of
(define TRIBBLE-PRICE 10)
(check-expect (animal-price S1) 100)
(check-expect (animal-price TONY) 1000000000)
(check-expect (animal-price D1-LIVE) LIVE-DILLO-PRICE)
(check-expect (animal-price D2-DEAD) 0)
(check-expect (animal-price 14) TRIBBLE-PRICE)

(define (animal-price a)
  (cond [(snake? a) (snake-price a)] ;call a snake fn
        [(tiger? a) (tiger-price a)] ;call a tiger fn
        [(dillo? a) (dillo-price a)] ;call an armadillo fn
        [(number? a) TRIBBLE-PRICE ])) ;a Tribble is a Number (atomic/primitive)

;; animal-name: Animal --> String
;; finds the name of any animal.
;; Armadillos are just called "some armadillo"
;; Tribbles are called "a tribble"
;; PARAMETERS
;;  Animal a: an animal to find the name of
#|
(check-expect (animal-name S1) "bill")
(check-expect (animal-name TONY) "Tony")
(check-expect (animal-name D1-LIVE) "some armadillo")
(check-expect (animal-name 10) "a tribble")
|#

(define (animal-name a)
  "")



;;----------------------------------------------
;; STORE: a struct with embedded structs
;;----------------------------------------------

(define-struct store (name ani1 ani2))
;; make-store: String Animal Animal --> Store
;; INTERPRETATION:
;;  String name: the name of the store
;;  Animal a1:   the animal in cage 1
;;  Animal a2:   the animal in cage 2.
;; [CONSTRUCTOR]
;; make-store: String Animal Animal --> Store
;; [SELECTORS]
;;   store-a1: Store -> Animal
;;   store-a2: Store -> Animal
;; [TYPE PREDICATE]
;; store?: ANY -> boolean
(define STORE1 
  (make-store "Jim's Snake Emporium"
              S1
              (make-snake "sally" 15 "white")))
(define STORE2 
  (make-store "Betty's Animal Kingdom"
              TONY
              D1-LIVE))
;; Store Template
#;(define (store-fun store)
  ... (store-name store) ;String
  ... (animal-fun (store-ani1 store)) ;Animal
  ... (animal-fun (store-ani2 store)) ) ;Animal


;; stock-value : Store --> Number
;; compute the value of all the animals in the store
;; PARAMETERS
;;  Store s: a store to compute total snake value on
;(check-expect (stock-value STORE1) (+ (snake-price S1) (snake-price (make-snake "sally" 15 "white"))))
;(check-expect (stock-value STORE2) (+ 1000000 (dillo-price D1-LIVE)))





(require 2htdp/image)
(define STORE-FONT-SIZE 50)
(define STORE-FONT-COLOR "black")
(define ANIMAL-FONT-SIZE 40)
(define ANIMAL-FONT-COLOR "blue")
;; store-sign: Store --> Image
;; creates a sign to hang over the store with the
;; name of the store, and the names of our current animals.
;(check-expect (store-sign STORE1)
;              (above (text "Jim's Snake Emporium" STORE-FONT-SIZE STORE-FONT-COLOR)
;                     (text "bill and sally" ANIMAL-FONT-SIZE ANIMAL-FONT-COLOR)))
;(check-expect (store-sign STORE2)
;              (above (text "Betty's Animal Kingdom" STORE-FONT-SIZE STORE-FONT-COLOR)
;                     (text "Tony and some armadillo" ANIMAL-FONT-SIZE ANIMAL-FONT-COLOR)))







