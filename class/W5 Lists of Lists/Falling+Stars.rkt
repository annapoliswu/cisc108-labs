;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Falling+Stars) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; CONSTANTS                                                                                

(define WIDTH 500) ;scene width in pixels
(define HEIGHT 400) ;scene height in pixels
(define STAR-MAX 50) ;largest star outer width in pixels
(define STAR-MIN 5) ;smallest star inner width in pixels
(define RGB-MAX 255) ;largest RGB (Red/Blue/Green/Alpha) value
(define ALPHA-MIN 100) ;smallest Alpha channel (transparency) value
(define MAX-POINTS 20) ;the most points on a star
(define MAX-FALL-RATE 10) ;the max vertical descent in pixels per tick
(define MIN-FALL-RATE 0.5) ;the min vertical descent in pixels per tick

(define POSN1 (make-posn 0 0))

;; delta-posn-y: Posn Number --> Posn
;; PARAMETERS
;;  aposn (Posn): the position to change
;;  delta-y (Number): the change in the y value for the posn
(check-expect (delta-posn-y (make-posn 0 10) 100)
              (make-posn 0 110))
(define (delta-posn-y aposn delta-y)
  (make-posn (posn-x aposn)
             (+ (posn-y aposn) delta-y)))
                   

;; a FallingStar (FStar) is a structure
(define-struct fstar (points in-r out-r color posn rate))
;; make-fstar: Integer Number Number Color Posn Number--> FStar
;; FIELDS interpretation
;;   points (Integer): number of points on star
;;   in-r (Number): the inner radius of the star
;;   out-r (Number): the outer radius of the star
;;   color (Color): the color of the star (built-in image library
;;                  struct of Red-Blue-Green-Alpha values)
;;   posn (Posn): the x,y position of the star
;;   rate (Number): the falling rate of the star pixels per tick
#| TEMPLATE
(define (fstar-fun afstar)
  ... (fstar-points afstar) ;Integer
  ... (fstar-in-r afstar)   ;Number
  ... (fstar-out-r afstar)  ;Number
  ... (fstar-color afstar)  ;Color
  ... (fstar-posn afstar)   ;Posn
  ... (fstar-rate afstar) ) ;Number
|#
(define REDSTAR
  (make-fstar 5
              STAR-MIN
              STAR-MAX
              (make-color RGB-MAX 0 0 (floor (* .7 RGB-MAX)));red at 70% alpha
              (make-posn 100 100)
              2))

;; fstar->image: FStar --> Image
;; PARAMETERS
;;   afstar (Fstar): the star to create an image of.
;; creates an image of the star passed in.
;; Note that it does NOT place the star on top of anything.
(define (fstar->image afstar)
  (radial-star
   (fstar-points afstar) ;Integer
   (fstar-in-r afstar)   ;Number
   (fstar-out-r afstar)  ;Number
   "solid"
   (fstar-color afstar)))  ;Color

;; drop-fstar: FStar --> FStar
;; PARAMETERS
;;  afstar (FStar): star to fall
;; makes a new falling star same as the old one
;; but after falling fstar-rate pixels (change in posn-y)
(check-expect (drop-fstar REDSTAR)
              (make-fstar 5
                          STAR-MIN
                          STAR-MAX
                          (make-color RGB-MAX 0 0 
                                      (floor (* .7 RGB-MAX)));red at 70% alpha
                          (make-posn 100 102)
                          2))
(define (drop-fstar afstar) 
  (make-fstar (fstar-points afstar) ;Integer
              (fstar-in-r afstar)   ;Number
              (fstar-out-r afstar)  ;Number
              (fstar-color afstar)  ;Color
              (delta-posn-y (fstar-posn afstar) (fstar-rate afstar))  ;Posn
              (fstar-rate afstar) )) ;Number
  
;; make-random-fstar: Number --> FStar
;; PARAMETERS
;;  y (Number): the y axis position of the new random FStar. 
;;  Typically 0. The other parameters of the star are randomized.
(define (make-random-fstar y)
  (make-fstar 
   (+ 2 (random MAX-POINTS)) ;radial star has min of 2 points
   (+ STAR-MIN (random STAR-MAX))
   (+ STAR-MIN (random STAR-MAX))
   (make-color
    (random RGB-MAX)
    (random RGB-MAX)
    (random RGB-MAX)
    (+ ALPHA-MIN (random (- RGB-MAX ALPHA-MIN)))) ;alpha range [ALPHA-MIN --> RGB-MAX]
   (make-posn (random WIDTH) y)
   (+ MIN-FALL-RATE (random MAX-FALL-RATE )))) ;Number


;; a List-of-fstars [LOFS] is either
;; -- empty
;; -- (cons FStar LOFS)
#|TEMPLATE
(define (lofs-fun alofs)
  (cond 
    [(empty? alofs) ...]
    [(cons? alofs)
     ... (fstar-fun (first alofs)) ;a FStar
     ... (lofs-fun (rest alofs)) ])) 
|#  

;; make-n-random-fstars: NatNum --> LOFS
;; PARAMETERS
;;  n (NatNum): the number of FStars to create on the list.
;; creates a list of n random FStars.
;(check-expect (make-n-random-fstars 0) ...)
;(check-expect (make-n-random-fstars 10) ...)
              

;; RENDER-SCENE
;; Draw every star to screen [the VIEW]
;; render-scene: LOFS --> image
#;(check-expect (render-scene empty) (empty-scene WIDTH HEIGHT))
#;(check-expect (render-scene (cons REDSTAR empty))
              (place-image
               (fstar->image REDSTAR)
               (posn-x (fstar-posn REDSTAR))
               (posn-y (fstar-posn REDSTAR))
               (empty-scene WIDTH HEIGHT)))
(define (render-scene afstar)
  (place-image
               (fstar->image afstar)
               (posn-x (fstar-posn afstar))
               (posn-y (fstar-posn afstar))
               (empty-scene WIDTH HEIGHT)))

;; TICK HANDLER
;; every star falls at specified rate
;; handle-tick: LOFS --> LOFS
#;(check-expect (handle-tick empty) empty)
#;(check-expect (handle-tick (cons REDSTAR empty))
              (cons (drop-fstar REDSTAR) empty))

(define (handle-tick  alofs)
  (cond [(empty? alofs) empty]
        [(cons? alofs) (cond[(<(fstar-posn (first alofs)) HEIGHT)
                       (cons( drop-fstar (first alofs)))
                       (handle-tick (rest alofs))]
                       [else (handle-tick(rest alofs))])]))

;; KEY HANDLER
;; new star on spacebar 
;; handle-key: LoFS KeyEvent --> FS
(define (handle-key alofs akey)
  (cond [(key=? akey " ") (cons(make-random-fstar 0)alofs]
        [else alofs]))

;; MAIN
;; Calls Big Bang on initial world
(define (main init-world)
  (big-bang init-world 
            (on-tick handle-tick) 
            (on-draw render-scene)
            (on-key handle-key)
            ))

;; TODO: convert this to a list of falling stars
(main (cons REDSTAR empty))