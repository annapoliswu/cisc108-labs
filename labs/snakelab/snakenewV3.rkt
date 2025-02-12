;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snakenewV3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require srfi/1)

;NAMES: Zihan (Anna) Wu & Debra Lymon

;---write function  that chops off the lower half of a list
;---use function in shrinkingfood thing

;---rewrite snake with a color option
;---insert color parameter into all make-snakes

;rewrite end-game to test for color

;----change on-which-food? to return foodtype  instead of string
;----make a function to remove the food that the snake consumed -> found this online

;---make a generate random food function. (input type )

;rewrite render-end-game to print correct image on endgame
        

(define BACKGROUND (empty-scene 600 600))
;(define RADIUS 4)
;(define DIAMETER (* 2 RADIUS))
;(define SEGMENT (circle RADIUS  "solid" "red"))


;(define FOODPIECE (circle RADIUS  "solid" "green"))

;Food is :
(define-struct food (posn type))


(define (random-food type)
  (local ((define (food-hits-wall? afood)
  (local ((define x (posn-x (food-posn afood)))
          (define y (posn-y  (food-posn afood))))
  (or
           (and (> x 200)(< x 400)(> y 298)(< y 302))
           (and (> y 200)(< y 400)(> x 298)(< x 302))
           (and (> x 0)(< x 200) (> y 98)(< y 102))
           (and (> x 400)(< x 600)(> y 498) (< y 502))
           (and (> y 0)(< y 200)(> x 498) (< x 502))
           (and (> y 400)(< y 600)(> x 98)(< x 102))
           (and (> x 250)(< x 350)(> y 250)(< y 350)))
          ))
          (define randfood  (make-food (make-posn (- (* (random 0 75 )8)4)(- (* (random 0 75 )8)4)) type)))
    (if (food-hits-wall? randfood) (random-food type) randfood))
  )





;8 * something - 4
(define foodlist1 (list (make-food (make-posn 76 76) "regular" )(make-food (make-posn 156 156)"shrinking" )(make-food (make-posn 156 300) "jumping" )))

;add listoffood and difficulty to world and add size to either world or snake
; change world and snake definitions

(define (draw-food lof bg radius)
  (cond [(empty? lof) bg]
        [(string=? "regular" (food-type (first lof)))
         (place-image (circle radius "solid" "green")
          (posn-x (food-posn (first lof))) (posn-y (food-posn (first lof))) (draw-food (rest lof) bg radius))]
       [(string=? "shrinking" (food-type (first lof)))
         (place-image (circle radius "solid" "pink")
           (posn-x (food-posn (first lof))) (posn-y (food-posn (first lof))) (draw-food (rest lof) bg radius))]
       [(string=? "jumping" (food-type (first lof)))
         (place-image (circle radius "solid" "purple")
           (posn-x (food-posn (first lof))) (posn-y (food-posn (first lof))) (draw-food (rest lof) bg radius))]
    #|   [(string=? "teleport" (food-type (first lof)))
        (place-image (circle radius "solid" "blue")
          (posn-x (teleportfood-posn2 (first lof)) ) (posn-y (teleportfood-posn2 (first lof)))
         (place-image (circle radius "solid" "blue")
          (posn-x (teleportfood-posn1 (first lof)) ) (posn-y (teleportfood-posn1 (first lof))) (draw-food (rest lof) bg radius)))]
|#
       ))
;consumes snake alof 
; returns food type
(define (on-which-food? w)
  (local
((define (snake-on-which-food? asnake alof )
  (cond [(empty? alof) (make-food (make-posn 0 0 ) "")]
        [(and ( = (posn-x(food-posn (first alof))) (posn-x(first (snake-segments asnake)))) ( = (posn-y(food-posn (first alof))) (posn-y(first (snake-segments asnake)))))
         (first alof)]
        [else (snake-on-which-food? asnake (rest alof) ) ]
        )))
 (snake-on-which-food? (snakeworld-snake w) (snakeworld-lof w) )
 ))
          



;A Direction is
; -"up" , "down" , "left" ,or "right" 

;A Segment is
(define seg1 (make-posn 20 20 ))

;A Snake is
(define-struct snake(segments direction radius color))
(define snake1 (make-snake (list seg1) "right" 4 "red"))
(define snake2 (make-snake (list (make-posn 20 20) (make-posn 30 20)(make-posn 40 20)(make-posn 50 20 )(make-posn 60 20 ) (make-posn 70 20 )(make-posn 80 20 )) "right" 4 "red"))

;A SnakeWorld is
(define-struct snakeworld (snake level score lof difficulty ))
(define sw1 (make-snakeworld snake1 1 0 empty 2))
(define sw2 (make-snakeworld snake2 1 0 foodlist1 1))


;remove-last : List -> List
; consumes : List alist
; produces : the list with the last item removed
(define (remove-last alist)
  (cond
        [(empty? alist) empty]
        [(empty? (rest alist)) empty]
        [else (cons (first alist) (remove-last (rest alist)))])
        )

;remove-half: returns first half of a list

(define (remove-half lst)
  (cond [(= (modulo (length lst) 2) 0)
         (take (drop lst 0) (- (/ (length lst) 2) 0))]
        [else
         (take (drop lst 0) (- (/ (+(length lst)1) 2) 0))]
  ))



(define (remove-item x alist)
  (cond [(empty? alist) '()]
        [(eqv? x (first alist))
            (remove-item x (rest alist))]
        [else (cons (first alist) (remove-item x (rest alist)))]
))



;draw-snake : ListofSegments Image -> Image
; consumes : List los, Image bg
; produces : an image of the full snake placed in the the given background image

(define (draw-snake asnake background)
  ( local ((define (draw-snake-helper los bg radius color) ;CHANGE COLOR HERE
             (cond [(empty? los) bg]
                   [else (place-image (circle radius "solid" color)  (posn-x (first los)) (posn-y (first los)) (draw-snake-helper(rest los) bg radius color) )]
                   )))
     (draw-snake-helper (snake-segments asnake) background (snake-radius asnake) (snake-color asnake))
     ))

;render-scene : SnakeWorld -> Image
; consumes : SnakeWorld w
; produces : the game screen / image of current SnakeWorld
(define (render-scene w)
  (above/align "left"
         (cond
           [(= (snakeworld-difficulty w) 1)(draw-snake (snakeworld-snake w) (draw-food (snakeworld-lof w) BACKGROUND (snake-radius (snakeworld-snake w))))]
           [(= (snakeworld-difficulty w) 2)
            (place-image (rectangle 200  8 "solid" "black") 300 300
            (place-image (rectangle 8 200 "solid" "black") 300 300
        (draw-snake (snakeworld-snake w)  (draw-food (snakeworld-lof w) BACKGROUND (snake-radius (snakeworld-snake w))) ) ))]
           [(= (snakeworld-difficulty w) 3)
            (place-image (rectangle 200 8 "solid" "black") 100 100
            (place-image (rectangle 200 8 "solid" "black") 500 500
            (place-image (rectangle 8 200 "solid" "black") 500 100
            (place-image (rectangle 8 200 "solid" "black") 100 500
            (place-image (rectangle 100 100 "solid" "black") 300 300
         (draw-snake (snakeworld-snake w) (draw-food (snakeworld-lof w) BACKGROUND (snake-radius (snakeworld-snake w))) ) )))))])
        (text/font (string-append "LEVEL: "(number->string(snakeworld-difficulty w))"     " "SCORE: "(number->string(snakeworld-score w))) 20 "black" "Gill Sans" 'swiss 'normal 'bold #f)             
   ))

;move-snake : Snake Direction -> Snake 
; consumes : Snake asnake, Direction 
; produces : a new snake with and moved in the specified direction
(define (move-snake asnake direction)
  (local ((define x (posn-x(first (snake-segments asnake))))
          (define y (posn-y(first (snake-segments asnake))))
          (define radius (snake-radius asnake))
          (define diameter (* 2 radius))
          (define color (snake-color asnake)))
  (cond 
    [(string=? "right" direction)(make-snake (cons (make-posn (+ x diameter) y) (remove-last (snake-segments asnake)))"right" radius color)]
    [(string=? "left" direction) (make-snake(cons (make-posn (- x diameter) y) (remove-last (snake-segments asnake)) )"left" radius color)]
    [(string=? "up" direction) (make-snake (cons (make-posn x (- y diameter) ) (remove-last (snake-segments asnake)) ) "up" radius color)]
    [(string=? "down" direction) (make-snake (cons (make-posn x (+ y diameter) )  (remove-last (snake-segments asnake)) ) "down" radius color) ])
    ))


;handle-key: WormWorld -> WormWorld 
(define (handle-key w akey)
  (local ((define (move-sw w direction)(make-snakeworld (move-snake (snakeworld-snake w) direction) (snakeworld-level w) (snakeworld-score w) (snakeworld-lof w) (snakeworld-difficulty w)) ))
    
  (cond [(and (key=? akey "left") (and (not (string=? (snake-direction(snakeworld-snake w)) "left")) (if (= (length (snake-segments (snakeworld-snake w))) 1) true (not(string=? (snake-direction(snakeworld-snake w)) "right")) )))
            (move-sw w "left")]
        [(and(key=? akey "right") (and (not (string=? (snake-direction(snakeworld-snake w)) "right")) (if (= (length (snake-segments (snakeworld-snake w))) 1) true (not(string=? (snake-direction(snakeworld-snake w)) "left")) )))
            (move-sw w "right")]
        [(and(key=? akey "up") (and (not (string=? (snake-direction(snakeworld-snake w)) "up")) (if (= (length (snake-segments (snakeworld-snake w))) 1) true (not(string=? (snake-direction(snakeworld-snake w)) "down")) )))
            (move-sw w "up")]
        [(and(key=? akey "down")(and (not (string=? (snake-direction(snakeworld-snake w)) "down")) (if (= (length (snake-segments (snakeworld-snake w))) 1) true (not(string=? (snake-direction(snakeworld-snake w)) "up")) )))
            (move-sw w "down")]
        [(key=? akey "1")
         (make-snakeworld (make-snake (list (make-posn 20 20)) "right" 4 (snake-color (snakeworld-snake w)))  0 0 (snakeworld-lof w) 1)]
         [(key=? akey "2")
          (make-snakeworld (make-snake (list (make-posn 20 20)) "right" 4 (snake-color (snakeworld-snake w)))  0 0 (snakeworld-lof w) 2)] 
          [(key=? akey "3")
          (make-snakeworld (make-snake (list (make-posn 20 20)) "right" 4 (snake-color (snakeworld-snake w)))  0 0 (snakeworld-lof w) 3)]
        
        [else w] 
        )))


;tick handler must not only move the worm; in addition it must manage the eating process and the creation of new food.

;food and increase snake seg goes here??
(define (handle-tick w)
  (local ((define asnake (snakeworld-snake w))
          (define x (posn-x(first (snake-segments asnake))))
          (define y (posn-y(first (snake-segments asnake))))
          (define radius (snake-radius asnake))
          (define diameter (* 2 radius))
          (define direction (snake-direction(snakeworld-snake w)))
         )
    
  (cond [(string=? "" (food-type(on-which-food? w))) (make-snakeworld (move-snake (snakeworld-snake w) direction) (snakeworld-level w) (snakeworld-score w) (snakeworld-lof w) (snakeworld-difficulty w))]
        [(string=? "regular" (food-type(on-which-food? w)))
         (make-snakeworld 
        (cond 
          [(string=? "right" direction)(make-snake (cons (make-posn (+ x diameter) y)(snake-segments asnake))"right" radius "red")]
          [(string=? "left" direction) (make-snake(cons (make-posn (- x diameter) y)(snake-segments asnake))"left" radius "red")]
          [(string=? "up" direction) (make-snake (cons (make-posn x (- y diameter) )(snake-segments asnake)) "up" radius "red")]
          [(string=? "down" direction) (make-snake (cons (make-posn x (+ y diameter) )(snake-segments asnake)) "down" radius "red") ])
        (snakeworld-level w) (+(snakeworld-score w) 10) (cons (random-food "regular")(remove-item (on-which-food? w)(snakeworld-lof w))) (snakeworld-difficulty w))
         ]
 ;ADD COLOR
 [(string=? "shrinking" (food-type (on-which-food? w))) 
         (make-snakeworld 
        (cond 
          [(string=? "right" direction)(make-snake (remove-half(snake-segments asnake))"right" radius "red")]
          [(string=? "left" direction) (make-snake(remove-half(snake-segments asnake))"left" radius "red")]
          [(string=? "up" direction) (make-snake (remove-half(snake-segments asnake)) "up" radius "red")]
          [(string=? "down" direction) (make-snake (remove-half(snake-segments asnake)) "down" radius "red") ])
        (snakeworld-level w) (+(snakeworld-score w) 10) (cons (random-food "shrinking")(remove-item (on-which-food? w)(snakeworld-lof w))) (snakeworld-difficulty w))
         ]
  [(string=? "jumping" (food-type (on-which-food? w))) 
         (make-snakeworld 
        (cond 
          [(string=? "right" direction)(make-snake (snake-segments asnake)"right" radius "black")]
          [(string=? "left" direction) (make-snake(snake-segments asnake)"left" radius "black")]
          [(string=? "up" direction) (make-snake (snake-segments asnake) "up" radius "black")]
          [(string=? "down" direction) (make-snake (snake-segments asnake) "down" radius "black") ])
        (snakeworld-level w) (+(snakeworld-score w) 10) (cons (random-food "jumping")(remove-item (on-which-food? w)(snakeworld-lof w))) (snakeworld-difficulty w))
         ]
        )
    ))

#|
(define (handle-tick w)
   (local ((define x (posn-x(first (snake-segments (snakeworld-snake w)))))
          (define y (posn-y(first (snake-segments (snakeworld-snake w)))))
          (define radius (snake-radius (snakeworld-snake w)))
          (define diameter (* 2 radius)))
  (cond 
    [(string=? "right" (snake-direction (snakeworld-snake w)))(make-snakeworld (make-snake (cons (make-posn (+ x diameter) y) (remove-last (snake-segments(snakeworld-snake w))))"right" radius)(snakeworld-level w) (snakeworld-score w)) ]
    [(string=? "left" (snake-direction (snakeworld-snake w))) (make-snakeworld (make-snake(cons (make-posn (- x diameter) y) (remove-last (snake-segments (snakeworld-snake w))) )"left" radius)(snakeworld-level w) (snakeworld-score w))]
    [(string=? "up" (snake-direction (snakeworld-snake w))) (make-snakeworld (make-snake (cons (make-posn x (- y diameter) ) (remove-last (snake-segments (snakeworld-snake w))) ) "up" radius)(snakeworld-level w) (snakeworld-score w))]
    [(string=? "down" (snake-direction (snakeworld-snake w))) (make-snakeworld (make-snake (cons (make-posn x (+ y diameter) )  (remove-last (snake-segments (snakeworld-snake w))) ) "down" radius)(snakeworld-level w) (snakeworld-score w)) ])
  
 ))

|#

;hits-self? : Snake -> Boolean
; consumes : Snake asnake
; produces : True if snake head hits tail, false if otherwise
(define (hits-self? asnake)
  (member? (first (snake-segments asnake)) (rest (snake-segments asnake)) )
)

(define (hits-border? asnake)
   (local ((define x (posn-x(first (snake-segments asnake))))
          (define y (posn-y(first (snake-segments asnake)))))
     (or
      (> x (image-width BACKGROUND))
      (< x   0)
      (> y  (image-width BACKGROUND))
      (< y  0))))

(define (hits-wall? w)
  (local ((define x (posn-x(first (snake-segments (snakeworld-snake w)))))
          (define y (posn-y(first (snake-segments (snakeworld-snake w))))))
    (cond [(= (snakeworld-difficulty w) 2)
           (or
           (and (> x 200)(< x 400)(> y 298)(< y 302))
           (and (> y 200)(< y 400)(> x 298)(< x 302)))]
          [(= (snakeworld-difficulty w) 3)
           (or
           (and (> x 0)(< x 200) (> y 98)(< y 102))
           (and (> x 400)(< x 600)(> y 498) (< y 502))
           (and (> y 0)(< y 200)(> x 498) (< x 502))
           (and (> y 400)(< y 600)(> x 98)(< x 102))
           (and (> x 250)(< x 350)(> y 250)(< y 350)))]
          [else false]
          )))


;end-game?: SnakeWorld -> Boolean
; consumes: SnakeWorld w
; produces: true Snake hits border or hits itself, false otherwise
(define (end-game? w)
(cond [(string=? "black" (snake-color(snakeworld-snake w)))
        (hits-border? (snakeworld-snake w))
       ]
      [else 

  (or
      (hits-border? (snakeworld-snake w))
      (hits-self? (snakeworld-snake w))
      (hits-wall? w)
      )
  ]))

;render-end-scene: SnakeWorld -> Image
; consumes: SnakeWorld w
; produces: the end screen image when the game ends
(define (render-end-scene w)
  (local ((define (print-end w astring)
            (above/align "left"
         (cond
           [(= (snakeworld-difficulty w) 1)(draw-snake (snakeworld-snake w) BACKGROUND )]
           [(= (snakeworld-difficulty w) 2)
            (place-image (rectangle 200  8 "solid" "black") 300 300
            (place-image (rectangle 8 200 "solid" "black") 300 300
        (draw-snake (snakeworld-snake w) BACKGROUND )))]
           [(= (snakeworld-difficulty w) 3)
            (place-image (rectangle 200 8 "solid" "black") 100 100
            (place-image (rectangle 200 8 "solid" "black") 500 500
            (place-image (rectangle 8 200 "solid" "black") 500 100
            (place-image (rectangle 8 200 "solid" "black") 100 500
            (place-image (rectangle 100 100 "solid" "black") 300 300
         (draw-snake (snakeworld-snake w) BACKGROUND)) ))))])
        (beside       
                 (text/font (string-append "LEVEL: "(number->string(snakeworld-difficulty w))"     " "SCORE: "(number->string(snakeworld-score w))) 20 "black" "Gill Sans" 'swiss 'normal 'bold #f)             
                 (text/font astring 20 "red" "Gill Sans" 'swiss 'normal 'bold #f)))
            ))
                          
  (cond [(hits-self? (snakeworld-snake w))
         (print-end w "         SNAKE HIT SELF")
          ]
        [(hits-border? (snakeworld-snake w))     
          (print-end w "         SNAKE HIT BORDER")
          ]
        [(hits-wall? w)     
        (print-end w "         SNAKE HIT WALL")
          ])))
        
 


(define (main init-world)
  (big-bang init-world
          [on-draw render-scene]
          [on-tick handle-tick .06] ;can change rate here
          [on-key handle-key]
          [stop-when end-game? render-end-scene]
          ))

;initiates game
(main sw2)




