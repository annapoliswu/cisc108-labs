;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snakenewV2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;NAMES: Zihan (Anna) Wu & Debra Lymon

;write function  that chops off the lower half of a list
;use function in shrinkingfood thing

;rewrite snake with a color option
;insert color parameter into all make-snakes
;rewrite end-game to test for color

;make a generate random food function. (input type )



(define BACKGROUND (empty-scene 600 600))
;(define RADIUS 4)
;(define DIAMETER (* 2 RADIUS))
;(define SEGMENT (circle RADIUS  "solid" "red"))


;(define FOODPIECE (circle RADIUS  "solid" "green"))

;Food is :
(define-struct food (posn type))


;8 * something - 4
(define foodlist1 (list (make-food (make-posn 76 76) "regular")(make-food (make-posn 156 156)"shrinking")(make-food (make-posn 300 300) "jumping")))

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
  (cond [(empty? alof) "" ]
        [(and ( = (posn-x(food-posn (first alof))) (posn-x(first (snake-segments asnake)))) ( = (posn-y(food-posn (first alof))) (posn-y(first (snake-segments asnake)))))
         (food-type (first alof))]
        [else (snake-on-which-food? asnake (rest alof) ) ]
        )))
 (snake-on-which-food? (snakeworld-snake w) (snakeworld-lof w) )
 ))
          



;A Direction is
; -"up" , "down" , "left" ,or "right" 

;A Segment is
(define seg1 (make-posn 20 20 ))

;A Snake is
(define-struct snake(segments direction radius))
(define snake1 (make-snake (list seg1) "right" 4))
(define snake2 (make-snake (list seg1(make-posn 30 20)(make-posn 40 20)(make-posn 50 20 )(make-posn 60 20 ) (make-posn 70 20 )(make-posn 80 20 )) "right" 4))

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

;remove-half






;draw-snake : ListofSegments Image -> Image
; consumes : List los, Image bg
; produces : an image of the full snake placed in the the given background image
(define (draw-snake los bg radius)
  ( local ((define SNAKESEGMENT (circle radius "solid" "red")))
  (cond [(empty? los) bg]
        [else (place-image SNAKESEGMENT  (posn-x (first los)) (posn-y (first los)) (draw-snake(rest los) bg radius) )]
        )))

;render-scene : SnakeWorld -> Image
; consumes : SnakeWorld w
; produces : the game screen / image of current SnakeWorld
(define (render-scene w)
  (above/align "left"
         (cond
           [(= (snakeworld-difficulty w) 1)(draw-snake (snake-segments (snakeworld-snake w)) (draw-food (snakeworld-lof w) BACKGROUND (snake-radius (snakeworld-snake w))) (snake-radius (snakeworld-snake w)))]
           [(= (snakeworld-difficulty w) 2)
            (place-image (rectangle 200  8 "solid" "black") 300 300
            (place-image (rectangle 8 200 "solid" "black") 300 300
        (draw-snake (snake-segments (snakeworld-snake w))  (draw-food (snakeworld-lof w) BACKGROUND (snake-radius (snakeworld-snake w))) (snake-radius (snakeworld-snake w)))))]
           [(= (snakeworld-difficulty w) 3)
            (place-image (rectangle 200 8 "solid" "black") 100 100
            (place-image (rectangle 200 8 "solid" "black") 500 500
            (place-image (rectangle 8 200 "solid" "black") 500 100
            (place-image (rectangle 8 200 "solid" "black") 100 500
            (place-image (rectangle 100 100 "solid" "black") 300 300
         (draw-snake (snake-segments (snakeworld-snake w))  (draw-food (snakeworld-lof w) BACKGROUND (snake-radius (snakeworld-snake w))) (snake-radius (snakeworld-snake w))) )))))])
        (text/font (string-append "LEVEL: "(number->string(snakeworld-difficulty w))"     " "SCORE: "(number->string(snakeworld-score w))) 20 "black" "Gill Sans" 'swiss 'normal 'bold #f)             
   ))

;move-snake : Snake Direction -> Snake 
; consumes : Snake asnake, Direction 
; produces : a new snake with and moved in the specified direction
(define (move-snake asnake direction)
  (local ((define x (posn-x(first (snake-segments asnake))))
          (define y (posn-y(first (snake-segments asnake))))
          (define radius (snake-radius asnake))
          (define diameter (* 2 radius)))
  (cond 
    [(string=? "right" direction)(make-snake (cons (make-posn (+ x diameter) y) (remove-last (snake-segments asnake)))"right" radius)]
    [(string=? "left" direction) (make-snake(cons (make-posn (- x diameter) y) (remove-last (snake-segments asnake)) )"left" radius)]
    [(string=? "up" direction) (make-snake (cons (make-posn x (- y diameter) ) (remove-last (snake-segments asnake)) ) "up" radius)]
    [(string=? "down" direction) (make-snake (cons (make-posn x (+ y diameter) )  (remove-last (snake-segments asnake)) ) "down" radius) ])
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
         (make-snakeworld (make-snake (list (make-posn 0 30)) "right" 4)  0 0 empty 1)]
         [(key=? akey "2")
          (make-snakeworld (make-snake (list (make-posn 0 30)) "right" 4)  0 0 empty 2)] 
          [(key=? akey "3")
          (make-snakeworld (make-snake (list (make-posn 0 30)) "right" 4)  0 0 empty 3)]
        
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
            (define direction (snake-direction(snakeworld-snake w)) ))
    
  (cond [(string=? "" (on-which-food? w)) (make-snakeworld (move-snake (snakeworld-snake w) direction) (snakeworld-level w) (snakeworld-score w) (snakeworld-lof w) (snakeworld-difficulty w))]
        [(string=? "regular" (on-which-food? w))
         (make-snakeworld 
        (cond 
          [(string=? "right" direction)(make-snake (cons (make-posn (+ x diameter) y)(snake-segments asnake))"right" radius)]
          [(string=? "left" direction) (make-snake(cons (make-posn (- x diameter) y)(snake-segments asnake))"left" radius)]
          [(string=? "up" direction) (make-snake (cons (make-posn x (- y diameter) )(snake-segments asnake)) "up" radius)]
          [(string=? "down" direction) (make-snake (cons (make-posn x (+ y diameter) )(snake-segments asnake)) "down" radius) ])
        (snakeworld-level w) (+(snakeworld-score w) 10) (snakeworld-lof w) (snakeworld-difficulty w))
         ]
#|         [(string=? "shrinking" (on-which-food? w))
         (make-snakeworld 
        (cond 
          [(string=? "right" direction)(make-snake (reverse(list-tail (reverse(snake-segments asnake))))"right" radius)]
          [(string=? "left" direction) (make-snake(reverse(list-tail (reverse(snake-segments asnake))))"left" radius)]
          [(string=? "up" direction) (make-snake (reverse(list-tail (reverse(snake-segments asnake)))) "up" radius)]
          [(string=? "down" direction) (make-snake (reverse(list-tail (reverse(snake-segments asnake)))) "down" radius) ])
        (snakeworld-level w) (snakeworld-score w) (snakeworld-lof w) (snakeworld-difficulty w))
         ]|#
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
   (or
      (hits-border? (snakeworld-snake w))
      (hits-self? (snakeworld-snake w))
      (hits-wall? w)
      ))

;render-end-scene: SnakeWorld -> Image
; consumes: SnakeWorld w
; produces: the end screen image when the game ends
(define (render-end-scene w)
  (local ((define (print-end w astring)
            (above/align "left"
         (cond
           [(= (snakeworld-difficulty w) 1)(draw-snake (snake-segments (snakeworld-snake w)) BACKGROUND (snake-radius (snakeworld-snake w)))]
           [(= (snakeworld-difficulty w) 2)
            (place-image (rectangle 200  8 "solid" "black") 300 300
            (place-image (rectangle 8 200 "solid" "black") 300 300
        (draw-snake (snake-segments (snakeworld-snake w)) BACKGROUND (snake-radius (snakeworld-snake w)))))]
           [(= (snakeworld-difficulty w) 3)
            (place-image (rectangle 200 8 "solid" "black") 100 100
            (place-image (rectangle 200 8 "solid" "black") 500 500
            (place-image (rectangle 8 200 "solid" "black") 500 100
            (place-image (rectangle 8 200 "solid" "black") 100 500
            (place-image (rectangle 100 100 "solid" "black") 300 300
         (draw-snake (snake-segments (snakeworld-snake w)) BACKGROUND (snake-radius (snakeworld-snake w))) )))))])
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
          ]
        
  )))


(define (main init-world)
  (big-bang init-world
          [on-draw render-scene]
          [on-tick handle-tick .06] ;can change rate here
          [on-key handle-key]
          [stop-when end-game? render-end-scene]
          ))

;initiates game
(main sw2)




