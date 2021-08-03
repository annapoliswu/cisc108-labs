;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snakenewversiontest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;NAMES: Zihan (Anna) Wu & Debra Lymon


(define BACKGROUND (empty-scene 600 600))
(define RADIUS 4)
(define DIAMETER (* 2 RADIUS))
(define SEGMENT (circle RADIUS  "solid" "red"))


(define FOODPIECE (circle RADIUS  "solid" "green"))

;Food is :
; regularfood, shrinkingfood, jumpingfood, teleportfood
(define-struct regularfood(posn))
(define-struct shrinkingfood (posn))
(define-struct jumpingfood (posn))
(define-struct teleportfood (posn1 posn2))

;add listoffood and difficulty to world and add size to either world or snake
; change world and snake definitions

;create wall structure -> black block with radius as height
; in renderscene- use conditional to place walls if the world difficulty equals - 1, 2 , 3
; define function to check if snake head hits wall
; in handle-key add keys 1, 2 , 3 to change difficulty in world. 


;A Direction is
; -"up" , "down" , "left" ,or "right" 

;A Segment is
(define seg1 (make-posn 20 20 ))

;A Snake is
(define-struct snake(segments direction))
(define snake1 (make-snake (list seg1(make-posn 30 20 )(make-posn 40 20 )(make-posn 50 20 )(make-posn 60 20 ) (make-posn 70 20 )(make-posn 80 20 )) "right")) 

;A SnakeWorld is
(define-struct snakeworld (snake level score lof difficulty ))
(define sw1 (make-snakeworld snake1 1 0))

;hits-self? : Snake -> Boolean
; consumes : Snake asnake
; produces : True if snake head hits tail, false if otherwise
(define (hits-self? asnake)
  (member? (first (snake-segments asnake)) (rest (snake-segments asnake)) )
)

;remove-last : List -> List
; consumes : List alist
; produces : the list with the last item removed
(define (remove-last alist)
  (cond
        [(empty? alist) empty]
        [(empty? (rest alist)) empty]
        [else (cons (first alist) (remove-last (rest alist)))])
        )
;draw-snake : ListofSegments Image -> Image
; consumes : List los, Image bg
; produces : an image of the full snake placed in the the given background image
(define (draw-snake los bg)
  (cond [(empty? los) bg]
        [else (place-image SEGMENT (posn-x (first los)) (posn-y (first los)) (draw-snake(rest los) bg) )]
        ))

;render-scene : SnakeWorld -> Image
; consumes : SnakeWorld w
; produces : the game screen / image of current SnakeWorld
(define (render-scene w)
  (above/align "left"
        (draw-snake (snake-segments (snakeworld-snake w)) BACKGROUND)

                
        (text/font (string-append "LEVEL: "(number->string(snakeworld-level w))"     " "SCORE: "(number->string(snakeworld-score w))) 20 "black" "Gill Sans" 'swiss 'normal 'bold #f)             
   ))

;move-snake : Snake Direction -> Snake 
; consumes : Snake asnake, Direction 
; produces : a new snake with and moved in the specified direction
(define (move-snake asnake direction)
  (local ((define x (posn-x(first (snake-segments asnake))))
          (define y (posn-y(first (snake-segments asnake)))))
  (cond 
    [(string=? "right" direction) (make-snake (cons (make-posn (+ x DIAMETER) y) (remove-last (snake-segments asnake)))"right") ]
    [(string=? "left" direction) (make-snake(cons (make-posn (- x DIAMETER) y) (remove-last (snake-segments asnake)) )"left" )]
    [(string=? "up" direction) (make-snake (cons (make-posn x (- y DIAMETER) ) (remove-last (snake-segments asnake)) ) "up")]
    [(string=? "down" direction) (make-snake (cons (make-posn x (+ y DIAMETER) )  (remove-last (snake-segments asnake)) ) "down") ])
    ))

;move-sw : SnakeWorld Direction -> SnakeWorld 
; consumes : SnakeWorld w, Direction 
; produces : a new SnakeWorld with a snake with and moved in the specified direction.
(define (move-sw w direction)
  (make-snakeworld (move-snake (snakeworld-snake w) direction) (snakeworld-level w) (snakeworld-score w))
  )

;handle-key: WormWorld -> WormWorld 
(define (handle-key w akey)
  (cond [(key=? akey "left") 
            (move-sw w "left")]
        [(key=? akey "right") 
            (move-sw w "right")]
        [(key=? akey "up") 
            (move-sw w "up")]
        [(key=? akey "down")
            (move-sw w "down")]
        [else (move-sw w (snake-direction (snakeworld-snake w)))]
        ))


;tick handler must not only move the worm; in addition it must manage the eating process and the creation of new food.

;food and increase snake seg goes here??
(define (handle-tick w)
 (move-sw w (snake-direction (snakeworld-snake w))) )


;end-game?: SnakeWorld -> Boolean
; consumes: SnakeWorld w
; produces: true Snake hits border or hits itself, false otherwise
(define (end-game? w)
  (local ((define x (posn-x(first (snake-segments (snakeworld-snake w)))))
          (define y (posn-y(first (snake-segments(snakeworld-snake w))))))
   (or
      (> x (image-width BACKGROUND))
      (< x   0)
      (> y  (image-width BACKGROUND))
      (< y  0)
      (hits-self? (snakeworld-snake w))
      )))

;render-end-scene: SnakeWorld -> Image
; consumes: SnakeWorld w
; produces: the end screen image when the game ends
(define (render-end-scene w)
  (cond [(hits-self? (snakeworld-snake w))
          (above/align "left"
                (draw-snake (snake-segments (snakeworld-snake w)) BACKGROUND)
                (beside       
                 (text/font (string-append "LEVEL: "(number->string(snakeworld-level w))"     " "SCORE: "(number->string(snakeworld-score w))) 20 "black" "Gill Sans" 'swiss 'normal 'bold #f)             
                 (text/font "         WORM HIT ITSELF" 20 "red" "Gill Sans" 'swiss 'normal 'bold #f)
        ))]
        [else      
         (above/align "left"
                (draw-snake (snake-segments (snakeworld-snake w)) BACKGROUND)
                (beside       
                 (text/font (string-append "LEVEL: "(number->string(snakeworld-level w))"     " "SCORE: "(number->string(snakeworld-score w))) 20 "black" "Gill Sans" 'swiss 'normal 'bold #f)             
                 (text/font "         WORM HIT BORDER" 20 "red" "Gill Sans" 'swiss 'normal 'bold #f)
        ))]
  ))


(define (main init-world)
  (big-bang init-world
          [on-draw render-scene]
          [on-tick handle-tick .06] ;can change rate here
          [on-key handle-key]
          [stop-when end-game? render-end-scene]
          ))

;initiates game
(main sw1)




