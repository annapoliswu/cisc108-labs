;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snakenewversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;NAMES: Zihan (Anna) Wu & Debra Lymon


(define RADIUS 10)
(define DIAMETER (* 2 RADIUS))
(define SEGMENT (circle RADIUS  "solid" "red"))
(define BACKGROUND (empty-scene 500 500))

(define seg1 (make-posn 20 20 ))

(define-struct snake(segments direction))
(define snake1 (make-snake (list seg1) "right")) 

(define-struct snakeworld (snake level score))
(define sw1 (make-snakeworld snake1 1 0))

(define (remove-last alist)
  (cond
        [(empty? alist) empty]
        [(empty? (rest alist)) empty]
        [else (cons (first alist) (remove-last (rest alist)))])
        )


(define (render-scene w)
  (above/align "left"
        (place-image SEGMENT (posn-x (first (snake-segments (snakeworld-snake w)))) (posn-y (first (snake-segments (snakeworld-snake w)))) BACKGROUND)

                
        (text/font (string-append "LEVEL: "(number->string(snakeworld-level w))"     " "SCORE: "(number->string(snakeworld-score w))) 20 "black" "Gill Sans" 'swiss 'normal 'bold #f)             
   ))

(define (movesnake asnake)
  (local ((define x (posn-x(first (snake-segments asnake))))
          (define y (posn-y(first (snake-segments asnake)))))
  (cond 
    [(string=? "right" (snake-direction asnake)) (make-snake (cons (make-posn (+ DIAMETER x) y) (remove-last (snake-segments asnake)))"right") ]
    [(string=? "left" (snake-direction asnake)) (make-snake(cons (make-posn (- x DIAMETER) y) (remove-last (snake-segments asnake)) )"left" )]
    [(string=? "up" (snake-direction asnake)) (make-snake (cons (make-posn x (+ y DIAMETER) ) (remove-last (snake-segments asnake)) ) "up")]
    [(string=? "down" (snake-direction asnake)) (make-snake (cons (make-posn x (- y DIAMETER) )  (remove-last (snake-segments asnake)) ) "down") ])
    ))

(define (movesnakeinworld w)
  (make-snakeworld (movesnake (snakeworld-snake w)) (snakeworld-level w) (snakeworld-score w))
  )

;handle-key: WormWorld -> WormWorld 
(define (handle-key w akey)
  (cond [(key=? akey "left") 
            (make-snakeworld (make-snake (snake-segments (snakeworld-snake w)) "left" ) (snakeworld-level w) (snakeworld-score w))]
        [(key=? akey "right") 
            (make-snakeworld (make-snake (snake-segments (snakeworld-snake w)) "right" ) (snakeworld-level w) (snakeworld-score w))]
        [(key=? akey "up") 
            (make-snakeworld (make-snake (snake-segments (snakeworld-snake w)) "up" ) (snakeworld-level w) (snakeworld-score w))]
        [(key=? akey "down")
            (make-snakeworld (make-snake (snake-segments (snakeworld-snake w)) "down" ) (snakeworld-level w) (snakeworld-score w))]
        ))

(define (handle-tick w)
 (movesnakeinworld w) )
   

(define (main init-world)
  (big-bang init-world
          [on-draw render-scene]
          [on-tick handle-tick] 
          [on-key handle-key]
         ; [stop-when end-game? render-end-scene]
          ))
(main sw1)