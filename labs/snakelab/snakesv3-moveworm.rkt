;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snakesv3-moveworm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;NAMES: Zihan (Anna) Wu & Debra Lymon

(define RADIUS 10)
(define WORMPART (circle RADIUS  "solid" "red"))
(define BACKGROUND (empty-scene 500 500))


(define worm1 (make-posn 20 20 ))

(define-struct wormworld (worm segments direction speed level score))
(define ww1 (make-wormworld worm1 1 "right" 2 1 0))



;moveworm: WormWorld String -> WormWorld 
(define (moveworm w direction)
  (cond 
    [(string=? direction "left" )
     (make-wormworld 
               (make-posn (-(posn-x (wormworld-worm w))(wormworld-speed w)) (posn-y (wormworld-worm w)))
               (wormworld-segments w) "left" (wormworld-speed w)(wormworld-level w)(wormworld-score w))]
    [(string=? direction "right" )
     (make-wormworld 
               (make-posn (+(posn-x (wormworld-worm w))(wormworld-speed w)) (posn-y (wormworld-worm w)))
               (wormworld-segments w)"right"(wormworld-speed w)(wormworld-level w)(wormworld-score w))]
    [(string=? direction "up" )
     (make-wormworld
               (make-posn (posn-x (wormworld-worm w)) (-(posn-y (wormworld-worm w))(wormworld-speed w)))
               (wormworld-segments w)"up"(wormworld-speed w)(wormworld-level w)(wormworld-score w))]
    [(string=? direction "down" )
      (make-wormworld
               (make-posn (posn-x (wormworld-worm w)) (+(posn-y (wormworld-worm w))(wormworld-speed w)))
               (wormworld-segments w)"down"(wormworld-speed w)(wormworld-level w)(wormworld-score w))]
    ))
  
;render-scene: WormWorld -> WormWorld 
(define (render-scene w)
  (above/align "left"
   (place-image WORMPART (posn-x(wormworld-worm w)) (posn-y(wormworld-worm w))BACKGROUND)
   (text/font (string-append "LEVEL: "(number->string(wormworld-level w))"     " "SCORE: "(number->string(wormworld-score w))) 20 "black" "Gill Sans" 'swiss 'normal 'bold #f)             
   ))



;handle-key: WormWorld -> WormWorld 
(define (handle-key w akey)
  (cond [(key=? akey "left") 
            (moveworm w "left")]
        [(key=? akey "right") 
            (moveworm w "right")]
        [(key=? akey "up") 
            (moveworm w "up")]
        [(key=? akey "down")
            (moveworm w "down")]
        ))

(check-expect (handle-key (make-wormworld (make-posn 20 20) 1 "right" 2 1 0) "left")
              (make-wormworld (make-posn 18 20 ) 1 "left" 2 1 0))


(define (handle-tick w)
 (moveworm w (wormworld-direction w))
   )

(define (end-game? w)
   (or
      (> (posn-x (wormworld-worm w)) (- (image-width BACKGROUND)RADIUS))
      (< (posn-x (wormworld-worm w))  RADIUS)
      (> (posn-y (wormworld-worm w))  (- (image-width BACKGROUND)RADIUS))
      (< (posn-y (wormworld-worm w))  RADIUS)
      ))

(define (render-end-scene w)
        (above/align "left"
                     (place-image WORMPART (posn-x(wormworld-worm w)) (posn-y(wormworld-worm w))BACKGROUND)
                     (beside
                     (text/font (string-append "LEVEL: "(number->string(wormworld-level w))"     " "SCORE: "(number->string(wormworld-score w))) 20 "black" "Gill Sans" 'swiss 'normal 'bold #f)
                     (text/font "         WORM HIT BORDER" 20 "red" "Gill Sans" 'swiss 'normal 'bold #f)))
       )



(define (main init-world)
  (big-bang init-world
          [on-draw render-scene]
          [on-tick handle-tick]
          [on-key handle-key]
          [stop-when end-game? render-end-scene]
          ))
(main ww1)