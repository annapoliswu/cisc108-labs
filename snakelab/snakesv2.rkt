;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snakesv2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;NAMES: Zihan (Anna) Wu & Debra Lymon

(define RADIUS 10)
(define DIAMETER (* 2 RADIUS))
(define WORMPART (circle RADIUS  "solid" "red"))


(define worm1 (make-posn 20 20 ))

(define-struct wormworld (worm segments direction score))
(define ww1 (make-wormworld worm1 1 0))



;(define (draw-worm aworm )
  
;render-scene: WormWorld -> WormWorld 
(define (render-scene w)
   (place-image/align (text/font (string-append "SCORE: "(number->string(wormworld-score w))) 20 "black" "Gill Sans" 'swiss 'normal 'bold #f)
                5 0 "left" "top"  
   (place-image WORMPART
               (posn-x(wormworld-worm w)) (posn-y(wormworld-worm w))     
   (empty-scene 500 500)
   )))


(define (handle-tick w)
  (cond
    [(string=? (wormworld-direction w) "left" 
     (make-wormworld
       (make-posn (posn-x (wormworld-worm w)) (posn-y (wormworld-worm w)))
       (wormworld-segments w)(wormworld-score w))

   )

;(define (moveworm w)
  


(define (handle-key w akey)
  (cond [(key=? akey "left")
            (make-wormworld 
               (make-posn (-(posn-x (wormworld-worm w))DIAMETER) (posn-y (wormworld-worm w)))
               (wormworld-segments w)"left"(wormworld-score w))]
        [(key=? akey "right")
            (make-wormworld 
               (make-posn (+(posn-x (wormworld-worm w))DIAMETER) (posn-y (wormworld-worm w)))
               (wormworld-segments w)"right"(wormworld-score w))]
        [(key=? akey "up")
            (make-wormworld
               (make-posn (posn-x (wormworld-worm w)) (-(posn-y (wormworld-worm w))DIAMETER))
               (wormworld-segments w)"up"(wormworld-score w))]
        [(key=? akey "down")
            (make-wormworld
               (make-posn (posn-x (wormworld-worm w)) (+(posn-y (wormworld-worm w))DIAMETER))
               (wormworld-segments w)"down"(wormworld-score w))]
        ))




(define (main init-world)
  (big-bang init-world
          [on-draw render-scene]
          [on-tick handle-tick]
       ;   [on-key handle-key]
       ;   [stop-when end-game? render-end-scene]
          ))
