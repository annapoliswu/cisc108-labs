;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname snakesv4-evenmorecomplexsnakestruct) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;NAMES: Zihan (Anna) Wu & Debra Lymon

(define RADIUS 10)
(define WORMPART (circle RADIUS  "solid" "red"))
(define BACKGROUND (empty-scene 500 500))

(define-struct worm (posn segments direction speed )) 
(define worm1 (make-worm(make-posn 20 20 ) 5 "right" 2))

(define-struct wormworld (worm level score))
(define ww1 (make-wormworld worm1 1 0))



;NEED TO CHANGE: if already moving up/down -> cannot move up/down. if already moving left/right, cannot move left/right
;moveworm: WormWorld String -> WormWorld 
(define (moveworm w direction)
  (local ((define worm-posn-x (posn-x (worm-posn(wormworld-worm w))))
          (define worm-posn-y (posn-y (worm-posn(wormworld-worm w))))
          )
  (cond 
    [(string=? direction "left" )
     (make-wormworld
           (make-worm
               (make-posn (- worm-posn-x (worm-speed (wormworld-worm w)) ) worm-posn-y )
               (worm-segments (wormworld-worm w)) "left" (worm-speed (wormworld-worm w)))
           (wormworld-level w)(wormworld-score w))]
    
    [(string=? direction "right" )
     (make-wormworld
           (make-worm
                (make-posn (+ worm-posn-x (worm-speed (wormworld-worm w))) worm-posn-y)
                (worm-segments (wormworld-worm w)) "right" (worm-speed (wormworld-worm w)))
          (wormworld-level w)(wormworld-score w))]
    
    [(string=? direction "up" )
     (make-wormworld
           (make-worm
               (make-posn worm-posn-x (- worm-posn-y (worm-speed (wormworld-worm w))))
                (worm-segments (wormworld-worm w)) "up" (worm-speed (wormworld-worm w)))
          (wormworld-level w)(wormworld-score w))]
    
    [(string=? direction "down" )
      (make-wormworld
           (make-worm
               (make-posn worm-posn-x (+ worm-posn-y (worm-speed (wormworld-worm w))))
                (worm-segments (wormworld-worm w))"down" (worm-speed (wormworld-worm w)))
          (wormworld-level w)(wormworld-score w))]
    )))

;render-scene: WormWorld -> Image
(define (render-scene w)
  (local ((define worm-posn-x (posn-x (worm-posn(wormworld-worm w))))
          (define worm-posn-y (posn-y (worm-posn(wormworld-worm w))))
          )
  (above/align "left"
        (place-image WORMPART worm-posn-x worm-posn-y BACKGROUND)
    
 
                
        (text/font (string-append "LEVEL: "(number->string(wormworld-level w))"     " "SCORE: "(number->string(wormworld-score w))) 20 "black" "Gill Sans" 'swiss 'normal 'bold #f)             
   )))


; lg->image: lg -> image
; Produces an image of a log
#;(define (lg->image alg)
  (beside-n LG-IMAGE (/ (lg-width alg) BLOCK-SIZE)))


;note: probably not in draw-worm..  try in render scene???? or handletick
;Worm -> Image : draws a worm in accordance to # segments and direction. 
#;(define (draw-worm w)
  (cond
    [(= (worm-segments(wormworld-worm w)) 1) WORMPART]
    [else (cond [(string=?(worm-direction (wormworld-worm w)) "up")
                 (above WORMPART (draw-worm (make-worm (worm-posn w) (sub1 (worm-segments w))(worm-direction w) (worm-speed w)) ))]
                [(string=?(worm-direction (wormworld-worm w)) "down")
                 (above WORMPART (draw-worm (make-worm (worm-posn w) (sub1 (worm-segments w))(worm-direction w) (worm-speed w)) ))]
                [(string=?(worm-direction (wormworld-worm w)) "left")
                 (beside WORMPART (draw-worm (make-worm (worm-posn w) (sub1 (worm-segments w))(worm-direction w) (worm-speed w)) ))]
                [(string=?(worm-direction (wormworld-worm w)) "right")
                 (beside (draw-worm (make-worm (worm-posn w) (sub1 (worm-segments w))(worm-direction w) (worm-speed w))) WORMPART)]

)]))
     
    
  



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

(check-expect (handle-key (make-wormworld (make-worm (make-posn 20 20) 1 "right" 2) 1 0) "left")
              (make-wormworld (make-worm(make-posn 18 20 ) 1 "left" 2) 1 0))


(define (handle-tick w)
 (moveworm w (worm-direction (wormworld-worm w)) )
   )

(define (end-game? w)
  (local ((define worm-posn-x (posn-x (worm-posn(wormworld-worm w))))
          (define worm-posn-y (posn-y (worm-posn(wormworld-worm w))))
          )
   (or
      (> worm-posn-x (- (image-width BACKGROUND)RADIUS))
      (< worm-posn-x  RADIUS)
      (> worm-posn-y  (- (image-width BACKGROUND)RADIUS))
      (< worm-posn-y  RADIUS)
      )))

(define (render-end-scene w)
   (local ((define worm-posn-x (posn-x (worm-posn(wormworld-worm w))))
          (define worm-posn-y (posn-y (worm-posn(wormworld-worm w))))
          )
     
        (above/align "left"
                     (place-image WORMPART worm-posn-x worm-posn-y BACKGROUND)
                     (beside
                     (text/font (string-append "LEVEL: "(number->string(wormworld-level w))"     " "SCORE: "(number->string(wormworld-score w))) 20 "black" "Gill Sans" 'swiss 'normal 'bold #f)
                     (text/font "         WORM HIT BORDER" 20 "red" "Gill Sans" 'swiss 'normal 'bold #f)))
       ))



(define (main init-world)
  (big-bang init-world
          [on-draw render-scene]
          [on-tick handle-tick]
          [on-key handle-key]
          [stop-when end-game? render-end-scene]
          ))
(main ww1)