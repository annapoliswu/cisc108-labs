;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hole-in-wall) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; A hole-in-the-wall-game, HitWG, is one of
;; [-3,-2,-1, 0] number of seconds until a wall appears
;; [1..4] four Shapes that a hole could appear in the wall
;; "crash" losing state
;; "
(define start -3)

;; constants
(define SIZE 500)
(define ONE-THIRD (/ SIZE 3))
(define BACKGROUND (square SIZE "solid" "blue"))

; shape: number -> image
; Consumes a number between 1 and 4 and produces an image of a shape
(define (shape anumber)
  (cond [(= 1 anumber) (square ONE-THIRD "solid" "white")]
        [(= 2 anumber) (circle ONE-THIRD "solid" "white")]
        [(= 3 anumber) (triangle ONE-THIRD "solid" "white")]
        [(= 4 anumber) (star ONE-THIRD "solid" "white")]))

; draw-world: HitWG -> image
; Consumes a hole-in-the-wall-game state and produces an image:
; if it is a number <= 0 give the blank screen
; if it is a positive number, draw the shape on the screen
; if it is the string "crash", draw a text for losing
(define (draw-world w)
  (cond [(and (number? w) (<= w 0)) BACKGROUND]
        [(number? w) (place-image (shape w) (/ SIZE 2) (/ SIZE 2) BACKGROUND)]
        [(and (string? w) (string=? w "crash")) (text "You Lose!" 48 "red")]))

; handle-tick: HitWG -> HitWG
; Consumes a hole-in-the-wall-game state and produces the next state according to time:
; if it is a number < 0 increment by 1
; if it is the number 0, generate a random shape
; if it is a shape then the user has failed to press a key within the time, so the next state is "crash"
; if it is "crash" then it is still "crash"
(define (handle-tick w)
  (cond [(and (number? w) (< w 0)) (+ w 1)]
        [(and (number? w) (= w 0)) (+ 1 (random 4))]
        [(number? w) "crash"]
        [else w]))

; handle-key: HitWG key -> HitWG
; Consumes hole-in-the-wall-game state and produces the next state according to user input:
; if current state is a shape (number between 1 and 4) then if the user guessed correctly the next
;   state is a random wait time (-3 to -1 seconds), otherwise "crash"
; any other state the user input does not do anything
(define (handle-key w akey)
  (cond [(and (number? w) (<= 1 w 4) (<= 1 (string->number akey) 4))
         (cond [(= w (string->number akey)) (- -1 (random 3))]
               [else "crash"])]
        [else w]))

; start the universe with a -3 second delay until the first shape
(big-bang start
	(on-tick handle-tick 1)
        (on-key handle-key)
	(to-draw draw-world))