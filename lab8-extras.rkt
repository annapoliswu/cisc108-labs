#lang racket

(require 2htdp/image)

(provide board-ref
         board->image
         (struct-out gstate)
         gstate->image
         move-up
         move-down
         move-left
         move-right
         solution->image)

;; A Board is [listof [listof NaturalNumber]] where
;; -- size = N x N, where N is both the number of rows and columns
;; -- all numbers in the Board are in the range [1, N)

;; board-ref: NaturalNumber NaturalNumber Board -> NaturalNumber
(define (board-ref x y board)
  (list-ref (list-ref board y) x))

(define-struct gstate (x y board) #:transparent)
;; make-gstate: NaturalNumber NaturalNumber Board -> State
;; Interpretation:
;;  - x     is the column of the current position
;;  - y     is the row of the current position
;;  NOTE: both x and y must be greater than or equal to 0 and less than (board-n board)
;;  - board is state's board
#;
(define (gstate-fn astate)
  ... (gstate-x astate)
  ... (gstate-y astate)
  ... (gstate-board astate)
  )

(define TEXT-SIZE 12)
(define CELL-SIZE 20)
(define CELL (square CELL-SIZE "outline" "black"))
(define CIRCLE (circle 8 "outline" "blue"))

;; board->image: Board -> Image
(define (board->image board)
  (local [(define (draw-cell n)
            (overlay (text (number->string n) TEXT-SIZE "black")
                     CELL))
          (define (draw-row row)
            (apply beside (map draw-cell row)))]
    (frame (apply above (map draw-row board)))))

;; state->image: State -> Image
(define (gstate->image astate)  
  (place-image CIRCLE
               (+ (* (gstate-x astate) CELL-SIZE) (/ CELL-SIZE 2))
               (+ (* (gstate-y astate) CELL-SIZE) (/ CELL-SIZE 2))
               (board->image (gstate-board astate))))

;; An Outcome is either
;; -- false
;; -- State
;; Interp.
;; -- false indicates that the requested move was infeasible or that the state was already solved
;; -- State is the State resulting from the move

;; move-x: (Number Number -> Number) State -> Outcome
(define (move-x op astate)
  (local [(define n (length (gstate-board astate)))
          (define x (gstate-x astate))
          (define y (gstate-y astate))
          (define delta (board-ref x y (gstate-board astate)))
          (define new-x (op x delta))]
    (if (or (zero? delta) (not (< 0 new-x n)))
        false
        (make-gstate new-x y (gstate-board astate)))))

;; move-x: (Number Number -> Number) State -> Outcome
(define (move-y op astate)
  (local [(define n (length (gstate-board astate)))
          (define x (gstate-x astate))
          (define y (gstate-y astate))
          (define delta (board-ref x y (gstate-board astate)))
          (define new-y (op y delta))]
    (if (or (zero? delta) (not (< 0 new-y n)))
        false
        (make-gstate x new-y (gstate-board astate)))))

;; move-left: State -> Outcome
(define (move-left state)
  (move-x - state))

;; move-right: State -> Outcome
(define (move-right state)
  (move-x + state))

;; move-up: State -> Outcome
(define (move-up state)
  (move-y - state))

;; move-down: State -> Outcome
(define (move-down state)
  (move-y + state))

(define UP-IMG (text " --Up--> " 12 "black"))
(define DOWN-IMG (text " --Down--> " 12 "black"))
(define LEFT-IMG (text " --Left--> " 12 "black"))
(define RIGHT-IMG (text " --Right--> " 12 "black"))
          
(define (solution->image alom astate)
  (local [(define img (gstate->image astate))
          (define m (if (empty? alom) 'none (first alom)))
          (define img-fun
            (cond [(symbol=? 'up m) (list UP-IMG move-up)]
                  [(symbol=? 'down m) (list DOWN-IMG move-down)]
                  [(symbol=? 'left m) (list LEFT-IMG move-left)]
                  [(symbol=? 'right m) (list RIGHT-IMG move-right)]
                  [else empty]))]
    (if (empty? alom) img
        (beside img (first img-fun) (solution->image (rest alom) ((second img-fun) astate))))))