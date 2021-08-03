;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname local-with-abstractions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Design the function add-3-to-all. The function consumes a 
; list of Posns and adds 3 to the x coordinates of each of them.


; add-3-to-all: [List-of Posn] -> [List-of Posn]
; add 3 to each x coordinate on the given list
 
(check-expect (add-3-to-all (list (make-posn 30 10) (make-posn 0 0)))
              (list (make-posn 33 10) (make-posn 3 0)))



; Design a function that eliminates all Posns from a list that
; have a y coordinate of larger than 100.
; keep-good: [List-of Posn] -> [List-of Posn]
; eliminates Posns whose y coordinate is larger than 100
 
(check-expect (keep-good (list (make-posn 0 110) (make-posn 0 60)))
              (list (make-posn 0 60)))




(define (distance apt1 apt2)
  (sqrt (+ (sqr (- (posn-x apt1)
                   (posn-x apt2)))
           (sqr (- (posn-y apt1)
                   (posn-y apt2))))))

; Design a function that determines whether any of a list of Posns is close
; to some given position pt where “close” means a distance of at most 5 pixels.
; close?: [List-of Posn] Posn -> Boolean
; is any Posn on lop close to pt?
 
(check-expect (close? (list (make-posn 47 54) (make-posn 0 60))
                      (make-posn 50 50))
              true)
