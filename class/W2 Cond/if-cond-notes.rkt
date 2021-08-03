;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname if-cond-notes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; If x is smaller than small or bigger than large, return true
;   #true / # false   and true / false are the same thing.
;   Both denote booleans


;Can write
; (define (function x) ...)
; and have the program run; placeholder.



;(define (outlier? x small large)
;  (if( or (< x small) (> x large)) true false))
;^ redundant w/ true false

;(define (outlier? x small large)
;  ( or (< x small) (> x large)) )
;^can also write without the if and true false.

(define (outlier? x small large)
  (cond
  [ (or (> x large)(< x small)) true]
  [else false]
  )
  )

(check-expect (outlier? 0 20 30) true)
(check-expect (outlier? 2 0 9) false)


