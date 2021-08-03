;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lambdaEX) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(define (addone x)
  (+ 1 x))

(addone 5)

(define addone.v2 (λ (x) (+ 1 x)))

(addone.v2 5)

;((λ  (x) (+ 1 x)) 5)


; funcion call = var sub
; lambda definition and call kinda
   ;command backslash

;useful with map, can call lambda instead of define.

(define (convert-euro alon etd)
  (map (lambda (x) (* x etd)) alon))

;^lambda func also called anonymous, functions are just lambda expr named
; not useful if expr must be used more than once
; for simple non repetitive exprs