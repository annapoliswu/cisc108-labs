;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ISL-abstractions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ISL's built-in abstract functions for list-processing
;;============SIGS==========
; filter:    (X -> Boolean)   [List-of X] -> [List-of X]
; map:       (X -> Y)         [List-of X] -> [List-of Y]
; foldr:     (X Y -> Y)   Y   [List-of X] -> Y
; foldl:     (X Y -> Y)   Y   [List-of X] -> Y
; sort:      [List-of X] (X X -> Boolean) -> [List-of X]
; build-list: NatNumber     (NatNum -> X) -> [List-of X]
; andmap:    (X -> Boolean)   [List-of X] -> Boolean
; ormap:     (X -> Boolean)   [List-of X] -> Boolean
; Parens = give a function
;PARAMETRIC DATA DEFINITION^^^

;; WARNING: you cannot design build-list, build-string, or foldl 
;; with the design principles you know at this point; 
;; "Accumulators" [HTDP/2e Section VI] covers the necessary ideas.

; filter: (X -> Boolean) [List-of X] -> [List-of X]
; (filter p alox)
; produce a list from all those items on alox for which p holds
(check-expect (filter even? (list 1 2 3 4)) 
              (list 2 4))

; map: (X -> Y) [List-of X] -> [List-of Y]
; (map f alox)
; construct a list by applying f to each item on alox
;    (map f (list x-1 ... x-n)) = (list (f x-1) ... (f x-n))
(check-expect (map string-length (list "a" "short" "example"))
              (list 1 5 7))

; foldr: (X Y -> Y) Y [List-of X] -> Y
; (foldr f base alox)
; compute the result of applying f from RIGHT TO LEFT to all of
; alox and base, that is, apply f to
;    the last item in alox and base,
;    the penultimate item and the result of the first step,
;    and so on up to the first item
;    (foldr f base (list x-1 ... x-n)) = (f x-1 ... (f x-n base))
(check-expect (foldr string-append "BASE" (list "a" "b" "c"))
              "abcBASE")
(check-expect (foldr string-append "BASE" (list "a" "b" "c"))
              (string-append "a" (string-append "b" (string-append "c" "BASE"))))


; foldl: (X Y -> Y) Y [List-of X] -> Y
; (foldl f base alox)
; compute the result of applying f from LEFT TO RIGHT to all of
; alox and base, that is, apply f to
;    the first item in alox and base,
;    the second item and the result of the first step,
;    and so on up to the last item:
;    (foldl f base (list x-1 ... x-n)) = (f x-n ... (f x-1 base))
(check-expect (foldl string-append "BASE" (list "a" "b" "c"))
              "cbaBASE")
(check-expect (foldl string-append "BASE" (list "a" "b" "c"))
              (string-append "c" (string-append "b" (string-append "a" "BASE"))))

; sort: [List-of X] (X X -> Boolean) -> [List-of X]
; (sort alox compare-fn)
; produce a variant of alox that is sorted according to given compare-fn.
(check-expect (sort (list "a" "short" "example") string>?)
              (list "short" "example" "a"))

; build-list: NatNum (NatNum -> X) -> [List-of X]
; (build-list n f)
; construct a list by applying f to 0, 1, ..., (sub1 n)
;    (build-list n f) = (list (f 0) ... (f (- n 1)))
(check-expect (build-list 4 sqr)
              (list 0 1 4 9))

; andmap: (X -> Boolean) [List-of X] -> Boolean
; (andmap p alox)
; determine whether p holds for every item on alox
;    (andmap p (list x-1 ... x-n)) = (and (p x-1) ... (p x-n))
(check-expect (andmap odd? (list 7 5 3 1)) true)
(check-expect (andmap odd? (list 7 5 2 1)) false)

; ormap: (X -> Boolean) [List-of X] -> Boolean
; (ormap p alox)
; determine whether p holds for at least one item on alox
;    (ormap p (list x-1 ... x-n)) = (or (p x-1) ... (p x-n))
(check-expect (ormap string? (list 4 3 "4" 9)) true)
(check-expect (ormap string? (list 4 3 4 9)) false)

;;============SIGS==========
; filter:    (X -> Boolean)   [List-of X] -> [List-of X]
; map:       (X -> Y)         [List-of X] -> [List-of Y]
; foldr:     (X Y -> Y)   Y   [List-of X] -> Y
; foldl:     (X Y -> Y)   Y   [List-of X] -> Y
; sort:      [List-of X] (X X -> Boolean) -> [List-of X]
; build-list: NatNumber     (NatNum -> X) -> [List-of X]
; andmap:    (X -> Boolean)   [List-of X] -> Boolean
; ormap:     (X -> Boolean)   [List-of X] -> Boolean




(define JANUS
  (list #i31
        #i2e+34
        #i-1.2345678901235e+80
        #i2749
        #i-2939234
        #i-2e+33
        #i3.2e+270
        #i17
        #i-2.4e+270
        #i4.2344294738446e+170
        #i1
        #i-8e+269
        #i0
        #i99))











