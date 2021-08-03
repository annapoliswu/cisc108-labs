;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname groceries-exercise) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; a grocery-item is a string listed on amazon.com under Grocery & Gourmet Food

;; VERSION 1: specific list
;; a list of grocery-items, LoGI, is one of
;; - empty
;; - (make-grocery-list grocery-item LoGI)
(define-struct grocery-list (item next))

(define MY-GROCERIES (make-grocery-list "banana"
                                (make-grocery-list "coffee"
                                           (make-grocery-list "yogurt"
                                                      (make-grocery-list "turkey" empty)))))


#| template

(define (gl-fun agl)
  (cond [(empty? agl) ...]
        [(grocery-list? agl) ... (grocery-list-item agl)
                             ... (gl-fun (grocery-list-next agl))]))

|#

; contains?: grocery-list string --> boolean
; Consumes a grocery list and a grocery item and produces true
; if the grocery item appears in the list
(check-expect (contains?.v1 MY-GROCERIES "eggs") false)
(check-expect (contains?.v1 MY-GROCERIES "coffee") true)

; VERSION 1 for contains?
(define (contains?.v1 agl anitem)
  (cond [(empty? agl) false]
        [(grocery-list? agl) (or (string=? (grocery-list-item agl) anitem)
                                 (contains?.v1 (grocery-list-next agl) anitem))]))

;; VERSION 2: generalized list
;; a list of grocery-items, LoGI, is one of
;; - empty
;; - (cons grocery-item LoGI)
(define MY-GROCERIES2 (cons "banana"
                            (cons "coffee"
                                  (cons "yogurt"
                                        (cons "turkey" empty)))))

;; defined already in Dr. Racket:
;empty 	= a special value, mostly to represent the empty list
;empty?	= a predicate that recognizes empty and nothing else
;cons	= a checked constructor for a two-field structure
;first	= the selector that extracts the last item added
;rest	= the selector that extracts the extended list
;cons?	= a predicate for instances created by cons

;(check-expect (contains?.v2 MY-GROCERIES2 "eggs") false)
;(check-expect (contains?.v2 MY-GROCERIES2 "coffee") true)
; VERSION 2 for contains?
; contains: LoGI -> boolean
; Consumes a LoGI and produces true
; if the grocery item appears in the list

; TODO: implement version 2 (we will do this together)








; replace-item: LoGI grocery-item grocery-item -> LoGI
; Produces a LoGI where each appearance of the bad grocery-item
; has been replaced by the good grocery-item
#|
(check-expect (replace-item MY-GROCERIES2 "turkey" "ham") 
              (cons "banana"
                    (cons "coffee"
                          (cons "yogurt"
                                (cons "ham" empty)))))
(check-expect (replace-item MY-GROCERIES2 "yogurt" "ice cream") 
              (cons "banana"
                    (cons "coffee"
                          (cons "ice cream"
                                (cons "turkey" empty)))))
(check-expect (replace-item MY-GROCERIES2 "peanut butter" "gnutella") 
              MY-GROCERIES2) ; peanut butter not in list
(check-expect (replace-item empty "milk" "chocolate") empty) ; nothing to replace
|#
; TODO: implement replace-item










; perishable?: grocery-item -> boolean
; Consumes a grocery item string and produces true if it is
; one of the items we are defining as "perishable items"
(check-expect (perishable? "banana") true)
(check-expect (perishable? "coffee") false)
(check-expect (perishable? "yogurt") true)
(check-expect (perishable? "turkey") true)
(define (perishable? anitem)
  (or (string=? "banana" anitem)
      (string=? "yogurt" anitem)
      (string=? "turkey" anitem)))
  




; how-many-perishable: LoGI -> number
; produce the count of perishable items in the list
(check-expect (how-many-perishable MY-GROCERIES2) 3)
(check-expect (how-many-perishable empty) 0)

; TODO: implement how-many-perishable:
(define sum 0)

(define (how-many-perishable al)
  (cond [(empty? al) 0]
        [(cons? al) (cond[(perishable? (first al)) (+ (how-many-perishable (rest al)) 1)]
                         [ else (how-many-perishable (rest al))])]))

(define (how-many-perishable al)
  (cond [(empty? al) 0]
        [(cons? al) (if(perishable? (first al)) 1 0)
                         (+ how-many-perishable (rest al))]))
#|       
(define (list-fun al)
  (cond [(empty? al) ...]  ;if empty does something
        [(cons? al) ... (first al)  ;basically like a for loop
                     ... (list-fun (rest al))]))
        

|#



; perishable-items: LoGI -> LoGI
; produce a list of perishable items
(check-expect (perishable-items MY-GROCERIES2) (cons "banana" (cons "yogurt" (cons "turkey" empty))))
(check-expect (perishable-items empty) empty)


; TODO: implement perishable-items:

(define (list-fun al)
  (cond [(empty? al) empty]  ;if empty does something
        [(cons? al) (cond [(perishable? (first al)) (cons (first al)(perishable-items (rest al)))] ;if perishabl, construct first item in list and then rest??
            )]))



















; something wrong with this function we will fix after list-numbers

; comma-separated: LoGI -> string
; Produces a string representation of the list of grocery items
; where each item is separated by a comma+space: ", "
#|
(check-expect (comma-separated MY-GROCERIES2) "banana, coffee, yogurt, turkey")
(define (comma-separated agl)
  (cond [(empty? agl) ""]
        [(cons? agl) (string-append (first agl) ", "
                                    (comma-separated (rest agl)))]))
|#

