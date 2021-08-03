;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname child) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
 ;; MUTUALLY REFERENTIAL DATA DEFINITIONS: FAMILY TREES

;; An EyeColor is a Symbol, one of
;; -- 'blue
;; -- 'brown

;; an Unknown is a (constant) struct representing an unknown FT
(define-struct unknown [])
(define UNKNOWN (make-unknown))

;; a FamilyTree [FT] is either
;; -- UNKNOWN
;; -- (make-child FT FT String Number EyeColor)
(define-struct child [mom dad name bd ec])
;; Field Interpretation
;;  FT      mom: the child's mom, or UNKNOWN
;;  FT      dad: the child's dad, or UNKNOWN
;;  String name: the child's first name
;;  Number   bd: the year of child's birth
;;  EyeColor ec: the child's eye color

;; TEMPLATE FOR FT
#;(define (ft-fun ft)
    (cond [(unknown? ft) ...]  ;deined as struc above, empty? would complicate things..
          [else (child-name ft)
                ...(child-bd ft)
                ...(child-ec ft)
               (ft-fun (child-mom ft)) ;anything emb. structure, needs function on it?
               (ft-fun (child-dad ft))
    ]))

;; EXAMPLES          
(define FT-BLUE
  (make-child 
   (make-child UNKNOWN UNKNOWN "Tonya" 1969 'brown)
   (make-child 
    (make-child UNKNOWN UNKNOWN "June" 1939 'brown)
    (make-child UNKNOWN UNKNOWN "John" 1936 'blue)
    "Kevin" 1968 'blue)
   "Keegan" 1995 'blue))

;^ Read: Keegan-> Mom Tonya
;              -> Dad Kevin -> Mom June
;                           -> Dad John

(define FT-ALL-BROWN
  (make-child 
   (make-child UNKNOWN UNKNOWN "Tonya" 1969 'brown)
   (make-child 
    (make-child UNKNOWN UNKNOWN "June" 1939 'brown)
    (make-child UNKNOWN UNKNOWN "John" 1936 'brown)
    "Kevin" 1968 'brown)
   "Keegan" 1995 'brown))

;; alternate definition method!
(define TONYA (make-child UNKNOWN UNKNOWN "Tonya" 1969 'brown))
(define JUNE (make-child UNKNOWN UNKNOWN "June" 1939 'blue))
(define JOHN (make-child UNKNOWN UNKNOWN "John" 1936 'brown))
(define KEVIN (make-child  JUNE JOHN "Kevin" 1968 'brown))
(define KEEGAN (make-child TONYA KEVIN "Keegan" 1995 'brown))
(define FT-GRANDMA-BLUE KEEGAN)
  
;; count-persons : FT --> Number
;; Consumes
;;  FT ft
;; Produces the number of people in ft
(check-expect (count-persons UNKNOWN) 0)
(check-expect (count-persons FT-BLUE) 5)

(define (count-persons ft)
      (cond [(unknown? ft) 0]  
          [else
               (+ 1
               (count-persons (child-mom ft)) 
               (count-persons (child-dad ft)))
    ]))

;Nat -Matt, Amber
;Jos -Amy, Joyelle
;Ask them to ask their friends...

;; blue-eyed-ancestor? : FT ->Boolean
;; Consumes:
;;  FT ft
;; Produces true if this person (ft) or any ancestor has blue eyes.
(check-expect (blue-eyed-ancestor? UNKNOWN) false)
(check-expect (blue-eyed-ancestor? FT-BLUE) true)
(check-expect (blue-eyed-ancestor? FT-ALL-BROWN)  false)
(check-expect (blue-eyed-ancestor? FT-GRANDMA-BLUE) true)

(define (blue-eyed-ancestor? ft)
    (cond [(unknown? ft) false]  
          [else (or (equal? (child-ec ft) 'blue) 
               (blue-eyed-ancestor? (child-mom ft))
               (blue-eyed-ancestor? (child-dad ft)))
    ]))

;; list-names : FT --> [ListOf String]
;; Consumes
;;  FT ft
;; Produces list of all names in tree ft
;; pre-order, in-order, post-order
;; IN-ORDER
(check-expect (list-names FT-BLUE) '("Keegan" "Tonya" "Kevin" "June" "John" ))
(check-expect (list-names UNKNOWN) '())
(define (list-names ft)
  (cond [(unknown? ft) empty]  ;deined as struc above, empty? would complicate things..
          [else
               (append
                (cons
               (child-name ft)
               (list-names (child-mom ft))) 
               (list-names (child-dad ft)))
    ]))








;; a DescendantTree [DT] is either
;; -- UNKNOWN
;; -- (make-parent ListOfDT String Number EyeColor)
(define-struct parent [kids name bd ec])
;; Field Interpretation
;;  ListOfDT  kids: the parent's kids
;;  String name: the parent's first name
;;  Number   bd: the year of parent's birth
;;  EyeColor ec: the parent's eye color

;; a ListOfDT is either
;; -- empty
;; -- (cons Parent ListOfDT)


;; alternate definition method!
(define JUNE2 (make-parent '(KEVIN2) "June" 1939 'blue))
(define JOHN2 (make-parent '(KEVIN2) "John" 1936 'brown))
(define TONYA2 (make-parent '(KEEGAN2) "Tonya" 1969 'brown))
(define KEVIN2 (make-parent '(KEEGAN2) "Kevin" 1968 'brown))
(define KEEGAN2 (make-parent empty "Keegan" 1995 'blue))
(define DT-GRANDCHILD-BLUE2 JOHN2)