;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab-6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; CISC106, Fall 2017, Lab 6 Starter

(require 2htdp/batch-io)
#|
;; --------- Part 1 --------------

; reads some state-based information from US Department of Commerce, 1977
(define STATE-INFO (read-csv-file "states.csv"))
(define HEADER (first STATE-INFO))
(define STATES (rest STATE-INFO))
; define a few of the lines of data for tests later
(define ALABAMA (first STATES))
(define DELAWARE (eighth STATES))
(define ALASKA (second STATES))


; a state-information-record, SIR, is a list of strings where the position in the list
; determines the interpretation of the string:
; (list "Name" "Population" "Income" "Illiteracy" "Life Exp" "HS Grad" "Frost" "Area")
;
; The first position in the list is the state's name, the second position its population,
; the third its income, etc.

; A LLS is one of:
; – empty
; – (cons Los LLS)
; interp. a list of lines, each line is a list of strings

; total-population: LLS -> number
; Produces the total US population (in thousands) from a list of state information
; records.  
(check-expect (total-population (list ALABAMA DELAWARE)) 4194)
(check-expect (total-population empty) 0)

(define (total-population aLLS)
  (cond [(empty? aLLS) 0]
        [(cons? aLLS)(+ (string->number (second (first aLLS)))
                        (total-population (rest aLLS)))]))

; total US population in 1977 : 212321 thousand

(define LIFE-EXPECTANCY (list 0 0 0 1 0 0 0))
; score: LoS LoN -> number
; Produces a score which is the sum of the products between LoS and LoN.
; Multiplies each coefficient in the list of numbers by the value in the 
; same position in the list of strings and sums all of these products.
; Converts each value in the LoS to a number. If the two lists are of different
; lengths it will produce the shortest common sum-product.
(check-expect (score (list "5") (list 1 1)) 5) ; 5*1 => 5
(check-expect (score (list "5" "6") empty) 0) ; no score possible
(check-expect (score (list "5" "6") (list 1 1)) 11) ; 5*1 + 6*1 => 11
(check-expect (score (list "5" "6" "7") (list 1 0 2)) 19) ; 5*1 + 6*0 + 7*2 => 19
(check-expect (score (rest DELAWARE) (list 1 1 1 1 1 1 1)) 7598.56)
(check-expect (score (rest DELAWARE) LIFE-EXPECTANCY) 70.06)

(define (score aLOS aLON)
  (cond [(or(empty? aLOS)(empty? aLON)) 0]
        [else (+ (* (string->number(first aLOS)) (first aLON))
                 (score (rest aLOS) (rest aLON) ))
              ]
   ))



; higher-score: SIR SIR LoN -> SIR
; Produces the state-information-record with the higher score using the LoN.
; If either SIR is empty, produce the non-empty SIR.
(check-expect (higher-score DELAWARE ALASKA LIFE-EXPECTANCY) DELAWARE)
(check-expect (higher-score DELAWARE empty LIFE-EXPECTANCY) DELAWARE)
(check-expect (higher-score empty ALASKA LIFE-EXPECTANCY) ALASKA)

(define (higher-score SIR1 SIR2 LON)
  (cond [(empty? SIR1)SIR2]
        [(empty? SIR2) SIR1]
        [else
         (cond [(> (score (rest SIR1) LON) (score (rest SIR2) LON)) SIR1]
               [(< (score (rest SIR1) LON) (score (rest SIR2) LON)) SIR2])]))
              



; max-score: LLS LoN -> SIR
; Produces the state-information-record with the highest overall score using
; the LoN.
(check-expect (first (max-score STATES LIFE-EXPECTANCY)) "Hawaii")
(check-expect (max-score empty LIFE-EXPECTANCY) empty)

(define (max-score aLLS aLON)
  (cond [(empty? aLLS) empty]
        [(cons? aLLS)
         (higher-score (first aLLS)  (max-score (rest aLLS) aLON) aLON)]))
               




; Figure out where you would want to live in 1977!!
; I want a lower population state with high income, low illiteracy,
; high life expectancy, high HS graduation rates, a decent amount of frost,
; and a small area
(define MY-QUALITY-OF-LIFE (list -0.01 0.01 -5 0.1 0.1 0.05 -0.0001))
(max-score STATES MY-QUALITY-OF-LIFE)
; TODO: define your own quality of life metric and call max-score here
; to find out where you should live!

(define DEBRA-QUALITY-OF-LIFE (list  .01 .02 -.03 .003 .01 .06 -.04))
(max-score STATES DEBRA-QUALITY-OF-LIFE)

(define ANNA-QUALITY-OF-LIFE (list  -.01 .06 .09 .02 .02  .05 -.003))
(max-score STATES ANNA-QUALITY-OF-LIFE)

#|
;; --------- Part 2 --------------
(define-struct track (species id miles season))
;; make-track: String Number Number String -> Track
;; interp.
;; species (String): animal species ["FOX" or "COYOTE"]
;; id (Number): a unique id number on tracking collar of animal
;; miles (Number): the number of miles they traveled during season
;; season (String): the season the animal was tracked
;;    ["FALL", "WINTER", etc.]

;; convert-to-tracks : [ListOf String 4 elements long] -> Track
;; ID and Miles can be interpreted as numbers, so we use string-->number
;; we know that the input list is EXACTLY 4 elements, so we use
;; first, second, third, fourth on the input list
(define (convert-to-tracks l)
 (make-track (first l)
             (string->number (second l))
             (string->number (third l))
             (fourth l)))
; reads the file into a List of Lists of Strings, skipping the first line
(define ANIMAL-LINES
  (rest (read-csv-file "Animals.csv"))) 

;; You need to write the function signatures, purpose, unit tests,
;; and definition for problems 2.1-2.5


;; --------- Part 3 --------------
;; You need to re-write the function signatures, purpose, unit tests,
;; and definition for problems 3.1-3.5

|#
|#


; PROBLEM 4.1

;A Word is either
; - empty, or
; - (cons Letter Word)
;where Letter is a length 1 String in ["a", "b", ..., "z"]

(define word1 (list "a" "s" "k"))
(define word2 (list "n" "e" "a" "t" "r" ))
(define word3 (list "c" "d" "e" "n" "e"))

; A List-of-Words [LoW] is either
;  - empty
;  - (cons Word LoW)

(define low1 (list word1 word2 word3))
(define low2 (list (list "a" "e" "c" "t") (list "v" "e" "k")))

#|

;insert-everywhere : Letter Word -> Word
; Consumes: Letter aletter, Word aword
; Produces: A word with the letter input inserted at
;           beginning, end, and inbetween all letters.

(define (insert-everywhere aletter aword)
  (cond [(empty? aword)(cons aletter empty)]
        [(cons? aword)
            (append (list aletter (first aword))
            (insert-everywhere aletter (rest aword)))
            ]))
   
(check-expect (insert-everywhere "b" (list "a" "n" "d" "s"))
              (list "b" "a" "b" "n" "b" "d" "b" "s" "b"))
(check-expect (insert-everywhere "v" '() ) (cons "v" empty))


;insert-everywhere/in-all-words : Letter LoW -> LoW
; Consumes: Letter aletter, LoW alow
; Produces: A list of words with the letter inserted at the
;           between all lettters and at beginning and end of all words in that list. 


(define (insert-everywhere/in-all-words aletter alow)
   (cond [(empty? alow) empty]
         [(cons? alow)
           (cons 
            (insert-everywhere aletter (first alow))
            (insert-everywhere/in-all-words aletter (rest alow)))]))


(check-expect (insert-everywhere/in-all-words "d"
              (list (list "e" "r") (list"r" "e")))
              (list (list "d" "e" "d" "r" "d") (list "d" "r" "d" "e" "d")))
(check-expect (insert-everywhere/in-all-words "d" '()) '() )



;; arrangements : Word -> List-of-Words
;; to create a list of all rearrangements of the letters in a-word

(define (arrangements aword)
 (cond
   [(empty? aword) (cons empty empty)]
   [(cons? aword) (insert-everywhere/in-all-words
                     (first aword)
                     (arrangements (rest aword)))]
   ))


(arrangements word1)




;insert-everywhere : Letter Word -> LoW
; Consumes: Letter aletter, Word aword
; Produces: A list of words with the letter input inserted at
;           beginning, end, and inbetween all letters.

(define (insert-everywhere aletter aword)
  (cond [(empty? aword) (cons aletter empty)]
        [(cons? aword)
            (list
              (append (list aletter) aword)
              (append (first aword) (list aletter) (rest aword))

              )]))
            
   
(check-expect (insert-everywhere "b" (list "a" "n"))
              (list (list "b" "a" "n")(list  "a" "b" "n")(list  "a" "n" "b")))
(check-expect (insert-everywhere "b" '()) '("b"))

(check-expect (insert-everywhere "b" (list(list "a" "n")(list "v" "c")))
              (list (list "b" "a" "n")(list  "a" "b" "n")(list  "a" "n" "b")
                     (list "b" "v" "c")(list  "v" "b" "c")(list  "v" "c" "b")))


;insert-everywhere/in-all-words : Letter LoW -> LoW
; Consumes: Letter aletter, LoW alow
; Produces: A list of words with the letter inserted at the
;           between all lettters and at beginning and end of all words in that list. 


(define (insert-everywhere/in-all-words aletter alow)
   (cond [(empty? alow) empty]
         [(cons? alow)
           (cons 
            (insert-everywhere aletter (first alow))
            (insert-everywhere/in-all-words aletter (rest alow)))]))


(check-expect (insert-everywhere/in-all-words "d"
              (list (list "e" "r") (list"r" "e")))
              (list (list "d" "e" "d" "r" "d") (list "d" "r" "d" "e" "d")))
(check-expect (insert-everywhere/in-all-words "d" '()) '() )

|#

;insert : Number String ListofStrings -> ListofStrings
; Number index: the index at which to insert
; String astring: the string that should be inserted
; LoS aLOS:


#;(define (insert index astring aLOS)
  (cond [(empty? aLOS) astring]
        [(cons? aLOS)
            (cond [(= index (length aLOS)) (list* astring aLOS) ]
                  [else (cons (first aLOS)(insert index astring (rest aLOS))) ]
                  )]))
#;(define (insert index astring aLOS)
  (cond [(empty? aLOS) (cons astring empty)]
        [(cons? aLOS)
            (cond [(>= index (length aLOS)) (cons aLOS astring)]
                  [( = index 0) (cons astring aLOS)]
                  [(= index (length aLOS)) (list* astring aLOS) ]
                  [else (cons (first aLOS)(insert index astring (rest aLOS))) ]
                  )]))

#;(define (insert n alon)
  (cond [(empty? alon) (cons n empty) ]
        [(cons? alon)
         (cond[( < n (first alon)) (cons n alon)  ]
              [else (cons (first alon) (insert n (rest alon)))]
         )
]))

#;(define (insert index astring aLOS)
        (cons (list-ref aLOS 0)(replace index astring aLOS)) 

)
;(check-expect (insert 2 "a" (list "b" "b" "c" "c")) (list "b" "b" "a" "c" "c"))
;(check-expect (insert 0 "a" (list "b" "b" "c" "c")) (list "a" "b" "b" "c" "c"))


;(append (list "a" "b" "c") (list "d" "e"))


(define (addfirstletter aletter aLOW)
  (cond
    [(empty? aLOW) empty]
    [(cons? aLOW)
          (cons (cons aletter (first aLOW)) (addfirstletter aletter (rest aLOW)))]))

(define (insert-everywhere aletter aword)
  (cond
    [(empty? aword) (cons (cons aletter aword) empty)]
    [else (cons (cons aletter aword)
          (addfirstletter (first aword) (insert-everywhere aletter (rest aword))))]))

;(addtolist "s" (list (list "v" "v" "v")(list "c" "c" "c")))




                    