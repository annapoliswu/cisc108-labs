;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab6-FINAL2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; CISC106, Fall 2017, Lab 6 Starter
;; Names: Debra Lymon, Anna Wu

(require 2htdp/batch-io)

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

;; 2.1

;; lines->tracks : aLLS --> aLOT
;; Consumes a list of lists of strings
;; Produces a list of tracks


(define (lines->tracks aLLS)
  (cond [(empty? aLLS) empty]
        [(cons? aLLS)
         (cons (convert-to-tracks (first aLLS))
                             (lines->tracks  (rest aLLS)))]))

(check-expect (lines->tracks (list (list "FOX" "1" "0" "winter")(list "COYOTE" "3" "2" "fall")))
                             (list (make-track "FOX" 1 0 "winter") (make-track "COYOTE" 3 2 "fall")))
(check-expect (lines->tracks '()) '())


;; 2.2

;; count-coyote : aLOT --> Number
;; Consumes a list of tracks
;; Produces the number of "COYOTE" tracks in the list of tracks


(define (count-coyote aLOT)
  (cond [(empty? aLOT) 0 ]
        [(cons? aLOT)
            (cond [(string=? "COYOTE" (track-species(first aLOT)))
                      (+ 1 (count-coyote (rest aLOT)))]
                  [ else (count-coyote (rest aLOT)) ]
                  )]))
                       

(define ANIMAL-TRACKS (lines->tracks ANIMAL-LINES))
; 12 coyotes

(check-expect (count-coyote ANIMAL-TRACKS) 12)
(check-expect (count-coyote '() ) 0)


;; 2.3

;; gt6miles : aLOT --> aLOS
;; Consumes a list of tracks
;; Produces a list of seasons (strings) from the animals that have traveled
;; more than 6 miles

(define (gt6miles aLOT)
  (cond [(empty? aLOT) empty]
        [(cons? aLOT)
         (cond [(> (track-miles (first aLOT)) 6)
                (cons (track-season (first aLOT))
                      (gt6miles (rest aLOT)))]
               [else (gt6miles (rest aLOT))])]))
                     
(check-expect (gt6miles ANIMAL-TRACKS) (list "spring" "summer" "fall" "spring" "summer"))
(check-expect (gt6miles '()) '())


;; 2.4


;; add-miles : aLOT --> Number
;; Consumes a list of tracks
;; Produces the sum of all the miles in the tracks

(define (add-miles aLOT)
  (cond [(empty? aLOT) 0]
        [(cons? aLOT)
          (+ (track-miles (first aLOT)) (add-miles (rest aLOT)))]))

(check-expect (add-miles ANIMAL-TRACKS) 107)
(check-expect (add-miles '()) 0)

;; average-miles : aLOT --> Number
;; Consumes a list of tracks
;; Produces the average miles traveled for all tracks

(check-expect (average-miles ANIMAL-TRACKS) (/ 107 24))
(check-expect (average-miles '()) 0)

(define (average-miles aLOT)
  (cond
    [(empty? aLOT) 0]
    [(cons? aLOT) (/ (add-miles aLOT) (length aLOT))]))

;; 2.5

;; average-column : aLOT column-selector-fun --> Number
;; Consumes a list of tracks and column-selector function
;; Produces the average of that column



    
(define (add-column aLOT afun)
  (cond [(empty? aLOT) 0]
        [(cons? aLOT)
          (+ (afun (first aLOT)) (add-column (rest aLOT) afun))]))

(define (average-column aLOT afun)
  (cond
   [(empty? aLOT) 0]
    [(cons? aLOT) (/ (add-column aLOT afun) (length aLOT))])) 

(check-expect (average-column ANIMAL-TRACKS track-miles) (/ 107 24))
(check-expect (average-column ANIMAL-TRACKS track-miles) (average-miles ANIMAL-TRACKS))



;; --------- Part 3 --------------
;; You need to re-write the function signatures, purpose, unit tests,
;; and definition for problems 3.1-3.5

;; 3.1

;; lines->tracks.v2 : aLLS --> aLOT
;; Consumes a list of lists of strings
;; Produces a list of tracks


(define (lines->tracks.v2 aLLS)
            (map convert-to-tracks aLLS)

  )

(check-expect (lines->tracks.v2 (list (list "FOX" "1" "0" "winter")(list "COYOTE" "3" "2" "fall")))
                             (list (make-track "FOX" 1 0 "winter") (make-track "COYOTE" 3 2 "fall")))
(check-expect (lines->tracks.v2 '()) '())





;; 3.2

;; count-coyote.v2 : aLOT --> Number
;; Consumes a list of tracks
;; Produces the number of "COYOTE" tracks in the list of tracks


(define (iscoyote? atrack)
  (string=? "COYOTE" (track-species atrack)))

(define (count-coyote.v2 aLOT)
     (length (filter iscoyote? aLOT)))              




(check-expect (count-coyote.v2 ANIMAL-TRACKS) 12)
(check-expect (count-coyote.v2 '() ) 0)



;; 3.3

;; gt6miles.v2 : aLOT --> aLOS
;; Consumes a list of tracks
;; Produces a list of seasons (strings) from the animals that have traveled
;; more than 6 miles

(define (gt6? atrack)
  (> (track-miles atrack) 6))

(define (gt6miles.v2 aLOT)
   (map track-season (filter gt6? aLOT)))

                     
(check-expect (gt6miles.v2 ANIMAL-TRACKS) (list "spring" "summer" "fall" "spring" "summer"))
(check-expect (gt6miles.v2 '()) '())


;; 3.4

;; average-miles.v2 : aLOT --> Number
;; Consumes a list of tracks
;; Produces the average miles traveled for all tracks



(define (average-miles.v2 aLOT)
  (cond [(= (length aLOT)0) 0] 
        [else (/ (foldr + 0 (map track-miles aLOT)) (length aLOT))]))


(check-expect (average-miles.v2 ANIMAL-TRACKS) (/ 107 24))
(check-expect (average-miles.v2 '()) 0)

;; 3.5

;; average-column.v2 : aLOT column-selector-fun --> Number
;; Consumes a list of tracks and column-selector function
;; Produces the average of that column


(define (average-column.v2 aLOT afun)
  (cond
   [(empty? aLOT) 0]
    [(cons? aLOT) (/ (foldr + 0 (map afun aLOT)) (length aLOT))]
    )) 

(check-expect (average-column.v2 ANIMAL-TRACKS track-miles) (/ 107 24))
(check-expect (average-column.v2 ANIMAL-TRACKS track-miles) (average-miles ANIMAL-TRACKS))



; PROBLEM 4.1

;A Word is either
; - empty, or
; - (cons Letter Word)
; where Letter is a length 1 String in ["a", "b", ..., "z"]

(define word1 (list "a" "s" "k"))
(define word2 (list "n" "e" "a" "t" "r" ))
(define word3 (list "c" "d" "e" "n" "e"))

; A List-of-Words [LoW] is either
;  - empty
;  - (cons Word LoW)

(define low1 (list word1 word2 word3))
(define low2 (list (list "a" "e" "c" "t") (list "v" "e" "k")))



;; 4.2

;; addfirstletter : Letter LoW -> LoW
;Consumes: a letter and a list of words
;Produces: a list of words with the letter added to the beginning of each word

(define (addfirstletter aletter aLOW)
  (cond
    [(empty? aLOW) '()]
    [(cons? aLOW)
          (cons (cons aletter (first aLOW))
          (addfirstletter aletter (rest aLOW)))]))

(check-expect (addfirstletter "d" '()) '())
(check-expect (addfirstletter "f"
                    (list (list "v" "v" "v")(list "c" "c" "c")))
                    (list (list "f" "v" "v" "v") (list "f" "c" "c" "c")))



;; insert-everywhere : Letter Word -> LoW
;; Consumes: a letter and a word
;; Produces: a list of words with that letter inserted
;            into every possible position in the word

(define (insert-everywhere aletter aword)
  (cond
    [(empty? aword) (cons (cons aletter aword) empty)]
    [(cons? aword) (cons (cons aletter aword)
          (addfirstletter (first aword) (insert-everywhere aletter (rest aword)))
          )]))

(check-expect (insert-everywhere "v" '()) (list (list "v")))

(check-expect (insert-everywhere "v" (list "c")) (list(list "v""c")(list "c" "v")))

(check-expect (insert-everywhere "v" (list "a" "a"))
              (list (list "v" "a" "a")(list "a""v" "a")(list "a" "a" "v")))


;; insert-everywhere/in-all-words : Letter LoW -> LoW
; Consumes a letter and a list of words
; Produces a list of words with the letter inserted at
; every possible position in every word 

(define (insert-everywhere/in-all-words aletter aLOW)
  (cond
    [(empty? aLOW) '()]
    [else (append
           (insert-everywhere aletter (first aLOW))
           (insert-everywhere/in-all-words aletter (rest aLOW)))]))


(check-expect (insert-everywhere/in-all-words "v" '()) '())  

(check-expect (insert-everywhere/in-all-words "v" (list (list "a")(list "b")))
(list (list "v" "a")(list "a" "v")(list "v" "b")(list "b" "v")))    

(check-expect (insert-everywhere/in-all-words "v" (list(list "a" "a")(list "b" "b")))
              (list (list "v" "a" "a")(list "a""v" "a")(list "a" "a" "v")
                    (list "v" "b" "b")(list "b" "v" "b")(list "b" "b" "v")))
                             

;; arrangements : Word -> List-of-Words
;; Consumes: a word
;; Produces:  a list of all rearrangements of the letters in a-word

(define (arrangements aword)
 (cond
   [(empty? aword) (cons empty empty)]
   [(cons? aword)  (insert-everywhere/in-all-words
                     (first aword)
                     (arrangements (rest aword)))]
   ))


(check-expect (arrangements '()) (list '()) )
(check-expect (arrangements (list "v")) (list (list "v")) )
(check-expect (arrangements (list "a" "b"))
              (list (list "a" "b") (list "b" "a")))

             


;; 4.3

;; arrange-main : String --> LoS
;; Consumes a string
;; Produces all of its possible re-arrangements as a LoS
(define (arrange-main astring)
  (cond [(empty? astring) empty]
        [else (map implode (arrangements (explode astring)))]))


(check-expect (arrange-main "bc") (list "bc" "cb"))
(check-expect (arrange-main "b") (list "b"))
(check-expect (arrange-main '()) '())



