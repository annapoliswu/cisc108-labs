;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname data-definitions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; DATA DEFINITIONS // TYPE DEFINITIONS

;; INTERVALS
;; well-defined subset of Number

;; Number
;; Integer
;; PosInt
;; NatNum i.e. "Natural Numbers", "Counting Numbers", 0 plus Positive Integers...
;; EvenNum
;; etc...

;; Example
;; string-length : String -> NatNum

;;===========================================================================
;; CUSTOM INTERVALS
;; "[" indicates "closed" or inclusive bound
;; "(" indicates "open" or exclusive bound

;; Example Custom Interval Data Definitions
;; Freezing is a temperature interval in Farenheit (interpretation!) (..32]
;; corresponding test: (<= x 32)

;; a BJCPOktoberfestIBU interval is [20..28]
;; indicating acceptable bitterness of Oktoberfest beers
;; by the BJCP (Beer Judge Certification Program)
;; measured in IBUs (International Bittering Units)
;; corresponding test: (<= 20 x 28)

;;===========================================================================
;; ITEMIZATIONS OF INTERVALS
;; Often a data definiton includes SEVERAL intervals
;; This has a huge effect on the typical code body
;; for functions that either consume or produce this data type

;; TESTING: programs with custom intervals should be tested AT THE BOUNDARIES 

;; Example Itemizations of Intervals
;; a WaterTemp is a Number interpreted as a Farenheit temperature
;; that falls in one of three intervals, either
;;  - (..32]    frozen,
;;  - (32..212] liquid, or
;;  - (212..)   steam (yes I know I'm simplifying by ignoring, e.g. pressure, plasma...)

;; Body Template
;; Any function that consumes data of type WaterTemp will either look like
;;  - a composition of existing WaterTemp functions, or
;;  - a conditional with 3 clauses (because there are 3 subintervals in the itemization)
#;(define (water-temp-fun wt)
  (cond [(<= wt 32)     ...]  ;frozen
        [(<= 32 wt 212) ...]  ;liquid
        [(<= 212 wt)    ...]));steam

;; USING THE TEMPLATE FOR A FUNCTION BODY
;; describe-water : WaterTemp --> String
;; Consumes
;;  WaterTemp wt: a water temperature in Farenheit
;; Produces a textual description of what water is like at that temperature

;; TESTING THE FIRST BOUNDARY @ 32
(check-expect (describe-water 32) "This is ice!")
(check-expect (describe-water 33) "This is liquid water, but it still could be too hot to drink")
;; TESTING THE SECOND BOUNDARY @ 212
(check-expect (describe-water 212) "This is liquid water, but it still could be too hot to drink")
(check-expect (describe-water 213) "This is steam, so be careful not to burn yourself")

(define (describe-water wt)
  (cond [(<= wt 32)     "This is ice!"]     ;frozen
        [(<= 32 wt 212) "This is liquid water, but it still could be too hot to drink"] ;liquid
        [(<= 212 wt)    "This is steam, so be careful not to burn yourself"]))  ;steam

;; CAREFUL: FUNCTION BODY MIGHT ALSO BE A FUNCTION COMPOSITION!!
;; describe-water-result : WaterTemp Number --> String
;; Consumes
;;  WaterTemp current: the current water temperature in Farenheit
;;  Number    delta:   the change in temperature to be applied (might be +/-)
;; Produces a textual description of what the water is like after the change in temperature
(check-expect (describe-water-result 33 -1) "This is ice!")
(define (describe-water-result current-temp delta)
  (describe-water (+ current-temp delta)))

;;===========================================================================
;; ENUMERATIONS
;; an enumeration is just a finite set of values from some other type.

;; Examples you've already seen:

;; a ColorString is any String in the DrRacket color-database (look up in Help Desk)

;; a Mode is either
;;  - "outline", or
;;  - "solid"

;; a Boolean is either
;;  - #true, or
;;  - #false

;; CUSTOM ENUMERATIONS

;; a CatState is one of
;;  - "sleeping",
;;  - "playing", or
;;  - "eating"

;; a DayOfWeek is one of
;;  - "Monday",
;;  - "Tuesday",
;;  - "Wednesday",
;;  - "Thursday",
;;  - "Friday",
;;  - "Saturday", or
;;  - "Sunday"

;; Enumeration also says something about the function body template.
;; Example:
;; A function that CONSUMES a CatState probably looks like this (or is a fn composition):
;; cat-state-consumer-fn : CatState --> ???
#;
(define (cat-state-consumer-fn acatstate)
  (cond [(string=? acatstate "sleeping") ...]
        [(string=? acatstate "playing")  ...]
        [(string=? acatstate "eating")   ...]))

;; A function that PRODUCES a CatState probably looks like this (or is a fn composition):
;; cat-state-consumer-fn : ??? --> CatState
#;
(define (cat-state-consumer-fn acatstate)
  (cond [... "sleeping"]
        [... "playing"]
        [... "eating"]))

;; weekend? : DayOfWeek --> Boolean
;; Consumes
;;  DayOfWeek day: the current day of the week
;; Produces #true if it is Saturday or Sunday
(check-expect (weekend? "Saturday") #true)
(check-expect (weekend? "Sunday") #true)
(check-expect (weekend? "Monday") #false)

(define (weekend? day)
  (cond [(string=? day "Sunday") #true]
        [(string=? day "Saturday") #true]
        [else #false])) ;DISCUSS: WHEN IS THIS GOOD? WHEN BAD?

;; PRO TIP: ANY FUNCTION THAT PRODUCES BOOLEAN WILL NEVER NEED TO USE COND!!!!!!
(define (weekend?.v2 day)
  (or (string=? day "Sunday")
      (string=? day "Saturday")))

;;===========================================================================
;; SIMPLE ITEMIZATIONS
;; an ITEMIZATION is just a finite set of values from MORE THAN ONE TYPE.
;; The TEMPLATE is again, either a simple function composition, OR
;; a conditional with one clause per possible Type.
          

;; Example you've already seen:
;; a Number/#false is either
;;  - Number, or
;;  - Boolean #false

;; string->number : String --> Number/#false

(define SCENE-WIDTH 100)
;; a CarState is either
;;  - a Number in [0..SCENE-WIDTH], interpreted as the x location of the car or
;;  - String "stopped", interpreted as the car located at x location SCENE-WIDTH
;; CarState TEMPLATE
#;
(define (car-state-fn cs)
  (cond [(and (number? cs)
              (<= 0 cs SCENE-WIDTH)) ...] ;Number
        [(string? cs) ...])) ;"stopped"

;; next-car-state : CarState --> CarState
;; Consumes
;;  CarState cs: the current car state
;; Produces the next state, which is one larger if the current state is <= SCENE-WIDTH,
;; or "stopped" otherwise.
(check-expect (next-car-state 0) 1)
(check-expect (next-car-state SCENE-WIDTH) (+ 1 SCENE-WIDTH))
(check-expect (next-car-state (+ 1 SCENE-WIDTH)) "stopped")
(check-expect (next-car-state "stopped") "stopped")

(define (next-car-state cs)
  (cond [(and (number? cs)
              (<= 0 cs SCENE-WIDTH)) (+ 1 cs)] ;Number, increment by one
        [else "stopped"])) ;Handles both cs > SCENE-WDITH and cs value is "stopped"