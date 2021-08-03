;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab5VER2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Lab 5
;Name: Zihan (Anna) Wu

(require 2htdp/batch-io)

;QUESTION 1

;mph->knots : Number -> Number
;Consumes: Number mph(miles per hour)
;Produces: The knots(nautical miles per hour) equal to mph that was input

(define (mph->knots mph)
 (* mph (/ 5280 6076))
   )

;TESTING
(check-within (mph->knots 60) 52.14 .001)
(check-within (mph->knots 1) (/ 52.14 60) .001)


;QUESTION 2

;largest-of : Number Number Number -> Number
;Consumes : Number number1, Number number2, Number number3
;Produces : The number that is the largest of the three input.

(define (largest-of number1 number2 number3)
 (cond[(and (>= number1 number2)(>= number1 number3)) number1]
      [(and (>= number2 number1)(>= number2 number3)) number2]
      [(and (>= number3 number1)(>= number3 number2)) number3]
      )
  )

;TESTING 
(check-expect (largest-of 3 4 0) 4)
(check-expect (largest-of 4 8 4) 8)
(check-expect (largest-of 1 2 4) 4)
(check-expect (largest-of 1 4 -3) 4)
(check-expect (largest-of 2 2 2) 2)




;3.1

;A letter-order is (lined color)
; Number qty : The quantity of sheets in the order
; Boolean lined : If the paper is lined or not
; String color : Color of the paper - "white", "yellow", or "green"

(define-struct letter-order (qty lined color))

;EX
(define letterorder1 (make-letter-order 30 true "white"))


;A a4-order is (qty color)
; Number qty : The quantity of sheets in the order
; String color : Color of the paper - "white", "yellow", or "green"

(define-struct a4-order (qty color))

;EX
(define a4order1 (make-a4-order 60 "yellow"))



;3.2

; A paper-order is either a
; - letter-order
; - a4-order



;3.3

; price : paper-order -> number
; consumes : paper-order apo - a general paper-order (letter or a4)
; produces : the price of the paper order in dollars

;CONSTANT DEFINITIONS
;cost per sheet of lined letter paper
(define linedlettercost .05)
;cost per sheet of unlined letter paper
(define unlinedlettercost .02)
;cost per sheet of a4 paper in orders of less than 100
(define lessa4cost .07)
;cost per sheet of a4 paper in orders of 100 or more
(define morea4cost .05)


(define (price apo)
  
  ( cond[(letter-order? apo)
            (cond [(letter-order-lined apo) (*(letter-order-qty apo) linedlettercost)]
                  [(not(letter-order-lined apo)) (*(letter-order-qty apo) unlinedlettercost)] )]
        
        [(a4-order? apo)
            (cond [(< (a4-order-qty apo) 100) (*(a4-order-qty apo) lessa4cost)]
                  [(>= (a4-order-qty apo) 100) (*(a4-order-qty apo) morea4cost)] )]
   ))

;TESTING
(check-expect (price (make-letter-order 100 true "white")) 5)
(check-expect (price (make-letter-order 100 false "white")) 2)
(check-expect (price (make-a4-order 100 "white")) 5)
(check-expect (price (make-a4-order 10 "white")) .7)
(check-expect (price (make-a4-order 0 "white")) 0)





;4.1

(define-struct date (day month year))
; A Date is a (number number number)
; Number day : The day of the date
; Number month : The month of the date
; Number year : The year of the date
;Ex
(define date1 (make-date 02 22 1998))


(define-struct time (hour minute))
; A Time is a (number number)
; Number hour : The hour of the time (military hours)
; Number minute : The minutes of the time
;Ex
(define time1 (make-time 13 30))

(define-struct time-reminder (text date time))
; A Time-Reminder is a (string date time)
; String text : the text / message of the reminder
; Date date : the date of when to remind 
; Time time : the time of when to remind 
;Ex
(define time-reminder1 (make-time-reminder "Hi" date1 time1))

(define-struct GPSloc (latitude longitude))
; A GPSloc is a (number number)
; Number latitude : the latitude of our location
; Number longitude : the longitude of our location
;Ex
(define GPSloc1 (make-GPSloc 34.333 65.80))

(define-struct GPS-Reminder (GPSloc text arrivalremind))
; A GPS-Reminder is a (GPSloc string boolean)
; GPSloc GPSloc : the GPSlocation (latitude longitude) of our reminder
; String text : the message of our reminder
; Boolean arrivalremind : true if it reminds when you arrive at a location, false if it reminds when you leave. 
;Ex
(define GPSremind (make-GPS-Reminder GPSloc1 "meeeh" true))


; A Reminder is either a:
;  - Time-Reminder
;  - GPS-Reminder



;4.2

;TEMPLATE
#;(define (item-fun aitem)
   (cond [(discreteitem? aitem) (discreteitem-fun aitem)]
         [(bulkitem? aitem) (bulkitem-fun aitem)]))




;5

(define-struct ddate (dday month year))
; A ddate is (make-ddate number number number)
; interp: day is the day of the date
; month is the month of the date
; year is the year of the date

; *posn already defined by DrRacket
;(define-struct posn (x y))
; A posn is (make-posn number number)
; interp: x is the x coordinate of the position
; y is the y cooridinate of the position

(define-struct event (when where))
; An event is (make-event ddate posn)
; interp: ddate is date when the event occurred
; position is position where the event occurred



;overlappingevent? : event event -> boolean
;consumes : Event event1 , Event event2
;produces: true if event1 and event2 occur on the same date and at the same posn.
(define (overlappingevent? event1 event2)
  (and (equal? (event-when event1)(event-when event2))
       (equal? (event-where event1)(event-where event2))
        
   ))


;TESTING
(define posn1(make-posn 2 3))
(define ddate1(make-ddate 12 1 2000))

(define event1(make-event ddate1 posn1))
(define event2(make-event (make-ddate 12 1 2000)(make-posn 2 3) ))
(define event3(make-event (make-ddate 1 10 2000)(make-posn 2 3) ))
(define event4(make-event (make-ddate 12 1 2000)(make-posn 3 3) ))

(check-expect (overlappingevent? event1 event1) true)
(check-expect (overlappingevent? event1 event2) true)
(check-expect (overlappingevent? event1 event3) false)
(check-expect (overlappingevent? event1 event4) false)




;6.1

;A ListOfStrings (LOS) is a list of Strings.
; - '()
; - (cons String ListOfStrings)

(define LOS1 (list "Victor Hen" "Korren Asper" "Stewie Stevens" "Korren Asper" "Korren Asper" "Jenny Floor"))
(define LOS2 (list "Lisa Samus" "Eli Smith" "Meena Ryans"))
(define LOS3 (list "Bennie Vault" "Lauren Dennis"))
(define LOS4 (list "Bennie Vault" "Bennie Vault" "Meena Ryans" "Meena Ryans"))

;TEMPLATE
#; (define (LOS-fun aLOS)
     (cond [(empty? aLOS) ... ]
           [(cons? aLOS) ... (first aLOS)
                         ... (LOS-fun(rest aLOS))]
           ))

;6.2
;; num-starts: string LOS -> number
;; consumes a name and a list of short-stop-starts and
;; produces the number of times the name appears on the list

(define (num-starts astring aLOS)
     (cond [(empty? aLOS) 0 ]
           [(cons? aLOS)
            (cond [(string=? (first aLOS) astring) (+ 1 (num-starts astring (rest aLOS)))]
                  [ else (num-starts astring (rest aLOS))]
           )]))

(check-expect (num-starts "Korren Asper" LOS1) 3)
(check-expect (num-starts "Korren Asper" LOS2) 0)
(check-expect (num-starts "Bennie Vault" LOS3) 1)
(check-expect (num-starts "Craig Silver" '()) 0)

;6.3

;; more-starts: string string LOS -> string
;; consumes 2 names and a list of short-stop-starts and
;; produces the name of the player that started more times
;; (or the first player if they had the same number of stops)

(define (more-starts string1 string2 aLOS)
  (cond [(empty? aLOS) string1] ;same number of stops ? so just returning first player
           [(cons? aLOS)
                (cond [(= (num-starts string1 aLOS) (num-starts string2 aLOS)) string1] ;same number, return first player name
                      [(> (num-starts string1 aLOS) (num-starts string2 aLOS)) string1] ;player1's stops > player2's stops, return 2nd player name
                      [(< (num-starts string1 aLOS) (num-starts string2 aLOS)) string2] ;player1's stops < player2's stops, return 1st player name
   )]))

(check-expect (more-starts "Bennie Vault" "Meena Ryans" LOS4) "Bennie Vault")
(check-expect (more-starts "Meena Ryans" "Bennie Vault" LOS4) "Meena Ryans")
(check-expect (more-starts "Korren Asper" "Victor Hen" LOS1) "Korren Asper")
(check-expect (more-starts "Korren Asper" "Victor Hen" '()) "Korren Asper")


