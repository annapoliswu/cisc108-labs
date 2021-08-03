;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chooseYourOwnAdventureGameVedit) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Names:
;;Sarah Joyce
;;Kelly Mi
;;Zihan (Anna) Wu
;;Alex Donahue

(require 2htdp/image)
(require 2htdp/universe)

;upgrade the game to include states and a small inventory; ex: a number of keys??

;Path 1: go to defeat the space dragon. Path 2: Shoot something random out of your wand.

;DEFINED PAGES
(define page0 "Welcome to Choose Your Own Adventure. You are a wizard who lives on a cliff on the moon. You are lonely on the moon. You are going to find some friends. Press a to go left or c to shoot something random out of your wand.")
(define page1 "You see a Wanted poster about a space dragon. This space dragon is destroying everything! Press b to go on an adventure to defeat the space dragon. Press c to shoot something random out of your wand.")
(define page2 "You set out to find the dragon. You go back to your house to gather supplies. Press d to pick a sword and e to pick a bottle of water.")
(define page3 "You cast a spell and frogs appear. Press b to continue and press h to shoot something else.")
(define page4 "You enter the dragon's lair. Press v to continue")
(define page4a "You enter the dragon's lair. The dragon comments on your large sword. You approach the dragon and it breathes fire at you and you die.")
(define page4b "You enter the dragon's lair. You find the space dragon in his lair. The dragon laughs at your water bottle. The dragon breathes fire and you throw the water bottle at it which saves you. Good job, you won!")
(define page5 "You cast a spell and frogs appear. Press a to continue and press i to shoot something else.")
(define page6 "You cast a spell and frogs appear. Press a to continue and press j to shoot something else.")
(define page7 "You cast a spell and frogs appear. Press a to continue.")



;Maps which keys/choices goes to which page
(define (handle-key previous new)
  (cond
    [(key=? new "s") page0]
    [(key=? new "a") page1]
    [(key=? new "b") page2]
    [(key=? new "c") page3]
    [(key=? new "d") (make-inventory "sword") page4]  ;does not work; can't create multiple actions for cond??
    [(key=? new "e") (make-inventory "water") page4]  ;does not work; can't create multiple actions for cond??
    [(key=? new "v")
     (cond [(string=? (inventory-item1 PLAYERINV)"sword") page4a]  ;does not work; can't refer back to inventory because it does not have a set name when made
           [(string=? (inventory-item1 PLAYERINV)"water") page4b]  ;does not work; can't refer back to inventory because it does not have a set name when made          
     ] 
    [(key=? new "h") page5]
    [(key=? new "i") page6]
    [(key=? new "j") page7]
    ))

;Renders the text/pages as images on screen
(define (render-text str)
  (text/font str 12 "black" "Gill Sans" 'decorative 'normal 'bold #f))

;One item inventory
(define-struct inventory (item1))

(define PLAYERINV (make-inventory "howdoyousetthisstring"))



(define (main init)
  (big-bang init
            (to-draw render-text)
            (on-key handle-key)))

(main page0)


;(cond[ sword(define PLAYERINV1 (make-inventory "sword"))]
;(define PLAYERINV1 (make-inventory "waterbottle"))

;Give character small inventory -> a number of keys? / single number
;(define playerinventory (make-inventory "" "" ""))

;(define (grabitem item)
;  (make-inventory(player item)))

;(define-struct sword(element length))
;(define-struct waterbottle(size))




