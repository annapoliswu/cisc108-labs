;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname chooseYourOwnAdventureGameV4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Names:
;;Sarah Joyce
;;Kelly Mi
;;Zihan (Anna) Wu
;;Alex Donahue

(require 2htdp/image)
(require 2htdp/universe)

;upgrade the game to include states and a small inventory

;When you try to shoot something out of your wand, one random object comes out.
;Path 1: go to defeat the space dragon. Path 2: Shoot something random out of your wand.
;page0: a -> page1 b -> page3
;page1: a ->page2 b -> page3
;page2: a -> page4 b -> page5
;page3: a -> page2 b-> page8
;page4: a -> page6 b-> page6
;page5: a-> page7 b->page6
;page6: End
;page7: End
;page8: a-> page1 b-> page9
;page9: a-> page1 b-> page10
;page10: a->page1
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




(define (handle-key previous new)
  (cond
    [(key=? new "s") page0]
    [(key=? new "a") page1]
    [(key=? new "b") page2]
    [(key=? new "c") page3]
    [(key=? new "d") (cond [(string=? (inventory-item1 PLAYERINV1) "sword") page4a])]
    [(key=? new "e") (cond [(string=? (inventory-item1 PLAYERINV1) "water") page4b])]
    [(key=? new "v")
     (cond [(string=? (inventory-item1 PLAYERINV1)"sword") page4a]
           [(string=? (inventory-item2 PLAYERINV1)"water") page4b]
           )
     ] 
    [(key=? new "h") page5]
    [(key=? new "i") page6]
    [(key=? new "j") page7]
    ))

(define (render-text str)
  (text/font str 12 "black" "Gill Sans" 'decorative 'normal 'bold #f))

(define (main init)
  (big-bang init
            (to-draw render-text)
            (on-key handle-key)))

(define-struct inventory (item1 item2))
(define PLAYERINV1 (make-inventory "sword" "water"))

;Give character small inventory -> a number of keys? / single number
;(define playerinventory (make-inventory "" "" ""))

;(define (grabitem item)
;  (make-inventory(player item)))

(define-struct sword(element length))
(define-struct waterbottle(size))



(main page0)

