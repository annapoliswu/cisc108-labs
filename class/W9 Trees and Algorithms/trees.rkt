;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A Binary Tree [BT] is either:
;; -- empty
;; -- (make-node Number String BT BT)

(define-struct node [key value left right])
;; a node is a struct (make-node Number String BT BT)
;; FIELDS
;;  Number   key: identifier, usually unique, for a value
;;  X value: the value we are storing. NOTE that this could
;;                  really be anything (e.g. a big structure)
;;  BT      left: the left child of this node
;;  BT     right: the right child of this node
;;TEMPLATE
#;
(define (bt-fun bt)
  (cond [(empty? bt) ...]
        [(node? bt)
         ... (node-key bt) ;Number
         ... (node-value bt) ;X
         ... (bt-fun (node-left bt)) ;BT
         ... (bt-fun (node-right bt))   ])) ;BT


;; EXAMPLE
(define BT1
  (make-node 45 "Bill"
             (make-node 16 "Xinxin"
                        (make-node 4 "Salim" empty empty)
                        (make-node 20 "Alicia" empty empty))
             (make-node 75 "Kim"
                        empty
                        (make-node 99 "Jill"
                                   (make-node 80 "Pedro" empty empty)
                                   empty))))
;; count-nodes : BT --> Number
;; Consumes
;;  BT bt
;; Produces the number of nodes in bt
(check-expect (count-nodes empty) 0)
(check-expect (count-nodes BT1) 7)
(define (count-nodes bt)
  (cond [(empty? bt) 0]
        [(node? bt)
         (+ 1
            (count-nodes (node-left bt)) ;BT
            (count-nodes (node-right bt)))])) ;BT

;; contains-bt? : Number BT --> Boolean
;; Consumes
;;  Number key, is the key we are looking for
;;  BT bt
;; Produce #true if key matches the key of some node in bt.
(check-expect (contains-bt? 99 empty) #false)
(check-expect (contains-bt? 99 BT1) #true)
(check-expect (contains-bt? 468 BT1) #false)
(define (contains-bt? key bt)
  (cond [(empty? bt) #false]
        [(node? bt)
         (or
          (= key (node-key bt)) ;Number
          (contains-bt? key (node-left bt)) ;BT
          (contains-bt? key (node-right bt)))])) ;BT




;;===================================================
;; Mutual-Referential

; def: tree refers to LOT refers to tree...

;; a Tree is either
;; -- empty
;; -- (make-tree Number X [ListOf Tree])
(define-struct tree [key value kids])
;; Interpretation
;;  Number key: the node key
;;  X value: the node's value (could be some big structure in a database)
;;  [ListOf Tree] kids: the node's children, could be empty (0) or any number of kids..

;; a [ListOf Tree] is either (as usual)
;; -- empty
;; -- (cons Tree [ListOf tree])

;Functions below are NOT recursive, more like mutually recursive??
;They refer to each other.. 

#|
(define (tree-fun tree)
  (cond [(empty? tree) ...]
        [(tree? tree)
         ... (tree-key tree) ;Number
         ... (tree-value tree) ;String
         ... (lot-fun (tree-kids tree)) ;List of Tree fn
         ]))
(define (lot-fun lot)
  (cond [(empty? lot) ...]
        [(cons? lot)
         ... (tree-fun (first lot)) ;a Tree
         ... (lot-fun (rest lot))
         ]))
|#
(define T1 (make-tree 1 "a" empty))
(define T2 (make-tree 1 "a" (list (make-tree 2 "b" empty))))
(define T3 (make-tree 1 "a" (list (make-tree 2 "b" empty)
                                  (make-tree 3 "c" (list (make-tree 6 "f" empty)
                                                         (make-tree 7 "g" empty)))
                                   (make-tree 4 "d" empty)
                                   (make-tree 5 "e" empty))))

;; count-tnodes : Tree --> Number
;; Consumes
;;  Tree tree: the tree to count nodes in
;; Produces the number of nodes in the tree
(check-expect (count-tnodes '()) 0)
(check-expect (count-tnodes T1) 1)
(check-expect (count-tnodes T2) 2)
(check-expect (count-tnodes T3) 7)

(define (count-tnodes tree)
  (cond [(empty? tree) 0]
        [(tree? tree) (+ 1 (count-kids (tree-kids tree)))]))

(define (count-kids lot)
  (cond [(empty? lot) 0]
        [(cons? lot)
         (+ (count-tnodes (first lot)) ;a Tree
            (count-kids  (rest lot)))]))

;; Note: These functions always work together, so we can write them that way:
(check-expect (count-tnodes.v2 '()) 0)
(check-expect (count-tnodes.v2 T1) 1)
(check-expect (count-tnodes.v2 T2) 2)
(check-expect (count-tnodes.v2 T3) 7)

(define (count-tnodes.v2 tree)
  (local [(define (count-kids lot)
            (cond [(empty? lot) 0]
                  [(cons? lot)
                   (+ (count-tnodes.v2 (first lot)) ;a Tree
                      (count-kids  (rest lot)))]))]
    (cond [(empty? tree) 0]
          [(tree? tree) (+ 1 (count-kids (tree-kids tree)))])))