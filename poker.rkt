;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname poker) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ******************************************
;; Yang Xiang
;; Recreational Personnal Project - Poker Odds Calculator
;; 09 December 2016
;; ******************************************
;;

;; ==================================================
;; Structure definitions:
;; ==================================================

(define-struct card (value suit))
;; value is either an Int or a Sym ('A 'K 'Q 'J)
;; suit is either 'spades, 'hearts, 'clubs, or 'diamonds

;; ==================================================
;; Constant definitions:
;; ==================================================

(define priority (list 'SF '4 'FH 'F 'S '3 '2P 'P 'HC))
;; Where: 'SF is a straight flush
;;        '4 is a 4-of-a-kind
;;        'FH is a full-house
;;        'F is a flush
;;        'S is a straight
;;        '3 is a 3-of-a-kind
;;        '2P is two-pair
;;        'p is a pair
;;        'HC is a high card

(define deck (list (make-card 2 'spades)
                   (make-card 2 'hearts)
                   (make-card 2 'clubs)
                   (make-card 2 'diamonds)
                   (make-card 3 'spades)
                   (make-card 3 'hearts)
                   (make-card 3 'clubs)
                   (make-card 3 'diamonds)
                   (make-card 4 'spades)
                   (make-card 4 'hearts)
                   (make-card 4 'clubs)
                   (make-card 4 'diamonds)
                   (make-card 5 'spades)
                   (make-card 5 'hearts)
                   (make-card 5 'clubs)
                   (make-card 5 'diamonds)
                   (make-card 6 'spades)
                   (make-card 6 'hearts)
                   (make-card 6 'clubs)
                   (make-card 6 'diamonds)
                   (make-card 7 'spades)
                   (make-card 7 'hearts)
                   (make-card 7 'clubs)
                   (make-card 7 'diamonds)
                   (make-card 8 'spades)
                   (make-card 8 'hearts)
                   (make-card 8 'clubs)
                   (make-card 8 'diamonds)
                   (make-card 9 'spades)
                   (make-card 9 'hearts)
                   (make-card 9 'clubs)
                   (make-card 9 'diamonds)
                   (make-card 10 'spades)
                   (make-card 10 'hearts)
                   (make-card 10 'clubs)
                   (make-card 10 'diamonds)
                   (make-card 'J 'spades)
                   (make-card 'J 'hearts)
                   (make-card 'J 'clubs)
                   (make-card 'J 'diamonds)
                   (make-card 'Q 'spades)
                   (make-card 'Q 'hearts)
                   (make-card 'Q 'clubs)
                   (make-card 'Q 'diamonds)
                   (make-card 'K 'spades)
                   (make-card 'K 'hearts)
                   (make-card 'K 'clubs)
                   (make-card 'K 'diamonds)
                   (make-card 'A 'spades)
                   (make-card 'A 'hearts)
                   (make-card 'A 'clubs)
                   (make-card 'A 'diamonds)))

(define deck-number 52)

;; ==================================================
;; Global helper function definitions:
;; ==================================================

;; (straight-flush? list) consumes a list of cards and determines
;;      if in the list there is a straight flush available
;; straight-flush?: (listof Card) -> Bool
;; Requires: In this case, the list always contains 7 cards

(define (straight-flush? list)
  (and (straight? list) (flush? list)))


;; (four? list) consumes a list of cards and determines
;;      if in the list there is a four of a kind available
;; four?: (listof Card) -> Bool
;; Requires: In this case, the list always contains 7 cards

(define (four? list)
  (local
    [(define (four-value? list value accumulator)
       (cond
         [(and (empty? list) (= accumulator 4)) true]
         [(empty? list) false]
         [(equal? value (card-value (first list)))
          (four-value? (rest list) value (add1 accumulator))]
         [else (four-value? (rest list) value accumulator)]))]
    
    (cond
      [(empty? list) false]
      [(four-value? list (card-value (first list)) 0) true]
      [else (four? (rest list))])))

;; (full-house? list) consumes a list of cards and determines
;;      if in the list there is a full house available
;; full-house?: (listof Card) -> Bool
;; Requires: In this case, the list always contains 7 cards

(define (full-house? list)
  (and (three? list) (pair? list)))

;; (flush? list) consumes a list of cards and determines
;;      if in the list there is a flush available
;; flush?: (listof Card) -> Bool
;; Requires: In this case, the list always contains 7 cards

(define (flush? list)
  (local
    [(define (flush-suit? list suit accumulator)
       (cond
         [(and (empty? list) (= accumulator 5)) true]
         [(empty? list) false]
         [(symbol=? suit (card-suit (first list)))
          (flush-suit? (rest list) suit (add1 accumulator))]
         [else (flush-suit? (rest list) suit accumulator)]))]
    
    (or (flush-suit? list 'spades 0)
        (flush-suit? list 'hearts 0)
        (flush-suit? list 'clubs 0)
        (flush-suit? list 'diamonds 0))))

;; (straight? list) consumes a list of cards and determines
;;      if in the list there is a straight available
;; straight?: (listof Card) -> Bool
;; Requires: In this case, the list always contains 7 cards

(define (straight? list)
  (local
    [(define (straight-acc? value list acc)
       (cond
         [(= acc 5) true]
         [(= value 14) false]
         [(card-member? (cond
                          [(= value 1) 'A]
                          [(= value 11) 'J]
                          [(= value 12) 'Q]
                          [(= value 13) 'K]
                          [(= value 14) 'A]
                          [else value]) list) (straight-acc? (add1 value) list (add1 acc))]
         [else false]))                      

     (define (card-member? value list)
       (cond
         [(empty? list) false]
         [(equal? value (card-value (first list))) true]
         [else (card-member? value (rest list))]))]

    (cond
      [(empty? list) false]
      [(symbol? (card-value (first list)))
       (cond
         [(and (symbol=? 'A (card-value (first list))) (straight-acc? 1 list 0)) true]
         [else (straight? (rest list))])]
      [(straight-acc? (card-value (first list)) list 0) true]
      [else (straight? (rest list))])))

;; (three? list) consumes a list of cards and determines
;;      if in the list there is a three of a kind available
;; three?: (listof Card) -> Bool
;; Requires: In this case, the list always contains 7 cards

(define (three? list)
  (local
    [(define (three-value? list value accumulator)
       (cond
         [(and (empty? list) (= accumulator 3)) true]
         [(empty? list) false]
         [(equal? value (card-value (first list)))
          (three-value? (rest list) value (add1 accumulator))]
         [else (three-value? (rest list) value accumulator)]))]
    
    (cond
      [(empty? list) false]
      [(three-value? list (card-value (first list)) 0) true]
      [else (three? (rest list))])))

;; (pair-value? list) consumes a list of cards and determines
;;      if in the list there is a pair of the consumed value 
;; four?: (listof Card) (anyof Sym Int) Nat -> Bool
;; Requires: In this case, the list always contains 7 cards

(define (pair-value? list value accumulator)
       (cond
         [(and (empty? list) (= accumulator 2)) true]
         [(empty? list) false]
         [(equal? value (card-value (first list)))
          (pair-value? (rest list) value (add1 accumulator))]
         [else (pair-value? (rest list) value accumulator)]))

;; (two-pair? list) consumes a list of cards and determines
;;      if in the list there is a two-pair available
;; two-pair?: (listof Card) -> Bool
;; Requires: In this case, the list always contains 7 cards

(define (two-pair? list)
  (local
    [(define (two-pair-acc? list acc)
       (cond
         [(and (empty? list) (< 1 acc)) true]
         [(empty? list) false]
         [(pair-value? list (card-value (first list)) 0) (add1 acc)]
         [else (two-pair-acc? (rest list) acc)]))]

    (two-pair-acc? list 0)))

;; (pair? list) consumes a list of cards and determines
;;      if in the list there is a pair available
;; pair?: (listof Card) -> Bool
;; Requires: In this case, the list always contains 7 cards

(define (pair? list)
    (cond
      [(empty? list) false]
      [(pair-value? list (card-value (first list)) 0) true]
      [else (pair? (rest list))]))



;; ==================================================
;; Main function definition
;; ==================================================

;; (odds? hand field shown-cards) produces the probability of acquiring every type of hand according to
;;    the player current hand, the cards shown on the table (field) and the cards that are already
;;    shown and that cannot be drawn from the dealer.
;; odds?: (list Card Card) (listof Card) (listof Card) -> (listof Chance Chance Chance Chance Chance Chance Chance Chance Chance)

(list 'SF '4 'FH 'F 'S '3 '2P 'P 'HC)


(define (odds? hand field shown-cards)
  (local
    [(define (used-deck shown-cards)
       (filter (lambda (card) (not (member? card shown-cards))) deck))
     

     
             
         
     ]

    ...))




;; ==================================================
;; Test
;; ==================================================

(check-expect (straight? (list (make-card 'A 'random)
                               (make-card '3 'random)
                               (make-card '5 'random)
                               (make-card '3 'random)
                               (make-card '2 'random)
                               (make-card 'K 'random)
                               (make-card '4 'random))) true)





















  
