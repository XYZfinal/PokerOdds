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

(define-struct card (value suit))
;; value is either an Int or a Sym ('A 'K 'Q 'J)
;; suit is either 'spades, 'hearts, 'clubs, or 'diamonds

(define-struct chance (type probability))
;; type is one of 'SF, '4, 'FH, 'F, 'S, '3, '2P, 'P, 'HC
;; Probability is a Num between 0 to 1

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



(define deck-number 54)

;; (odds? hand field shown-cards) produces the probability of acquiring every type of hand according to
;;    the player current hand, the cards shown on the table (field) and the cards that are already
;;    shown and that cannot be drawn from the dealer.
;; odds?: (list Card Card) (listof Card) (listof Card) -> (listof Chance Chance Chance Chance Chance Chance Chance Chance Chance)

(list 'SF '4 'FH 'F 'S '3 '2P 'P 'HC)
'spades, 'hearts, 'clubs, or 'diamonds

(define (odds? hand field shown-cards)
  (local
    [(define (used-deck shown-cards)
       (filter (lambda (card) (not (member? card shown-card))) deck))

     (define (straight-flush? list)
       (and (straight? list) (flush? list)))
     
     (define (four? list)
       (local
         [(define (four-value? list value accumulator)
            (cond
              [(and (empty? list) (= accumulator 4)) true]
              [(empty? list) false]
              [(= value (card-value (first list)))
               (four-value? (rest list) value (add1 accumulator))]
              [else (four-value? (rest list) value accumulator)]))]

         (cond
           [(four-value? list (card-value (first list)) 0) true]
           [else (four? (rest list))])))

     (define (full-house? list)
       (and (three? list) (pair? list)))

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

     (define (straight? list)
       ...)

     (define (three? list)
       (local
         [(define (three-value? list value accumulator)
            (cond
              [(and (empty? list) (= accumulator 3)) true]
              [(empty? list) false]
              [(= value (card-value (first list)))
               (three-value? (rest list) value (add1 accumulator))]
              [else (three-value? (rest list) value accumulator)]))]

         (cond
           [(three-value? list (card-value (first list)) 0) true]
           [else (three? (rest list))])))

     (define (two-pair? list)
       ...)

     (define (pair? list)
       (local
         [(define (pair-value? list value accumulator)
            (cond
              [(and (empty? list) (= accumulator 2)) true]
              [(empty? list) false]
              [(= value (card-value (first list)))
               (pair-value? (rest list) value (add1 accumulator))]
              [else (pair-value? (rest list) value accumulator)]))]

         (cond
           [(pair-value? list (card-value (first list)) 0) true]
           [else (pair? (rest list))])))
             
         
     ]

    (list (make-chance 'SF (SF-odds? ...))
          (make-chance '4 (4-odds? ...))
          (make-chance 'FH (FH-odds? ...))
          (make-chance 'F (F-odds? ...))
          (make-chance 'S (S-odds? ...))
          (make-chance '3 (3-odds? ...))
          (make-chance '2P (2P-odds? ...))
          (make-chance 'P (P-odds? ...))
          (make-chance 'HC (HC-odds? ...)))))



























  
