#lang racket

(module+ main
  
  (define *plays*
    (for/list ([line (in-list (with-input-from-file "input.txt" port->lines))])
      (read-play line)))

  ;; Part 1
  (for/sum ([ply  (in-list (sort *plays* strength<=?))]
            [rank (in-naturals 1)])
    ;    (printf "~a -- ~a\n" rank (show-play ply))
    (* rank (play-bid ply)))

 ;; Part 2
  (let ([new-plays (map replace-jack-with-joker *plays*)])
    (for/sum ([ply  (in-list (sort new-plays strength<=?))]
              [rank (in-naturals 1)])
      (* rank (play-bid ply))))
  
  )

;; ------------------------------------------------------------

;; A card is an integer between 0 and 13
(define card? (integer-in 0 13))

(define card-ranks (string->list "*23456789TJQKA"))

(define card-joker 0)

(define/contract (char->card c)
  (-> char? card?)
  (index-of card-ranks c char=?))

(define/contract (card->char c)
  (-> card? char?)
  (list-ref card-ranks c))

(define (card<? c₁ c₂)
  (< c₁ c₂))

(define (card=? c₁ c₂)
  (= c₁ c₂))


;; A hand-type is an integer between 0 and 6
(define hand-type? (integer-in 0 6))

(define hand-names
  '("high card"
    "one pair"
    "two pair"
    "three of a kind"
    "full house"
    "four of a kind"
    "five of a kind"))

(define (hand-type-name t)
  (list-ref hand-names t))

(define (hand-type<? t₁ t₂)
  (< t₁ t₂))

(define (hand-type=? t₁ t₂)
  (= t₁ t₂))

;; hand->hand-type : [list-of card?] -> hand-type?
;; Return the hand type of a hand
(define (hand->hand-type cs)
  (let* ([groups   (group-by identity cs card=?)]
         [ngroups  (length groups)]
         [group-ns (sort (map length groups) >)])
    (cond
      [(= ngroups 1)           6] ; Five of a kind
      [(= ngroups 2)
       (cond
         [(= (car group-ns) 4) 5] ; Four of a kind
         [(= (car group-ns) 3) 4] ; Full house
         )]
      [(= ngroups 3)
       (cond
         [(= (car group-ns) 3) 3] ; Three of a kind
         [(= (car group-ns) 2) 2] ; Two pair
         )]
      [(= ngroups 4)           1] ; One pair
      [(= ngroups 5)           0] ; High card
      )))


;; A play is a list of five cards, a hand-type, and a bid-amount
(struct play (cards type bid) #:transparent)

(define (show-play p)
  (format "~a (~a): Bid ~a"
          (list->string (map card->char (play-cards p)))
          (hand-type-name (play-type p))
          (play-bid p)))

(define (read-play str)
  (match-let ([(list hd bd) (string-split str)])
    (let ([cs (map char->card (string->list hd))])
      (play
       cs
       (hand->hand-type cs)
       (string->number bd)))))

(define (card-list<=? cs₁ cs₂)
  (or (null? cs₁)
      (card<? (car cs₁) (car cs₂))
      (and (card=? (car cs₁) (car cs₂))
           (card-list<=? (cdr cs₁) (cdr cs₂)))))

(define (strength<=? p₁ p₂)
  (or (hand-type<? (play-type p₁) (play-type p₂))
      (and (hand-type=? (play-type p₁) (play-type p₂))
           (card-list<=? (play-cards p₁) (play-cards p₂)))))

;; ------------------------------------------------------------

;; For part 2

(define ((swap c₁ c₂) c)
  (if (card=? c c₁) c₂ c))

(define make-jack-joker
  (swap (char->card #\J) card-joker))

(define (joker-best-type cards)
  ;; At this point, there is no J in cards
  ;; Replace * with each card in cards
  (apply max
         (for/list ([new-c (in-list cards)])
           (hand->hand-type (map (swap card-joker new-c) cards)))))

;; For a given play, do two things:
;; 1. Replace the Jack with the Joker
;; 2. Replace the type with the best possible type over all
;;    replacements of the now-Joker with "normal" cards.
(define (replace-jack-with-joker ply)
  (match-let ([(play cards _ bid) ply])
    (let ([hand (map make-jack-joker cards)]) ; list of cards with J replaced by *
      (play hand (joker-best-type hand) bid))))


;; ------------------------------------------------------------

(module+ test

  (define *input* #<<EOF
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
EOF
    )

  (define *plays*
    (for/list ([line (in-list (with-input-from-string *input* port->lines))])
      (read-play line)))

  (for/sum ([ply  (in-list (sort *plays* strength<=?))]
            [rank (in-naturals 1)])
    (* rank (play-bid ply)))

  ;; Part 2
  (let ([new-plays (map replace-jack-with-joker *plays*)])
    (for/sum ([ply  (in-list (sort new-plays strength<=?))]
              [rank (in-naturals 1)])
      (* rank (play-bid ply))))

  )
