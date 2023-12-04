#lang racket


;; A card is a list of winning numbers and a list of scratched off numbers
(struct card (winning scratched) #:transparent)

(module+ main

  (define *cards*
    (map read-card
         (with-input-from-file "input.txt" port->lines)))

  ;; Part 1
  (apply + (map card-score *cards*))

  ;; Part 2
  ;; Starting with a list of all 1s, take the first element
  ;; (that's the number of the first card) and repeatedly increment
  ;; the next n elements, where n is the number of wins of the first
  ;; card.
  (define-values (*n-cards* _)
    (for/fold ([result '()]
               [remaining (make-list (length *cards*) 1)])
              ([wins (in-list (map card-winners-n *cards*))])
      (let ([next (car remaining)]
            [rest (cdr remaining)])
        (values
         (cons next result)
         (increment-first-n next wins rest)))))

  (apply + *n-cards*)

  )


;; ------------------------------------------------------------

;; Parse a line of the input
;; read-card : string? -> card?
(define (read-card str)
  (let* ([nums (cadr (string-split str ":"))]   ; Split off the initial "Card n:"
         [split-nums (string-split nums "|")]   ; Split the numbers into two parts
         [winning (map string->number (string-split (car split-nums)))]
         [scratched (map string->number (string-split (cadr split-nums)))])
    (card winning scratched)))

;; Return a list, possibly empty, of winning numbers
(define (card-winners crd)
  (filter (λ (n)
            (member n (card-winning crd)))
   (card-scratched crd)))

;; How many winning numbers are there?
(define (card-winners-n crd)
  (length (card-winners crd)))

;; Score this card (part 1)
(define (card-score crd)
  (let ([n-winners (card-winners-n crd)])
    (if (> n-winners 0)
        (expt 2 (- n-winners 1))
        0)))

;; Add m to the first n entries of a list
(define (increment-first-n m n xs)
  (if (zero? n)
      xs
      (cons (+ m (car xs))
            (increment-first-n m (- n 1) (cdr xs)))))



;; ------------------------------------------------------------

(module+ test

  (define *input* #<<EOF
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
EOF
    )

  (define *cards*
    (map read-card
         (with-input-from-string *input* port->lines)))
  
  (apply + (map card-score *cards*))

  (define-values (*n-cards* _)
    (for/fold ([result '()]
               [remaining (make-list (length *cards*) 1)])
              ([wins (in-list (map card-winners-n *cards*))])
      (let ([next (car remaining)]
            [rest (cdr remaining)])
        (values
         (cons next result)
         (increment-first-n next wins rest)))))

  (apply + *n-cards*)
  
  )
