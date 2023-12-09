#lang racket

(module+ main

  (define *report*
    (with-input-from-file "input.txt"
      (thunk
       (for/list ([line (in-lines)])
         (map string->number (string-split line))))))

  ;; Part 1
  (apply +
         (map predict-next-value *report*))

  ;; Part 2
  (apply +
         (map predict-next-value-r *report*))


  )




;; Take the first differences of vs
(define (Δ vs)
  (for/list ([a (in-list vs)]
             [b (in-list (cdr vs))])
    (- a b)))

;; vs are the list, reversed
;; To predict the next value, add the first element
;; to the next value of the derivative.
;; Not tail-recursive
(define (predict-next-value-r vs)
  (cond
    [(andmap zero? vs) 0]         ; Stop early if we can
    [(null? (cdr vs))  (car vs)]
    [else (+ (car vs)
             (predict-next-value-r (Δ vs)))]))

(define (predict-next-value vs)
  (predict-next-value-r (reverse vs)))



(module+ test

  (define *report*
    '((0 3 6 9 12 15)
      (1 3 6 10 15 21)
      (10 13 16 21 30 45)))

  (apply +
         (map predict-next-value *report*))

  (apply +
         (map predict-next-value-r *report*))
  
  )


