#lang racket

(module+ main

  ;; Example case here, to avoid publishing my input

  ;; Part 1
  (apply *
         (map race-win-ways
              '(  7  15  30)
              '(  9  40 200)))
  
  ;; Part 2
  (race-win-ways 71530 940200)

  )


;; Solve t(T-t) > D
;; by solving t^2 - Tt + D = 0
(define (race-win-timings T D)
  (let ([s (sqrt (- (* T T) (* 4 D)))])
    (values
     ;; The extra 1s ensure the the inequality is strict
     (floor   (+ (* 0.5 (- T s)) 1))
     (ceiling (- (* 0.5 (+ T s)) 1)))))

(define (race-win-ways T D)
  (let-values ([(lower upper) (race-win-timings T D)])
    (+ 1 (- upper lower))))

