#lang racket


(define *input* #<<EOF
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
EOF
  )

(define (parse-map-line str)
  (values
   (substring str 0 3)
   (cons (substring str 7 10)
         (substring str 12 15))))

;; ------------------------------------------------------------

(define in (open-input-file "input.txt"))
;; (define in (open-input-string *input*))

(define *directions* (read-line in))

(read-line in) ; Skip blank line

(define *map*
  (for/hash ([line (in-lines in)])
    (parse-map-line line)))

;; Part 1 -----------------------------------------------------

(define (count-steps-from start is-end?)
  (for/fold ([loc  start]
             [step     0]
             #:result step)
            ([dir (in-cycle *directions*)])
    #:break (is-end? loc)
    (let* ([node (hash-ref *map* loc)]
           [next (cond
                   [(char=? dir #\L) (car node)]
                   [(char=? dir #\R) (cdr node)])])
      (values next (+ step 1)))))

(count-steps-from "AAA" (curry string=? "ZZZ"))

;; Part 2 -----------------------------------------------------

(define (is-start? loc)
  (string-suffix? loc "A"))

(define (is-end? loc)
  (string-suffix? loc "Z"))

(apply lcm
 (map (λ (loc) (count-steps-from loc is-end?)) (filter is-start? (hash-keys *map*))))
