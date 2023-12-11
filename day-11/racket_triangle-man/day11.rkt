#lang racket


(module+ main

  (define *input* #<<EOF
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
EOF
    )
  
  ;; (define *image*
  ;;   (map string->list
  ;;        (with-input-from-string *input* port->lines)))

  (define *image*
    (map string->list
         (with-input-from-file "input.txt" port->lines)))

  (define *galaxies* (lists-indexes-of (expand-galaxy *image*) #\#))

  ;; Part 1
  (for/sum ([gg (in-combinations *galaxies* 2)])
    (match-let ([(list (cons x1 y1) (cons x2 y2)) gg])
      (+ (abs (- x1 x2)) (abs (- y1 y2)))))

  ;; Part 2
  
    )



;; ------------------------------------------------------------

(define (blank? x)
  (eq? x #\.))

(define (blank-row? xs)
  (andmap blank? xs))

(define (transpose xss)
  (apply map list xss))

(define (double-blank-rows xss)
  (reverse
   (let loop ([rest xss]
              [acc  '()])
     (if (null? rest)
         acc
         (let ([row (car rest)])
           (if (blank-row? row)
               (loop (cdr rest) (cons row (cons row acc)))
               (loop (cdr rest) (cons row acc))))))))

(define (expand-galaxy xss)
  (double-blank-rows
   (transpose (double-blank-rows (transpose xss)))))

(define (lists-indexes-of xss x)
  (append*
   (for/list ([xs (in-list xss)]
              [row (in-naturals)])
     (map (λ (col) (cons row col)) (indexes-of xs x)))))

;; ------------------------------------------------------------

(define (display-lists css)
  (display 
   (string-join
    (for/list ([cs (in-list css)])
      (list->string cs))
    "\n")))

