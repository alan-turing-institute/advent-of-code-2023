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

  (define *galaxies* (lists-indexes-of *image* #\#))
  
  ;; Part 1

  (define *row-offsets* (distances *image* 2))
  (define *col-offsets* (distances (transpose *image*) 2))

  (for/sum ([gg (in-combinations *galaxies* 2)])
    (match-let ([(list (cons r1 c1) (cons r2 c2)) gg])
      (let ([x1 (list-ref *row-offsets* r1)]
            [y1 (list-ref *col-offsets* c1)]
            [x2 (list-ref *row-offsets* r2)]
            [y2 (list-ref *col-offsets* c2)])
        (+ (abs (- x1 x2)) (abs (- y1 y2))))))

  ;; Part 2

  (define *row-offsets2* (distances *image* 1000000))
  (define *col-offsets2* (distances (transpose *image*) 1000000))

  (for/sum ([gg (in-combinations *galaxies* 2)])
    (match-let ([(list (cons r1 c1) (cons r2 c2)) gg])
      (let ([x1 (list-ref *row-offsets2* r1)]
            [y1 (list-ref *col-offsets2* c1)]
            [x2 (list-ref *row-offsets2* r2)]
            [y2 (list-ref *col-offsets2* c2)])
        (+ (abs (- x1 x2)) (abs (- y1 y2))))))

    )


;; ------------------------------------------------------------

(define (blank? x)
  (eq? x #\.))

(define (blank-row? xs)
  (andmap blank? xs))

(define (distances rows expansion-factor)
  (for/fold ([offset 0]
             [dists  '()]
             #:result (reverse dists))
            ([row (in-list rows)])
    (values (+ offset (if (blank-row? row) expansion-factor 1))
            (cons offset dists))))

(define (transpose xss)
  (apply map list xss))

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

