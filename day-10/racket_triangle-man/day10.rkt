#lang racket

(require "grid.rkt")

(module+ main

;;     (define *input*
;;     (with-input-from-string
;;       #<<EOF
;; ...........
;; .S-------7.
;; .|F-----7|.
;; .||.....||.
;; .||.....||.
;; .|L-7.F-J|.
;; .|..|.|..|.
;; .L--J.L--J.
;; ...........
;; EOF
;;       (thunk
;;        (lists->grid
;;         (map (λ (line) (map char->tile (string->list line)))
;;              (port->lines))))))
  
  (define *input*
    (with-input-from-file "input.txt"
      (thunk
       (lists->grid
        (map (λ (line) (map char->tile (string->list line)))
             (port->lines))))))

  (define *start* (grid-member *input* 'start))
  
  ;; Pick the first tile adjacent to start that isn't ground
  ;; and which has an exit to start
  (define *first-tile*
    (adjacent-non-ground *input* *start*))

  ;; Part 1 -----------------------------------------------------

  (define *loop*
    (let loop ([this  *start*]
               [next  *first-tile*]
               [tiles '()])
      (if (eq? (grid-ref *input* next) 'start)
          (cons this tiles)
          (let ([from-vec (pos- next this)]
                [to-vecs  (map dir->vec (grid-ref *input* next))])
            (let ([to (pos+ from-vec (pos+ (car to-vecs) (cadr to-vecs)))])
              (loop next (pos+ next to) (cons this tiles)))))))

  (/ (length *loop*) 2)

  ;; Part 2 -----------------------------------------------------

  ;; Figure out what the start symbol should be replaced by
  (define *last-tile* (car *loop*))

  (define *start-tile*
    (let ([v1 (vec->dir (pos- *first-tile* *start*))]
          [v2 (vec->dir (pos- *last-tile* *start*))])
      (sort (list v1 v2) dir<)))

  ;; Make a new grid with just the loop
  (define *new-grid*
    (let ([temp (make-grid (grid-nrows *input*) (grid-ncols *input*) #f)])
      (for ([tile (in-list *loop*)])
        (grid-set! temp tile (grid-ref *input* tile)))
      (grid-set! temp *start* *start-tile*)
      temp))

  ;; Flood fill
  
  (for/sum ([row (in-list (grid->lists *new-grid*))])
    (for/fold ([inside? #f]
               [count   0]
               #:result count)
              ([tile (in-list row)])
      (cond 
        [(not tile)
         (if inside?
             (values inside? (+ count 1))
             (values inside? count))]
        [(eq? (car tile) 'north)
         (values (not inside?) count)]
        [else
         (values inside? count)])))
  
  )

(define (char->tile tile)
  (match tile
    [#\. #f]
    [#\| '(north south)]
    [#\- '(east west)]
    [#\L '(north east)]
    [#\J '(north west)]
    [#\7 '(south west)]
    [#\F '(south east)]
    [#\S 'start]))

(define (dir->vec dir)
  (match dir
    ['north '(-1 .  0)]
    ['east  '( 0 .  1)]
    ['south '( 1 .  0)]
    ['west  '( 0 . -1)]))

(define (vec->dir vec)
  (match vec
    ['(-1 .  0) 'north]
    ['( 0 .  1) 'east]
    ['( 1 .  0) 'south]
    ['( 0 . -1) 'west]))

(define (dirn dir)
  (match dir
    ['north 0]
    ['south 1]
    ['east  2]
    ['west  3]))

(define (dir< d1 d2)
  (< (dirn d1) (dirn d2)))



;; Find the first adjacent square with an exit back to this one
(define (adjacent-non-ground g this-pos)
  (findf
   (λ (next-pos)
     (let ([exits (grid-ref g next-pos)])
       (and exits
            (member (pos- this-pos next-pos) (map dir->vec exits)))))
   (grid-adjacents g this-pos)))



