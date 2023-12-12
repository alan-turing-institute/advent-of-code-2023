#lang racket

(module+ test

  (define *input* #<<EOF
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
EOF
)

  ;; Part 1 -- brute force (30s)
  (for/sum ([ln (in-lines (open-input-file "input.txt"))])
    (match-let ([(list rs gs) (parse-line ln)])
      (count-valid-records gs (string->list rs))))
  



)


;; ------------------------------------------------------------

(define (parse-line str)
  (match-let ([(list record groups) (string-split str)])
    (list
     record
     (map string->number (string-split groups ",")))))


;; ------------------------------------------------------------

;; Does this list xs satisfy the grouping?

;; Let's write a little state-machine
;; States:
;; - "Skipping": reading "." until the next "#" or EOS (also the start state)
;; - "In-group": in a group, reading "#" until the group count reaches 0
;; - "at-end" : at the end of the string, should be no more groups left.

;; End. Valid if there are no remaining groups
(define (is-valid/at-end? groups)
  (or (null? groups)            
      (and (null? (cdr groups)) 
           (= (car groups) 0)))) 

;; Start
(define (is-valid/skipping? groups xs)
  (if (null? xs)
      (is-valid/at-end? groups)
      (let ([x (car xs)] 
            [r (cdr xs)])
        (cond
          [(char=? x #\.) (is-valid/skipping? groups r)]
          [else                ; next char is "#"
           (and (pair? groups) ; some groups left!
                (is-valid/in-group? (group-deduct-one groups) r))])))) 

(define (group-deduct-one groups)
  (cons (- (car groups) 1) (cdr groups)))

;; At this point, groups is '(g . gs), where g = 0 if there should be no more #s
(define (is-valid/in-group? groups xs)
  (match-let ([(cons g gs) groups])
    (if (null? xs)
        (is-valid/at-end? groups)
        (let ([x (car xs)]
              [r (cdr xs)]
              [g (car groups)]
              [gs (cdr groups)])
          (cond
            [(char=? x #\.) (and (= g 0)
                                 (is-valid/skipping? gs r))]
            [else           (and (> g 0)
                                 (is-valid/in-group? (group-deduct-one groups) r))])))))
        

(define (count-valid-records gs rs)
  (length
   (filter-map (curry is-valid/skipping? gs) (flatten-forest (make-forest rs)))))


;; list? -> forest?
(define (make-forest xs)
  (if (null? xs)
      '()
      (match (car xs)
        [#\? (list (cons #\# (make-forest (cdr xs)))
                   (cons #\. (make-forest (cdr xs))))]
        [v   (list (cons v (make-forest (cdr xs))))])))

 ;; tree : '(c . f) 
(define (tree? t)
  (and (pair? t)
       (forest? (cdr t))))

;; forest : '(t1 t2 ...)
(define (forest? f)
  (andmap tree? f))

  
(define/contract (flatten-forest f)
  (-> forest? any/c)
  (append*
   (map flatten-tree f)))

(define/contract (flatten-tree t)
  (-> tree? any/c)
  (match t
    [(cons c '()) (list (list c))]
    [(cons c f)
     (map (λ (xs) (cons c xs))
          (flatten-forest f))]))


 
