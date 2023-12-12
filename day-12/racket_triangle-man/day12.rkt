#lang racket

(module+ test

  (define *eg* (string->list "#.?.?"))

  (define (make-tree xs)
    (if (null? xs)
        null
        (match (car xs)
          [#\? (list (cons #\# (make-tree (cdr xs)))
                     (cons #\. (make-tree (cdr xs))))]
          [v   (list (cons v (make-tree (cdr xs))))])))

 
  ;; tree : '(rt1 rt2 ...)
  (define (flatten-tree t)
    (append*
     (map flatten-rooted-tree t)))
  
 ;; rooted-tree : '(c . (t1 t2 ...))
  (define (flatten-rooted-tree rt)
    (map (λ (t) (cons (car rt) t))
         (map flatten-tree (cdr rt))))

  
  )

