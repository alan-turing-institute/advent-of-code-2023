#lang racket

(module+ test

  (define *eg* (string->list "#.?.?"))


)

(define (is-valid? xs groups skipping?)
  (if (null? xs)
      (null? groups)
      (let ([x (car xs)]
            [r (cdr xs)])
        (or (and (char=? x #\.)
                 skipping?
                 (is-valid? r groups #t))
            (and (char=? x #\#)
                 (not skipping?)
                 )
            [()])))



  ;; list? -> forest?
  (define (make-forest xs)
    (if (null? xs)
        '()
        (match (car xs)
          [#\? (list (cons #\# (make-forest (cdr xs)))
                     (cons #\. (make-forest (cdr xs))))]
          [v   (list (cons v (make-forest (cdr xs))))]))))

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


 
