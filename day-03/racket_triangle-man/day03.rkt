#lang racket

(module+ main
  
  ;; *schematic* : [list-of string?]
  (define *schematic*
    (with-input-from-file "input.txt" port->lines))  

  (define *numbers* (locate-numbers *schematic*))
  (define *symbols* (locate-symbols *schematic*))

  ;; Part 1
  (apply +
   (map cadr
        (filter (λ (num)
                  (ormap (λ (sym) (adjacent? sym num)) *symbols*))
                *numbers*)))
  
  ;; Part 2
  (define *stars*
    (filter (λ (sym) (char=? (cdr sym) #\*)) *symbols*))

  (define *gears*
    (filter (λ (nums) (= (length nums) 2))
            (map (λ (sym)
                   (filter (curry adjacent? sym) *numbers*))
                 *stars*)))
  
  (apply +
         (map (λ (gear)
                (* (cadr (car gear)) (cadr (cadr gear))))
              *gears*))
  )

;; ------------------------------------------------------------

(define (sym? c)
  (and (not (char=? c #\.))
       (not (char-numeric? c))))

;; locate-symbols : [list-of string?] -> [list-of (pos? . char?)]
;; where pos? is a pair of (row . col)
(define (locate-symbols schm)
  (append*
   (for/list ([(row nrow) (in-indexed schm)])
     (for/list ([(c ncol) (in-indexed row)]
                #:when (sym? c))
       (cons (cons nrow ncol) c)))))

;; locate-numbers : [list-of string?] -> [list-of (pos? . (n . len))]
(define (locate-numbers schm)
  (append*
   (for/list ([(row nrow) (in-indexed schm)])
     (let ([locations (regexp-match-positions* #rx"[0-9]+" row)])
       (map (match-lambda
              [(cons st nd)
               `((,nrow . ,st) . (,(string->number (substring row st nd)) . ,(- nd st)))]
              )
            locations)))))

;; Fine. O(N^2) is fine.
;; Is number adjacent to symbol?
(define (adjacent? sym num)
  (let ([sym-pos (car sym)]
        [num-pos (car num)]
        [num-len (cddr num)])
    (and
     ;; rows within 1
     (<= (abs (- (car sym-pos) (car num-pos))) 1)
     ;; cols
     (>= (cdr sym-pos) (- (cdr num-pos) 1))
     (<= (cdr sym-pos) (+ (cdr num-pos) num-len)))))



;; ------------------------------------------------------------

(module+ test
  
  (define *input* #<<EOF
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
EOF
    )

  ;; *schematic* : [list-of string?]
  (define *schematic*
    (with-input-from-string *input* port->lines))  
  
  (define *numbers* (locate-numbers *schematic*))
  (define *symbols* (locate-symbols *schematic*))

  ;; Part numbers
  (apply +
   (map cadr
        (filter (λ (num)
                  (ormap (λ (sym)
                           (adjacent? sym num))
                         *symbols*))
                      *numbers*)))

  ;; Gear ratios
  (define *stars*
    (filter (λ (sym) (char=? (cdr sym) #\*)) *symbols*))

  (define *gears*
    (filter (λ (nums) (= (length nums) 2))
            (map (λ (sym)
                   (filter (curry adjacent? sym) *numbers*))
                 *stars*)))
  
  (apply +
         (map (λ (gear)
                (* (cadr (car gear)) (cadr (cadr gear))))
              *gears*))
  )
