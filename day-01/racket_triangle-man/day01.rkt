#lang racket

;; *input1* : [List-of [List-of char?]]
(define *input1* 
  (map string->list
       (with-input-from-file "input.txt"
         port->lines)))

;; ------------------------------------------------------------
;; Part 1

(define (string-calibration-value chars)
  (let ([digits (filter char-numeric? chars)])
    (string->number
     (string (first digits) (last digits)))))

(displayln (apply + (map string-calibration-value *input1*)))


;; ------------------------------------------------------------
;; Part 2

(define *digits*
  (map string->list '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine")))

(define *nums*
  (list->vector (string->list "123456789")))

;; Returns a pair, of the digit represented by the first word, and the rest of the list; or #f

;; Old version -- did not account for overlapping words
;; (define (take-first-word cs)
;;   (let ([d (index-where *digits* (λ (wd) (list-prefix? wd cs)))])
;;     (and d
;;          (cons d (drop cs (length (list-ref *digits* d)))))))

(define (take-first-word cs)
  (let ([d (index-where *digits* (λ (wd) (list-prefix? wd cs)))])
    (and d
         (cons d (cdr cs)))))

(define (digits-or-words->digits cs)
  (define (helper cs acc)
    (cond
      [(null? cs)               acc]
      [(char-numeric? (car cs)) (helper (cdr cs) (cons (car cs) acc))]
      [else (let ([d (take-first-word cs)])
              (if d
                  (helper (cdr d) (cons (vector-ref *nums* (car d)) acc))
                  (helper (cdr cs) acc)))]))
  (helper cs null))


(define (string-calibration-value2 chars)
  (let ([digits (digits-or-words->digits chars)])
    (string->number
     ;; digits-or-words->digits returns the list of digits in reverse order
     (string (last digits) (first digits)))))

(displayln (apply + (map string-calibration-value2 *input1*)))
