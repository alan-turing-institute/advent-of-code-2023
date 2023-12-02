#lang racket


(module+ main

  (define *games*
    (map parse-line
         (with-input-from-file "input.txt" port->lines)))

  ;; Part 1
  (apply + (possible-games 12 13 14 *games*))

  ;; Part 2
  (apply + (map game-power *games*))
  
  )

;; ------------------------------------------------------------


;; A game is a list of plays
;; A play is an association list of colours and counts

;; " 3 blue" -> '("blue" . 3)
(define (parse-cube cube)
  (match-let ([(list n col) (string-split cube " ")])
    (cons col (string->number n))))

;; " 3 blue, 4 red, 1 green" -> '(("blue" . 3) ("red" . 4) ("green" . 1))
(define (parse-play play)
  (let ([cubes (string-split play ",")])
    (map parse-cube cubes)))

(define (parse-line line)
  ;; Split after "Game n: " and then split at semicolons 
  (let ([plays (string-split (cadr (string-split line ": ")) ";")])
    (map parse-play plays)))

(define ((play-possible? red green blue) play)
  (and (<= (dict-ref play "red" 0) red)
       (<= (dict-ref play "green" 0) green)
       (<= (dict-ref play "blue" 0) blue)))

(define ((game-possible? red green blue) plays)
  (andmap (play-possible? red green blue) plays))

;; Find indices of possible games
(define (possible-games red green blue games)
  (map add1 (indexes-where games (game-possible? red green blue))))

;; Find max number of cubes for each colour
;; Just traverse the list 3 times. 
(define (game-power plays)
  (for/product ([colour (in-list '("red" "green" "blue"))])
    (apply max
           (map (λ (play) (dict-ref play colour 0)) plays))))


;; ------------------------------------------------------------
;; Test

(module+ test

  (define *test*
    #<<EOF
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
EOF
    )

  (define *games*
    (map parse-line
         (with-input-from-string *test* port->lines)))

  (apply + (possible-games 12 13 14 *games*))
  
  (apply + (map game-power *games*))
  
  )
