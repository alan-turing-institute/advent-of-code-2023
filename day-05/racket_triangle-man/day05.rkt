#lang racket


(module+ main

  (define-values (*seeds* *almanac*)
    (parse-input
     (with-input-from-file "input.txt" port->lines)))

  ;; Part 1
  (apply min
         (map (λ (sd) (almanac-propagate *almanac* sd)) *seeds*))

 ;; Part 2
  (time 
   (apply min
          (map rng-min
               (almanac-propagate-ranges (seeds->ranges *seeds*) *almanac*))))
  
  
  )


;; ------------------------------------------------------------

;; A submap is a source start, destination start, and length
(struct submap (src dst len) #:transparent)

;; A rng is a pair, (min len) representing the range
;; [min, min + len)
(struct rng (min len) #:transparent)

;; ------------------------------------------------------------
;; For part 2

;; Convert mistaken part 1 view of seeds into the part 2
;; interpretation
(define (seeds->ranges seeds)
  (reverse
   (let loop ([ranges '()]
              [seeds seeds])
     (if (null? seeds)
         ranges
         (loop (cons (rng (car seeds) (cadr seeds)) ranges)
               (cddr seeds))))))

;; Make a range (max is exclusive)
(define (make-rng min max)
  (rng min (- max min)))

;; Return three ranges, any of which might be #f:
;; 1. The bit of [min1, max1) that is below min2;
;; 2. The bit of [min1, max1) that is above max2
;; 3. The overlap of [min1, max1) and [min2, max2)
(define (overlap rng1 rng2)
  (match-let ([(rng min1 len1) rng1]
              [(rng min2 len2) rng2])
    (let ([max1 (+ min1 len1)]
          [max2 (+ min2 len2)])
      (list
       (if (< min1 min2)
           (make-rng min1 (min max1 min2))
           #f)
       (if (> max1 max2)
           (make-rng (max min1 max2) max1)
           #f)
       (if (and (< min1 max2)
                (> max1 min2))
           (make-rng (max min1 min2) (min max1 max2))
           #f)))))

;; submap? rng? -> [list-of rng?] (must have same total length!)
;; Propagate a seed range through a submap.
;; Returns a pair:
;; - a list of zero, one, or two ranges that have not been caught by the submap
;; - up to one range that was caught and mapped by the submap, or #f
(define (submap-propagate-range in sm)
  (match-let ([(submap src-min dst-min submap-len) sm])
    (let ([splits (overlap in (rng src-min submap-len))])
      (cons (filter values (take splits 2))
            (let ([jump-rng (third splits)])
              (and jump-rng
                   (rng (+ (rng-min jump-rng) (- dst-min src-min)) 
                        (rng-len jump-rng))))))))


;; Propagate a range of seedmaps through the series of submaps in a
;; map.  The ranges are sent through the first submap, which may
;; result in some mapped ranges (which are then captured) and some
;; leftover ranges. The leftover ranges are then sent through the
;; subsequent submaps.  a list of ranges

;; [list-of rng?] -> [list-of submap?] -> [list-of rng?]
(define (map-propagate-ranges ins submaps)
  ;; Iterate through submaps
  (call-with-values
   (thunk
    (for/fold ([mapped-ranges     '()]
               [to-process-ranges ins])
              ([sm (in-list submaps)])
      (let ([outs (map (λ (in) (submap-propagate-range in sm)) to-process-ranges)])
        (values
         (append (filter-map cdr outs) mapped-ranges)
         (append-map car outs)))))
   append))

(define (almanac-propagate-ranges ins almanac)
  (for/fold ([outs ins])
            ([submaps (in-list almanac)])
    (map-propagate-ranges outs submaps)))

;; ------------------------------------------------------------
;; Mostly for Part 1

(define (almanac-propagate almanac seed)
  (for/fold ([out seed])
            ([submaps (in-list almanac)])
    (map-propagate submaps out)))

;; Transition a seed through one map
(define (map-propagate submaps in)
  (let loop ([submaps submaps])
    (if (null? submaps)
        in
        (match-let ([(submap src dst len) (car submaps)])
          (if (and (>= in src)
                   (<  in (+ src len)))
              (+ dst (- in src))
              (loop (cdr submaps)))))))


;; ------------------------------------------------------------
;; Parsing input

(define (parse-input lines)
  (let ([gps (group-lines lines)])
    (let ([seeds (map string->number
                      (string-split
                       (cadr (string-split (caar gps) ": "))))]
          [maps (map parse-map (cdr gps))])
      (values seeds maps))))

(define (parse-map lines)
  ;; Drop name
  (map (λ (ln)
         (let ([ns (map string->number ln)])
           (submap (second ns) (first ns) (third ns))))
       (map string-split (cdr lines))))

;; Partition lines at blank lines 
(define (group-lines lines)
  (define (notblank? s)
    (not (string=? "" s)))
  
  (reverse
   (let loop ([lines lines]
              [groups '()])
     (let-values ([(gp rst) (splitf-at lines notblank?)])
       (if (null? rst)
           (cons gp groups)
           (loop (cdr rst) (cons gp groups)))))))


(module+ test

  (define *input* #<<EOF
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
EOF
    )


  (define-values (*seeds* *almanac*)
    (parse-input
     (with-input-from-string *input* port->lines)))

  ;; Part 1 test
  (apply min
         (map (λ (sd) (almanac-propagate *almanac* sd)) *seeds*))

  ;; Part 2 test
  (apply min
         (map rng-min
              (almanac-propagate-ranges (seeds->ranges *seeds*) *almanac*)))
  
  )
