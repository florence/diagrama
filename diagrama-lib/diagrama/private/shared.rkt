#lang racket/base
(provide
 (struct-out diagram)
 (struct-out diagram-state)
 unit
 to-coord
 new-state
 move-state-to
 state-add-tag
 state-get-tag
 state-set-unit
 state-set-color
 state-set-line-width)
(require pict/convert pict racket/match racket/draw racket/class
         file/convertible)

(struct diagram (f)
  #:property prop:procedure (struct-field-index f)
  #:property prop:pict-convertible
  (lambda (x) (draw-diagram (diagram-f x)))
  #:property prop:convertible
  (lambda (v r d)
    (convert (pict-convert v) r d)))

(define (draw-diagram c)
  (define-values (draw state) (c (new-state 0 0)))
  (match-define
    (diagram-state x y min-x min-y max-x max-y unit _ _ _)
    state)
  (define margin (+ 2 unit))
  (define w (- max-x min-x))
  (define h (- max-y min-y))
  (dc
   (lambda (dc dx dy)
     (define m (send dc get-transformation))
     (send dc set-smoothing 'smoothed)
     (send dc translate
           (+ dx (to-coord unit (- min-x)))
           (+ dy (to-coord unit (- min-y))))
     (draw dc)
     (send dc set-transformation m))
   (+ margin (to-coord unit w))
   (+ margin (to-coord unit h))))

(define unit 12)

(define (to-coord unit m)
  (+ (* m unit) (/ unit 2)))

(struct diagram-state
  (x y min-x min-y max-x max-y unit line-width color coord-tags)
  #:transparent)

(define (new-state x y)
  (diagram-state x y x y x y unit 1 "black" (hash)))

(define (move-state-to s x y)
  (match-define (diagram-state _ _ vx vy ^x ^y unit lw c tags) s)
  (diagram-state x y
                 (min x vx) (min y vy)
                 (max x ^x) (max y ^y)
                 unit lw c
                 tags))
(define (state-add-tag s t)
  (match-define (diagram-state x y vx vy ^x ^y unit lw c tags) s)
  (diagram-state x y vx vy ^x ^y
                 unit lw c
                 (hash-set tags t (list x y))))

(define (state-get-tag s t)
  (hash-ref (diagram-state-coord-tags s) t))

(define (state-set-unit s u)
  (match-define (diagram-state x y vx vy ^x ^y _ lw c tags) s)
  (diagram-state x y vx vy ^x ^y
                 u lw c
                 tags))

(define (state-set-color s c)
  (match-define (diagram-state x y vx vy ^x ^y u lw _ tags) s)
  (diagram-state x y vx vy ^x ^y
                 u lw c
                 tags))

(define (state-set-line-width s lw)
  (match-define (diagram-state x y vx vy ^x ^y u _ c tags) s)
  (diagram-state x y vx vy ^x ^y
                 u lw c
                 tags))