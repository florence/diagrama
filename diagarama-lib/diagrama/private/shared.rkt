#lang racket/base
(provide
 (struct-out cpict)
 (struct-out cpict-state)
 line-width
 unit
 to-coord
 new-state
 move-state-to
 state-add-tag
 state-get-tag
 state-set-unit)
(require pict/convert pict racket/match racket/draw racket/class)

(struct cpict (f)
  #:property prop:procedure (struct-field-index f)
  #:property prop:pict-convertible
  (lambda (x) (draw-diagram (cpict-f x))))

(define (draw-diagram c)
  (define-values (draw state) (c (new-state 0 0)))
  (match-define (cpict-state x y min-x min-y max-x max-y unit _) state)
  (define margin unit)
  (define w (- max-x min-x))
  (define h (- max-y min-y))
  (dc
   (lambda (dc dx dy)
     (define s (send dc get-smoothing))
     (define m (send dc get-transformation))
     (send dc set-smoothing 'smoothed)
     (send dc translate
           (+ dx (to-coord unit (- min-x)))
           (+ dy (to-coord unit (- min-y))))
     (draw dc)
     (send dc set-transformation m))
   (+ margin (to-coord unit w))
   (+ margin (to-coord unit h))))

(define line-width 1)
(define unit 12)



(define (to-coord unit m)
  (+ (* m unit) (/ unit 2)))

(struct cpict-state
  (x y min-x min-y max-x max-y unit coord-tags)
  #:transparent)

(define (new-state x y)
  (cpict-state x y x y x y unit (hash)))

(define (move-state-to s x y)
  (match-define (cpict-state _ _ vx vy ^x ^y unit tags) s)
  (cpict-state x y
               (min x vx) (min y vy)
               (max x ^x) (max y ^y)
               unit
               tags))
(define (state-add-tag s t)
  (match-define (cpict-state x y vx vy ^x ^y unit tags) s)
  (cpict-state x y
               vx vy ^x ^y
               unit
               (hash-set tags t (list x y))))

(define (state-get-tag s t)
  (hash-ref (cpict-state-coord-tags s) t))

(define (state-set-unit s u)
  (match-define (cpict-state x y vx vy ^x ^y _ tags) s)
  (cpict-state x y
               (min x vx) (min y vy)
               (max x ^x) (max y ^y)
               u
               tags))