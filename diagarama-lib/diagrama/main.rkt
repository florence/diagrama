#lang racket/base
(require racket/contract)
(provide
 (contract-out
  [cpict? predicate/c]
  [to-coord (-> positive? real? real?)]
  [units (-> positive? cpict?)]
  [pure (-> pict-convertible? cpict?)]
  [move-to (-> real? real? cpict?)]
  [tag-location (-> any/c cpict?)]
  [move-right (-> real? cpict?)]
  [move-left (-> real? cpict?)]
  [move-down (-> real? cpict?)]
  [move-up (-> real? cpict?)]
  [move-to-tag (-> any/c cpict?)]
  [line-to (-> real? real? cpict?)]
  [line-left (-> real? cpict?)]
  [line-right (-> real? cpict?)]
  [line-down (-> real? cpict?)]
  [line-up (-> real? cpict?)]
  [line-to-tag (-> any/c cpict?)]
  [save (-> cpict? ... cpict?)]
  [after (-> cpict? ... cpict?)]
  [before (-> cpict? cpict? ... cpict?)]
  [<* (-> cpict? ... cpict?)]
  [*> (-> cpict? ... cpict?)]
  [split (-> cpict? cpict? cpict?)]
  [label (-> string? (or/c 'up 'down 'left 'right) cpict?)]
  [nothing cpict?]
  [with-state
   (-> (-> real? real? real? real? real? real?
           real? ;; really?
           (hash/c any/c (list/c real? real?))
           cpict?)
       cpict?)]
  [with-loc (->
             (-> real? real? cpict?)
             cpict?)]
  [with-bounds
   (->
    (-> real? real? real? real? cpict?)
    cpict?)]
 [start-at (-> cpict? #:ud (or/c 'up 'down) #:lr (or/c 'left 'right)
               cpict? ...
               cpict?)]
 [cwhen (-> any/c cpict? ... cpict?)]))
(require pict racket/draw pict/convert
         "private/shared.rkt"
         racket/match
         racket/class)

(define (tag-location tag)
  (cpict
   (lambda (state)
     (values
      void
      (if tag (state-add-tag state tag) state)))))

(define (units u)
  (cpict
   (lambda (s)
     (values void (state-set-unit s u)))))


(define (line-right x)
  (cpict
   (lambda (state)
     ((line-to (+ (cpict-state-x state) x)
               (cpict-state-y state))
      state))))

(define (line-down y)
  (cpict
   (lambda (state)
     ((line-to (cpict-state-x state) (+ (cpict-state-y state) y))
      state))))
(define (line-left x)
  (line-right (- x)))
(define (line-up x)
  (line-down (- x)))

(define (line-to-tag tag)
  (cpict
   (lambda (state)
     ((apply line-to (hash-ref (cpict-state-coord-tags state) tag))
      state))))

(define (line-to x y)
  (cpict
   (lambda (state)
     (define-values (d s) ((line-to* x (cpict-state-y state)) state))
     (define-values (d2 s2) ((line-to* (cpict-state-x s) y) s))
     (values (lambda (dc) (d dc) (d2 dc))
             s2))))

(define (line-to* x* y*)
  (cpict
   (lambda (state)
     (match-define (cpict-state sx* sy* _ _ _ _ unit _) state)
     (define x (to-coord unit x*))
     (define y (to-coord unit y*))
     (define sx (to-coord unit sx*))
     (define sy (to-coord unit sy*))
     (values 
      (lambda (dc)
        (send dc draw-line
              sx sy
              x y))
      (move-state-to state x* y*)))))

(define (move-right x)
  (cpict
  (lambda (state)
    ((move-to (+ x (cpict-state-x state)) (cpict-state-y state))
     state))))
(define (move-down y)
  (cpict
   (lambda (state)
     ((move-to (cpict-state-x state) (+ y (cpict-state-y state)))
     state))))
(define (move-left x)
  (move-right (- x)))
(define (move-up x)
  (move-down (- x)))

(define (move-to-tag tag)
  (cpict
   (lambda (state)
     ((apply move-to (hash-ref (cpict-state-coord-tags state) tag))
      state))))

(define (move-to x y)
  (cpict
   (lambda (state)
     (values
      void
      (move-state-to state x y)))))

(define (pure maybe-pict)
  (define p
    (if (pict?  maybe-pict)
        (lambda (s) maybe-pict)
        maybe-pict))
  (cpict
   (lambda (state)
     (match-define (cpict-state x y vx vy ^x ^y unit tags) state)
     (define pict (p state))
     (values
      (pict-drawer pict state x y)
      (cpict-state
       x y
       (min vx (- x (/ (pict-width pict) (* unit 2))))
       (min vy (- y (/ (pict-height pict) (* unit 2))))
       (max ^x (+ x (/ (pict-width pict) (* unit 2))))
       (max ^y (+ y (/ (pict-height pict) (* unit 2))))
       unit
       tags)))))

(define (pict-drawer pict s x y)
  (define u (cpict-state-unit s))
  (lambda (dc)
    (draw-pict pict dc
               (- (to-coord u x) (/ (pict-width pict) 2))
               (- (to-coord u y) (/ (pict-height pict) 2)))))

(define (after . a)
  (match a
    [(list)
     (cpict (lambda (state) (values void state)))]
    [(cons f a)
     (define r (apply after a))
     (cpict 
      (lambda (state)
        (define-values (draw state1) (f state))
        (define-values (draw2 state2) (r state1))
        (values
         (lambda (dc) (draw dc) (draw2 dc))
         state2)))]))

(define (<* f . a)
  (cpict 
   (lambda (state)
     (define-values (draw state1) (f state))
     (define-values (draw2 state2)
       ((apply after a)
        (state-set-unit
         (move-state-to state1 (cpict-state-x state) (cpict-state-y state))
         (cpict-state-unit state))))
     (values
      (lambda (dc) (draw dc) (draw2 dc))
      (move-state-to
       state2
       (cpict-state-x state1)
       (cpict-state-y state1))))))

(define (*> f . a)
  (cpict 
   (lambda (state)
     (define-values (draw state1) (f state))
     (define-values (draw2 state2)
       ((apply after a)
        (state-set-unit
         (move-state-to state1 (cpict-state-x state) (cpict-state-y state))
         (cpict-state-unit state))))
     (values
      (lambda (dc) (draw dc) (draw2 dc))
      state2))))

(define nothing
  (cpict (lambda (state) (values void state))))

(define (with-state thunk)
  (cpict
   (lambda (s)
     (match-define (cpict-state x y x0 y0 xm ym unit hash) s)
     ((thunk x y x0 y0 xm ym unit hash)
      s))))

;                                                                        
;                                                                        
;                                                                        
;                                    ;;                              ;   
;   ;;;;;;                           ;;                               ;; 
;   ;;   ;;                                                           ;; 
;   ;;    ;;                                                          ;; 
;   ;;     ;    ;;;;     ;;  ;;;   ;;;;     ;;     ;    ;;;;      ;;; ;; 
;   ;;     ;   ;;  ;;     ; ;  ;      ;      ;    ;;   ;;  ;;    ;;  ;;; 
;   ;;     ;   ;    ;;    ;;   ;      ;      ;    ;    ;    ;;  ;;    ;; 
;   ;;     ;  ;;    ;;    ;;          ;      ;;   ;   ;;    ;;  ;;    ;; 
;   ;;     ;  ;;;;;;;;    ;;          ;       ;  ;;   ;;;;;;;;  ;;    ;; 
;   ;;    ;;  ;;          ;;          ;       ;  ;    ;;        ;;    ;; 
;   ;;    ;;   ;          ;;          ;       ;; ;     ;        ;;    ;; 
;   ;;   ;;    ;;   ;     ;;          ;        ;;      ;;   ;    ;;  ;;; 
;   ;;;;;;      ;;;;;;   ;;;;      ;;;;;;;     ;;       ;;;;;;    ;;; ;; 
;                                                                        
;                                                                        
;                                                                        
;                                                                        
;

(define (save . thunks)
  (*> (apply after thunks) nothing))

(define (with-loc thunk)
  (with-state
   (lambda (x y x0 y0 xm ym unit hash)
     (thunk x y))))
(define (with-bounds thunk)
  (with-state
   (lambda (x y x0 y0 xm ym unit hash)
     (thunk x0 y0 xm ym))))

(define (start-at #:ud ud #:lr lr . b)
  (for/fold ([p nothing])
            ([h (in-list (reverse b))])
    (after h
           (with-bounds
            (lambda (x0 y0 xm ym)
              (after
               (move-to
                (case lr
                  [(left) x0]
                  [(right) xm])
                (case ud
                  [(up) y0]
                  [(down) ym]))
               p))))))

(define (before a . f)
  (<* (apply after f)
      a))

(define (label t dir)
  (save
   (after
    ((case dir
       [(up) move-up]
       [(down) move-down]
       [(right) move-right]
       [(left) move-left])
     1)
    (pure (text t)))))

(define (cwhen c . p)
  (if c
      (apply after p)
      nothing))

(define dot
  (disk (* 5 line-width)))

(define (split thunk1 thunk2)
  (after
   (pure dot)
   (save thunk1)
   thunk2))
