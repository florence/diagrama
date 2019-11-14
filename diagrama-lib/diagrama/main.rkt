#lang debug racket/base
(require racket/contract)
(provide
 for/after for*/after
 for/*> for*/*>
 for/<* for*/<*
 (contract-out
  [diagram? predicate/c]
  [to-coord (-> positive? real? real?)]
  [units (-> positive? diagram?)]
  [color (-> (or/c string? (is-a?/c color%))
             diagram?)]
  [line-width (-> (real-in 0 255) diagram?)]
  [pure (-> pict-convertible? diagram?)]
  [move-to (-> real? real? diagram?)]
  [tag-location
   (->i ([_ any/c])
        ([x real?]
         [y real?])
        #:pre/name (x y)
        "May not give only one of X and Y"
        (equal? (equal? x the-unsupplied-arg) (equal? y the-unsupplied-arg))
        [_ diagram?])]
  [move-right (-> real? diagram?)]
  [move-left (-> real? diagram?)]
  [move-down (-> real? diagram?)]
  [move-up (-> real? diagram?)]
  [move-to-tag (-> any/c diagram?)]
  [line-to (->* (real? real?)
                (#:h-first any/c)
                diagram?)]
  [line-left (-> real? diagram?)]
  [line-right (-> real? diagram?)]
  [line-down (-> real? diagram?)]
  [line-up (-> real? diagram?)]
  [line-to-tag (->* (any/c)
                    (#:h-first any/c)
                    diagram?)]
  [save (-> diagram? ... diagram?)]
  [save/bounds (-> diagram? ... diagram?)]
  [after (-> diagram? ... diagram?)]
  [before (-> diagram? diagram? ... diagram?)]
  [<* (-> diagram? diagram? ... diagram?)]
  [*> (-> diagram? diagram? ... diagram?)]
  [split (-> diagram? diagram? diagram?)]
  [label (-> string? (or/c 'up 'down 'left 'right) diagram?)]
  [nothing diagram?]
 
  [with-loc (->
             (-> real? real? diagram?)
             diagram?)]
  [with-bounds
   (->
    (-> real? real? real? real? diagram?)
    diagram?)]
  [with-unit
   (-> (-> real? diagram?) diagram?)]
  [with-line-width
   (-> (-> (real-in 0 255) diagram?) diagram?)]
  [with-color
   (-> (-> (or/c string? (is-a?/c color%)) diagram?)
       diagram?)]
 [start-at (-> #:ud (or/c 'up 'down) #:lr (or/c 'left 'right)
               diagram? ...
               diagram?)]
 [line-between
  (-> any/c any/c diagram?)]
 [unit-grid diagram?]))
(require pict racket/draw pict/convert
         "private/shared.rkt"
         racket/match
         racket/class
         racket/list
         racket/math
         (for-syntax racket/base syntax/parse))

(define (tag-location tag [x #f] [y #f])
  (after
   (if x (move-to x y) nothing)
   (diagram
    (lambda (state)
      (values
       void
       (if tag (state-add-tag state tag) state))))))
  

(define (units u)
  (diagram
   (lambda (s)
     (values void (state-set-unit s u)))))

(define (color c)
  (diagram 
   (lambda (s)
     (values void (state-set-color s c)))))

(define (line-width lw)
  (diagram 
   (lambda (s)
     (values void (state-set-line-width s lw)))))

(define (line-right x)
  (diagram
   (lambda (state)
     ((line-to (+ (diagram-state-x state) x)
               (diagram-state-y state))
      state))))

(define (line-down y)
  (diagram
   (lambda (state)
     ((line-to (diagram-state-x state) (+ (diagram-state-y state) y))
      state))))
(define (line-left x)
  (line-right (- x)))
(define (line-up x)
  (line-down (- x)))

(define (line-to-tag tag #:h-first [h #t])
  (diagram
   (lambda (state)
     ((apply line-to #:h-first h (hash-ref (diagram-state-coord-tags state) tag))
      state))))

(define (line-to x y #:h-first [h-first #t])
  (define (h s)
    ((line-to* x (diagram-state-y s)) s))
  (define (v s)
    ((line-to* (diagram-state-x s) y) s))
  (diagram
   (lambda (state)
     (define-values (d s) ((if h-first h v) state))
     (define-values (d2 s2) ((if h-first v h) s))
     (values (lambda (dc) (d dc) (d2 dc))
             s2))))

(define (line-to* x* y*)
  (diagram
   (lambda (state)
     (match-define
       (struct* diagram-state
                ([x sx*] [y sy*] [unit unit] [color c] [line-width lw]))
       state)
     (define x (to-coord unit x*))
     (define y (to-coord unit y*))
     (define sx (to-coord unit sx*))
     (define sy (to-coord unit sy*))
     (values 
      (lambda (dc)
        (define p (send dc get-pen))
        (send dc set-pen c lw 'solid)
        (send dc draw-line
              sx sy
              x y)
        (send dc set-pen p))
      (move-state-to state x* y*)))))

(define (move-right x)
  (diagram
  (lambda (state)
    ((move-to (+ x (diagram-state-x state)) (diagram-state-y state))
     state))))
(define (move-down y)
  (diagram
   (lambda (state)
     ((move-to (diagram-state-x state) (+ y (diagram-state-y state)))
     state))))
(define (move-left x)
  (move-right (- x)))
(define (move-up x)
  (move-down (- x)))

(define (move-to-tag tag)
  (diagram
   (lambda (state)
     ((apply move-to (hash-ref (diagram-state-coord-tags state) tag))
      state))))

(define (move-to x y)
  (diagram
   (lambda (state)
     (values
      void
      (move-state-to state x y)))))

(define (pure p)
  (diagram
   (lambda (state)
     (match-define (diagram-state x y vx vy ^x ^y unit lw c tags) state)
     (define pict (p state))
     (values
      (pict-drawer pict state x y)
      (diagram-state
       x y
       (min vx (exact-round (- x (/ (pict-width pict) (* unit 2)))))
       (min vy (exact-round (- y (/ (pict-height pict) (* unit 2)))))
       (max ^x (exact-round (+ x (/ (pict-width pict) (* unit 2)))))
       (max ^y (exact-round (+ y (/ (pict-height pict) (* unit 2)))))
       unit lw c
       tags)))))

(define (pict-drawer pict s x y)
  (define u (diagram-state-unit s))
  (lambda (dc)
    (draw-pict pict dc
               (- (to-coord u x) (/ (pict-width pict) 2))
               (- (to-coord u y) (/ (pict-height pict) 2)))))

(define (after . a)
  (match a
    [(list)
     (diagram (lambda (state) (values void state)))]
    [(cons f a)
     (define r (apply after a))
     (diagram 
      (lambda (state)
        (define-values (draw state1) (f state))
        (define-values (draw2 state2) (r state1))
        (values
         (lambda (dc) (draw dc) (draw2 dc))
         state2)))]))

(define (<* f . a)
  (if (empty? a)
      f
      (diagram 
       (lambda (state)
         (define-values (draw state1) (f state))
         (define-values (draw2 state2)
           ((apply <* a)
            (state-set-unit
             (move-state-to state1 (diagram-state-x state) (diagram-state-y state))
             (diagram-state-unit state))))
         (values
          (lambda (dc) (draw dc) (draw2 dc))
          (move-state-to
           state2
           (diagram-state-x state1)
           (diagram-state-y state1)))))))

(define (*> f . a)
  (if (empty? a)
      f
      (diagram 
       (lambda (state)
         (define-values (draw state1) (f state))
         (define-values (draw2 state2)
           ((apply *> a)
            (state-set-unit
             (move-state-to state1 (diagram-state-x state) (diagram-state-y state))
             (diagram-state-unit state))))
         (values
          (lambda (dc) (draw dc) (draw2 dc))
          state2)))))

(define nothing
  (diagram (lambda (state) (values void state))))

(define (with-state thunk)
  (diagram
   (lambda (s)
     ((thunk s) s))))

(define (save/bounds . thunks)
  (define f (apply after thunks))
  (diagram
   (lambda (s)
     (define-values (d s2) (f s))
     (match-define (diagram-state x y x0 y0 xm ym unit lw c hash) s)
     (values d
             (diagram-state x y x0 y0 xm ym unit lw c
                            (diagram-state-coord-tags s2))))))
      

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
   (lambda (s)
     (thunk (diagram-state-x s) (diagram-state-y s)))))
(define (with-bounds thunk)
  (with-state
   (lambda (s)
     (match-define
       (struct* diagram-state
                ([min-x x0] [min-y y0] [max-x xm] [max-y ym]))
       s)
     (thunk x0 y0 xm ym))))

(define (with-unit thunk)
  (with-state
   (lambda (s)
     (thunk (diagram-state-unit s)))))

(define (with-color thunk)
  (with-state
   (lambda (s)
     (thunk (diagram-state-color s)))))
(define (with-line-width thunk)
  (with-state
   (lambda (s)
     (thunk (diagram-state-line-width s)))))

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

(define (dot lw)
  (pure (disk (* 5 lw))))

(define (split thunk1 thunk2)
  (after
   (with-line-width dot)
   (save thunk1)
   thunk2))

(define (line-between a b #:h-first [h-fit #t])
  (save
   (move-to-tag a)
   (line-to-tag b #:h-first h-fit)))

(define unit-grid
  (with-bounds
   (lambda (x0 y0 xm ym)
     (define w (add1 (- xm x0)))
     (define h (add1 (- ym y0)))
     (save/bounds
      (color (make-object color% 128 128 128 .5))
      (for*/after ([x (in-range (add1 w))])
        (after (move-to (- (+ x x0) 1/2) (- y0 1/2))
               (line-down h)))
      (for*/after ([y (in-range (add1 h))])
        (after (move-to (- x0 1/2) (- (+ y y0) 1/2))
               (line-right w)))))))


;                                                                                            
;                                                                                            
;                                                                                            
;    ;;;;;;;                                 ;;                                              
;    ;;                                      ;;                                              
;    ;;                                      ;;                                              
;    ;;         ;;;;     ;;  ;;              ;;         ;;;;      ;;;;     ; ;;;      ;;;;   
;    ;;        ;;  ;;     ; ; ;              ;;        ;;  ;;    ;;  ;;    ;;  ;;    ;    ;  
;    ;;       ;;    ;     ;;  ;              ;;       ;;    ;   ;;    ;    ;    ;    ;       
;    ;;;;;;   ;;    ;     ;;                 ;;       ;;    ;   ;;    ;    ;    ;    ;;      
;    ;;       ;;    ;     ;                  ;;       ;;    ;   ;;    ;    ;    ;     ;;;;   
;    ;;       ;;    ;     ;                  ;;       ;;    ;   ;;    ;    ;    ;        ;;  
;    ;;       ;;    ;     ;                  ;;       ;;    ;   ;;    ;    ;    ;         ;  
;    ;;        ;;  ;;     ;                  ;;        ;;  ;;    ;;  ;;    ;;  ;;   ;;   ;;  
;    ;;         ;;;;     ;;;;                ;;;;;;;    ;;;;      ;;;;     ;;;;;     ;;;;;   
;                                                                          ;                 
;                                                                          ;                 
;                                                                          ;                 
;                                                                                            
;                                                                                            


(define-for-syntax (make-for-folder func folder)
  (with-syntax ([f func] [fold folder])
    (syntax-parser
      [(_ clauses body ... final)
       #`(fold #,this-syntax
               ([p nothing])
               clauses
               body ...
               (f p final))])))
(define-for-syntax (make-for-folders func)
  (values (make-for-folder func #'for/fold/derived)
          (make-for-folder func #'for*/fold/derived)))

(define-syntaxes (for/after for*/after)
  (make-for-folders #'after))

(define-syntaxes (for/*> for*/*>)
  (make-for-folders #'*>))

(define-syntaxes (for/<* for*/<*)
  (make-for-folders #'<*))
          
  