#lang racket/base
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
  [img
   (->* (pict-convertible?)
        ((or/c 'lt 'ct 'rt 'lc 'cc 'rc 'lb 'cb 'rb))
        diagram?)]
  [path (->* ((is-a?/c dc-path%))
             ((or/c 'odd-even 'winding))
             diagram?)]
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
  [label (-> (or/c string?
                   pict-convertible?)
             (or/c 'up 'down 'left 'right) diagram?)]
  [nothing diagram?]

  [pin-here
   (-> diagram? any/c diagram?)]
 
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
  [with-locations-of
   (-> any/c ... procedure?
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
    ((line-to* (- x (diagram-state-x s)) 0) s))
  (define (v s)
    ((line-to* 0 (- y (diagram-state-y s))) s))
  (diagram
   (lambda (state)
     (define-values (d s) ((if h-first h v) state))
     (define-values (d2 s2) ((if h-first v h) s))
     (values (lambda (dc) (d dc) (d2 dc))
             s2))))

(define (line-to* x y)
  (define p (new dc-path%))
  (send p move-to 0 0)
  (send p line-to x y)
  (send p close)
  (path p))

(define (path p* [fill-mode 'odd-even])
  (define p (new dc-path%))
  (send p append p*)
  (send p close)
  (define-values (ps _) (send p get-datum))
  (define-values (x y)
    (match (filter (lambda (x) (not (empty? x))) ps)
      [(list) (values 0 0)]
      [(list _ ... (list _ ... (vector _ ... x y)))
       (values x y)]))
  (define-values (x0 y0 w h) (send p get-bounding-box))
  (define xm (+ w x0))
  (define ym (+ h y0))
  (diagram
   (lambda (s)
     (match-define (diagram-state x1 y1 vx vy ^x ^y unit lw c tags) s)
     (values
      (draw-path-with-drawing-state p s fill-mode)
      (diagram-state
       (+ x x1) (+ y y1)
       (min vx (+ x0 x1))
       (min vy (+ y0 y1))
       (max ^x (+ xm x1))
       (max ^y (+ ym y1))
       unit lw c
       tags)))))

(define (draw-path-with-drawing-state p state fill-mode)
  (lambda (dc)
    (define p2 (new dc-path%))
    (send p2 append p)
    (send p2 close)
    (match-define
      (struct* diagram-state
               ([x sx] [y sy] [unit unit] [color c] [line-width lw]))
      state)
    (define pen (send dc get-pen))
    (define m (send dc get-transformation))
    (send dc set-pen c lw 'solid)
    (send p2 transform
          (vector unit 0
                  0 unit
                  (to-coord unit sx) (to-coord unit sy)))
    (send dc draw-path p2 0 0 fill-mode)
    (send dc set-pen pen)
    (send dc set-transformation m)))
      

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

(define (img pict [align 'cc])
  (define-values (horz vert)
    (let ([v (symbol->string align)])
      (values
       (string->symbol (string (string-ref v 0)))
       (string->symbol (string (string-ref v 1))))))
  (define w (pict-width pict))
  (define h (pict-height pict))
  
  (diagram
   (lambda (state)
     (match-define (diagram-state x y vx vy ^x ^y unit lw c tags) state)
     (define x*
       (- x
          (case horz
            [(l) 0]
            [(c) (/ w (* unit 2))]
            [(r) (/ w unit)])))
     (define y*
       (- y
          (case vert
            [(t) 0]
            [(c) (/ h (* unit 2))]
            [(b) (/ h unit)])))
     (values
      (pict-drawer pict state x* y*)
      (diagram-state
       x y
       (min vx (exact-round x*))
       (min vy (exact-round y*))
       (max ^x (exact-round (+ x* (/ w unit))))
       (max ^y (exact-round (+ y* (/ h unit))))
       unit lw c
       tags)))))
        

(define (pict-drawer pict s x y)
  (define u (diagram-state-unit s))
  (lambda (dc)
    (draw-pict pict dc
               (to-coord u x)
               (to-coord u y))))

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

(define (pin-here other tag)
  ;; this is horribly slow. sorry, don't care atm
  (define-values (_ other-state)
    ((diagram-f other) (new-state 0 0)))
  (match-define (list ox oy) (state-get-tag other-state tag))
  (after
   (move-left ox)
   (move-up oy)
   other))

      

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

(define (with-locations-of . args)
  (define a (reverse args))
  (define thunk (first a))
  (define tags (reverse (rest a)))
  (with-state
   (lambda (s)
     (apply thunk
            (append-map
             (lambda (tag)
               (hash-ref (diagram-state-coord-tags s) tag))
             tags)))))

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
    (img
     (if (pict? t)
         t
         (text t))))))

(define (cwhen c . p)
  (if c
      (apply after p)
      nothing))

(define (dot lw)
  (img (disk (* 5 lw))))

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
          
  