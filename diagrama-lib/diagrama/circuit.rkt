#lang racket/base
(require racket/contract)
(provide
 (contract-out
  [or-gate
   (->* ()
        (#:in1 any/c
         #:in2 any/c
         #:in3 any/c
         #:out any/c
         #:tag-in1 any/c
         #:tag-in2 any/c
         #:tag-in3 any/c
         #:tag-out any/c)
        diagram?)]
  [and-gate
   (->* ()
        (#:in1 any/c
         #:in2 any/c
         #:in3 any/c
         #:out any/c
         #:tag-in1 any/c
         #:tag-in2 any/c
         #:tag-in3 any/c
         #:tag-out any/c)
        diagram?)]
  [buffer (->* ()
               (#:in2 any/c
                #:out any/c
                #:tag-in2 any/c
                #:tag-out any/c)
               diagram?)]))
(require "main.rkt"
         racket/draw
         racket/class
         racket/match
         racket/string
         racket/list
         pict)

(define (gate-size s)
  (* s 3))

(define (interpret-path-commands commands)
  (define p (new dc-path%))
  (interpret-path-commands! p commands)
  p)
(define (interpret-path-commands! p commands)
  (match commands
    [(list) (void)]
    [(cons a b)
     (interpret-single-path-command! p a)
     (interpret-path-commands! p b)]))

(define (interpret-single-path-command! p a)
  (match a
    [`(M (,x ,y))
     (send p move-to x y)]
    [`(C (,x1 ,y1) (,x2 ,y2) (,x3 ,y3))
     (send p curve-to x1 y1 x2 y2 x3 y3)]
    [`(L (,x ,y))
     (send p line-to x y)]
    [`(z) (send p close)]))
     

(define (parse-path-commands s)
  (map parse-single-path-command
       (map string-trim
            (regexp-match* #rx"[MmLlHhVvQqTtAaZzCcSs][^MmLlHhVvQqTtAaZzCcSs]*" s))))

(define (parse-single-path-command s)
  (define (valueize x)
    (read (open-input-string x)))
  (define bits (string-split s))  
  (cons (valueize (first bits))
        (map (lambda (x) (map valueize (string-split x ",")))
             (rest bits))))

(define or-gate-path
  (interpret-path-commands
        (parse-path-commands
         "M 0.61775626,0.19872161 L 0.62004791,0.20387782 C 0.62157568,0.2073153 0.62253054,0.21017987 0.62291248,0.21247152 C 0.62329442,0.21476317 0.62346803,0.21705482 0.62346803,0.21934647 C 0.62346803,0.22162076 0.62329442,0.22391242 0.62291248,0.22620407 C 0.62253054,0.22849572 0.62157568,0.23136029 0.62004791,0.23479776 C 0.61852015,0.23823524 0.61775626,0.23995398 0.61775626,0.23995398 C 0.61775626,0.23995398 0.62004791,0.23995398 0.62463122,0.23995398 C 0.62921452,0.23995398 0.632652,0.23995398 0.63494365,0.23995398 C 0.63721794,0.23995398 0.64013459,0.23976301 0.64367624,0.23938107 C 0.64720052,0.23899913 0.65082897,0.23804427 0.65456158,0.2365165 C 0.65827684,0.23498874 0.66166223,0.23309639 0.66471777,0.23080473 C 0.6677733,0.22851308 0.67025593,0.22603046 0.67216564,0.22335686 C 0.67407535,0.22068327 0.6750302,0.21934647 0.6750302,0.21934647 C 0.6750302,0.21934647 0.67407535,0.21800968 0.67216564,0.21533608 C 0.67025593,0.21266249 0.6677733,0.21017987 0.66471777,0.20788821 C 0.66166223,0.20561392 0.65827684,0.20368685 0.65456158,0.20217645 C 0.65082897,0.20064868 0.64720052,0.19969382 0.64367624,0.19931188 C 0.64015195,0.19892994 0.63723531,0.19873897 0.63494365,0.19873897 C 0.632652,0.19873897 0.62921452,0.19873897 0.62463122,0.19873897 L 0.61775626,0.19872161 z ")))
(define and-gate-path
  (interpret-path-commands
   (parse-path-commands
    "M 0.62119374,0.033722662 C 0.63150617,0.033722662 0.64181861,0.033722662 0.65213104,0.033722662 C 0.65365881,0.033722662 0.65537755,0.033913633 0.65728726,0.034295575 C 0.65919697,0.034677517 0.66110668,0.035441401 0.66299903,0.036587227 C 0.66490874,0.037715692 0.66681845,0.039451792 0.66872816,0.041726083 C 0.67063787,0.044017735 0.67197467,0.04670869 0.67273855,0.049746865 C 0.67350243,0.052802401 0.67350243,0.055857937 0.67273855,0.058913473 C 0.67197467,0.061969009 0.67063787,0.064642603 0.66872816,0.066934255 C 0.66681845,0.069208546 0.66490874,0.070927285 0.66299903,0.072090472 C 0.66110668,0.073218937 0.65919697,0.073982821 0.65728726,0.074364763 C 0.65537755,0.074746705 0.65365881,0.074937676 0.65213104,0.074937676 C 0.65060328,0.074937676 0.64506512,0.074937676 0.63551657,0.074937676 C 0.62596802,0.074937676 0.62119374,0.074937676 0.62119374,0.074937676 C 0.62119374,0.074937676 0.62119374,0.06806272 0.62119374,0.054312808 L 0.62119374,0.033722662 z ")))
(define buffer-gate-path
  (let ()
    (define path (new dc-path%))
    ;; the actual path
    (send path move-to 0 1/8)
    (send path line-to 0 7/8)
    (send path line-to 1 1/2)
    (send path close)
    path))

(define (make-gate-pict-maker p*
                              nn1 nn2 nn3
                              #:override-path-width [ow #f]
                              #:override-path-height [oh #f]
                              #:override-path-left [ol #f]
                              #:override-path-top [ot #f])
  (lambda (n1 n2 n3 n4)
    (with-state
     (match-lambda**
      [(_ _ _ _ _ _ unit _)
       (pure
        (dc
         (lambda (dc dx dy)
           (define p (new dc-path%))
           (send p append p*)
           (define-values (l t w h)
             (send p get-bounding-box))
           (send p transform
                 (vector 1 0 0 1 (- (or ol l)) (- (or ot t))))
           (send p transform
                 (vector (* (/ 1 (or ow w)) (gate-size unit)) 0 0 (* (/ 1 (or oh h)) (gate-size unit)) 0 0))
           (send dc draw-path p dx dy)
           (when n1
             (send dc draw-path (not-at nn1 (to-coord unit 0)) dx dy))
           (when n2
             (send dc draw-path (not-at nn2 (to-coord unit 1)) dx dy))
           (when n3
             (send dc draw-path (not-at nn3 (to-coord unit 2)) dx dy))
           (when n4
             (send dc draw-path (not-at (gate-size unit) (to-coord unit 1)) dx dy)))
         (gate-size unit) (gate-size unit)))]))))

(define (make-gate-combinator gate)
  (lambda (#:in1 [n1 #f]
           #:in2 [n2 #f]
           #:in3 [n3 #f]
           #:out [n4 #f]
           #:tag-in1 [tag-n1 #f]
           #:tag-in2 [tag-n2 #f]
           #:tag-in3 [tag-n3 #f]
           #:tag-out [tag-n4 #f])
    (save
     (before
      (gate n1 n2 n3 n4)
      (if tag-n1
          (save (move-up 1)
                (line-left 3)
                (tag-location tag-n1))
          nothing)
      (if tag-n2
          (save (line-left 3)
                (tag-location tag-n2))
          nothing)
      (if tag-n3
          (save (move-down 1)
                (line-left 3)
                (tag-location tag-n3))
          nothing)
      (if tag-n4
          (save (line-right 3)
                (tag-location tag-n4))
          nothing)))))

(define (not-at x y)
  (define s 5)
  (define p (new dc-path%))
  (send p ellipse (- x (/ s 2)) (- y (/ s 2)) s s) 
  p)

(define and-gate
  (make-gate-combinator
   (make-gate-pict-maker and-gate-path 0 0 0)))
(define or-gate
  (make-gate-combinator
   (make-gate-pict-maker or-gate-path 2 3 2)))
(define buffer
  (make-gate-combinator
   (make-gate-pict-maker buffer-gate-path 0 0 0
                   #:override-path-width 1
                   #:override-path-height 1
                   #:override-path-left 0
                   #:override-path-top 0)))