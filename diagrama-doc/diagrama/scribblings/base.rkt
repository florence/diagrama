#lang racket/base
(require
 scribble/examples
 (for-label racket pict pict/convert diagrama diagrama/circuit racket/draw))

(provide
 diag
 (all-from-out scribble/examples)
 (for-label
  (all-from-out racket pict pict/convert diagrama diagrama/circuit racket/draw)))

(define diag
  (make-base-eval
   #:lang 'racket
   '(require diagrama diagrama/circuit pict racket/draw)))