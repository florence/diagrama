#lang racket/base
(require
 scribble/examples
 (for-label racket pict pict/convert diagrama diagrama/circuit))

(provide
 diag
 (all-from-out scribble/examples)
 (for-label
  (all-from-out racket pict pict/convert diagrama diagrama/circuit)))

(define diag
  (make-base-eval
   #:lang 'racket
   '(require diagrama diagrama/circuit pict)))