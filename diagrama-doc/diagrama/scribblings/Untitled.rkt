#lang racket
(require diagrama diagrama/circuit)

(define input-1 (tag-location 'input-1))
(define input-2
  (save (tag-location 'input-2)
        (move-down 10)))
(define top-gate
  (save
   (move-to 'input-1)
   (move-down 1)
   (move-right 10)
   (and-gate
    #:out #t
    #:tag-in1 'and-A
    #:tag-in3 'and-B)))

(define lower-gate
  (save
   (move-to 'input-1)
   (move-down 7)
   (move-right 10)
   (and-gate
    #:out #t
    #:tag-in1 'and-A
    #:tag-in3 'and-B)))

input-1 input-2 top-gate lower-gate