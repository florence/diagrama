#lang info

(define collection 'multi)

(define deps '(("base" #:version "7.4")))

(define build-deps '("diagrama-lib"
                     "pict-lib"
                     "draw-doc"
                     "draw-lib"
                     "pict-doc"
                     "racket-doc"
                     "scribble-lib"))

(define pkg-desc "Documentation part of `diagrama`")

(define version "0.1")
