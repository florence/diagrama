#lang scribble/manual
@title{Diagrama API Reference}
@(require "base.rkt")
@defmodule[diagrama #:use-sources (diagrama)]

@defproc[(diagram? [it any/c])
         boolean?]{

 Is @racket[it] a Diagram. Diagrams are computations that draw,
 well, diagrams. Diagrams have a state which consists of a
 current drawing location and a notion of units, which is
 uses to convert from that location to distance into the
 resulting @racket[pict?]. The computation may result in a
 new state.
 
 Diagrams are @racket[pict-convertible?]. When diagrams are
 drawn the whole image is shifted such that the minimum x and
 y coordinate are shifted to the origin. When diagrams are
 converted to @racket[pict?]s the starting coordinates are
 always (@racket[0],@racket[0]).

}

@section{Basic diagram constructors}

@defthing[nothing diagram?]{
 An empty diagram.
}

@defproc[(pure [p pict-convertible?]) diagram?]{
 Convert this @racket[p] into a diagram which just draws @racket[p]
 centered at the current location.
}


@defproc[(line-to [x real?] [y real?] [#:h-first h-fit any/c #t])
         diagram?]{
                                                
 Creates a diagram which draws a line from the current
 location to (@racket[x],@racket[y]). The line moves only
 horizontally and vertically. If @racket[h-first] is
 not @racket[#f] it moves horizontally then vertically, otherwise
 it does the reverse.
 

 @examples[#:eval diag
           (line-to 3 2)]
 
}
@deftogether[(@defproc[(line-left [d real?]) diagram?]
               @defproc[(line-right [d real?]) diagram?]
               @defproc[(line-up [d real?]) diagram?]
               @defproc[(line-down [d real?]) diagram?])]{
 Create a diagram which draws a line from the current location
 @racket[d] away in the given direction.
 @examples[#:eval diag
           (line-right 5)
           (line-up 5)]
}

@defproc[(move-to [x real?] [y real?]) diagram?]{
                                                 
 Makes an empty diagram which moves the current drawing
 location to (@racket[x],@racket[y]), with
 (@racket[0],@racket[0]) being in the upper left.

 @examples[#:eval diag
           (after
            (move-to 3 3)
            (line-right 5))]
 
}

@deftogether[(@defproc[(move-left [d real?]) diagram?]
               @defproc[(move-right [d real?]) diagram?]
               @defproc[(move-up [d real?]) diagram?]
               @defproc[(move-down [d real?]) diagram?])]{
 Makes an empty diagram which moves the current location
 by @racket[d] in the corresponding direction.
}



@defproc[(tag-location [name any/c]
                       [x real? #f]
                       [y real? #f])
         diagram?]{
 Make an empty diagram that names ta location @racket[name].
 If @racket[x] and @racket[y] given that location is
 named, otherwise the current location is names.
 This will overwrite any existing locations which have a name @racket[equal?]
 to @racket[name].
 
}

@defproc[(move-to-tag [name any/c]) diagram?]{
                                              
 Move to the location with the given @racket[name]. Errors
 if no location has that @racket[name].
                                     
}

@defproc[(line-to-tag [name any/c] [#:h-first h-fit any/c #t])
         diagram?]{
                                              
 Draw a line from the current location to the location with
 the given @racket[name]. Errors if no location has that
 @racket[name]. The line is drawn like @racket[line-to].
 
 @examples[#:eval diag
           (after
            (tag-location 'here 3 3)
            (move-to 0 0)
            (line-to-tag 'here))]                          
}

@defproc[(line-between [start any/c] [end any/c] [#:h-first h-fit any/c #t]) diagram]{

 Draw a line between the two named coordinates. See also
 @racket[line-to-tag] and @racket[line-to].

 @examples[#:eval diag
           (after
            (tag-location 'here 1 2)
            (tag-location 'there 4 0)
            (line-between 'here 'there))]

}

@defproc[(units [u positive?]) diagram?]{

 Create an empty diagram that changes the current
 size of the coordinate system to @racket[u]. The default
 is @racket[12].
 
 @examples[#:eval diag
           (define l (line-right 1))
           l
           (after (units 36) l)]
 
}

@defproc[(color [c (or/c string? (is-a?/c color%))]) diagram?]{

 Create an empty diagram that changes
 the current line color.

 @examples[#:eval diag
           (after (units 36) (color "red")
                  (line-right 2))]
 
}

@defproc[(line-width [l (real-in 0 255)]) diagram?]{

 Create an empty diagram that changes
 the current line width.
 
}

@defproc[(label [t string?] [dir (or/c 'up 'down 'left 'right)])
         diagram?]{
                   
 Add text to the diagram one unit in the given direction.

 @examples[#:eval diag
           (after
            (units 24)
            (save (label "Line" 'right))
            (line-up 1)
            (line-down 2))]
 
}

@defthing[unit-grid diagram?]{

 Draws a grid over the current diagram with length/width of each
 cell of unit length.

 @examples[#:eval diag
           (define l (line-right 3))
           l
           (after l unit-grid)
           (after l (units 24) unit-grid)
           (after (units 24) l unit-grid)
           (after
            (save l)
            (move-down 1) (save l)
            (move-down 1) (save l)
            unit-grid)]
 
}

@section{Diagram composition}

@defproc[(after [d diagram?] ...) diagram?]{
 
 Draw all of the @racket[d]s one after another.

 @examples[#:eval diag
           (after
            (line-up 3)
            (line-right 3)
            (line-down 3)
            (line-left 3))]
}

@defproc[(before [d1 diagram?] [d diagram?] ...) diagram?]{
 
 Draw all of the @racket[d]s one after another, then
 draw @racket[d1] at with initial state of the diagram.

 @examples[#:eval diag
           (after (pure (disk 36 #:color "white"))
                  (line-right 3))
           (before (pure (disk 36 #:color "white"))
                   (line-right 3))]
 
}

@defproc[(save [d diagram?] ...) diagram?]{
 Draw all of @racket[d] one after another,
 then resort the current units and location
 to what they were at the start of the @racket[save].
}

@defproc[(save/bounds [d diagram?] ...) diagram?]{
                                                  
 Like @racket[save] except the bounds of the diagram are are
 restored after the @racket[d]s are draw. This allows for
 drawing outside of the bounds of the resulting @racket[pict?].
 
}

@defproc[(split [d1 diagram?] [d2 diagram])
         diagram?]{
 Draw @racket[d1] and @racket[d2] with the current
 state, and place a black dot at the current location. The
 resulting state is the state from @racket[d2].

 @examples[#:eval diag
           (after
            (line-right 3)
            (split
             (after (line-up 3) (line-right 3))
             (after (line-down 3) (line-right 3)))
            (line-down 3))]
}

@defproc[(*> [d1 diagram?] [d diagram?] ...) diagram?]{
                                                       
 Draw all of the diagrams in order, with each being drawn
 with the initial state. The resulting state is that of the
 last diagram.

 @examples[#:eval diag
           (after
            (units 36)
            (*> (line-up 1)
                (line-down 1)
                (line-left 1)
                (line-right 1))
            (line-down 1))]
 
}

@defproc[(<* [d1 diagram?] [d diagram?] ...) diagram?]{
                                                       
 Draw all of the diagrams in order, with each being drawn
 with the initial state. The resulting state is that of the
 first diagram.

 @examples[#:eval diag
           (after
            (units 36)
            (<* (line-up 1)
                (line-down 1)
                (line-left 1)
                (line-right 1))
            (line-right 1))]
 
}

@defproc[(start-at [#:ud ud (or/c 'up 'down)]
                   [#:lr lr (or/c 'left 'right)]
                   [d diagram?] ...)
         diagram]{
                  
 Draw the diagrams in order, with each starting at the
 location on the corner of the previous specified by
 @racket[ud] and @racket[lr]. The first diagram
 is drawn at the current location.
 
}

@section{Reflecting on the drawing state}

There are several ways to directly inspect the current
drawing state. These are all fairly low level operations
that are most likely useful for making new combinators, or when
making @racket[pict?]'s that scale to the current unit size (for
example @racket[unit-grid] and @racket[start-at] are defined
with these).

@defproc[(with-loc
          [builder (-> real? real? diagram?)])
         diagram?]{
 Build a diagram using the current (x,y) location.
}

@defproc[(with-bounds
          [builder (-> real? real? real? real? diagram?)])
         diagram?]{
 Build a diagram given the current bounding box. See @racket[with-state]
 for the order of arguments to @racket[builder].
}

@defproc[(with-color
          [builder (-> (or/c string? (is-a?/c color%)) diagram?)])
         diagram?]{

 Build a diagram using the current color.

}

@defproc[(with-line-width
          [builder (-> (real-in 0 255) diagram?)])
         diagram?]{

 Build a diagram using the current line width.

}

@defproc[(with-unit
          [builder (-> real? diagram?)])
         diagram?]{

 Build a diagram given the current units.

 @examples[#:eval diag
           (define unit-circle
             (with-unit (compose pure circle)))
           unit-circle
           (after (units 24) unit-circle)
           (after
            (units 24)
            (for*/fold ([p nothing])
                       ([x (in-range 3)]
                        [y (in-range 3)])
              (after p
                     (move-to x y)
                     unit-circle))
            unit-grid)]
            

}

@section{Circuit Helpers}

@defmodule[diagrama/circuit #:use-sources (diagrama/circuit)]

@racketmodname[diagrama/circuit] has helpers for drawing
circuit diagrams. Note that it is easy to accidentally draw
lines on top of gates: @racket[before] is designed to help
with this.

@deftogether[(@defproc[(or-gate
                        [#:n1 n1 any/c #f]
                        [#:n2 n2 any/c #f]
                        [#:n3 n3 any/c #f]
                        [#:out out any/c #f]
                        [#:tag-in1 tag1 any/c #f]
                        [#:tag-in2 tag2 any/c #f]
                        [#:tag-in3 tag3 any/c #f]
                        [#:tag-out tag4 any/c #f])
                       diagram?]
               @defproc[(and-gate
                         [#:n1 n1 any/c #f]
                         [#:n2 n2 any/c #f]
                         [#:n3 n3 any/c #f]
                         [#:out out any/c #f]
                         [#:tag-in1 tag1 any/c #f]
                         [#:tag-in2 tag2 any/c #f]
                         [#:tag-in3 tag3 any/c #f]
                         [#:tag-out tag4 any/c #f])
                        diagram?]
               @defproc[(buffer
                         [#:n2 n2 any/c #f]
                         [#:out out any/c #f]
                         [#:tag-in2 tag2 any/c #f]
                         [#:tag-out tag4 any/c #f])
                        diagram?])]{

 Make a diagram that draws the given gate, each gate facing
 to the right. @racket[or-gate] and @racket[and-gate]
 are three units square, and designed to take in up to three input wires.
 @racket[buffer] is roughly the same size, but is designed to take only one
 input. If @racket[n1], @racket[n2], or @racket[n3] are not @racket[#f],
 then the upper, middle, or lower input (respectively) are negated.
 @racket[out] does the same for the output. If @racket[tag1], @racket[tag2]
 or @racket[tag3] a wire is drawn for those inputs, and its endpoint
 is named by the given tag. @racket[tag4] does the same for the output.

 The drawn gate is centered at the current location.

 @examples[#:eval diag
           (define layer-1-x 6)
           (define layer-2-x 3)

           (define input-1 (tag-location 'input-1 0 0))
           (define input-2 (tag-location 'input-2 0 2))
           (define top-gate
             (after
              (move-down 1) (move-right layer-1-x)
              (and-gate #:out #t
                        #:tag-out 'and-out
                        #:tag-in1 'and-A
                        #:tag-in3 'and-B)))
           (define lower-gate
             (after
              (move-down 5) (move-right layer-1-x)
              (or-gate #:tag-out 'or-out
                       #:tag-in1 'or-A
                       #:tag-in3 'or-B)))
           (define last-gate
             (after
              (move-down 1) (move-right layer-2-x)
              (and-gate #:tag-in1 'and-in
                        #:tag-in3 'or-in
                        #:tag-out 'result)))
           (define (connect-input input g1 g2 split-point)
             (after
              (move-to-tag input)
              (line-right split-point)
              (split
               (line-to-tag g1)
               (line-to-tag g2 #:h-first #f))))
           (define xor
             (after
              input-1 input-2
              (move-to-tag 'input-1)
              (save top-gate) (save lower-gate)
              (move-to-tag 'and-out) last-gate
              (connect-input 'input-1 'and-A 'or-A 3)
              (connect-input 'input-2 'and-B 'or-B 2)
              (line-between 'and-out 'and-in)
              (line-between 'or-out 'or-in)
              (move-to-tag 'result)
              (line-right 1)))
           (scale xor 2)
           (scale (after xor unit-grid) 2)]

}

