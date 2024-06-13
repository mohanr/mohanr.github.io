---
layout: post
title: Game Of Life in Racket
published: true
---
I know of a few methods to learn Functional Programming languages. One could read a book or read source
code. My attempt involves source code that I port to my favorite language. And I learn two different languages
at the same time. The source and the target. It has been very productive for me.

I will continue to post code here directly as and when I manage to compile it. This code is directly
ported from OCaml and I will add the link to the source once it is finished.

# coord.rkt

{% highlight racket %}

#lang typed/racket/base
(provide preimg)
(require racket/set)
(require racket/hash)

 (require "coord.rkt")

(: preimg ( ( Integer -> Boolean  )(HashTable Coord Integer) -> (Setof Coord)))
(define (preimg p  m )
    ;; (let ([ s :(Setof Integer)  (list->set '())])

    (let ([ s :(Setof Coord)  (set)])
    (hash-for-each m
                   (lambda ([k :  Coord]
                            [v :  Integer]
                            )
                     (when (p v)
                       (set! s (set-add s k))
                       (print s)
                       ) )  )
    s
    )
  )
{% endhighlight %}

# gameset.rkt

{% highlight racket %}

#lang typed/racket
(provide mapper oflist mem)
(require racket/set)
(require "coord.rkt")


(: mapper ((Integer -> Integer) (Setof Integer) ->
                                (Setof Integer)))
(define (mapper f s)
(list->set (map f (set->list s))))
  ;; (set-map s f))
  ;;

(: oflist ( (Listof Integer) -> (Setof Integer)))
(define (oflist lst )
(foldl (lambda ([x :  Integer] [s : (Setof Integer)]) ( set-add  s x ))
          (list->set '()) lst))

(: coord-comparator ( (Pairof Integer Integer ) (Pairof Integer Integer )-> Integer))
(define (coord-comparator a b)
    (cond
      [(equal a b) 0]
      [(false? (compare a b)) -1]
      [else 1]))

(define empty (make-hasheq))


(: add (  (HashTable (Pairof Integer Integer  ) Integer)
          (Pairof Integer Integer )  ->
          (Immutable-HashTable (Pairof Integer Integer ) Integer)))
(define (add s c) (hash-set s c 1))

(: mem (  (HashTable (Pairof Integer Integer  ) Integer)
          (Pairof Integer Integer )  ->
           Boolean))
(define (mem s c) (hash-has-key? s c))
{% endhighlight %}

# gamemap.rkt

{% highlight racket %}

#lang typed/racket/base
(provide preimg)
(require racket/set)
(require racket/hash)

 (require "coord.rkt")

(: preimg ( ( Integer -> Boolean  )(HashTable Coord Integer) -> (Setof Coord)))
(define (preimg p  m )
    ;; (let ([ s :(Setof Integer)  (list->set '())])

    (let ([ s :(Setof Coord)  (set)])
    (hash-for-each m
                   (lambda ([k :  Coord]
                            [v :  Integer]
                            )
                     (when (p v)
                       (set! s (set-add s k))
                       (print s)
                       ) )  )
    s
    )
  )
{% endhighlight %}

# game.rkt

#lang typed/racket


(require typed/rackunit)
(module I typed/racket
(require "datatypemacro.rkt" "coord.rkt" "gameset.rkt")
(provide t Hcompose width height Empty dim)

(struct dim ([width : Integer] [height : Integer]))
(define-datatype t
    ( Hcompose t t dim)
    ( Vcompose t t dim)
     Empty
)


(: width : ( t  ->  Integer ))
(define  (  width datatype )
   (match datatype
     [(Hcompose left right  d) (dim-width d)]
     [(Vcompose left right  d) (dim-width d )]
     [(Empty) 0]))


(: height : ( t  ->  Integer ))
(define  ( height datatype )
   (match datatype
     [(Hcompose left right  d) (dim-height d)]
     [(Vcompose left right  d) (dim-height d )]
     [(Empty) 0]))

(provide <#> <-> )

(: <#> : ( t t -> t ))
(define ( <#> t1 t2)
  (match (list t1 t2)
    [ (cons _ Empty) t1]
    [ (cons Empty _) t2]
    [ _ (let* ([w  (+ (width t1)  (width t2))]
               [ h  (max (height t1) (height t2))])
                (Hcompose t1  t2 (dim w h))
                )
        ]
    )
)

(: <-> : ( t t -> t ))
(define ( <-> t1 t2)
  (match (list t1 t2)
    [ (cons _ Empty) t1]
    [ (cons Empty _) t2]
    [ _ (let* ([w  (max (width t1) (width t2))]
               [h  (+ (height t1)  (height t2))])
               (Vcompose t1 t2 (dim w h))
                )
        ]
    )
)
)


(module Shape typed/racket

 (require (submod ".." I))
 (require threading)
 (require racket/set)
 (require  "gameset.rkt" "coord.rkt" "gamemap.rkt")

 (provide step torus erem linspcm background )

 (: erem (  Integer Integer ->
                          Integer))
 (define (erem x y)
   (modulo (+ (modulo x y) y)  y)
   )

 (: square ( (Pairof Integer Integer) (Pairof Integer Integer) ->
                         (Pairof Real Real)))
 (define (square wh ab)
  (match  ab
    [(cons  x  y)
          (cond
            [(or (< (car ab)  0) (>= (car ab)  (car wh)) (< (cdr ab)  0) (>= (cdr ab) (cdr wh)))
            (cons -1 -1)]
            [else  ab])]
    ))

 (: torus ( Coord Coord ->
                         Coord))
 (define (torus wh ab)
    (cons (erem (car ab) (car wh)) (erem (cdr ab) (cdr wh)))
 )

 (: mobius ( (Pairof Integer Integer) (Pairof Integer Integer) ->
                         (Pairof Real Real)))
 (define (mobius wh ab)
 (match  ab
    [(cons  x  y)
          (cond
            [(or (< (car ab)  0) (>= (car ab) (car wh)))
            (cons (erem (car ab) (car wh)) (- (- (cdr wh) (cdr ab )) 1 ))]
            [else  ab])]
    ))


( : neigh : (Coord
             (Pairof Integer Integer) -> (Listof (Pairof Integer Integer))))
(define (neigh topo ab)
  (let* ([a (car topo)]
         [b (cdr topo)]
         [a-1 (sub1 (car topo))]
         [a+1 (add1 (car topo))]
         [b-1 (sub1 (cdr topo))]
         [b+1 (add1 (cdr topo))])
          `((,a-1 . ,b)
                (,a+1 . ,b)
                (,a-1 . ,b-1)
                (,a-1 . ,b+1)
                (,a . ,b-1)
                (,a . ,b+1)
                (,a+1 . ,b-1)
                (,a+1 . ,b+1))  ))


(: background : (Integer (Pairof Integer Integer) ->
                         t  ))
(define (background step nm)
  (let*  ([k  (* 24.  (sin (/ (+ (+ step (cdr nm))  (car nm))  10.))) ]
         [q  (quotient ( exact-round k) 10)])
  (cond [(> q  0) (Hcompose (Empty) (Empty) (dim 50  50))]
        [else (Hcompose (Empty) (Empty) (dim 5 5))]
        ))
)


(define-syntax-rule (@) append)

(: linspcm : (t Integer Integer (Integer -> t) (t t -> t) -> t))
(define (linspcm z x n f op)
  (match n
    [0 z]
    [1 (f x)]
    [_ (let* ([m (quotient n 2)])
         (op (linspcm z x m f op)
             (linspcm z (+ x m) (- n m) f op)))]))

(: tabulate : ( Integer Integer
              (Integer Integer ->  t) -> t))
(define (tabulate m n f)
  (let* ([m (max m 0)]
         [n (max n 0)])
  (linspcm (Empty)  0 n (lambda (y)
                                  (linspcm (Empty)  0 m (lambda (x)
                                                               (f x y))
                            <#>))
         <->)
    )
)

(: step : (  Coord (Listof (Pairof Integer Integer)) ->
           (Setof Coord)))
(define (step topo life)
(: nlive : ((Pairof Integer Integer) ->
           Integer))
  (define  (nlive pt)
    ( let* ([neighbours (neigh topo pt )])
          (length
           ( filter (lambda (neighbour) (set-member? life neighbour )) neighbours )
          )
    )
  )

(: f1 : ( Coord (HashTable Coord Integer)->  (HashTable Coord Integer)))
  (define  (f1 pt acc )
    ( let* ([neighbour (cons pt  (neigh topo pt))])
      (foldl
         (lambda ([pt : Coord ][acc : (HashTable Coord Integer)])
           (match pt
           [ (cons -1  -1)  acc]
           [ pt (if (mem acc pt) acc acc) ]
           [ pt
               (let* ([n ( nlive pt )])
                      (hash-set acc pt
                        (if (and (or (= n 3) (= n 2)) ( set-member? life pt))
                        0
                        1)
                        ))]))
         acc neighbour )
    )
  )

(: eliminate : -> ( HashTable Coord Integer))
(define (eliminate)
  (for/fold ([acc : (HashTable Coord Integer) (make-hasheq)])  ; Initialize an empty mutable hash table
          ([pair : Coord life])  ; Iterate over each pair in pairs
    (f1 pair acc))
 )

 (preimg (lambda ([ x : Number] )( = x 0))  (eliminate))

)


(: render : ( Integer Integer Integer (HashTable Coord Integer) ->
                         t ))
(define (render w h step life )
  (tabulate w (- h  1) (lambda (x y)
     (let* ([pt  (cons x  y)])
      (if (mem life pt )
      (background step pt)
      (background step pt)
      )
     )
   )
  )
)
)


(module Gui typed/racket

(require typed/racket/gui)
(require (submod ".." Shape))
 (require (submod ".." I))
 (require "coord.rkt")
(provide start)

(define black-brush (new brush% [color "black"]))
(define dc #f)

(define black-pen
  (new pen%
       [color  "red"]
       [width  10]
       [style  'solid]
       [cap    'round]
       [join   'round]
       [stipple #f]))



(define game-window%
  (class frame%
    (super-new)
    ;; (define/augment (on-close) ;; stop timer
    ;; )
  )
)

(define lifeseed
             '((2 . 1) (3 . 2) (1 . 3) (2 . 3) (3 . 3 )))

(: generate : (Setof Coord))
(define generate
  (step (torus (cons 100 100) (cons 0 0)) lifeseed )
)

(: draw-coord : ( -> (Listof Integer)))
(define (draw-coord )
  (let* ([bc : t (background 1 ( cons 1  2))])
           (list (width bc) (height bc) (width bc) (height bc)))
)

;; The GUI frame showing our game
(define the-frame (new game-window% [label "Game of Life"] [width 800] [height 450]))
(define game-canvas
  (class canvas%
    (super-new)
    ( define/override (on-paint)
      (define dc (send this get-dc))
      (send dc set-font (make-font #:size 24 #:face "Fira Code"))
      (send dc set-pen "black" 0 'solid)
      (send dc set-smoothing 'unsmoothed)
      (send dc set-brush "black" 'transparent)
      (send dc set-pen black-pen)
      (let* ([dco  (draw-coord)] )
         (send dc draw-rectangle (max 0.0 (exact->inexact (car dco)))
                                 (max 0.0 (exact->inexact (cadr dco)))
                                 (max 0.0 (exact->inexact (caddr dco)))
                                 (max 0.0 (exact->inexact (cadddr dco))))
      )
      (send this suspend-flush)
      (send this resume-flush)
      )
      (send the-frame show #t)
      (send the-frame focus)
  )
)

(define game-console
  (new game-canvas
       [parent the-frame]
      ))

(define (handle-on-timer)
    (send game-console on-paint)
    (send game-console refresh)
)
(define (start)
  (define timer (new timer%
                     [notify-callback handle-on-timer]
                     [interval  1000])) ; milliseconds
  (send timer start 1)
  )
)


( require 'Gui)
(require 'Shape)
;; (start)

(: fn1 ( Integer -> Integer))
(define (fn1 x )
  (displayln x)
  (+ x 1)
 )
{% endhighlight %}


# game-test.rkt

{% highlight racket %}

#lang typed/racket
(require "gameset.rkt" "gamemap.rkt")

(require racket/match)
(require racket/hash)
(require typed/rackunit)

;; Unused
(: fn ( Integer -> Integer))
(define (fn obj)
  (+ obj 1)
 )

( check-equal? (mapper fn (set 1 2)) (set 2 3) "Test unsuccessfull")
( check-equal? (oflist '(2 3)) (set 2 3) "Test unsuccessfull")
( check-equal? (preimg (lambda (x) (= x 0))
                       (make-hash '([3 . 1] [1 . 2] [10 . 0]))) (set 10) "Test unsuccessfull")
{% endhighlight %}

# Racket GUI
Almost all the code shown above except the UI code is directly ported from OCaml. See the references.
Almost all the code shown below is from a template. It is just Racket's way of creating a GUI.

{% highlight racket %}

(define black-brush (new brush% [color "black"]))
(define dc #f)

(define black-pen
  (new pen%
       [color  "red"]
       [width  10]
       [style  'solid]
       [cap    'round]
       [join   'round]
       [stipple #f]))



(define game-window%
  (class frame%
    (super-new)
       ))

;; The GUI frame showing our game
(define the-frame (new game-window% [label "Game of Life"] [width 800] [height 450]))
(define game-canvas
  (class canvas%
    (super-new)
    ( define/override (on-paint)
      (define dc (send this get-dc))
       (let ((dc (send this get-dc)))
         (send dc set-font (make-font #:size 24 #:face "Fira Code"))
         (send dc set-pen "black" 0 'solid)
         (send dc set-smoothing 'unsmoothed)
         (send dc set-brush "black" 'transparent))
         (send dc set-pen black-pen)
         (send dc draw-rectangle 50 50 100 100)
         (send this suspend-flush)
         (send this resume-flush)
      )
      (send the-frame show #t)
      (send the-frame focus)
)
)

(define game-console
  (new game-canvas
       [parent the-frame]
      ))

(define (handle-on-timer)
    (send game-console on-paint)
)
(define (start)
  (define timer (new timer%
                     [notify-callback handle-on-timer]
                     [interval  1000])) ; milliseconds
  (send timer start 1)
  )
(start)

{% endhighlight %}

# Experiment with Syntax-rule

the syntax-rule I needed is minimal. I also learnt that the infix between two dots (. (@) .) becomes a prefix.

{% highlight racket %}

(define-syntax-rule (@) append)

;; (define-syntax-rule  (linspcm z (@) x n f)
(: linspcm : ((Listof Integer) Integer Integer
              (Integer ->  Integer) -> (Listof Integer)))
(define  (linspcm z  x n f)

  (match n
    [0 z]
    [1 (list (f x))]
    [_ (let* ([m (quotient n 2)])
         (displayln n)
         ((linspcm z    x m f) . (@) .
         (linspcm z  (+ x m) (- n m) f))
      )
    ]
  )
)
{% endhighlight %}

# Ported code from _notty.ml_ which is a OCaml library

At this stage just to render a UI I have to port some deep part of an OCaml library
to Racket. This wasn't envisaged.

{% highlight racket %}

(module I typed/racket
(require "datatypemacro.rkt" "coord.rkt" "gameset.rkt")
(provide t Hcompose width height Empty dim)

(struct dim ([width : Integer] [height : Integer]))
(define-datatype t
    ( Hcompose t t dim)
    ( Vcompose t t dim)
     Empty
)


(: width : ( t  ->  Integer ))
(define  (  width datatype )
   (match datatype
     [(Hcompose left right  d) (dim-width d)]
     [(Vcompose left right  d) (dim-width d )]
     [(Empty) 0]))


(: height : ( t  ->  Integer ))
(define  ( height datatype )
   (match datatype
     [(Hcompose left right  d) (dim-height d)]
     [(Vcompose left right  d) (dim-height d )]
     [(Empty) 0]))

(provide <#> <-> )

(: <#> : ( t t -> t ))
(define ( <#> t1 t2)
  (match (list t1 t2)
    [ (cons _ Empty) t1]
    [ (cons Empty _) t2]
    [ _ (let* ([w  (+ (width t1)  (width t2))]
               [ h  (max (height t1) (height t2))])
                (Hcompose t1  t2 (dim w h))
                )
        ]
    )
)

(: <-> : ( t t -> t ))
(define ( <-> t1 t2)
  (match (list t1 t2)
    [ (cons _ Empty) t1]
    [ (cons Empty _) t2]
    [ _ (let* ([w  (max (width t1) (width t2))]
               [h  (+ (height t1)  (height t2))])
               (Vcompose t1 t2 (dim w h))
                )
        ]
    )
)
)

{% endhighlight %}


_References
1. https://github.com/pqwy/notty ( This isn't utilized because my Racket code is just a canvas, Not
   a terminal )
2. https://github.com/mfelleisen/7GUI is the GUI referenced code.
