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
#lang typed/racket
(provide compare)
(require "datatypemacro.rkt")

(define-datatype t
  (U Integer Integer )
)

(: compare ((Pairof Integer Integer) (Pairof Integer Integer)   -> Boolean))
(define ( compare pair1 pair2)
    (let ([r (= (cdr pair1) (car pair2))])
    (match r
     [#t  (= (cdr pair1) (car pair2))]
     [#f r]
     )
))

(: equal ((Pairof Integer Integer) (Pairof Integer Integer)   -> Boolean))
(define ( equal pair1 pair2)
    (let ([r (= (cdr pair1) (cdr pair2))])
    (match r
     [#t  (= (car pair1) (car pair2))]
     [#f r]
     )
))

{% endhighlight %}

# gameset.rkt

{% highlight racket %}

#lang typed/racket
(provide mapper oflist)
(require racket/set)
(require "game.rkt")


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
{% endhighlight %}

# gamemap.rkt

{% highlight racket %}

#lang typed/racket/base
(provide preimg)
(require racket/set)
(require racket/hash)


(: preimg ( ( Integer -> Boolean  )(HashTable Integer Integer) -> (Setof Integer)))
(define (preimg p  m )
    ;; (let ([ s :(Setof Integer)  (list->set '())])

    (let ([ s :(Setof Integer)  (set)])
    (hash-for-each m
                   (lambda ([k :  Integer]
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

{% highlight racket %}
#lang typed/racket

(require typed/racket/gui
         )


(module Shape typed/racket

 (provide erem )

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

 (: torus ( (Pairof Integer Integer) (Pairof Integer Integer) ->
                         (Pairof Real Real)))
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


( : neigh : ((Pairof Integer Integer) (Pairof Integer Integer)
                -> (Pairof Integer Integer))
             (Pairof Integer Integer) -> (Listof (Pairof Integer Integer)))
(define (neigh topo ab)
  (let* ([a (car ab)]
         [b (cdr ab)]
         [a-1 (sub1 a)]
         [a+1 (add1 a)]
         [b-1 (sub1 b)]
         [b+1 (add1 b)])
    (map (lambda ([tuple : (Pairof Integer Integer)])
           ( topo tuple ab))
          `((,a-1 . ,b)
                (,a+1 . ,b)
                (,a-1 . ,b-1)
                (,a-1 . ,b+1)
                (,a . ,b-1)
                (,a . ,b+1)
                (,a+1 . ,b-1)
                (,a+1 . ,b+1))  )))
)


(: background : (Integer (Pairof Integer Integer) ->
                         (Listof (Pairof Integer Integer)) ))
(define (background step nm)
  (let*  ([k  (* 24.  (sin (/ (+ (+ step (cdr nm))  (car nm))  10.))) ]
         [q  (quotient ( exact-round k) 10)])
  (cond [(> q  0) '(( 50 . 50) ( 100 . 100))]
        [else '(( 50 . 50) ( 100 . 100))]
        ))
)

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

(: draw-coord : ( -> (Listof Integer)))
(define (draw-coord )
  (let* ([bc  (background 1 ( cons 1  2))]
         [f (first bc )]
         [l (last bc)]
         [x (car f )]
         [y (cdr f )]
         [x1 (car l)]
         [y1 (cdr l)])
  (list x x1 y y1 )
)
)

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
         (let* ([dco  (draw-coord)] )
         (send dc draw-rectangle (max 0.0 (exact->inexact (car dco)))
                                 (max 0.0 (exact->inexact (cadr dco)))
                                 (max 0.0 (exact->inexact (caddr dco)))
                                 (max 0.0 (exact->inexact (cadddr dco))))
             (send dc draw-rectangle  5 5 5 5 )
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
)
(define (start)
  (define timer (new timer%
                     [notify-callback handle-on-timer]
                     [interval  1000])) ; milliseconds
  (send timer start 1)
  )
(start)
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
(require "datatypemacro.rkt")

(struct dim ([width : Integer] [height : Integer]))

(define-datatype t
    ( Hcompose (Pairof t  t ) dim)
    ( Vcompose (Pairof t  t ) dim)
)


(: width : ( t  ->  Integer ))
(define  (  width datatype )
   (match datatype
    [ (Hcompose (cons t t) (cons w _))  w]
    [ (Vcompose (cons t t) (cons w _))  w]
   )
)


(: height : ( t  ->  Integer ))
(define  ( height datatype )
   (match datatype
    [ (Hcompose (cons t t) (cons _ h))  h]
    [ (Vcompose (cons t t) (cons _ h))  h]
   )
)


(provide <#> )

(: <#> : ( t t -> t ))
(define ( <#> t1 t2)
  (match (list t1 t2)
    [ (cons _ empty) t1]
    [ (cons empty _) t2]
    [ _ (let* ([w  (+ (width t1)  (width t2))]
               [ h  (max (height t1) (height t2))])
                (Hcompose (cons t1  t2) (dim w h))
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
