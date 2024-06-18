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
{% highlight racket %}
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
 (require  "gameset.rkt" "coord.rkt" "gamemap.rkt")
 (require typed/racket/gui)

 (provide render step torus erem linspcm background )

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

 (: torus : ( Coord -> (-> Coord
                         Coord)))
 (define (torus wh )
    (lambda ([ab : Coord])
      (cons (erem (car ab) (car wh)) (erem (cdr ab) (cdr wh)))
    )
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


( : neigh : (Coord -> Coord )
             (Pairof Integer Integer) -> (Listof (Pairof Integer Integer)))
(define (neigh topo ab)
  (let* ([a (car ab)]
         [b (cdr ab)]
         [a-1 (sub1 (car ab))]
         [a+1 (add1 (car ab))]
         [b-1 (sub1 (cdr ab))]
         [b+1 (add1 (cdr ab))]
         [neighbours
          `((,a-1 . ,b)
                (,a+1 . ,b)
                (,a-1 . ,b-1)
                (,a-1 . ,b+1)
                (,a . ,b-1)
                (,a . ,b+1)
                (,a+1 . ,b-1)
                (,a+1 . ,b+1))])
  (map topo  neighbours ))
  )



(: background : ( (Instance DC<%>) Integer (Pairof Integer Integer) ->
                         t  ))
(define (background dc step nm)
(let* ([float-sum (/ (exact->inexact (+ step (cdr nm) (car nm))) 10.0)]
       [sin-value (* 24.0 (sin float-sum))]
       [k (truncate sin-value)])
  (cond [(> k  0)
        (begin
          (printf "Drawing at ~a , ~a~n" (car nm ) (cdr nm))
          (define gray-value (make-object color% 1 2 3))
          (send dc set-pen gray-value 1 'solid)
          (send dc set-text-foreground gray-value)
          (send dc draw-text "●"  (cdr nm) (car nm))
        )
          (Hcompose (Empty) (Empty) (dim 50  50))
        ]
        [else
        (begin
          (send dc set-pen "red" 1 'solid)
          (send dc draw-rectangle (car nm) (cdr nm) 4 4)
        )
          (Hcompose (Empty) (Empty) (dim 5 5))
        ]))
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

(: step : ( (-> Coord Coord )(Listof (Pairof Integer Integer)) ->
           (Setof Coord)))
(define (step topo life)
(: nlive : ((Pairof Integer Integer) ->
           Integer))
  (define  (nlive pt)
    (printf " nlive ~a  " pt )
    ( let* ([neighbours (neigh topo pt )])
          (length
           ( filter (lambda (neighbour) (set-member? life neighbour )) neighbours )
          )
    )
  )

(: f1 : ( Coord (Immutable-HashTable Coord Integer)->  (Immutable-HashTable Coord Integer)))
  (define  (f1 pt acc )
    ;; (printf "pair ~a~n " pt )
    ;; (for/hash ([(k v) (in-hash acc)]) (values (printf " Key ~a" k) (printf "Value ~a~n" v)))
    ( let* ([neighbour (cons pt  (neigh topo pt))])
      ;; (printf "Neighbour ~a~n " neighbour )

      (foldl
         (lambda ([pt1 : Coord ][acc : (Immutable-HashTable Coord Integer)])
         (begin
           (match pt1
           [ (cons -1  -1) acc]
           [ (cons x1 x2)
                    #:when (mem acc pt1)
                    acc ]
           [ (cons x1 x2)
               (let* ([n ( nlive pt1 )])
                      (hash-set acc pt1
                        (if (and (or (= n 3) (= n 2)) ( set-member? life pt1))
                        0
                        1)
                        ))])))
         acc neighbour )
    )
  )
(define (eliminate)
  (define acc  : (Immutable-HashTable Coord Integer) (make-immutable-hasheq)) ; Initialize an empty mutable hash table
  (for/fold ([acc : (Immutable-HashTable Coord Integer) acc]) ; Use for/fold to accumulate results
            ([pair life])
    (begin
      (let ([accu (f1 pair acc)])
      accu)))
); Return acc after folding

 (preimg (lambda ([ x : Number] )( = x 0))  (eliminate))

)


(: render : (  (Instance DC<%>) Integer Integer Integer (HashTable Coord Integer) ->
                         t))
(define (render dc w h step life )
  (define lightred (make-object color% 255 10 10))
  (define gray (make-object color% 128 128 128))

  (tabulate w (- h  1) (lambda (x y)
     (let* ([pt  (cons x  y)])
      (if (mem life pt )
      (begin
        (send dc draw-text "●" x y )
        (Hcompose (Empty) (Empty) (dim 50  50)))
      (begin
        (background dc step pt))
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
 (require "coord.rkt" "gameset.rkt")
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


(: renderer : ( (Instance DC<%>) -> Void))
(define (renderer dc)
(let* ([life  (step (torus (cons 300 300)) lifeseed)]
       [hash : (Immutable-HashTable Coord Integer) (make-immutable-hasheq)])
      (begin
      (printf "Set count ~a\n" (set-count life))
      (for/fold ([hash : (Immutable-HashTable Coord Integer) hash]) ; Initial accumulator is hash
                ([pair (in-set life)])
        (begin
          (hash-set hash pair 0)

          (for/hash ([(k v) (in-hash hash)])
            (begin
              (printf " Key: ~a, Value: ~a~n" k v)
              (values k  v)))
          hash
        )
      )
      )

    (render dc 300 300 1 hash )
    (void)
   )
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
      (send this suspend-flush)
      (send this resume-flush)
      (renderer dc)
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
(start)
{% endhighlight %}


# game-test.rkt
A simple incomplete test shown here as an example.

{% highlight racket %}
( check-equal? (preimg (lambda (x) (= x 0))
                       (make-immutable-hash '([3 . 1] [1 . 2] [10 . 0]))) (set 10) "Test unsuccessfull")
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
