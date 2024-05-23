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
