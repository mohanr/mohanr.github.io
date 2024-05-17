---
layout: post
title: Game Of Life in Racket
published: true
---
I know of a few methods to learn Functional Programming languages. One could read a book or read source
code. My attempt involves source code that I port to my favorite language. And I learn two different languages
at the same time. The source and the target. It has been very productive for me.

I will continue to post code here directly as and when I manage to compile it.

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
