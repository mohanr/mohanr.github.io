---
layout: post
title: Game Of Life in Racket
published: false
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

(: compare ((Pairof Integer Integer) (Pairof Integer Integer)   -> Integer))
(define ( compare pair1 pair2)
    (match (= (cdr pair1) (car pair2))
     [0  (= (cdr pair1) (car pair2))]
))

{% endhighlight %}
