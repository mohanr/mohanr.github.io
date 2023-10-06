---
layout: post
title: Ray Tracer in Racket - Part I
published: true
---

This is my attempt to code a Ray Tracer in Racket. Details are forthcoming.

# Development environment

The IDE is the venerable emacs. 

# _Vec3d_ module

{% highlight Scheme %}
#lang racket

  (require rackunit (submod  "rays.rkt" Vec3d ))
  (check-equal? (vadd '#(1 2 3)  '#(0 1 2)) '#(1 3 5))
  (check-equal? (vminus '#(1 2 3)  '#(0 1 2)) '#(1 1 1))

{% endhighlight %}


# _rackunit_ tests

{% highlight Scheme %}
#lang racket
(module Vec3d racket
  (provide vadd vminus)
  (define ( vadd  v v1)
  (vector-map + v v1))
  (define ( vminus  v v1)
    (vector-map - v v1))
  (define ( vmult  v v1)
    (vector-map * v v1))
  (define ( vdiv  v v1)
    (vector-map / v v1))
)


{% endhighlight %}


