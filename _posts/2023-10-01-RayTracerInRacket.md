---
layout: post
title: Ray Tracer in Racket - Part I
published: true
---

This is my attempt to code a Ray Tracer in Racket. Details are forthcoming.

# Development environment

The IDE is the venerable emacs. 

# _Vec3d_ module

{% highlight racket %}
#lang racket

  (require rackunit (submod  "rays.rkt" Vec3d ))
  (check-equal? (vadd '#(1 2 3)  '#(0 1 2)) '#(1 3 5))
  (check-equal? (vminus '#(1 2 3)  '#(0 1 2)) '#(1 1 1))

{% endhighlight %}


# _rackunit_ tests

{% highlight racket %}
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

# Reading and writing files

{% highlight racket %}

#lang racket

(require rackunit (submod  "rays.rkt" Vec3d )(submod  "rays.rkt" IO))
  (check-equal? (vadd '#(1 2 3)  '#(0 1 2)) '#(1 3 5))
  (check-equal? (vminus '#(1 2 3)  '#(0 1 2)) '#(1 1 1))
  (with-handlers ([exn:fail?
                   (lambda (v)
                     (displayln "File operation problem")
                     (displayln (exn-message v) ))])
  (write-file))
  (print(read-file
           ))
  (check-equal? (read-file) "P3")
{% endhighlight %}

# _rackunit_ tests

{% highlight racket %}

 (with-handlers ([exn:fail?
                   (lambda (v)
                     (displayln "File operation problem")
                     (displayln (exn-message v) ))])
  (write-file))
  (print(read-file
           ))
  (check-equal? (read-file) "P3")

{% endhighlight %}

