---
layout: post
title: Ray Tracer in Racket - Part I
published: true
---

This is my attempt to code a Ray Tracer in Racket. Details are forthcoming.

The [Book Series]([http://arxiv.org/pdf/2302.13971.pdf](https://raytracing.github.io)) is cited
by many who code ray tracers using any languages they are familiar with. Recently some OCaml developers
showed how to code simple ray tracers. These videos inspired me to port their code to Racket.

1. I post code here as I develop it. It will eventually be committed to Git.
2. I also learn Racket as I code this. So I will add some explanation of the functional
   programming paradigm that Racket is based on.


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

# The _Pixel_ module

{% highlight racket %}

(module Pixel racket
(provide create write-pixel)
(define (create r g b)
  
  (let ((r (arithmetic-shift 16 ( bitwise-and r 255 )))
        (g  (arithmetic-shift 8 ( bitwise-and g 255 ) ))
        (b  (bitwise-and b 255 ))
        (x  ( bitwise-ior r ( bitwise-ior g b))))
     x)
) 
(define (write-pixel t)
  
  (format "~a ~a ~a" (bitwise-ior t 16)
                     (bitwise-ior 8 (bitwise-and t 255))
                     (bitwise-and t 255))
  
  )
)
{% endhighlight %}

