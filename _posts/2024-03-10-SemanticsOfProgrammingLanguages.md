---
layout: post
title: Semantics of Simple Programming Languages
published: true
---
I am reading about _Semantics of Programming Languages_ and here is an attempt to code Racket
to parse and interpret toy languages. More details will be added.

# Semantics of a simple languge

I use typed Racket and my IDE is Doom Emacs. But I may also code OCaml. The code here is ported from SML/NJ.

{% highlight racket %}

#lang typed/racket


(require racket/list)
(provide updates Some None)

(define-type Loc String)
(define-type Store (U Loc Integer))

(: operator : Char -> (Operator Char))
(define (operator c )
  (match c
     ['+' Plus]
     ['>=' GTEQ]))

(struct Plus())
(struct GTEQ())
(struct Expr ())
(define-type Value (U Integer Char))
(define-type If (U Expr Expr Expr))
(define-type (Op c) (U Expr (Operator c) Expr))
(define-type Assign (U Loc Expr))
(struct Deref())
(struct Seq())
(struct While())
(struct Skip())
(define-type (Operator c) (U Plus GTEQ))

(define-type (Expression c)
 (U Value

    (Op c)

    If

    Assign

    (U Deref Loc )

    (U Seq Expr Expr)

    (U While Expr Expr)

    Skip)
  )

(struct None ()
    #:transparent)
(struct (i) Some ([v : i])
    #:transparent)
(define-type (Opt a) (U None (Some a)))

(: lookup  ((Listof Number)  Number -> Number))
(define (lookup ls l)
  (match ls
    ['()  0]
    [(cons (cons (== l) n) ls) n]
    [(cons _ ls) (lookup ls l)]))


(: updates ((Listof Any) Number ->
                                 (Opt (Listof Any))))
(define (updates ls l)
  (match ls
    ['()   (None)]
    [(cons (cons (== l) n) ls) (Some (append ls (list (list l n))))]
    [(cons _ ls) (updates ls l)]))

{% endhighlight %} 


{% highlight racket %} 

#lang typed/racket

(require typed/rackunit "l1.rkt")


 (print( check-equal? (updates '((2,1)) 3) (None) "Test successfull"))
 (print( check-equal? (updates '((3,1)) 3) (Some '((3 (,1)))) "Test successfull"))
 ( check-equal? (updates '((3,1)) 3) (Some '((3 (,1)))) "Test successfull")

{% endhighlight %} 
