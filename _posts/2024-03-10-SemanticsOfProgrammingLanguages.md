---
layout: post
title: Semantics of Simple Programming Languages
published: true
---
I am reading about _Semantics of Programming Languages_ and here is an attempt to code Racket
to parse and interpret toy languages. More details will be added.
The code here is not the final version but it compiles. It shows the progress made as I learn the nuances of
Racket and types.

# Semantics of a simple language

I use typed Racket and my IDE is Doom Emacs. But I may also code OCaml. The code here is ported from SML/NJ.

# Pre-requisites

I have mentioned what was useful for me when I attempted to learn the subject.

1. Knowledge of SML because some key books use OCaml or SML.
2. Ability to understand basic proof techniques. This book helped me even though I haven't finished it.
3. Professor David Solow's lectures are available and he clearly explains the steps involved in writing and
   reading condensed proofs. Highly recommended.

![image-title-here](../images/DavidSolor.png){:class="img-responsive"}

## Attempt 1

1. This does not use Abstract Data Types.
2. This makes the pattern matchers hard to implement and reason about.

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

## Attempt 2
I had to use a macro and create a proper ADT(Abstract Data Type ) to proceed. The previous
code was too verbose. The ADT makes the pattern matcher easier to implement.
The current version is in my Git.



{% highlight racket %}

#lang typed/racket/base

(provide Boolean printexpr updates None Some Seq Deref Assign)

;;  Macro by Alexis King 
 (require (for-syntax racket/base
                     racket/sequence
                     racket/syntax
                     syntax/parse
                     syntax/stx)
         racket/match)
(begin-for-syntax
  (define-syntax-class type
    (pattern name:id
             #:attr [param 1] '()
             #:attr [field-id 1] '())
    (pattern (name:id param ...+)
             #:attr [field-id 1] (generate-temporaries #'(param ...)))))

(define-syntax define-datatype
  (syntax-parser
    [(_ type-name:type data-constructor:type ...)

     (define/with-syntax [data-type ...]
       (for/list ([name (in-syntax #'(data-constructor.name ...))])
         (if (stx-null? #'(type-name.param ...))
             name
             #`(#,name type-name.param ...))))

     #'(begin
         (struct (type-name.param ...) data-constructor.name
           ([data-constructor.field-id : data-constructor.param] ...)) ...
         (define-type type-name (U data-type ...)))]))
;;  End of Macro by Alexis King 

(define-type Loc String)
(struct Plus())
(struct GTEQ())
(define-type (Operator c) (U Plus GTEQ))

(: operator : (U Plus GTEQ)->  Char)
(define (operator c )
  (match c
    ['Plus #\+]
    ['GTEQ #\=]))

(define-datatype Expr
  ( Op  Expr (U Plus GTEQ) Expr)
  ( If  Expr Expr Expr)
  ( Assign  Loc Expr)
  ( Deref Loc )
  ( Seq Expr Expr)
  ( While Expr Expr)
    Skip
)


(: printexpr (Expr -> Void ))
(define (printexpr expr)
  (match expr
    [(Deref l)  (printf "( ~a ~a ~n)" "!"  l)]
    [( Op e1 operate e2  )
            (printf "( ~a ~a ~a~n)"  (printexpr e1)  (operator operate)
            (printexpr e2 ))]
    [( If e1 e2 e3  )
            (printf "( ~a ~a ~a~n)"  (printexpr e1)  (printexpr e2 )(printexpr e2 ))]
    [ (Assign l e ) =  (printf "~a := ~a" l (printexpr e ))]
    [ (Skip) ( printf "skip")]
    [ (Seq e1 e2 )   (printf "~a ;  ~a" (printexpr e1 )
                                      (printexpr e2))]
    [ (While  e1 e2 ) (printf  "while ~a do ~a " (printexpr e1 )
                                          (printexpr e2))]
  ))

(struct None ()
    #:transparent)
(struct (i) Some ([v : i])
    #:transparent)
(define-type (Opt a) (U None (Some a)))

(: lookup  ((Listof Number)  Number -> Number))
(define (lookup ls l)
  (match ls
    [(cons (cons (== l) n) ls) n]
    [(cons _ ls) (lookup ls l)]))


(: updates ((Listof Any) Positive-Byte ->
                                 (Opt (Listof Any))))
(define (updates ls l)
  (match ls
    ['()   (None)]
    [(cons (cons (== l) n) ls) (Some (append ls (list (list l n))))]
    [(cons _ ls) (updates ls l)]))
{% endhighlight %} 

But this code is not complete. It has some serious faults in as far as the types and patterns are
concerned. So I decided to pay close attention to the types and compare the SML output and the
output of my Racket code.
Even though I am porting from SML to Racket I don't use idiomatic Racket everywhere. Parts of
the code may not seem coherent but I ensure that the basic unit tests pass.

## Final Attempt

1. Fixed may type annotation bugs
2. Fixed all the patterns after reading the documentation.
3. Fixed most of the logic bugs. The code still doesn't behave exactly like the SML
   code.
4. This code is only one part of the attempt to learn and implement types and toy languages.
   
{% highlight racket %}
(: reduce ((Pairof (Opt Expr)  (Listof (Pairof Loc LocValue))) ->
                      (Pairof (Opt Expr ) (Listof  (Pairof Loc LocValue)))))
(define (reduce expr)
  (match expr
    [  (cons (Some  (Op  (? integer? n1) Plus (? integer? n2)))
             store)
       (cons (Some  (IntValue (+ n1  n2))) store)]
    [  (cons (Some ( Op  (? integer? n1) GTEQ (? integer? n2 ))) store)
       (cons (Some  (BoolValue (>=  n1  'n2))) store)]
    [  (cons (Some (Op  (? integer? n1) Skip (? boolean? n2))) store)
             (match  (reduce (cons (Some n2) store))
               [ (cons (Some  (IntValue nn2)) store)  (cons (Some ((Op n1 Skip nn2))) store)]
               [ (None)  (None) ]
               )
             (match  (reduce  n1 store)
               [ (cons (Some   (IntValue nn1)) store)  (cons (Some (Op nn1 Skip n2)) store)]
               [ (None)  (None) ]
               )]
    [ (cons (Some (IntValue n )) store) (cons (None) store )]
    [ (cons (Some (If e1 e2 e3)) store)
             (match e1
               [#t  (cons (Some e2) store)  ]
               [#f  (cons (Some e3) store) ]
               [_   (match (reduce (cons (Some e1) store ))
                     [ (cons (Some e1) store) ( cons (Some (If e1 e2 e3)) store) ]
                     [ (None)  (None) ]
               )]
     )]
    [ (cons (Some (Deref l)) store)
             (match (lookup  store l)
               [ (cons (Some n ) store )  (cons (Some  (IntValue n)) store)]
               [ (cons (None) store)  (cons (None) store )]
     )]
    [ (cons (Some (Assign l e )) store)
             (match e
               [(IntValue n)
                (match (updates store (cons (Loc l) (LocValue n)))
                [ (Some store ) (cons  (Some (Skip))  store)]
                [ (None)  (cons (None) store ) ]
                [ _  (match (reduce (cons (Some e) store))
                        [(cons (Some  e) store)  (cons (Some (Assign l e)) store)]
                        [ (None)  (None) ]
                     )
                ]
               )]
               )]

   [  (cons (Some (Seq e1 e2))  store)
            (match e1
              [Skip  (cons (Some  e2) store) ]
              [ _  ( match (reduce (cons (Some e1) store ))
                    [  (cons (Some  e1) store)
                       (cons (Some  (Seq  e1 e2))  store ) ]
                    [ (None)  (None) ]

               )]
               )]
   [ (cons (Some (Skip))  store) ( cons (None) store )]

   [ (cons (Some (While e1 e2)) store)
     (cons (Some  ( If e1 (Seq e2 (While e1 e2)) (Skip))) store)  ]

))


{% endhighlight %} 

