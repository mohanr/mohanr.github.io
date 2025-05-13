---
layout: post
title: Explore Lambda Calculus - Part I
published: false
---

# Simple Lambda Calculus interpreter

> This is inspired currently by Artem Pianykh's F# code( Video ).
But I do come across many more videos posted years back. I cling to the hope
to incorporate more aspects as I learn.

## Porting FSharp code to OCaml

The code is directly ported to OCaml. So the pattern matching aspects are exactly like
the F# code with slight modifications.

My only learning goal that lead to an improvement for me was the modularity in OCaml.

{% highlight ocaml%}

type arithmetic_fn = | Add | Sub | Mul | Div

module type ARITHMETIC_FN=
sig
  type t = arithmetic_fn
  type a = int
  type b = int
  val apply : t -> a -> b -> a
end

type comparison_fn =
    | Less
    | Equal
    | Greater

module type COMPARISON_FN=
sig
  type t = comparison_fn
  type a = int
  type b = int
  val apply : t -> a -> b -> a
end


module  Lang(ArithmeticType : ARITHMETIC_FN)
            (ComparisonType : COMPARISON_FN) = struct

  module ArithmeticType = ArithmeticType
  module ComparisonType = ComparisonType
  type var_name = string
  type btype =  int


exception Type_error


type builtin_fn =
    |Arithmetic of  ArithmeticType.t *  expr *  expr
    |Comparison of  ComparisonType.t *  expr *  expr
and
expr =
    | Var of var_name
    | Abs of  var_name *  expr
    | App of expr *  expr
    | Lit of btype
    | Builtin of builtin_fn
    | Cond of expr *  expr *  expr


type eval_error = WrongType of expr *  string

exception EvalException of eval_error

end

module Language =
  Lang(struct
    type t = arithmetic_fn
    type a = int
    type b = int
    let apply fn a b : int =
       match fn with
         | Add -> a + b
         | Sub -> a - b
         | Mul -> a * b
         | Div -> a / b
     end)
  (struct
    type t = comparison_fn
    type a = int
    type b = int
    let apply fn a b : int =
       match fn with
         | Less -> if a < b then 1 else 0
         | Greater ->  if a > b then 1 else 0
         | Equal ->  if a = b then 1 else 0
     end)


module Expr = struct
include Language

    let asInt = function
        | Lit btype  -> btype
        | other -> raise( EvalException(WrongType( other, "int" )) )
    let asAbs = function
        | Abs (var,  body ) -> var, body
        | other -> raise( EvalException(WrongType( other, "lambda" )) )


    let rec subst (replaceable  : var_name ) (replaceWith : expr ) (expr : expr) =
       let substFn = subst replaceable replaceWith in
       match expr with
        | Lit _ -> expr
        | Builtin ( Arithmetic( fn, opA, opB ) ) ->
             Builtin ( Arithmetic( fn, substFn opA, substFn opB ) )
        | Builtin ( Comparison( fn, opA, opB ) ) ->
             Builtin ( Comparison( fn,  substFn opA,  substFn opB ) )
        | Cond (pred, trueBranch, falseBranch) ->
             Cond ( substFn pred, substFn trueBranch, substFn falseBranch)
        | App ( expr, arg ) ->  App( substFn expr , substFn arg )
        | Var boundName -> if boundName = replaceable then replaceWith else expr
        | Abs (boundName, body ) -> if boundName = replaceable then expr else
                                     Abs( boundName, substFn body )


    and  eval( expr : expr ) : expr =
     match expr with
        | Lit _ -> expr
        | Builtin ( Arithmetic( fn, opA, opB ) ) ->
            let valA = eval opA  |> asInt in
            let valB = eval opB  |> asInt in
            Lit  (  ArithmeticType.apply fn valA valB)
        | Builtin ( Comparison( fn, opA, opB ) ) ->
            let lhs = eval opA  |> asInt in
            let rhs = eval opB  |> asInt in
            Lit (ComparisonType.apply fn lhs rhs  )
        | Cond (pred, trueBranch, falseBranch) ->
            let valPred = eval pred |> asInt in
            if valPred <> 0 then eval trueBranch else eval falseBranch
        | Abs _ -> expr

        | App( expr, arg ) ->
            let lambdaVar, lambdaBody = eval expr |> asAbs in
            subst lambdaVar arg lambdaBody |> eval

        | Var _ -> failwith  "Wrong evaluation "

end
    include Language
    let lit n  = Lit n
    let incrFn = Abs ("x", Builtin( Arithmetic( Add, Var "x", lit 1 )))
    let incrApp n = App(  incrFn,  lit n )

    let lazyFixpoint =
        let innerAbs =
            Abs(
                 "x",
                  App( Var "f",  App( Var "x", Var "x" ) )) in
        Abs(  "f",  App ( innerAbs,  innerAbs ))

    let fibStep =
        let xMinus n =  Builtin (Arithmetic( Sub, Var "x", lit n  )) in
        let fb = Builtin( Arithmetic( Add, App( Var "f", xMinus 1 ), App( Var "f", xMinus 2 ) ) ) in
        Abs(  "f",
              Abs(
                   "x",
                   Cond(
                          Builtin( Comparison( Less, Var "x", lit 2 ) ),
                          lit 1,
                          fb
                        )
                   )
         )

    let fib( n : int ) =
       let fn = App( lazyFixpoint, fibStep ) in
       App( fn, lit n )


{% endhighlight%}
