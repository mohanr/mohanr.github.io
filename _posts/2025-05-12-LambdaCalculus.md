---
layout: post
title: Explore Lambda Calculus - Part I
published: true
---

# Simple Lambda Calculus interpreter

> This is inspired currently by Artem Pianykh's F# code( Video ).
But I do come across many more videos posted years back. I cling to the hope
to incorporate more aspects as I learn.
> There is also [similary AST manipulation code here](https://github.com/nunchaku-inria/nunchaku/tree/a69d3ebce2fb83c40824420c4d93cc615c8a5fa1)
## Porting FSharp code to OCaml

The code is directly ported to OCaml. So the pattern matching aspects are exactly like
the F# code with slight modifications.

My only learning goal that lead to an improvement for me was the modularity in OCaml.

The aspects I learnt to use are

1. Module Types
2. Functors
3. Instantiation of modules like this

But truly the aspects I should have learnt are

1. Term Reduction by substitution
2. Closures
3. let-bindings

and other deeper concepts. That is now aspirational.

{% highlight ocaml%}

  module Language =
  Lang(struct
    type t = arithmetic_fn
    [@@deriving show]
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
    [@@deriving show]
    type a = int
    type b = int
    let apply fn a b : int =
       match fn with
         | Less -> if a < b then 1 else 0
         | Greater ->  if a > b then 1 else 0
         | Equal ->  if a = b then 1 else 0
     end)

{% endhighlight %}

This is the entire code. _[@@deriving show]_ is used in many locations to pass the test shown below.

{% highlight ocaml%}
type arithmetic_fn = | Add | Sub | Mul | Div
[@@deriving show]

module type ARITHMETIC_FN=
sig
  type t = arithmetic_fn
  [@@deriving show]
  type a = int
  type b = int
  val apply : t -> a -> b -> a
end

type comparison_fn =
    | Less
    | Equal
    | Greater
[@@deriving show]

module type COMPARISON_FN=
sig
  type t = comparison_fn
  [@@deriving show]
  type a = int
  type b = int
  val apply : t -> a -> b -> a
end


module  Lang(ArithmeticType : ARITHMETIC_FN)
            (ComparisonType : COMPARISON_FN) = struct

  module ArithmeticType = ArithmeticType
  module ComparisonType = ComparisonType
  type var_name = string
  [@@deriving show]
  type btype =  int
  [@@deriving show]


exception Type_error


type builtin_fn =
    |Arithmetic of  ArithmeticType.t *  expr *  expr
    |Comparison of  ComparisonType.t *  expr *  expr
[@@deriving show]
and
expr =
    | Var of var_name
    | Abs of  var_name *  expr
    | App of expr *  expr
    | Lit of btype
    | Builtin of builtin_fn
    | Cond of expr *  expr *  expr
[@@deriving show]

type eval_error = WrongType of expr *  string

exception EvalException of eval_error

end

module Language =
  Lang(struct
    type t = arithmetic_fn
    [@@deriving show]
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
    [@@deriving show]
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

{% endhighlight%}

## Test

{% highlight ocaml %}


let lazyFixpoint =
  let innerAbs =
    Abs(
      "x",
      App( Var "f",  App( Var "x", Var "x" ) )) in
  Abs(  "f",  App ( innerAbs,  innerAbs ))

let fibStep =
  let xMinus n =  Builtin (Arithmetic( Sub, Var "x", Lit n  )) in
  let fb = Builtin( Arithmetic( Add, App( Var "f", xMinus 1 ), App( Var "f", xMinus 2 ) ) ) in
  Abs(  "f",
        Abs(
          "x",
          Cond(
            Builtin( Comparison( Less, Var "x", Lit 2 ) ),
            Lit 1,
            fb
          )
        )
     )

let fib( n : int ) =
  let fn = App( lazyFixpoint, fibStep ) in
  App( fn, Lit n ) |> eval


let%expect_test _=

  Printf.printf "%s" (show_expr (fib 5));
  [%expect {| (Lang.Lang.Lit 8) |}]

{% endhighlight%}

## Closures

The next part of the code is this. Currently it throws 'Not_Found' but my learning goal
here was set by my lack of knowledge of how _[@@deriving_show]_ works.


{% highlight% ocaml}

open Containers
open Stdlib
type arithmetic_fn = | Add | Sub | Mul | Div
[@@deriving show]

module type ARITHMETIC_FN=
sig
  type t = arithmetic_fn
  [@@deriving show]
  type a = int
  type b = int
  val apply : t -> a -> b -> a
end

type comparison_fn =
    | Less
    | Equal
    | Greater
[@@deriving show]

module type COMPARISON_FN=
sig
  type t = comparison_fn
  [@@deriving show]
  type a = int
  type b = int
  val apply : t -> a -> b -> a
end


module  Lang(ArithmeticType : ARITHMETIC_FN)
            (ComparisonType : COMPARISON_FN) = struct

  module ArithmeticType = ArithmeticType
  module ComparisonType = ComparisonType
  type var_name = string
  [@@deriving show]
  type btype = BInt of int
  [@@deriving show]

  module EnvKey = struct
    type t = var_name
    let compare s s1 = if s < s1 then -1 else if s > s1 then 1 else 0
    (* String.compare *)
  end
exception Type_error


type builtin_fn =
    |Arithmetic of  ArithmeticType.t *  expr *  expr
    |Comparison of  ComparisonType.t *  expr *  expr
[@@deriving show]
and
expr =
    | Var of var_name
    | Abs of  var_name *  expr
    | App of expr *  expr
    | Lit of btype
    | Builtin of builtin_fn
    | Cond of expr *  expr *  expr
[@@deriving show]

module PPMap =CCMap.Make(EnvKey)


  type value =
    | VInt of int
    | Closure of closure
  and closure = {env : env ; var : var_name ; body : expr}
  and
  env =
  | EnvMap of value PPMap.t
      [@printer
        fun fmt map -> fprintf fmt "%a" (PPMap.pp CCString.pp pp_value) map]
[@@deriving show] (* only one call to `deriving show` is enough *)

type eval_error = WrongType of value *  string

exception EvalException of eval_error

end

module Language =
  Lang(struct
    type t = arithmetic_fn
    [@@deriving show]
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
    [@@deriving show]
    type a = int
    type b = int
    let apply fn a b : int =
       match fn with
         | Less -> if a < b then 1 else 0
         | Greater ->  if a > b then 1 else 0
         | Equal ->  if a = b then 1 else 0
     end)


module Value = struct
include Language

    let asInt = function
        | VInt i->  i
        | other -> raise( EvalException(WrongType( other, "int" )) )

    let asClosure = function
        | Closure c->  c
        | other -> raise( EvalException(WrongType( other, "closure" )) )


    let rec eval env  ( expr : expr ) : value =
     match expr with
        | Lit (BInt i) -> VInt i
        | Builtin ( Arithmetic( fn, opA, opB ) ) ->
            let valA = eval env opA  |> asInt in
            let valB = eval env  opB  |> asInt in
            VInt (ArithmeticType.apply fn valA valB )
        | Builtin ( Comparison( fn, opA, opB ) ) ->
            let lhs = eval env  opA  |> asInt in
            let rhs = eval env  opB  |> asInt in
            VInt( ComparisonType.apply fn lhs rhs )
        | Cond (pred, trueBranch, falseBranch) ->
            let valPred = eval env  pred |> asInt in
            if valPred <> 0 then eval env  trueBranch else eval env  falseBranch
        | Abs ( var, body ) -> Closure { env = env; var = var; body = body }

        | App( expr, arg ) ->
            let { env = closureEnv ; var = closureVar; body = closureBody} = eval env  expr |> asClosure in
            let argValue = eval env arg in
            let _var_name = PPMap.add closureVar argValue in
            Printf.printf "%s" (Format.asprintf "%a" pp_value argValue);
            eval  env closureBody

        | Var name ->
           let EnvMap map = env in
           PPMap.find name map

end

{% endhighlight%}



