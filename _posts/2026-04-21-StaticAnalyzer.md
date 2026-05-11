---
layout: post
title: Static Analyzer
published: true
---

This is a typical post in this blog in the sense that code is shown first and the narrative is built later if at all.
This has its disadvantages as the article is incomplete without the explanation.

But in many cases like this one the theory is somewhat inscrutable as it may involve Math. It has to be learnt in
a series of steps and working code seems to be a good motivating factor. Narrative and diagrams should be required
to complete this and other blog posts.

I will start anyway by showing OCaml code that I ported from Python. This Python code is [part](https://github.com/sree314/simple-abstract-interpreter)
of the Spring 2020 edition of CSC255/455 Software Analysis and Improvement taught at the University of Rochester.

The book recommended for that course is ![image-title-here](../images/Introduction_to_Static_Analysis.png){:width="10%"}{:class="img-responsive"}
# The ADT

This is the first cut and has to improve gradually. _expr_ should be refactored as there is repetition.
This is but one point that shows the disadvantage of ignoring the theory because the focus is more on the
quality of OCaml code and my Functional Programming skills than on the underlying topic.

Almost all the code is here but for a more complete version look at this [repo](https://github.com/mohanr/Algorithms/tree/master/ds/lib/abstract-interpreter)

{% highlight OCaml %}


open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib.Conv

type binaryOps =
    | Plus of char
    | Minus of char
    | Div of char
    | Neg of char
    | Mul of char
[@@deriving show ,compare, sexp]

type comparisonOps =
    | Less of char
    | Great of char
    | StructEqu of string
    | Less_Eq of string
    | Great_Eq of string
    | Not_Eq of string
[@@deriving show ,compare, sexp]

let operator c =
  match c with
     |'+' -> Plus c
     |'-' -> Minus c
     |'/' -> Div c
     | _ -> failwith "Wrong operator"
[@@deriving show ,compare, sexp]

(* Some types can be merged into 'expr*)
type expr =
  | Program of expr
  | BinOp of binaryOps * var * scalar
  | BinaryOps of binaryOps * expr * expr
  | ComparisonOps of comparisonOps * comparisonOps
  | Seq of expr * expr
  | Assign of var * expr
  | If of expr * expr * expr
  | Input of var
  | BoolExpr of comparisonOps * var *  scalar
  | BoolExprs of comparisonOps * expr*  expr
  | While of expr * expr
  | Vars  of char               (* Refactor*)
  | Const of scalar (* Refactor*)
  | Skip
and scalar =
  | Scalar of int
and var = Var of char
[@@deriving show ,compare, sexp]


 (* convenience function to turn a list into a sequence *)
let rec sequence l =
    match List.length l with
      | 0 -> failwith "Can't convert an empty list into a Seq"
      | 1 -> Seq ((List.nth l 0), Skip)
      | 2 ->  Seq ((List.nth l 0), (List.nth l 1))
      | _ -> Seq ((List.nth l 0),
                 sequence ( List.filteri
                              (fun i _ -> i >= 1 && i <=
                                                    (List.length l)) l ))

{% endhighlight %}

I have to remind myself that the code shows how source code is statically analyzed.
The theory behind it is still somewhat hard to understand at this point.

# Test

{% highlight OCaml %}

let%expect_test _=
    let x = Var 'x' in
    let y = Var 'y' in
    let t = Program(If ( BoolExpr((Less '<'), x, (Scalar 7)),
                           Assign (x, BinOp (( Neg  '-'), x, (Scalar 7))),
                           Assign (y, BinOp ((Minus '-'), x, (Scalar 7)))
               ))
              in
 print_endline (show_expr t);
  [%expect {|
    (Tinyest.Program
       (Tinyest.If (
          (Tinyest.BoolExpr ((Tinyest.Less '<'), (Tinyest.Var 'x'),
             (Tinyest.Scalar 7))),
          (Tinyest.Assign ((Tinyest.Var 'x'),
             (Tinyest.BinOp ((Tinyest.Neg '-'), (Tinyest.Var 'x'),
                (Tinyest.Scalar 7)))
             )),
          (Tinyest.Assign ((Tinyest.Var 'y'),
             (Tinyest.BinOp ((Tinyest.Minus '-'), (Tinyest.Var 'x'),
                (Tinyest.Scalar 7)))
             ))
          )))
    |}]
{% endhighlight %}

Thee tests are sparse but more will be added.

{% highlight OCaml %}

open Types
open Containers

module type ORDERED = sig
  type inter= Types.inter
  type value
  val compare : value -> value  -> int
end


module type IntervalPt = sig
  type elt
  type inter= Types.inter
  val eq : inter -> inter -> bool
  val lt : inter -> inter -> bool
end
module IntervalPoint = struct

module Make (Ord : ORDERED ) : (IntervalPt with type elt := Ord.value) =
      struct
        type value = int
        type inter= Types.inter
        type elt = Ord.value
   (* Repeated definition. Should belong in types.ml *)
   (* But interals.ml uses a certain pattern which may *)
   (* not reuse thie type from types.ml. Should be investigated *)

  let eq pt pt1 =
        (* this equates infinity, which should be okay *)
        match pt,pt1 with
        |inter1,inter2 ->
            Stdlib.compare inter1  inter2 = 0

  let lt pt pt1 =
        match pt,pt1 with
        |Pinf,_ -> false
             (* +inf, -inf/F; +inf, n/F; +inf, +inf/F *)
        |Ninf,_ ->
             (* -inf, -inf/F; -inf, n/T; -inf, +inf/T *)
        if Stdlib.compare pt  pt1 <> 0 then true else false

        |_,Ninf -> false
             (* n, -inf/F *)

        |_,Pinf -> true
             (* n, +inf/F *)

        |_,_->if Stdlib.compare pt  pt1 < 0 then true else false


    let le pt pt1 =
        match pt,pt1 with
        |Pinf,_ -> if Stdlib.compare pt1 pt = 0 then true else false  (* +inf == +inf *)
        |Ninf,_ ->
            true   (* -inf <= -inf, n, +inf *)
        |_,Ninf -> false
        |_,Pinf -> true
             (* _, +inf *)
        |_,_->if Stdlib.compare pt  pt1 < 0 then true else false

    let gt pt pt1 =
        match pt,pt1 with
        |Pinf,_ ->
            Stdlib.compare pt1 Pinf = 0
        |Ninf,_ -> false
        |_,Ninf -> true
        |_,Pinf -> false
        |_,_-> if Stdlib.compare pt pt1 > 0 then true else false

    let add pt o =

    let m =
        IntervalpointMap.empty
        |> IntervalpointMap.add
         (Ninf, Ninf) (Some Ninf)
        |> IntervalpointMap.add
         (Ninf, Pinf) None  (* undefined -inf + +inf *)
        |> IntervalpointMap.add
         ( Pinf,Ninf) None  (* undefined: +inf + -inf*)
        |> IntervalpointMap.add
         ( Pinf,Pinf) (Some Pinf)
         in
         let res =
         (match( IntervalpointMap.find_opt (pt,o) m) with
         | Some index ->
           let result =  IntervalpointMap.get (pt,o) m in
           (match result with
           | Some v -> v
           | None -> failwith "Addition not defined "
           )
         | None ->
          (match pt,o with
            |Types.Int i,Types.Int i1-> Some (Types.Int (i + i1))
            |p,o when Stdlib.compare p Ninf <> 0 && Stdlib.compare p Pinf <> 0 -> Some o
                        (* n + -inf = -inf, n + +inf = +inf *)
            |_,_ -> Some pt
                        (* -inf - n = -inf, +inf - n = +inf *)
           ))
          in res




end
end
{% endhighlight %}


{% highlight OCaml %}

open Bloomfilter__Intervals
open Containers
open Types

module NonRelationalAbstraction( Dom : ORDERED_FUNCTIONAL_SET) = struct

    let union m0 m1 =
      let acc =
       let rec loop_while_m0 acc1 m0  =
       (match m0 with
          |[] -> acc1
          |(x,hd) :: tl ->
           let acc =
             let rec loop_while_m1 acc m1 =
             (match m1 with
              |[] -> acc
              |(x1,hd1) :: tl1 ->
                if  Char.(=) x x1 then
                 loop_while_m1 (acc @ [(x,Dom.lub hd hd1)]) tl1
                else
                 loop_while_m1 acc tl1
             )
             in loop_while_m1 [] m1
           in
           loop_while_m0 (acc1 @ acc) tl
       )
     in loop_while_m0 [] m0   in
     acc


     (* construct an abstraction for a set of memories *)
    let phi m =
     let m_acc =
       let rec loop_while_accum m i m_accum =
        if i < List.length m  then (
        let m_abs =
         let mabs = CCArray.make (List.length (List.nth m i))  (' ',(Dom.Tup(Dom.Int 0,Dom.Int 0))) in
         let rec loop_while_abs m i  mabs =
           if Array.length m > i then
             match Array.get m i with
             | x, y ->
            let a,b =  Dom.phi y in
            let _ = Array.set mabs i (x,Dom.Tup (a, b)) in
            (* let _ = List.iter (fun (c,k) -> *)
            (*   print_char  c; *)
            (*   print_endline (Dom.show_interval k); *)
            (* ) (Array.to_list mabs) in *)
            loop_while_abs m (i + 1) mabs
           else
             mabs
          in loop_while_abs (Array.of_list (List.nth m i)) 0 mabs
          in
            let accum =
               if List.length m_accum = 0 then
                  Array.to_list m_abs
               else
                  union m_accum (Array.to_list m_abs)
               in
                   loop_while_accum  m (i + 1) accum
         ) else
            m_accum
         in loop_while_accum  m 0 []
        in
        (* also construct BOT TODO Investigate how this is used.*)
       m_acc

    let rec lte m0_abs m1_abs =
        match m0_abs, m1_abs with
          |[],[] -> true
          |hd :: tl,hd1 :: tl1 ->
            if not (Dom.lte hd hd1) then false else (lte tl tl1)
          |_,_ -> failwith "lte error"

    let widen m0 m1 =
      let acc =
       let rec loop_while acc m0 m1=
       match m0,m1 with
          |[],[] -> acc
          |hd :: tl,hd1 :: tl1 ->
            loop_while (acc @ [Dom.widen hd hd1]) tl tl1
          |_,_ -> failwith "widen error"

     in loop_while [] m0 m1 in
     acc

    (* convenience function *)
    (* let included m_conc m_abs = *)
    (*     let m_c_abs = phi m_conc in *)
        (* lte m_c_abs m_abs *)

end

module I_Params = struct
  type inter= | Int of int | Pinf | Ninf
  [@@deriving show]
  type interval  = |Bot |Tup of inter * inter
  [@@deriving show]
end

module IST = IntervalDomain( I_Params)
module NRA = NonRelationalAbstraction(IST)

{% endhighlight %}
