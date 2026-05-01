---
layout: post
title: Static Analyzer
published: false
---



{% highlight OCaml %}

type binaryOps =
    | Plus of char
    | Minus of char
    | Div of char
    | Neg of char
    | Mul of char
  [@@deriving show]

type comparisonOps =
    | Less of char
    | Great of char
    | StructEqu of string
    | Less_Eq of string
    | Great_Eq of string
    | Not_Eq of string
  [@@deriving show]

let operator c =
  match c with
     |'+' -> Plus c
     |'-' -> Minus c
     |'/' -> Div c
     | _ -> failwith "Wrong operator"

type expr =
  | BinOp of binaryOps * var * scalar
  | Seq of expr * expr
  | Assign of var * expr
  | If of expr * expr * expr
  | Input of var
  | BoolExpr of comparisonOps * var *  scalar
  | While of expr * expr
  | Skip
and scalar =
  | Scalar of int
and var = Var of char
[@@deriving show]


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

{% end highlight %}


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

{% end highlight %}
