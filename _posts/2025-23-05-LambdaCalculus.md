---
layout: post
title: Explore Lambda Calculus - Part II
published: false
---

{% highlight ocaml %}
open Scanner__Lang1.Language

type int_binary_op =
  |Add|Sub|Mul|Div|Less|Greater|Equal
[@@deriving show]

type int_unary_op = Neg
[@@deriving show]

type instr = Halt | IntConst of int | IntBinaryOp of int_binary_op
           | IntUnaryOp of int_unary_op

[@@deriving show]

(* TODO Unused *)
module type IntBinaryOp = sig
  type t
  val fromArithmeticFn  : Scanner__Lang1.arithmetic_fn -> t
  val fromComparisonFn  : Scanner__Lang1.comparison_fn -> t
end

module type ByteCodeGen =
sig
  type bytecode
  val generate : expr -> string list

  val emit_instr: instr -> string list
end



module Op  = struct

let fromArithmeticFn (fn : Scanner__Lang1.arithmetic_fn) =
  match fn with
  | Add -> IntBinaryOp Add
  | Sub -> IntBinaryOp Sub
  | Div -> IntBinaryOp Div
  | Mul -> IntBinaryOp Mul

let fromComparisonFn (fn : Scanner__Lang1.comparison_fn) =
  match fn with
  | Less -> IntBinaryOp Less
  | Greater -> IntBinaryOp Greater
  | Equal -> IntBinaryOp Equal
end


module Value = struct



  type value =
    | VInt of int
    | BlackHole

 type eval_error = WrongType of value *  string
 exception EvalException of eval_error
 let asInt = function
        | VInt i->  i
        | other -> raise( EvalException(WrongType( other, "int" )) )
end

module type Operation =
sig
  val execute : unit
  val current_value : Value.value option
end
module VM   = struct
  let bc_stack  = Stack.create()
  let instructions = []

  module VMOperation
                     ( Byte : ByteCodeGen )
                     ( Oper : Operation)= struct
  end


module VMOp =
VMOperation(struct
  type bytecode = instr array

  let  emit_instr instr =
     instructions @ [(Format.asprintf "%a" pp_instr (instr))]


  let rec generate expr =
    (*Unsure how the missing cases are handled*)
    match expr with
       | Lit (BInt i) -> emit_instr ( IntConst i )
       | Builtin ( Arithmetic( fn, opA, opB ) ) ->
           ignore (generate opA);
           ignore (generate opB);
           let op_type = Op.fromArithmeticFn fn in
           emit_instr  op_type
       | Builtin ( Comparison( fn, lhs, rhs) ) ->
           ignore (generate lhs);
           ignore (generate rhs);
           let op_type = Op.fromComparisonFn fn in
           emit_instr op_type
       | Builtin ( UnaryArithmetic( fn, expr) ) ->
           ignore (generate expr);
           match fn with
           | Neg -> emit_instr (IntUnaryOp Neg)
       |  _ -> failwith "TO Investigate"


     end)
  (struct

  let execute : unit=
    let halt = ref false in
    let rec loop cp =
      if (cp < (List.length instructions) && (!halt == true)) then
        let instr = List.nth instructions cp in
        match instr with
        | Halt -> halt := true
        | IntConst i -> Stack.push (Value.VInt i) bc_stack
        | IntBinaryOp op ->
          let arg2 = Stack.pop bc_stack |> Value.asInt in
          let arg1 = Stack.pop bc_stack |> Value.asInt in
          let result =
          (match op with
          | Add -> arg1 + arg2
          | Sub -> arg1 - arg2
          | Div -> arg1 / arg2
          | Mul -> arg1 * arg2
          | Less -> if arg1 < arg2 then 1 else 0
          | Greater -> if arg1 > arg2 then 1 else 0
          | Equal -> if arg1 == arg2 then 1 else 0)
          in
          Stack.push  (Value.VInt result) bc_stack
        | IntUnaryOp op ->
          let arg = Stack.pop bc_stack |> Value.asInt in
          let result =
          (match op with
            | Neg -> -arg ) in
          Stack.push  (Value.VInt result) bc_stack;
      loop (cp + 1);
    in
    loop 0

  let current_value =
    match (Stack.is_empty bc_stack) with
    |  false ->  Some (Stack.top bc_stack);
    |  true -> None
   end)

end
{% endhighlight %}
