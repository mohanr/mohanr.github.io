---
layout: post
title: Implementing a JIT compiled language using OCaml and LLVM
published: true
---

Notwithstanding the loftiness of the title, this is just a tutorial inspired by [Implementing a JIT Compiled Language with Haskell and LLVM](https://smunix.github.io/www.stephendiehl.com/llvm/index.html). I chose OCaml and decided to  select my own libraries to accomplish this.
So, for example, the parser using Angstrom, which I know nothing about.

While I code I also strive to learn.

1. The code will be developed and refactored incrementally.
2. The version posted here is compiled and tested but it is incomplete. The final version will be in Git if everything goes
   according to plan.

# Parser combinators

Ported from Haskell to OCaml and uses _Angstrom_. Lightly tested.

{% highlight ocaml %}
module LibAngstrom = struct
  open Angstrom

  let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false
  let ws = take_while is_whitespace
  let reservedops = ["+","*","-",";"]
  let reservednames = ["def","extern"]
  let comma = char ',' <* ws
  let colon = char ':' <* ws

  let is_comment = function
    | '#'  -> false
    | x -> not @@ is_whitespace x

let is_integer = function '0'..'9' -> true | _ -> false
let is_double= function | '0'..'9' | '.' -> true | _ -> false


let integer =
    take_while1 is_integer>>| int_of_string

let double=
    take_while1 is_double>>| float_of_string

let parens p =
  char '(' >>= fun _ ->
  p >>= fun x ->
  char ')' >>= fun _ -> return x


let reservedops = function
    |ops  -> ()

let reservednames = function
    |names -> ()

let lexer =
    fix(fun lexer ->
      ws
      *> choice
      [
        parens integer ;
      ])
end

let main text =
 Angstrom.parse_string ~consume:All LibAngstrom.lexer text
{% endhighlight %}

# The ADT

{% highlight ocaml %}

module Syntax = struct

type name = string

type 'a expr =
    Const of float
  | BinOp of 'a op * 'a expr * 'a expr
  | Var of string
  | Call of name * 'a expr list
  | Function of name * 'a expr list  * 'a expr
  | Extern of name * 'a expr list
and 'a op
  = Plus
  | Minus
  | Times
  | Divide

end

{% endhighlight %}

> TODO
