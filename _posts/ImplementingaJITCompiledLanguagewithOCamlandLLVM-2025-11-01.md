---
layout: post
title: Implementing a JIT compiler using OCaml and LLVM
published: false
---


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
        parens integer *> lexer;
      ])
end
{% endhighlght %}
