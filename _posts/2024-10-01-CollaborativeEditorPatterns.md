---
layout: post
title: Collaborative Editor
published: false
---
{% highlight OCaml %}


open Stdlib

type entry =
  | First
  | Last
  | Next
  | Score

let is_space= function ' ' -> true | _ -> false

let final_state (start, idx) =

  Printf.printf " Final state %d %d" !start idx

let break  text ideal_width max_width  =
  let start = ref 0 in
  let idx,_ =
  String.fold_left (fun (idx,word_or_space) c ->
                       match (is_space c, word_or_space)  with
                       | (true,true)
                           -> Printf.printf "Space at index %d, skipping\n" idx;
                           start := idx + 1;
                           (idx + 1,false);
                       | (true,false)
                           -> Printf.printf "No Space at index %d, skipping\n" idx;
                           (idx + 1,false) 
                       | (false,_)
                           ->(idx + 1,true)) (0,false) text
 in final_state (start,idx)
{% highlight OCaml %}
