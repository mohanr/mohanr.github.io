---
layout: post
title: Collaborative Editor
published: true
---

Collaborative editor patterns that I try to code could be of many kinds. There are
string manipulation algorithms, CRDTs and even LLM inference. So even though a full-fledged
editor is not in scope here, many algorithms like these can be explored and a functional
editor can be created.

# OCaml 5 Effect Handlers

I will add some sections like this to explain the reason for experimenting with new paradigms. In many cases the code is too dense and will seem complicated when new techniques are introduced needlessly but Effect handlers are interesting to learn. Application though should be selective. There will be many usecases for these in the future.

So the following is an experiment.

{% highlight OCaml %}

open Stdlib
open Effect.Deep
open Effect

type entry = {
  first : int;
  last : int;
  next : int;
  score  : int}


let is_space= function ' ' -> true | _ -> false

let final_state (start, idx) =

  Printf.printf " Final state %d %d%!" !start idx

type _ Effect.t += Skipping_spaces :  (int * int ) -> unit Effect.t

let break  text ideal_width max_width  =
  let start = ref 0 in
  let idx,_ =
  String.fold_left (fun (idx,word_or_space) c ->
                       match (is_space c, word_or_space)  with
                       | (true,true)
                           -> Printf.printf "Space at index %d, skipping\n%!" idx;
                           if !start < idx then
                                perform (Skipping_spaces (!start,idx));

                           start := idx + 1;
                           (idx + 1,false);
                       | (true,false)
                           -> Printf.printf "No Space at index %d, skipping\n%!" idx;
                           (idx + 1,false) 
                       | (false,_)
                           ->(idx + 1,true)) (0,false) text
 in

                           if !start < idx then
                                perform (Skipping_spaces (!start,idx));
                           final_state (start,idx)
let effective =
  Printf.printf "Handling effects%!";
  let l = ref [] in
  match_with (fun () -> break "text" 10 29)
    ()
  { effc = (fun (type c) (eff: c Effect.t) ->
      match eff with
      | Skipping_spaces (s,s1) -> Some (fun (k: (c,_) continuation) ->
              Printf.printf "Skipping spaces \"%d %d\"\n%!" s s1;
              let e = {first = s; last = s1; next = -1; score = -1} in
              l := !l @ [e];
              continue k ()
          )
      | _ -> None
  );
  exnc = (function
        | e -> raise e
  );
  (* retc = fun _ -> failwith "Fatal error" *)

  retc = (fun res -> Printf.printf "Computation returned: \n")
 }

let () =
  try effective
  with
  | exn -> Printf.printf "Unhandled exception: %s\n%!" (Printexc.to_string exn)
{% endhighlight OCaml %}
