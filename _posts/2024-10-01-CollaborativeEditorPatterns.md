---
layout: post
title: Collaborative Editor
published: true
---

Collaborative editor patterns that I try to code could be of many kinds. There are
string manipulation algorithms, CRDTs and even LLM inference. So even though a full-fledged
editor is not in scope here, many algorithms like these can be explored and a functional
editor can be created.

The code will be updated directly here in phases. I don't commit this to a separate repository.

Please note that this is a random collection of algorithms that eventually could be part of a simple editor.
There are too many books and papers that deal with a multitude of algorithms.

# OCaml 5 Effect Handlers

I will add some sections like this to explain the reason for experimenting with new paradigms. In many cases the code is too dense and will seem complicated when new techniques are introduced needlessly but Effect handlers are interesting to learn. Application though should be selective. There will be many usecases for these in the future.

So the following is an experiment.

# Breaking paragraphs into lines

The title describes the essence of once such algorithm( Plass and Knuth ) to break paragraphs.

{% highlight OCaml %}
open Stdlib
open Effect.Deep
open Effect

type entry = {
  first : int;
  last : int;
  mutable next : int;
  mutable score  : int}


let l = ref []

let is_space= function ' ' -> true | _ -> false

let final_state (start, idx) =

  let e = {first = start; last = idx; next = -1; score = -1} in
   l := !l @ [e];
  Printf.printf " Final state %d %d%!" start idx

type _ Effect.t += Skipping_spaces :  (int * int ) -> unit Effect.t

let parabreak  text ideal_width max_width  =
  let start = ref 0 in
  let idx,_ =
  String.fold_left (fun (idx,word_or_space) c ->
                       match (is_space c, word_or_space)  with
                       | (true,true)
                           ->
                           (* Printf.printf "Space at index %d, skipping\n%!" idx; *)

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
                           final_state (!start,idx)
let effective text =
  Printf.printf "Handling effects%!";
  match_with (fun () -> parabreak text 10 29)
    ()
  { effc = (fun (type c) (eff: c Effect.t) ->
      match eff with
      | Skipping_spaces (s,s1) -> Some (fun (k: (c,_) continuation) ->
              (* Printf.printf "Skipping spaces \"%d %d\"\n%!" s s1; *)

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


type _ Effect.t += Plass_break :  int -> unit Effect.t

let rec plassbreak indent  idx idealwidth maxwidth =
    let
    lastrecord = List.nth !l idx in
    let llen    = ref (lastrecord.last - lastrecord.first) in
    let bscore  = idealwidth - !llen in
    let bscore  = ref (bscore * 2) in
    let btail   = ref (idx + 1) in
    let _ =
    List.fold_left (fun acc entry ->

        match entry with
         | {first; last; next; score}  ->
            let wwidth  = last - first in
            if ((!llen + wwidth) >= maxwidth) then(
                perform (Plass_break acc);
                acc
            )else(

              let lscore  = ref (idealwidth - (!llen + wwidth)) in
              lscore      := !lscore * 2;
              llen        := !llen + wwidth + 1;

              if score == -1 then
                plassbreak  (indent + 1) (acc + 1) idealwidth maxwidth;

              let
                record = List.nth !l acc  in
                if ((!lscore + score) < !bscore) then(
                  bscore  := !lscore + record.score;
                  btail   := acc;
                );

           acc + 1;
           )
      ) idx !l in

  let record = List.nth !l idx in
  record.next <- !btail;
  record.score <- !bscore;

  if (record.next + 1) = List.length !l then (
       record.score <- 0;
  )

let pbreak =

  Printf.printf "Handling effects%!";
  match_with (fun () -> plassbreak 0 10 29)
    ()
  { effc = (fun (type c) (eff: c Effect.t) ->
      match eff with
      | Plass_break s -> Some (fun (k: (c,_) continuation) ->
              Printf.printf "Plass Break\"%d\"\n%!" s ;
              (* continue k () *)
          )
      | _ -> None
  );
  exnc = (function
        | e -> raise e
  );
  (* retc = fun _ -> failwith "Fatal error" *)

  retc = (fun res -> Printf.printf "Computation returned: \n")
 }

let rec loop_while line text lines idx next acc =
    if acc > next || (acc + 1) >= List.length !l then
        line
    else
        let {first; last; _} = List.nth lines acc in
        if (last - first) <= 0 then
            line
        else
            let new_line =
                line ^ (if acc = idx then "" else " ") ^
                String.sub text first (last - first)
            in
            loop_while  new_line text lines idx next (acc + 1)


let  layout text idealwidth maxwidth =
 let rec loop idx lines line =
    if idx < List.length !l then
        let {first; last; next; _} = List.nth lines idx in
        let line = loop_while line text lines idx next idx in
        if (String.length line < maxwidth)
        then
            let line = String.make (maxwidth - String.length line) ' ' in
            let subline = String.sub line 0 idealwidth in
            let subline1 = String.sub line idealwidth (String.length line - idealwidth) in
            let line = subline ^ "+" ^ subline1 ^ "|" in
            Printf.printf "%s\n" line;
        else Printf.printf "%s" line;
        loop (idx + 1) !l line
    in
    loop 0 !l ""

(* read the entire file *)
let read_file() =
  let contents = In_channel.with_open_text "/Users/anu/Documents/go/wiki.txt" In_channel.input_all in
  contents

let () =
  try
      let file_contents = read_file() in
      let _ = effective file_contents in
      let _ = pbreak in
      let _ = layout file_contents 10 30 in
      ()
  with
  | exn -> Printf.printf "Unhandled exception: %s\n%!" (Printexc.to_string exn)
{% endhighlight OCaml %}
