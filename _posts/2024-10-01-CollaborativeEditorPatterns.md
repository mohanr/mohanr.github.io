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

So the following is an experiment in the sense that the code quality will be fixed only later. So, for example, global references are used to complete the code even though they
are unnecessary.

# Breaking paragraphs into lines

The title describes the essence of one such algorithm( Plass and Knuth ) to break paragraphs.

{% highlight OCaml %}
open Stdlib
open Effect.Deep
open Effect

type entry = {
  first : int;
  last : int;
  mutable next : int;
  mutable score  : int}


let gl = ref []

let is_space= function ' ' -> true | _ -> false

let rec printlist l =
  match l with
  | hd  ::tl ->
  Printf.printf " %d %d %d %d\n" hd.first hd.last hd.next hd.score;
  printlist tl
  | []  -> ()

let final_state l (start, idx) =

  (* let e = {first = start; last = idx; next = -1; score = -1} in *)
  (*  l := !l @ [e]; *)
  let e = {first = -1; last = -1; next = -1; score = 0} in
   l := !l @ [e];
  Printf.printf " Final state %d %d\n" start idx;
  Printf.printf " Size of list is %d\n" (List.length !l);
  l

type _ Effect.t += Skipping_spaces :  (int * int ) -> unit Effect.t

let parabreak l text ideal_width max_width  =
  Printf.printf " parabreak\n";
  let start = ref 0 in
  let idx,_ =
  String.fold_left (fun (idx,word_or_space) c ->
                       match (is_space c, word_or_space)  with
                       | (true,true)
                           ->
                           Printf.printf "Space at index %d, skipping\n" idx;

                           if !start < idx then
                                perform (Skipping_spaces (!start,idx)  );

                           start := idx + 1;
                           (idx + 1,false);
                       | (true,false)
                           -> Printf.printf "No Space at index %d, skipping\n" idx;
                           (idx + 1,false) 
                       | (false,_)
                           ->
                           (idx + 1,true)) (0,false) text
 in

                           if !start < idx then
                                Printf.printf "Final - Skipping spaces %d %d\n" !start idx;
                                perform (Skipping_spaces (!start,idx));
                           final_state l (!start,idx)
let effective text l =
  match_with (fun () -> parabreak l text 10 29)
    ()
  { effc = (fun (type c) (eff1: c Effect.t) ->
      match eff1 with
      | Skipping_spaces (s,s1) -> Some (fun (k: (c,_) continuation) ->
              Printf.printf "Skipping spaces \"%d %d\"\n" s s1;

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
  (* retc = (fun res -> Printf.printf "Computation returned %d: \n" (List.length !l)) *)

  retc = (fun _ ->  l)

 }


let rec plassbreak indent  idx idealwidth maxwidth =

    let jdx = ref( idx + 1 ) in
    let lastrecord = List.nth !gl idx in
    let llen    = ref (lastrecord.last - lastrecord.first) in
    let bscore  = idealwidth - !llen in
    let bscore  = ref (bscore * bscore) in
    let btail   = ref !jdx in
    let rec loop_while j_dx =
      if j_dx < (List.length !gl) then(
         Printf.printf "llen: %d, bscore: %d, btail: %d\n" !llen !bscore !btail;
         let {first; last; next; score} = List.nth !gl j_dx in
            let wwidth  = last - first in
            if ((!llen + wwidth) < maxwidth) then(

              let lscore  = ref (idealwidth - (!llen + wwidth)) in
              lscore      := !lscore * !lscore;
              llen        := !llen + wwidth + 1;

              if score = -1 then
                begin
                plassbreak  (indent + 1) j_dx idealwidth maxwidth;
                end;

                let score1 = List.nth !gl !jdx in
                if ((!lscore + score1.score) < !bscore) then(
                  bscore  := !lscore + score1.score;
                  btail   := !jdx;
                );
                loop_while (j_dx + 1)
            )else ()
     )else ()
    in
    loop_while !jdx;
  let record = List.nth !gl idx in
  record.score <- !bscore;
  record.next <- !btail;
  if (record.next + 1) = List.length !gl then(
       record.score <- 0;
  )
  
let rec loop_while line text lines idx next acc =
    if acc > next || (acc + 1) >= List.length !gl then
    (
        line
    )
    else
        let {first; last; _} = List.nth lines acc in
        if (last - first) <= 0 then
        (
            line;
        )
        else(
            let new_line =
                line ^ (if acc == idx then "" else " ") ^
                String.sub text first (last - first)
            in
            loop_while  new_line text lines idx next (acc + 1)
        )


let  layout text idealwidth maxwidth =

 let rec loop idx lines line =
    if (idx < (List.length !gl - 1))  then

        let entry = List.nth lines idx in
        let line = loop_while line text lines idx entry.next idx in
        let line =
        if (String.length line < maxwidth)
        then
        (
            let line = line ^ String.make (maxwidth - String.length line) ' ' in
            line
        )else line
        in
        let line = Bytes.of_string line in
        let _ = Bytes.set line idealwidth '+' in
        let line = Bytes.to_string line ^ "|" in
        Printf.printf " %s \n" line;
        loop entry.next !gl ""
    in
    loop 0 !gl ""

(* read the entire file *)
let read_file() =
  let contents = In_channel.with_open_text "/Users/anu/Documents/go/testwiki.txt" In_channel.input_all in
  contents

let pbreak_main =
  try
      let l = ref [] in
      let file_contents = read_file() in

      let l1 = effective  file_contents l in
      gl := !l1;

      let _ =  plassbreak 0 0 10 29 in
      Printf.printf "Global list %d\n" (List.length !gl) ;
      printlist !gl;

      let _ = layout file_contents 10 20 in

      ()


  with
  | exn -> Printf.printf "Unhandled exception: %s\n" (Printexc.to_string exn)
{% endhighlight OCaml %}

# Validation of the algorithm

The test is the only validation now. The algorithm is not verified based on the original
reference to the algorithm. This OCaml code is ported from C++.
There is a single bug that increases the _score_ and as a consequence breaks the paragraph
at the wrong position. But even otherwise the breaks don't seem to be perfect.

This has to be surely improved when used by an editor. That is a task for the future.

 The first +         | 
 first know+         | 
 known use +         | 
 use of    +         | 
 of the    +         | 
 the terms.+         | 
 terms.    +         | 
