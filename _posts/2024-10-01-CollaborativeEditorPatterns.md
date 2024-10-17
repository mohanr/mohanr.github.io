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

let final_state l (start, idx) =

  let e = {first = start; last = idx; next = -1; score = -1} in
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
                           ->(idx + 1,true)) (0,false) text
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


type _ Effect.t += Plass_break :  int -> unit Effect.t

let rec plassbreak indent  idx idealwidth maxwidth =
    (* Printf.printf "Size of list  in plassbreak is %d\n " (List.length !gl); *)

   if idx < (List.length !gl) then(
    let jdx = idx + 1 in
    let lastrecord = List.nth !gl idx in
    Printf.printf "let record = List.nth !gl idx in - %d\n" idx;
    let llen    = ref (lastrecord.last - lastrecord.first) in
    let bscore  = idealwidth - !llen in
    let bscore  = ref (bscore * bscore) in
    let btail   = ref jdx in
    let _ =
    List.fold_left (fun acc entry ->

   if acc < (List.length !gl) then(
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
                plassbreak  (indent + 1) acc idealwidth maxwidth;

              let
                record = List.nth !gl acc  in
                if ((!lscore + score) < !bscore) then(
                  bscore  := !lscore + record.score;
                  btail   := acc;
                );

           acc + 1;
           )
     )else acc
      ) jdx !gl in

  let record = List.nth !gl idx in
  Printf.printf "let record = List.nth !gl idx in - %d\n" idx;
  record.next <- !btail;
  record.score <- !bscore;
  if (record.next + 1) = List.length !gl then(
       record.score <- 0;
  )
  )

let pbreak  () =

  match_with (fun () -> plassbreak 0 0 10 29)
()
  { effc = (fun (type c) (eff: c Effect.t) ->
      match eff with
      | Plass_break s -> Some (fun (k1: (c,_) continuation) ->
              Printf.printf "Plass Break %d\n" s ;
              (* continue k () *)
          )
      | _ -> None
  );
  exnc = (function
        | e -> raise e
  );
  retc = fun _ -> failwith "Fatal error"
  (* retc = (fun res -> Printf.printf "Computation returned: \n") *)

 }

let rec loop_while line text lines idx next acc =
    Printf.printf "%s\n" line;
    if acc > next || (acc + 1) >= List.length !gl then
        line
    else
        let {first; last; _} = List.nth lines acc in
        if (last - first) <= 0 then
            line
        else
            let new_line =
                line ^ (if acc == idx then "" else " ") ^
                String.sub text first (last - first)
            in
            loop_while  new_line text lines idx next (acc + 1)


let  layout text idealwidth maxwidth =

 let rec loop idx lines line =
     Printf.printf "loop %s\n" line;
    if idx < List.length !gl then

        let entry = List.nth lines idx in
        let line = loop_while line text lines idx entry.next idx in
        if (String.length line < maxwidth)
        then
        (
            let line = String.make (maxwidth - String.length line) ' ' in
            Printf.printf "%s\n" line;
        )
        else(
            let subline = String.sub line 0 idealwidth in
            let subline1 = String.sub line idealwidth (String.length line - idealwidth) in
            let line = subline ^ "+" ^ subline1 ^ "|" in
            Printf.printf "else %s" line;
            loop entry.next !gl line
        )
    in
    loop 0 !gl ""

(* read the entire file *)
let read_file() =
  let contents = In_channel.with_open_text "/Users/anu/Documents/go/wiki.txt" In_channel.input_all in
  contents

let pbreak_main =
  try
      let l = ref [] in
      let file_contents = read_file() in

      let l1 = effective  file_contents l in
      gl := !l1;

      pbreak();
      Printf.printf "Global list %d\n" (List.length !gl) ;

      let _ = layout file_contents 10 20 in

      ()

      (* List.iter( fun s -> Printf.printf "%d %d %d %d\n" s.first s.next s.last s.score ) !l *)

  with
  | exn -> Printf.printf "Unhandled exception: %s\n" (Printexc.to_string exn)
{% endhighlight OCaml %}
