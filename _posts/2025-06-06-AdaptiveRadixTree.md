---
layout: post
title: Adaptive Radix Tree
published: true
---


# tl;dr
1. The code will be gradually improved and  be committed to git finally. The version on this page is compiled but unfinished until
   I mention it here.
2. Performance considerations are not paramount here. But building lists like this again and again instread of using _Array_ is
   not at all recommended.
   {% highlight ocaml %}
     let n_4keys = List.mapi (fun j el -> if i = j then
                                   List.nth keys (i - 1)
                                  else el) keys in (* TODO Array is mutable and needed here*)
   {% endhighlight  %}
3. The algorithm seems complicated and nodes can be added wrongly. But I am trying to test is as thoroughly as possible.
4. My OCaml code is still improving. Loops I code seem to distract from the underlying logic. It should be more functional with
   proper comments.

# Adaptive Radix Tree

## Abstract Data Type

{% highlight ocaml %}

module type RadixNode = sig
type 'a t
end

 module MakeRadixNode ( RadixNode : RadixNode ) = struct
    type meta =
    | Prefix of (Bytes.t list)  * int * int
    [@@deriving show]
    and
	leaf_node =
	   KeyValue of keyvaluepair
    and
    keyvaluepair = {
	  key : Bytes.t list;
      mutable value   : int64
    }
    and
	inner_node =
		meta *
		node_type *
		Bytes.t list *
		children
    and
    node_type =
      Node4 of int
    | Node16 of int
    | Node48 of int
    | Node256 of int
    | Leaf of int
    and
	node =
	  | Inner_node of inner_node
	  | Leaf of leaf_node
      | Empty
    and
	level =
		Node of  node *
		int
    and
	tree =
		{root : node; size : int}
    and
    children = node CCArray.t
      (* [@printer *)
      (*   fun fmt arr -> fprintf fmt "%a" (CCArray.pp pp_node) arr] *)
    [@@deriving show] (* only one call to `deriving show` is not enough *)
end
{% end highlight %}

## The algorithm

{% highlight ocaml %}
open Batteries
open Bigarray
open Types
open Effect
open Effect.Deep
include Radix_intf


exception EmptyKeys


 module RADIX  = struct
    type 'a radix_node = 'a
    include MakeRadixNode (struct type 'a t = 'a radix_node end)

	let node4 = 0
	let  node16 = 1
	let  node48 = 2
	let  node256 = 3

	let  node4max = 4

	let  node16max = 16

	let node48max = 48

	let node256max = 256

	let max_prefix_len = 10


let char_list_to_byte_list cl =
    let bl  = [] in
    List.map ( fun c ->   bl @  [(Bytes.init 1 ( fun _ -> c  ))] ) cl

let count_non_empty_children children =
  let count = Array.fold_left( fun acc c ->
    match c with
    | Empty -> acc
    | _ -> acc + 1
  ) 0 children in
  Printf.eprintf "count_non_empty_children: found %d non-empty in array of length %d\n%!"
    count (Array.length children);
  count

let new_node4() =

	let b = Bytes.create max_prefix_len |> Bytes.to_seq |> List.of_seq in
    let keys = List.init 4 (fun _ -> Bytes.make 1 '\x00') in
    let children = CCArray.make 4 Empty in  (* Explicit array *)
  Printf.eprintf "DEBUG new_node4: array length=%d\n%!" (Array.length children);
  Array.iteri (fun i child ->
    match child with
    | Empty -> Printf.eprintf "  [%d] = Empty ✓\n%!" i
    | Leaf _ -> Printf.eprintf "  [%d] = Leaf ✗\n%!" i
    | Inner_node _ -> Printf.eprintf "  [%d] = Inner_node ✗\n%!" i
  ) children;

    let count = count_non_empty_children children in
    if count <> 0 then
    Printf.eprintf "ERROR: Created node4 with %d non-empty children!\n%!" count;

	let inn = (
		 (* Prefix ((List.map List.hd (char_list_to_byte_list b)), 0 , 0), (\*  Redundant *\) *)

		 Prefix (List.hd (char_list_to_byte_list b), 0 , 0),
		 Node4 node4,
         (* List.map List.hd (char_list_to_byte_list b1), *)
         keys,
         children)
	in
	inn

let new_node16() =
	let b = Bytes.create max_prefix_len |> Bytes.to_seq |> List.of_seq in
    let children = CCArray.make node16max Empty in  (* Explicit array *)
    let count = count_non_empty_children children in
    if count <> 0 then
    Printf.eprintf "ERROR: Created node4 with %d non-empty children!\n%!" count;

	let inn = (
		 Prefix (List.hd (char_list_to_byte_list b), 0 , 0),
		 Node16 node16,
         [],
		 children)
	in
	inn

let new_node48() =
	let b = Bytes.create max_prefix_len |> Bytes.to_seq |> List.of_seq in
    let children = CCArray.make node48max Empty in  (* Explicit array *)
    let count = count_non_empty_children children in
    if count <> 0 then
    Printf.eprintf "ERROR: Created node4 with %d non-empty children!\n%!" count;
		(Prefix (List.hd (char_list_to_byte_list b), 0 , 0),
   Node48 node48max, [], children)


let new_node256() =
	let b = Bytes.create max_prefix_len |> Bytes.to_seq |> List.of_seq in
    let children = CCArray.make node256max Empty in  (* Explicit array *)
    let count = count_non_empty_children children in
    if count <> 0 then
    Printf.eprintf "ERROR: Created node4 with %d non-empty children!\n%!" count;
	let inn = (
		Prefix (List.hd (char_list_to_byte_list b), 0 , 0),
		Node256 node256,                                              (* Keys can't be arbitrary *)
         [],
		 children)
	in
	inn

let trailing_zeros bitfield =

  let rec count c =
    if (Int32.logand (Int32.shift_right_logical bitfield c) (Int32.of_int 1 )) <> (Int32.of_int 1 ) then
      count (c + 1)
    else c
  in count 0

let index n key =
  match n with
  | (meta, node_type, keys, _) ->
    match node_type with
    | Node4 _ | Node16 _ ->
      (match meta with
       | Prefix (_, size, _) ->
         let rec loop j_dx =
           if j_dx < size then
             let () = Printf.printf "[  %s = %s ]"  (Bytes.to_string  key ) (Bytes.to_string (List.nth keys j_dx)) in
             if Bytes.compare (List.nth keys j_dx) key = 0 then
               Char.chr j_dx   (* return position as char *)
             else
               loop (j_dx + 1)
           else
             Char.chr 255  (* not found *)
         in
         loop 0
      )
    | Node48 _ ->
(match meta with
   | Prefix (_, _, _) ->
     let byte_key = Bytes.get_uint8 key 0 in
     let map_byte =
       Char.code (Bytes.get (List.nth keys byte_key) 0)
     in
     if map_byte = 0 then (
       Printf.printf "Node48 miss for %02X\n%!" byte_key;
       Char.chr 255
     ) else (
       Printf.printf "Node48 hit for %02X → child %d\n%!"
         byte_key (map_byte - 1);
       Char.chr (map_byte - 1)
     ))
    |  Node256 _ ->
      let () = Printf.printf "Node256 [  %c  ]" (Char.chr (Bytes.get_uint8 key 0  )) in  (* or just key.[0] *)
      (Char.chr (Bytes.get_uint8 key 0))  (* or just key.[0] *)


      (* |Node16  _ -> *)
      (*   ( match meta with *)
      (*     | Prefix (_, size , _) -> *)
      (*       let bitfield = Array1.create Int32 c_layout 1 in *)
      (*       bitfield.{0} <- Int32.of_int 0b00000000; *)
      (*       let rec loop_while j_dx= *)
      (*        if j_dx < size   then( *)
	  (*   	  if (Bytes.compare (List.nth keys j_dx) key = 0) then( *)
	  (*   		bitfield.{0} <-  Int32.logor bitfield.{0}  (Int32.shift_left 1l j_dx); *)
      (*         ); *)
      (*           loop_while ( j_dx + 1 ) *)
      (*        ) else () *)
      (*       in *)
      (*       loop_while 0; *)

	  (*       let mask =  Int32.to_int (Int32.shift_left (Int32.of_int 1) size) - 1 in *)
	  (*       bitfield.{0} <- Int32.logand  bitfield.{0} (Int32.of_int  mask); *)
	  (*      if bitfield.{0} != 0l then( *)
	  (*   	Bytes.make 1  (Char.chr (trailing_zeros bitfield.{0})) *)
      (*      ) *)
	  (*      else( *)
	  (*           Bytes.make 1 (Char.chr 255)  (\*TODO  Intended to indicate an exception now*\) *)
      (*      ) *)
      (*  ) *)
let find_child n key =
  let i = index n key in

  match n with
  | (meta, node_type, keys, children) ->
    match node_type with
    | Node4 _ | Node16 _ ->
        let idx = Char.code i in
        Printf.eprintf "find_child Node4/16: idx=%d, array_length=%d\n" idx (Array.length children);
        if idx = 255 then (
          Printf.eprintf "find_child Node4/16: not found (255)\n";
          Empty
        ) else if idx >= Array.length children then (
          Printf.eprintf "find_child Node4/16: out of bounds\n";
          Empty
        ) else (
          Printf.eprintf "find_child Node4/16: returning children[%d]\n" idx;
          let result = children.(idx) in
          (match result with
           | Empty -> Printf.eprintf "  -> was Empty\n"
           | Leaf _ -> Printf.eprintf "  -> was Leaf\n"
           | Inner_node _ -> Printf.eprintf "  -> was Inner_node\n");
          result
        )

    | Node48 _ ->
        let idx = Char.code i in
        Printf.eprintf "find_child Node48: idx=%d\n" idx;
        if idx = 255 then Empty
        else (
          let real_idx = idx in
          Printf.eprintf "find_child Node48: real_idx=%d\n" real_idx;
          if real_idx < 0 || real_idx >= Array.length children then Empty
          else children.(real_idx)
        )

    | Node256 _ ->
        Printf.eprintf "find_child Node256: using key byte directly\n";
        let idx = Char.code i in
        if idx = 255 then Empty
        else children.(Bytes.get_uint8 key 0)

    | Leaf _ ->
        Printf.eprintf "find_child: got Leaf, returning Empty\n";
        Empty

(* let find_child n key =          (\*  How do we check if n is None or Some ?*\) *)
(*                                 (\* Is that check needed ? *\) *)

(*   let i = index n key in *)
(*   Fmt.pr "Computed index = %d for key byte %c\n" *)
(*   (Bytes.get_uint8 i 0) *)
(*   (Char.chr (Bytes.get_uint8 key 0)); *)
(*    match n with *)
(* 	  | ( _, node_type, _, children ) -> *)
(*         match node_type with *)
(*         | Node4 _ *)
(*         | Node16 _ *)
(*         | Node48 _ *)
(*         | Leaf _ -> *)
(* 		let idx =  Bytes.get_uint8 i 0 in *)
(* if idx = 0 || idx > Array.length children then Empty else *)
(*             let () = Fmt.pr "Index BYTE representation :[ \\x%02X]\n"  (Char.code (Bytes.get  i 0)) in *)
(* let real_idx = idx - 1 in  (\* Node48 stores idx+1 *\) *)
(*             if real_idx < 0 || real_idx >= Array.length children then Empty *)
(*             else children.(real_idx) *)

(* 	    | Node256 _ -> *)
(* 		let idx =  Bytes.get_uint8 i 0 in *)
(* 		if idx = 255 then Empty else *)
(* 		 Array.get children (Bytes.get_uint8 key 0) (\*  TODO Exception handler*\) *)


let grow (n : inner_node ) =
	(match n with
	  | ( meta, node_type, keys, children ) ->
	   (match node_type with
       | Node4 _ ->

         let (  _,_,  _,  n_16children) = new_node16() in

           (match meta with
            | Prefix (l, i1, i2) ->
           let new_n_16keys =
            let rec loop_while old_idx new_idx modified_keys_acc =
            if old_idx <= i1 - 1  then(
			 let child = Array.get children old_idx in
             match child with
             | Empty ->
                loop_while (old_idx + 1) new_idx modified_keys_acc
             | _ ->
                let () = Array.set n_16children new_idx child in
                let modified_keys =
                    modified_keys_acc @ [List.nth keys old_idx] in

                loop_while (old_idx + 1) (new_idx + 1) modified_keys
            )
            else
                 modified_keys_acc
            in
            loop_while 0 0 [] in

let count = count_non_empty_children n_16children in
(Prefix (l, count, i2), Node16 node16, new_n_16keys, n_16children)

           )

	   | Node16 _ ->     (*Create a Node48 and set values  *)
         Printf.printf "Grow node16\n";
         let (  _, _,  _,  n_48children) = new_node48() in
         (match meta with
          | Prefix (l, i1, i2) ->
          let n_48keys = List.init 256 (fun _ -> Bytes.make 1 '\x00') in
           let map_48keys =
            let rec loop_while old_idx new_idx modified_keys_acc =
            if old_idx <= i1 - 1  then(
			 let child = Array.get children old_idx in
             match child with
             | Empty ->
                loop_while (old_idx + 1) new_idx modified_keys_acc
             | _ ->
                let () = Array.set n_48children new_idx child in
                let modified_keys =
           let key_byte = Bytes.get_uint8 (List.nth keys old_idx) 0 in
            (key_byte, new_idx + 1) :: modified_keys_acc in

                loop_while (old_idx + 1) (new_idx + 1) modified_keys
            )
            else
                 modified_keys_acc
            in
            loop_while 0 0 [] in

     let n_48keys =
       List.mapi (fun byte _ ->
         match List.assoc_opt byte map_48keys with
         | Some v -> Bytes.make 1 (Char.chr v)
         | None -> Bytes.make 1 '\x00'
       ) n_48keys
     in
       let count = count_non_empty_children n_48children in
       (Prefix (l, count, i2), Node48 node48, n_48keys, n_48children)
           )
       | Node48 _ ->
         Printf.printf "Grow node48\n";
         let (  _,_,  n_256keys,  n_256children) = new_node256() in
         (match meta with
          | Prefix (l, _, i2) ->


            let rec loop_while byte =
              if byte <= 255 then(
              let key_entry = List.nth keys byte in
              let mapped_val = Bytes.get_uint8 key_entry 0 in
              if mapped_val <> 0 then (
                let child_idx = mapped_val - 1 in
                let child = Array.get children child_idx in
                match child with
                | Empty -> loop_while (byte + 1)
                | _ ->
                    Array.set n_256children byte child;
                    loop_while (byte + 1)
              ) else
                    loop_while (byte + 1)
            ) in loop_while 0;
            let count = count_non_empty_children n_256children in
	             (
                 Prefix (l, count, i2),
		         Node256 node256,
                 n_256keys,
                 n_256children)
         )
       | Node256 _ -> n
       | Leaf _ -> n
       )
    )

let maxsize node_type =
	 match node_type with
      | Node4 _ -> node4max
      | Node16 _-> node16max
      | Node48 _-> node48max
      | Node256 _-> node256max

let rec add_child key parent child =

  (match parent with
   | (Prefix(_, size, _), _, _, children) ->
       Printf.eprintf "  parent size=%d nonempty=%d\n%!"
         size (count_non_empty_children children)
   | _ -> ());
	match parent with
	  | ( meta, node_type, keys, children ) ->
          let Prefix(_, size, _) = meta in

    if (count_non_empty_children children)== maxsize node_type then(
            let grow_n = grow parent in
            add_child key grow_n child)
    else (
	   match node_type with
       | Node4 _ ->
        let (  _, node_type,  n_4keys,  n_4children) = parent in
        (match meta with
          | Prefix (l, _, i2) ->
        let active_keys = List.filteri (fun i _ -> i < size) n_4keys in
        let idx =
            let rec loop_while id_x= (* TODO Check the spec. of 'compare' *)
            if id_x < List.length active_keys && Bytes.compare key (List.nth active_keys id_x) > 0 then(
                 loop_while (id_x + 1))
               else  id_x
             in
             loop_while 0;
             in
     let rec split_at i acc = function
       | [] -> (List.rev acc, [])
       | h::t as lst -> if i = 0 then (List.rev acc, lst) else split_at (i-1) (h::acc) t
     in
     let before, after = split_at idx [] active_keys in
     let new_keys = before @ [key] @ after in
Printf.eprintf "add_child Node4: new_keys after insert (total %d keys):\n" (List.length new_keys);
List.iteri (fun i k ->
  Printf.eprintf "  [%d]: '%s' (len=%d bytes)\n"
    i
    (Bytes.to_string k)
    (Bytes.length k)
) new_keys;
Printf.eprintf "add_child Node4: storing key byte=%02X ('%c') at idx=%d\n%!"
    (Bytes.get_uint8 key 0)
    (Char.chr (Bytes.get_uint8 key 0))
    idx;
     if size < Array.length n_4children then (

       let rec loop_while i =
          if i >=  idx + 1 then(
                let () = Array.set n_4children i (Array.get n_4children (i-1)) in
                loop_while (i + 1);
          ) else ()
      in loop_while size;
     );

        if idx < Array.length n_4children then(
          n_4children.(idx) <- child;
          let() =  Printf.eprintf "Added child at %d -> %d\n%!" idx size in
	             (
                 Prefix (l, size + 1, i2), (* TODO Increment size of the parent and child properly*)
		         node_type,
                 new_keys,
                 n_4children)
        )
        else(
            Printf.eprintf "ERROR: key_idx=%d >= array_len=%d\n%!"
            idx (Array.length n_4children);
	             (
                 Prefix (l, size, i2), (* TODO Increment size of the parent and child properly*)
		         node_type,
                 new_keys,
                 n_4children)
      ))
       | Node16 _ ->
    let (_, node_type, n16_keys, n16_children) = parent in
    (match meta with
     | Prefix (l, size, i2) ->

       let rec find_idx i =
         if i >= size then size
         else if Bytes.compare key (List.nth n16_keys i) < 0 then i
         else find_idx (i + 1)
       in
       let idx = find_idx 0 in

       let rec split_at i acc = function
         | [] -> (List.rev acc, [])
         | h::t as lst -> if i = 0 then (List.rev acc, lst) else split_at (i-1) (h::acc) t
       in
       let before, after = split_at idx [] n16_keys in
       let new_keys = before @ [key] @ after in
       Printf.eprintf "add_child Node4: storing key byte=%02X ('%c') at idx=%d\n%!"
       (Bytes.get_uint8 key 0)
       (Char.chr (Bytes.get_uint8 key 0))
       idx;
       let rec loop_while i =
          if i >=  idx + 1 then(
                let () = Array.set n16_children i (Array.get n16_children (i-1)) in
                loop_while (i + 1);
          ) else ()
      in loop_while size;
          Array.set n16_children idx  child;

          Printf.eprintf "Added child at %d -> %s\n%!" idx
            (match child with
             | Empty -> "Empty"
             | Inner_node _ -> "Inner_node"
             | Leaf _ -> "Leaf_node");

          (Prefix (l, size + 1, i2), node_type, new_keys, n16_children)
    )

(*         let (  _, node_type,  _,  n_16children) = parent in *)
(* (\*         (match meta with *\) *)
(*           | Prefix (l, i1, i2) -> *)
(* 		let idx = i1 in *)
(*         let bitfield = Array1.create Int32 c_layout 1 in *)
(*         bitfield.{0} <- Int32.of_int 0b00000000; *)
(*         let rec loop_while jdx= *)
(*             if idx < jdx then *)
(* 			if Bytes.compare (List.nth keys jdx)  key >= 0 then *)
(* 		    bitfield.{0} <- Int32.logor  bitfield.{0} (Int32.shift_left (Int32.of_int 1) jdx) *)
(* 			else *)
(*             loop_while (jdx + 1); *)
(*         in *)
(*         loop_while 0; *)
(* 		let mask =  Int32.to_int (Int32.shift_left (Int32.of_int 1) i1) - 1 in *)
(* 		bitfield.{0} <- Int32.logand  bitfield.{0} (Int32.of_int  mask); *)
(*         let idx = *)
(* 		if (Int32.lognot bitfield.{0} ) == 0l then( *)
(* 			 trailing_zeros bitfield.{0} *)
(* 		) else idx in *)


| Node48 _->
      Printf.printf "Node48";
      let (_, node_type, n_48keys, n_48children) = parent in
      (match meta with
      | Prefix (l, _, i2) ->
          let rec find_empty i =
            if i >= Array.length n_48children then failwith "Node48 full"
            else if n_48children.(i) = Empty then i
            else find_empty (i + 1)
          in
          let idx = find_empty 0 in

          let byte_key = Bytes.get_uint8 key 0 in
          Printf.eprintf "add_child Node48: storing mapping for byte=%02X ('%c')\n%!"
          byte_key
          (Char.chr byte_key);
          let n_48keys =
            List.mapi (fun i el ->
              if i = byte_key then Bytes.make 1 (Char.chr (idx + 1))
              else el
            ) n_48keys
          in

          n_48children.(idx) <- child;

          Printf.eprintf "Added child at idx=%d for byte=%d\n%!" idx byte_key;

          (Prefix (l, size + 1, i2), node_type, n_48keys, n_48children)
      )

| Node256 _->
    Printf.printf "Node256";
    let byte_key = Bytes.get_uint8 key 0 in
Printf.eprintf "add_child Node48: storing mapping for byte=%02X ('%c')\n%!"
    byte_key
    (Char.chr byte_key);
    let (_, node_type, n_256keys, n_256children) = parent in
    match meta with
    | Prefix (l, _, i2) ->
    n_256children.(byte_key) <- child;
    (Prefix (l, size + 1, i2), node_type, n_256keys, n_256children)
  )

let  add_child_logged key parent child =
  let count_nonempty = count_non_empty_children (match parent with (_,_,_,children) -> children | _ -> [||]) in
  let keys_list =
    match parent with
    | (_, _, keys, _) -> List.map (fun b -> Printf.sprintf "%02X" (Bytes.get_uint8 b 0)) keys
    | _ -> []
  in
  let () = Printf.eprintf "add_child called: key=%02X parent_size=%d nonempty_children=%d keys=[%s]\n%!"
      (Bytes.get_uint8 key 0)
      (match parent with
       | (Prefix(_, s, _), _, _, _) -> s
       | _ -> -1)
      count_nonempty
      (String.concat "," keys_list)
  in

  (* Call original add_child *)
  let updated_parent = add_child key parent child in

  (* Log the result *)
  let updated_keys =
    match updated_parent with
    | (_, _, keys, _) -> List.map (fun b -> Printf.sprintf "%02X" (Bytes.get_uint8 b 0)) keys
    | _ -> []
  in
  let updated_nonempty =
    match updated_parent with
    | (_, _, _, children) -> count_non_empty_children children
    | _ -> -1
  in
  Printf.eprintf "add_child returned: updated_keys=[%s] nonempty_children=%d\n%!"
    (String.concat "," updated_keys) updated_nonempty;

  updated_parent


let rec minimum node =

	match node with
	  |Inner_node inn ->
       (match inn with
        | ( _, node_type, keys, children )->
	    (match node_type with
	     | Node4 _ |Node16 _ ->
		     minimum (Array.get  children 0)
         | Node48 _ ->
          let i =
            let rec loop_while idx =
            if Bytes.compare (List.nth keys idx)  (Bytes.make 1 (Char.chr 0)) == 0 then
              loop_while (idx + 1 )
            else
              idx
            in
            loop_while 0
            in
            let child = Array.get children   (Int.of_string (Bytes.to_string (List.nth keys i)) - 1)    in
            minimum child

        | Node256 _ ->
            let i =
            let rec loop_while idx =
            if (Bytes.compare (List.nth keys idx) (Bytes.make 1  '\x00')) == 0 then
              loop_while (idx + 1 )
            else
              idx
            in
            loop_while 0
            in
            let child = Array.get children   (Int.of_string (Bytes.to_string (List.nth keys i)) - 1)    in
            minimum child
	    | Leaf _  -> node
        )
       )



let compare_keys key key1 =
 (* List.iter ( fun k -> *)
 (*    Fmt.pr "Searching BYTE representation :[ \\x%02X]\n" (Char.code (Bytes.get  k 0)) *)
 (*  ) key; *)
 (* List.iter ( fun k -> *)
 (*    Fmt.pr "Searching BYTE representation :[ \\x%02X]\n" (Char.code (Bytes.get  k 0)) *)
 (*  ) key1; *)
 if List.length key <> List.length key1 then
    -1
  else
   let rec compare comp elem elem1 =
    match elem, elem1 with
      | [], [] -> comp
      | hd :: tl, hd1 :: tl1->
            let comp =  Bytes.compare hd hd1 in
            if comp == 0 then
              compare comp tl tl1
            else
              comp
      | _, _ -> raise EmptyKeys
    in compare 0 key key1

let  prefix_match_index1 inner_node key level =
  match inner_node with
	|Inner_node inn ->
    let id_x =
    (match inn with
	| ( meta, _,_,_ )->
        match meta with
          | Prefix (prefix, _, prefix_len) ->
             let rec loop_while idx pref =
               if idx < prefix_len && (level + idx) < List.length key &&
                  (Bytes.equal (List.nth key (level + idx))  (List.nth pref idx)) then(

                 if idx == (max_prefix_len-1) then(
                     match (minimum inner_node) with
                       |Leaf l ->
                        match l with
                        |KeyValue kv ->
                         loop_while (idx + 1 ) (List.filteri (fun i _ -> i >= level &&
                                                                         i < (List.length kv.key )) kv.key)
                       |_ -> failwith "prefix_match_index1"
                 )
                 else loop_while (idx + 1 ) pref
                )
                else idx
             in
             loop_while 0 prefix
    )
    in id_x
let  prefix_match_index l kv level =
    match l with
    |KeyValue kv1 ->

    let limit = Int.min (List.length kv1.key - level)
                        (List.length kv.key) - level
    in
    let result =
    let rec loop_while i  =
      if i < limit then(
		if (Bytes.compare (List.nth kv1.key  (level + i))
		   (List.nth kv.key  (level + i)) <> 0) then(
        i
        )
        else
        loop_while (i + 1)
      )else i;
    in
    loop_while 0;
    in result

let copy key_list_src key_list_dest level =
   List.mapi (fun j el -> if j <= level then
                          el
                          else (List.nth  key_list_dest j)) key_list_src(* TODO Array is mutable*)

let terminate key =

  let result = match (List.find_index (fun elt ->
    Bytes.compare (Bytes.make 1 '\x00') elt == 0) key) with
    | Some _ -> key
    | None -> List.append key [(Bytes.make 1 '\x00')]
  in

  result
(* Get the byte at 'level' from a flattened key *)
let rec insert (tr : tree) node key value level  =

  (match node with
  | Empty ->
    let new_key = List.map( fun x -> x ) key in
    let kv = {key = new_key; value = value }in
    (true,  Leaf (KeyValue  kv))
  | Leaf l ->
             (match l with
              | leaf_node  ->
             (match leaf_node with | KeyValue kv ->
              Printf.eprintf "INSERT: Leaf case - existing key=%s, new key=%s\n%!"
                    (String.concat "" (List.map Bytes.to_string kv.key))
                    (String.concat "" (List.map Bytes.to_string key));

             if compare_keys  kv.key  key == 0 then(
             kv.value <- value;
             (true,  Leaf (KeyValue  kv))
             ) else(
             Printf.eprintf "INSERT: Splitting leaf, creating Node4\n%!";
             let new_key = List.map( fun x -> x ) key in (*  Create a new leaf*)
             let kv_new = {key = new_key; value = value }in
             let new_leaf = KeyValue kv_new in
             let limit = prefix_match_index l kv_new  level in
             Printf.eprintf "DEBUG: level=%d, limit=%d, new_level will be=%d\n%!"
               level limit (level + limit);
             let new_node = new_node4() in
             (match new_node with
             | ( meta, node_type, keys, children )->
              let count = count_non_empty_children children in
              if count <> 0 then
                Printf.eprintf "WARNING: new_node4 has %d non-empty children!\n%!" count;
               (match meta with
                   | Prefix (prefix, i1, _) ->
                     (* copy_bytes prefix key level; *)
                     let shared_prefix = List.filteri (fun i _ ->
                             i >= level && i < (level + limit)
                           ) key in

                     Printf.eprintf "INSERT: shared prefix length=%d\n%!" (List.length shared_prefix);

                     let new_level = level + limit in
                     Printf.eprintf "INSERT: new_level=%d\n%!" new_level;
                     Printf.eprintf "INSERT: old_key length=%d, new_key length=%d\n%!"
                        (List.length kv.key) (List.length key);
                     if new_level >= List.length kv.key || new_level >= List.length key then (
                       Printf.eprintf "ERROR: new_level=%d exceeds key length!\n%!" new_level;
                       failwith "Key too short for level"
                     );

                     let old_key_byte = List.nth kv.key new_level in
                     let new_key_byte = List.nth key new_level in

                     Printf.eprintf "INSERT: old leaf goes at byte=%02X\n%!"
                       (Bytes.get_uint8 old_key_byte 0);
                     Printf.eprintf "INSERT: new leaf goes at byte=%02X\n%!"
                       (Bytes.get_uint8 new_key_byte 0);

                     let parent = (
                       Prefix (List.hd (char_list_to_byte_list
                         (Bytes.create max_prefix_len |> Bytes.to_seq |> List.of_seq)),
                         0,  (* size starts at 0 *)
                         limit),  (* prefix_len is the shared part *)
                       Node4 node4,
                       keys,
                       children
                     ) in
                     Printf.eprintf "DEBUG: old leaf key=%s, new leaf key=%s\n"
                       (String.concat "" (List.map Bytes.to_string kv.key))
                       (String.concat "" (List.map Bytes.to_string kv_new.key));

                     (* ... later when calling add_child ... *)
                     Printf.eprintf "DEBUG: Adding old leaf with key byte=%02X\n"
                       (Bytes.get_uint8 old_key_byte 0);
Printf.eprintf "INSERT Leaf split: new_level=%d, old_key_byte='%s' (byte=%02X), new_key_byte='%s' (byte=%02X)\n%!"
  new_level
  (Bytes.to_string old_key_byte)
  (Bytes.get_uint8 old_key_byte 0)
  (Bytes.to_string new_key_byte)
  (Bytes.get_uint8 new_key_byte 0);

                     let updated_node = add_child_logged old_key_byte parent node in

                     Printf.eprintf "DEBUG: Adding new leaf with key byte=%02X\n"
                       (Bytes.get_uint8 new_key_byte 0);
                     let changed_node = add_child_logged new_key_byte updated_node (Leaf new_leaf) in
                     (false,Inner_node changed_node )
                )
             ))))
    | Inner_node inn ->
      match inn with
        ( meta_in, node_type_in, keys_in, children_in ) as inn->
                 match meta_in with
                   | Prefix (prefix_in, i1_in, prefix_len_in) ->
                      if prefix_len_in != 0 then(
                        let prefix_match_result = prefix_match_index1 node key level in
                        if prefix_match_result != prefix_len_in then(
                          let new_node = new_node4() in

                         (match new_node with
                          | ( meta, node_type, keys, children )->
                              let count = count_non_empty_children children in
                                  Printf.eprintf "DEBUG: new_node4 created with %d non-empty children!\n%!" count;
                                  if count > 0 then
                                    Printf.eprintf "ERROR: new_node4 should have 0 children!\n%!";
                              match meta with
                              | Prefix (new_prefix, i1, prefix_len) ->

                                let copied_prefix = copy prefix_in new_prefix prefix_match_result in

                                let new_node4=
                                  (
                                  Prefix (copied_prefix, i1,  prefix_match_result),
                                  node_type,
                                  keys,
                                  children) in
                                  if prefix_len < max_prefix_len then(
                                      let _ = add_child (List.nth prefix_in prefix_match_result) new_node4 node in
                                      let copied_prefix = copy prefix_in (List.filteri (fun i _ -> i >= (prefix_match_result+1) && i < (List.length prefix_in )) prefix_in)  (Int.min prefix_len_in max_prefix_len) in
                                      let new_node4=
                                               (
                                               Prefix (copied_prefix, i1_in,  prefix_len_in - (prefix_match_result + 1)) ,
                                               node_type_in,
                                               keys_in,
                                               children_in)
                                  in
                                  let kv = {key = key; value = value }in
                                  let new_leaf = KeyValue kv in
                                  let changed_node = add_child_logged (List.nth key (level + prefix_match_result)) new_node4 (Leaf new_leaf) in
                                  (false, Inner_node changed_node)
                                  )
                                  else(
                                      let prefix_len_in = prefix_len_in - (prefix_match_result + 1) in
                                      match (minimum node) with
                                        |Leaf l ->
                                         match l with
                                         |KeyValue kv ->
                                      let _ = add_child (List.nth kv.key (level + prefix_match_result)) new_node4 node in

                                      let copied_prefix = copy prefix_in (List.filteri (fun i _ -> i >= (prefix_match_result + level + 1) && i < (List.length kv.key )) kv.key)  (Int.min prefix_len_in max_prefix_len) in
                                      let new_node4=
                                               (
                                               Prefix (copied_prefix, i1_in,  prefix_len_in) ,
                                               node_type_in,
                                               keys_in,
                                               children_in)
                                         in
                                         let kv = {key = key; value = value }in
                                         let new_leaf = KeyValue kv in
                                         let changed_node = add_child (List.nth key (level + prefix_match_result)) new_node4 (Leaf new_leaf) in
                                         (false, Inner_node changed_node)
                                  )
                         )
                        )
                        else (false, Inner_node inn)

             ) else

             let level = level + prefix_len_in in

             let kv = {key = key; value = value }in
             let new_leaf = KeyValue kv in (*TODO Remove duplicate Construction of new_leaf*)
          let next = find_child inn (List.nth  key level ) in
    Printf.eprintf "INSERT Inner_node: level=%d, List.nth key level = '%s' (first byte=%02X)\n%!"
      level
      (Bytes.to_string (List.nth key level))
      (Bytes.get_uint8 (List.nth key level) 0);
             (match next with
             | Empty  ->  let modified_node = add_child_logged (List.nth  key level ) inn (Leaf new_leaf) in (false,Inner_node modified_node)
             | _ ->  insert tr  next key value (level+1)

             )
    )

(* Size is not updated now TODO  *)
let insert_tree tree key value =
	let key = terminate key in

	let updated_tree = insert tree tree.root key value 0 in
	match (updated_tree) with
	|_, node->
    node

let rec search node key level =

	match ( node ) with
    | Leaf l ->
             (match l with
              | leaf_node  ->
              match leaf_node with | KeyValue kv ->
			  let result = compare_keys key kv.key in
              Printf.printf "Search key %c compared with %c " (Char.chr (Bytes.get_uint8 (List.nth key 0) 0))
                                                              (Char.chr (Bytes.get_uint8 (List.nth kv.key 0) 0));
			  if result == 0 then
				Some kv.value
              else None
             )
    | Inner_node n ->
         (match n with
          |( meta, _,_,_ ) ->
                 (match meta with
                 | Prefix (_, _, prefix_len) ->
                 let pmi =  prefix_match_index1 node key level in
                 let() = Printf.printf " prefix_match_index1 node key level %d  prefix_len %d\n" pmi prefix_len in
                 if prefix_match_index1 node key level != prefix_len then
                     None
                 else
                     let level = level + prefix_len in
                     let () = Fmt.pr "\nLength of key %d\n "  (List.length key) in
                     let () = List.iter ( fun k ->
                        Fmt.pr "[ %c]\n" (Char.chr (Bytes.get_uint8  k 0))
                      ) key in
                     Printf.printf "Level %d\n" level;
                     if level >= (List.length key) then
                       None
                     else
                     let child = find_child n (List.nth  key level ) in
                     Printf.printf "search: find_child returned: ";
                     (match child with
                      | Empty -> Printf.printf "Empty\n"
                      | Leaf _ -> Printf.printf "Leaf\n"
                      | Inner_node _ -> Printf.printf "Inner_node\n");
                     match ( child ) with
                     | Empty -> None
                     | _ -> search child key (level + 1)
                     )
        )
     | Empty -> None

let log_keys node =
     (match node with
      | Inner_node inn ->
        (match inn with
           |(  Prefix(_, _, _), _, keys,_ ) ->
Printf.printf "Searching in Node keys: [";
           List.iter ( fun k ->
               Fmt.pr "BYTE representation :[ %s]\n" (Bytes.to_string k)
             ) keys;
Printf.printf "]\n%!";)
      |Leaf _ -> Printf.printf "Searching leaves";
      |Empty -> Printf.printf "Searching empty nodes";
    )


type _ Effect.t +=
  | Log_keys : node -> unit Effect.t

let search_after_terminating node key level =
  ignore ( perform (Log_keys node) );
  search node (terminate  key) level


let search_with_log_handler  node key level =
  match_with (fun () ->
                search_after_terminating node key level )
  ()
  {
    retc =
      (fun result ->  result);
    exnc = (fun e -> raise e);
    effc =
      (fun (type c) (e : c Effect.t) ->
        match e with
        | Log_keys node ->
            Some
              (fun (k : (c, _) continuation) ->
              let () = log_keys node in
              continue k ()
              )
      | _ -> None
      );
 }


end

module RADIXOp = RADIX

{% endhighlight %}

## Test to view the tree

This is a convenient way to view the tree for debugging but not a testing procedure.

{% highlight ocaml %}
open Bitcask__Adaptive_radix_tree.RADIXOp
let%expect_test _=
  let n = new_node4() in
	match n with
	  | ( _, _, _, children ) ->
         match children with
          |  node ->
            Array.iter (fun n -> Printf.printf "%s" (Format.asprintf "%a" pp_node n)) node;
  [%expect {|
    DEBUG new_node4: array length=4
      [0] = Empty ✓
      [1] = Empty ✓
      [2] = Empty ✓
      [3] = Empty ✓
    count_non_empty_children: found 0 non-empty in array of length 4
    Types.MakeRadixNode.EmptyTypes.MakeRadixNode.EmptyTypes.MakeRadixNode.EmptyTypes.MakeRadixNode.Empty
    |}]


{% endhighlight %}

Since this is being tested in stages I will add which test is reasonably good.

1. A _node4_ is added
2. _node4_ grows to _node16, _node16 grows to _node48_ .
3. ~~When the fresh nodes are created initially the _Bytes_ list should not contain _\x00_ bytes. This is considered a valid
   value by the code and the number of keys increase. This is a bug now.~~
4. _size_ and number of _children_ seem to match.



