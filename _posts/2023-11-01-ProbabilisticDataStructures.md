---
layout: post
title: Probabilistic and other Data Structures
published: true
toc: true
---

# tl;dr
1. The code will be gradually improved and  be committed to git finally.
2. Performance considerations are not paramount here.
3. The language is OCaml and it is imperative even though I will attempt to use functional Data structures.
4. The repository is [this](https://github.com/mohanr/Algorithms)

# Development Environment

![image-title-here](../images/dev_ide.png){:class="img-responsive"}

![image-title-here](../images/dune_auto_promote.png){:class="img-responsive"}


# Probabilistic and other Data Structures

## Bloom Filter

{% highlight ocaml %} 

 let jenkins ss : int32 =
   let rec hash_accu ( accu, l ):int32  =
    match l with
      | [] ->
        let hs = Int32.add accu (Int32.shift_left accu 3) in
        let hs1 = Int32.logxor hs (Int32.shift_right_logical hs 11) in
        Int32.add (Int32.shift_left hs1 15) hs1
      | hd :: tl ->
        let h = Int32.add accu hd in
        let accu = Int32.add h (Int32.shift_left h 10) in
        hash_accu (Int32.logxor accu (Int32.shift_right_logical accu 6), tl)
     (*  | [] -> *)
    (*           let hs = accu + (accu lsl 3) in *)
    (*           let hs1 = hs lxor (hs lsr 11) in *)
    (*           Int32.of_int (hs1 + (hs1 lsl 15)) *)
    (* | hd :: tl ->let h = accu + hd in *)
    (*              let accu = h + (h lsl 10) in *)
    (*   hash_accu ((accu lxor (accu lsr 6) ), tl) *)
   in
   hash_accu  ((Int32.of_int 0 ),ss)

{% endhighlight %} 

### Initial set of tests

As I mentioned I am not considering space allocation here as the focus is on
working code.

{% highlight ocaml %} 


let string_to_print_list s =

  let str = s |> String.to_seq |> List.of_seq in
  let int_list = List.map int_of_char str in
  List.iter (fun c -> Printf.printf "%d\n" c) int_list


let string_to_int_list s =

  let str = s |> String.to_seq |> List.of_seq in
  let int_list = List.map int_of_char str in
  List.map (fun c -> Int32.of_int c) int_list

let%expect_test _=
  let hash = Bloomfilter.Ds.jenkins (string_to_int_list "Hello") in
  Printf.printf "%d\n" (Int32.to_int  hash);
  [%expect {| 1901914092 |}]

let%expect_test _=
  string_to_print_list "Hello";
  [%expect {|
    72
    101
    108
    108
    111 |}]
{% endhighlight %} 

### Initial version with a list of hash functions.

{% highlight ocaml %} 
type 'hf element =
  { value : 'hf
  ; mutable next : 'hf element option
  }
type 'hf  t = 'hf element option ref 

let insert_hashfunc t value =
  let new_hashfunc = { next = !t; value } in
  (match !t with
   | Some old_hashfunc  -> old_hashfunc.next
                             <- Some new_hashfunc
   | None -> ());
  t := Some new_hashfunc;
  new_hashfunc

{% endhighlight %} 

### Test
{% highlight ocaml %} 
let%expect_test "hash" =
  let empty_list() : 'hf Bloomfilter.Ds.t = ref None in
  let l = empty_list() in
  let hf = Bloomfilter.Ds.insert_hashfunc l Bloomfilter.Ds.jenkins in
  let hash = hf.value (string_to_int_list "Hello") in
  Printf.printf "%d\n" (Int32.to_int  hash);
  [%expect {| 1901914092 |}]

{% endhighlight %} 

### Test for Bit set and get

{% highlight ocaml %} 
let%expect_test "bitset" =
  let empty_list() : 'hf Bloomfilter.Ds.t = ref None in
  let l = empty_list() in
  let hf = Bloomfilter.Ds.insert_hashfunc l Bloomfilter.Ds.jenkins in
  let bit = Bloomfilter.Ds.set_indices (Bloomfilter.Ds.create_filter 9) "Hello" hf.value
  in
  Batteries.BitSet.print (BatInnerIO.output_channel stdout) bit ;
  [%expect {| 0000000000001000 |}]

let%expect_test "bitget" =
  let empty_list() : 'hf Bloomfilter.Ds.t = ref None in
  let l = empty_list() in
  let hf = Bloomfilter.Ds.insert_hashfunc l Bloomfilter.Ds.jenkins in
  let bit = Bloomfilter.Ds.get_indices (Bloomfilter.Ds.create_filter 9) "Hello" hf.value in
  Printf.printf "%s\n" (string_of_bool bit);
  [%expect {| true |}]

{% endhighlight %} 

### Bit set and get

The code will be further refactored and committed to my repository.

{% highlight ocaml %} 
let set_indices filt  element hf =
  let length = Batteries.BitSet.capacity filt.bits in
  let hash = hf (string_to_int_list element) in
  let () = Batteries.BitSet.set filt.bits ((Int32.to_int hash) mod length) in
  filt.bits


let get_indices filt  element hf =
  let length = Batteries.BitSet.capacity filt.bits in
  let hash = hf (string_to_int_list element) in
  let () = Batteries.BitSet.set filt.bits ((Int32.to_int hash) mod length) in
  let bit = Batteries.BitSet.mem filt.bits ((Int32.to_int hash ) mod length) in
  bit
{% endhighlight %} 

# An implementation from the source of RocksDB

RocksDB(rocksdb.org) is an embeddable persistence key-value store 

This is the code based on research papers intended for serious production systems. I've tried to port one
of their implementations to OCaml. I am building gradually.

This particular implementation found in _RocksDB_ source is considered legacy as they have a better implementation.
Nevertheless I have attempted to port it to OCaml.

This is a simple test to convince myself that OCaml _Int32_ has to be shift_right_logical to avoid overflow that result in a negative
number. _Int64_ doesn't need this as it is bigger. I think it is not apparent as it is bigger.


{% highlight ocaml %} 

        let  high : int32 = 2100000000l in
        let  low : int32 = 2000000000l in
        Printf.printf "mid using >>> 1 = %ld mid using / 2   = %ld"
          (Int32.shift_right_logical (Int32.add low  high) 1) (Int32.div (Int32.add low high)  (Int32.of_int 2)) ;

{% endhighlight %} 

> mid using >>> 1 = 2050000000 mid using / 2   = -97483648

> This should be tested thoroughly as _unsigned_ integers are absent in OCaml. 
{% highlight ocaml %} 


open Batteries

module type BLOOM_MATH = sig

  val standard_fprate :  float -> float -> float
  val finger_print_fprate : float -> float -> float
  val cache_local_fprate : float -> float -> float -> float
  val independent_probability_sum  :  float -> float -> float

end

module  Bloom : BLOOM_MATH = struct

  let standard_fprate bits_per_key num_probes : float =
     Float.pow (1. -. Float.exp (-. num_probes /. bits_per_key)) num_probes

  let cache_local_fprate bits_per_key num_probes
                                 cache_line_bits =
    if bits_per_key <= 0.0 then
      1.0
    else

    let keys_per_cache_line = cache_line_bits /. bits_per_key in
    let keys_stddev = sqrt keys_per_cache_line in
    let crowded_fp = standard_fprate (
        cache_line_bits /. (keys_per_cache_line +. keys_stddev)) num_probes in
    let uncrowded_fp = standard_fprate (
        cache_line_bits /. (keys_per_cache_line -. keys_stddev)) num_probes in
    (crowded_fp +. uncrowded_fp) /. 2.

  let finger_print_fprate num_keys fingerprint_bits : float =
    let inv_fingerprint_space = Float.pow 0.5 fingerprint_bits in
    let base_estimate = num_keys *. inv_fingerprint_space in
    if base_estimate > 0.0001 then
      1.0 -. Float.exp (-.base_estimate)
    else
      base_estimate -. (base_estimate *. base_estimate *. 0.5)

  let independent_probability_sum rate1 rate2 =
    rate1 +. rate2 -. (rate1 *. rate2)

end

   open Bloom

   type 'bloombits filter =
   {
     bits : Batteries.BitSet.t
   }

   let estimated_fprate keys bytes num_probes =
        let bits_per_key = 8.0 *. bytes /. keys in
        let filterRate = cache_local_fprate bits_per_key num_probes 512. in (* Cache line size is 512 *)
        let filter_rate  = filterRate +. 0.1 /. (bits_per_key *. 0.75 +. 22.) in
        let finger_print_rate = finger_print_fprate keys 32. in
        independent_probability_sum filter_rate finger_print_rate

   let  getline (h:int32)  (num_lines:int32) : int32 =
         Int32.rem h  num_lines

   let add_hash filt (h:int32)  (num_lines:int32) num_probes  (log2_cacheline_bytes:int) =


        let log2_cacheline_bits = Int32.add (Int32.of_int log2_cacheline_bytes)  (Int32.of_int 3) in
        let  base_offset = Int32.shift_left (getline h num_lines)  log2_cacheline_bytes in
        let delta = Int32.logor (Int32.shift_right_logical h  17)
                    (Int32.shift_left h  15) in

        let rec probe i  numprobes base_offset =
            let log2c = Int32.shift_left (Int32.of_int 1) (Int32.to_int log2_cacheline_bits)   in
            let bitpos = Int32.sub  log2c  (Int32.of_int 1) in
            let byteindex = (Int32.add base_offset  (Int32.div bitpos  (Int32.of_int 8))) in
            let () = Batteries.BitSet.set filt.bits (Int32.to_int (Int32.logor byteindex (Int32.shift_left (Int32.rem bitpos  (Int32.of_int 8)) 1))) in
            if i < num_probes then
              probe (i + 1) numprobes base_offset
            else
              (Int32.add h delta)
        in  probe 0 num_probes base_offset

        (* Recommended test to just check the effect of logical shift on int32. *)
        (* int64 doesn't seem to need it *)

        (* let  high : int32 = 2100000000l in *)
        (* let  low : int32 = 2000000000l in *)
        (* Printf.printf "mid using >>> 1 = %ld mid using / 2   = %ld" *)
        (*   (Int32.shift_right_logical (Int32.add low  high) 1) (Int32.div (Int32.add low high)  (Int32.of_int 2)) ; *)


    let hash_maymatch_prepared filt h  num_probes offset log2_cacheline_bytes =
        let log2_cacheline_bits = Int32.add (Int32.of_int log2_cacheline_bytes)  (Int32.of_int 3) in
        let delta = Int32.logor (Int32.shift_right_logical h  17)
                    (Int32.shift_left h  15) in

        let rec probe h i  numprobes base_offset =
            let log2c = Int32.shift_left (Int32.of_int 1) (Int32.to_int log2_cacheline_bits)   in
            let bitpos = Int32.sub  log2c  (Int32.of_int 1) in
            let byteindex = (Int32.add base_offset  (Int32.div bitpos  (Int32.of_int 8))) in
            let () = Batteries.BitSet.set filt.bits (Int32.to_int (Int32.logor byteindex
                                                                     (Int32.shift_left (Int32.of_int 1)
                                                                        (Int32.to_int (Int32.rem bitpos  (Int32.of_int 8))) ))) in
            if i < num_probes then
              let h = (Int32.add h delta) in
              probe h (i + 1) numprobes base_offset;
        in  probe  h 0 num_probes offset


    let hash_may_match filt h num_lines num_probes  log2_cacheline_bytes =
        let  base_offset = Int32.shift_left (getline h num_lines)  log2_cacheline_bytes in
        hash_maymatch_prepared filt h num_probes  base_offset log2_cacheline_bytes
{% endhighlight %} 

# An attempt to port _Murmurhash_ to OCaml to test the Bloom filter.


> This should be tested thoroughly as _unsigned_ integers are absent in OCaml.
> I think that this is correct based on some tests.



{% highlight ocaml %} 
let murmurhash chunks len seed =
  let c1 =  0xcc9e2d51l in
  let c2 =  0x1b873593l in
  let r1:int32 = (Int32.of_int 15) in
  let r2:int32 = (Int32.of_int 13) in
  let m = (Int32.of_int 5) in
  let n =  (Int32.of_string "0xe6546b64") in
  let h = ref Int32.zero in
  let k = ref Int32.zero in
  let l = Int32.div len (Int32.of_int 4) in
  h := seed;

 (* Printf.eprintf " %ld"  l; *)

 for i = 0 to (Int32.to_int l) - 1 do
  k :=
  Int32.logor
       (Int32.logor
         (Int32.of_int (Bytes.get_uint8 chunks (i * 4)))
         (Int32.shift_left (Int32.of_int (Bytes.get_uint8 chunks ((i * 4)+ 1))) 8))
       (Int32.logor
         (Int32.shift_left (Int32.of_int (Bytes.get_uint8 chunks ((i * 4)+ 2))) 16)
         (Int32.shift_left (Int32.of_int (Bytes.get_uint8 chunks ((i * 4)+ 3))) 24));

  k := Int32.mul !k c1 ;
  k := Int32.logor (Int32.shift_left !k  (Int32.to_int r1))  (Int32.shift_right_logical !k  (Int32.to_int (Int32.sub (Int32.of_int 32)  r1)));
  k := Int32.mul !k c2;

  h := Int32.logxor !h !k;
  h := Int32.logor (Int32.shift_left !h  (Int32.to_int r2))  (Int32.shift_right_logical !h  (Int32.to_int (Int32.sub (Int32.of_int 32)  r2)));
  h := Int32.add ( Int32.mul !h m)  n;

  done;

  let k = ref (Int32.of_int 0) in
let tail = Int32.to_int (Int32.mul l 4l) in
let l = (Int32.to_int len) - tail in

if l >= 3 then k := Int32.logxor !k (Int32.shift_left (Int32.of_int (Bytes.get_uint8 chunks (tail + 2))) 16);
if l >= 2 then k := Int32.logxor !k (Int32.shift_left (Int32.of_int (Bytes.get_uint8 chunks (tail + 1))) 8);
if l >= 1 then begin
  k := Int32.logxor !k (Int32.of_int (Bytes.get_uint8 chunks tail));

  (* if l >= 3l then k :=  Int32.logxor  !k (Int32.shift_left  (Int32.of_int (Bytes.get_uint8 chunks  2)) 16); *)
  (* if l >= 2l then k := Int32.logxor !k (Int32.shift_left (Int32.of_int (Bytes.get_uint8 chunks 1)) 8); *)
  (* if l >= 1l then begin *)
  (*     k := Int32.logxor !k (Int32.of_int ((Char.code (Bytes.get chunks 0)))); *)
      k :=  Int32.mul !k c1;
      k := Int32.logxor (Int32.shift_left  !k (Int32.to_int r1))
          (Int32.shift_right_logical !k  (Int32.to_int (Int32.sub (Int32.of_int 32)  r1)));
      k :=  Int32.mul !k c2;
      h := Int32.logxor !h !k;
  end;

  h := Int32.logxor !h  len;

  h := Int32.logxor !h (Int32.shift_right_logical !h 16);
  h := Int32.mul !h  (Int32.of_string "0x85ebca6b");
  h := Int32.logxor !h (Int32.shift_right_logical !h 13);
  h := Int32.mul !h  (Int32.of_string "0xc2b2ae35");
  h := Int32.logxor !h (Int32.shift_right_logical !h 16);

  !h

{% endhighlight %} 

# Splay Tree

## Initial set of tests

{% highlight ocaml %} 

type 'a r_tree = Leaf | Node of 'a node1
and 'a node1 = { value : 'a; left : 'a r_tree; right : 'a r_tree; }

let rec check_splay_tree t = 
  match t with
  |Leaf ->  false
  | Node {left; value = v; right}->
    match left, right with
    | Node { left = _; value = v0;  _}, Node {left =  _; value = v1;  _} -> v == v1 + v0 + 1 
    | Node { left ;   _}, Leaf -> check_splay_tree left
    | Leaf, Node { left = _ ;value = _;  right} -> check_splay_tree right
    | _ -> false



let insert=
  Node {
    value = 2;
    left = Node {value = 1; left = Leaf; right = Leaf};
    right = Node {value = 3; left = Leaf; right = Leaf}
  }


let%expect_test _=
  Printf.printf  "%s" (string_of_bool (check_splay_tree insert));
  [%expect {| false |}]


{% endhighlight %} 

We can print a small tree like this for debugging.

{% highlight ocaml %} 

let rec print_sTree (sTree : int s_tree ) (d : int) : unit =
  match sTree with
  |Leaf -> () 
  | Node { left  ;value ;  right} ->
                                   print_sTree right (d + 1);
                                   for __i=0 to  (d - 1) do
                                       Printf.printf "  "
                                   done;
                                   Printf.printf "%d\n" value;
                                   print_sTree left  (d+1) 

{% endhighlight %} 

_dune runtest --auto-promote_ updates the test output automatically.

![image-title-here](../images/print_splay.png.png){:class="img-responsive"}


## Core Splay algorithm

At this stage the compiler is happy but very less progress is made. There is a steep learning curve here as I have to learn the language
deeply. 


## Insert Key into a binary tree

At this stage the mutable imperative style is hard to debug.Moreover _None_ and _Some Leaf_ are
both used redundantly. This led to a bug.

{% highlight ocaml %} 

let rec insert_key (k : int ) (t : int splay_tree option ref) : int splay_tree option ref=
 match !t with
  | None |Some Leaf ->
    let new_node = Node { key = k; value = 0; left = None; right = None } in
    t := Some new_node;
    t
  | Some tree  ->
    let  insert_node tree =

      match tree with
      |  Node old_key ->
        begin match old_key with
          |  ok  ->
            if k > ok.key then(
              match ok.right with
              | None | Some Leaf ->
              let r = ref (Some (Node { key = k ;value = 0 ; right = Some Leaf; left = Some Leaf} ))in
               ok.right <- !r;
               t
             | Some _r ->
             insert_key k (ref (ok.right ))
             )
            else 
            if k < ok.key then(
              match ok.left with
              | None ->
               let l = ref (Some (Node { key = k ;value = 0 ; right = Some Leaf; left = Some Leaf} ))in 
              ok.left <- !l;
              t 
             | Some _l ->
             insert_key k (ref (ok.left)); 
            )
          else
             t
        end;
     |Leaf ->t
    in
    insert_node tree

{% endhighlight %} 

## Porting SML to OCaml

I spent several days coding a Splay tree using my inefficient _mutable ref_ data structure. It didn't work satisfactily.
Eventually I picked up basic SML and ported the SML code to OCaml. This was a great learning experience as I learnt how
to use _Functors_ and abstractions and modules.

{% highlight ocaml %} 

let rec splay (l, v, r) (k:Params.key) =
    match compare k (keyOf (v)) with
    | EQUAL -> (l, v, r)
        | LESS ->
        (match l with
             | Empty -> (l, v, r) (* not found *)
             | Node (ll, lv, lr) ->
               match compare k (keyOf (lv)) with
                 | EQUAL -> (ll, lv, Node(lr, v, r)) (* 1: zig *)
                 | LESS ->
                 (match ll with
                      | Empty -> (Empty, lv, Node(lr, v, r))
				(* not found *)
                      | Node (lln, lvn, lrn) as n -> (* 2: zig-zig *)
                        let (lll, llv, llr) = splay (lln, lvn, lrn) k  in
                        (lll,llv,Node(llr,lv,Node(lr,v,r)))
                      )
                | GREATER ->
                    (match lr with
                        | Empty -> (ll, lv, Node(Empty, v, r))
                        |Node (lln, lvn, lrn) as n ->  (* 3: zig-zag *)
                          let (lrl, lrv, lrr) = splay (lln, lvn, lrn) k  in
                           (Node(ll,lv,lrl),lrv,Node(lrr,v,r))
                         ))
        | GREATER ->
	(match r with
	    | Empty -> (l, v, r) (* not found *)
	    | Node (rl, rv, rr) ->
	     match compare k (keyOf (rv)) with
		     |EQUAL -> (Node(l,v,rl),rv,rr) (* 1: zag *)
         | GREATER ->
		(match rr with
		   | Empty -> (Node(l,v,rl),rv,rr) (* not found *)
		 | Node (lln, lvn, lrn) as n -> (* 3: zag-zag *)
		 let (rrl, rrv, rrr) = splay (lln, lvn, lrn) k  in
			(Node(Node(l,v,rl),rv,rrl),rrv,rrr)
			)
		| LESS ->
		(match rl with
	| Empty -> (Node(l,v,rl),rv,rr) (* not found *)
 | Node (lln, lvn, lrn) as n -> (* 2: zag-zig *)
	 let (rll, rlv, rlr) = splay (lln, lvn, lrn) k  in
			(Node(l,v,rll),rlv,Node(rlr,rv,rr))
			))

  let size s tr  = s

  type 'b folder = ((elem*'b)->'b) -> 'b -> key -> set -> 'b

  let rec add ((size,tr):set) (e:elem) = let
    ((l,v,r), b) = add_tree !tr e  in
    let node = splay (l,v,r)  (keyOf(e)) in
    let size' = if b then size else size+1
    in
    let _ = Printf.printf "Size %d" size' in
    ((size', ref (Node((l,v,r)))),b) and

   add_tree  (t: tree) (e: elem) :node * bool =
    match t with
    |Empty -> ((Empty, e, Empty), false)
    | Node (l,v,r) ->
      (match compare (keyOf(v)) (keyOf(e)) with
       | EQUAL -> ((l,e,r),true)
       (* | GREATER -> let (n',b) = add_tree l e  in *)
       (*                    ((Node(n'),v,r),b) *)
       (* | LESS ->    let (n',b) = add_tree r e in *)
       (*                    ((l,v,Node(n')),b) *)
       | GREATER -> let ((x,y,z),b) = add_tree l e  in
         ((Node(x,y,z),v,r),b)
       | LESS ->    let ((x,y,z),b) = add_tree r e in
         ((l,v,Node (x,y,z)),b)
      )



{% endhighlight %} 

## Range-Minimum-Query

I set up the basic code for this. There is no query now. Code is in Git.

{% highlight ocaml %} 
let  preprocess_a l mk =
       let ps =
         let k = 0 --  mk
         and i = 0 -- (List.length l -1 ) in
         (k,i) in

         let v = Array.make ((List.length l) * ( mk   + 1)) 0 in

           List.iter (fun (k, i) -> 

               let () = Printf.printf "[mk %d] [k %d] [i %d]\n" mk k i in
               let ind  = indx (List.length l )  in
               match k with
               | 0 ->
                      let index = ind i k in
                      let value = List.nth l (ind i 0) in
                      (* let () = Printf.printf "Value set is %d [k %d] [i %d]\n" value k i in *)

                      let v' = Array.set v index value in
                      Array.iter (fun elem -> Printf.printf " %d " elem) v

               | _ ->
                 let i' = i + (Batteries.Int.pow 2 ( k - 1)) in
                 let p1 = Array.get v ( ind i (k - 1) ) in
                 let p2 = Array.get v ( ind i' (k - 1)) in
                 (* let () = Printf.printf "p1 is %d p2 is %d [k %d] [i %d]\n" p1 p2 k i in *)

                 let v' = Array.set v (ind i k ) ( min p1 p2) in
                 Array.iter (fun elem -> Printf.printf " %d " elem) v
         ) (enum_to_list ps)


{% endhighlight %} 
