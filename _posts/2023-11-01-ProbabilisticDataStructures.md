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

## Initial set of tests

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

## Initial version with a list of hash functions.

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

## Test
{% highlight ocaml %} 
let%expect_test "hash" =
  let empty_list() : 'hf Bloomfilter.Ds.t = ref None in
  let l = empty_list() in
  let hf = Bloomfilter.Ds.insert_hashfunc l Bloomfilter.Ds.jenkins in
  let hash = hf.value (string_to_int_list "Hello") in
  Printf.printf "%d\n" (Int32.to_int  hash);
  [%expect {| 1901914092 |}]

{% endhighlight %} 

# Test for Bit set and get

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

# Bit set and get

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