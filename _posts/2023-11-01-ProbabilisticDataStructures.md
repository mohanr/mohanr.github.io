---
layout: post
title: Probabilistic and other Data Structures
published: true
---

# tl;dr
1. The code will be gradually improved and finally will be committed to git.
2. Performance considerations are not paramount here.


# Probabilistic and other Data Structures

## Bloom Filter

{% highlight OCaml %} 

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
