---
layout: post
title: Generate Snowflake ID
published: true
---

> Snowflake is a network service for generating unique ID numbers at high scale with some simple guarantees.
> I am trying to code this in OCaml.



# Experimenting with *Timedesc* and *Mtime*

The documention for the time and date utilities is rather sketchy. So error could be lurking in the code.
Moreover I use 2 libraries instead of one as I pick the easier and obvious parts of the API. 

{% highlight ocaml %}
open Timedesc
open Bigarray
open Eio.Std




type node = {
	mu  :  Eio.Mutex.t;
	epoch : int64;
	node :   int64;
	step :   int64;
	nodemax :    int64;
	nodemask :   int64;
	stepmask :   int64;
	time_shift :  int64;
	node_shift :  int64;
}

(* 1288834974657 *)
(* Nov 04 2010 01:42:54 *)
(* This need not be called everytime *)
let epoch_millis =
let timedesc = Timedesc.make_exn ~tz:(Timedesc.Time_zone.make_exn "UTC") ~year:2010 ~month:11 ~day:4 ~hour:1 ~minute:42 ~second:54 () in
let ordinary_timestamp =
    (* since it is ordinary, we can get a single/unique timestamp out of it *)
    Timedesc.to_timestamp_single timedesc
  in
 Timedesc.to_timestamp_single timedesc


let node  =  Int64.of_int 10
let step  =  Int64.of_int 12
let epoch = Int64.of_int 1288834974657 (* epoch_millis need not be called everytime *)

let get monotonic_clock =
   let now = Mtime_clock.now () in
   (* 1 second delay in nanoseconds *)
   let ns =  Int64.mul (Int64.rem epoch (Int64.of_int 1000))  (Int64.of_int 1000000) in
   (* convert a nanoseconds delay into a time span *)
   let time_ns = Mtime.Span.of_uint64_ns ns in
   let s = Int64.div  epoch (Int64.of_int 1000) in
   let time_s = Mtime.Span.of_uint64_ns s in

   (* compute target time *)
   let monotonic_times = Mtime.add_span now (Mtime.Span.add time_s time_ns) in
   monotonic_times


let create_snowflake_node node=
    let mutex = Eio.Mutex.create() in
    (* node bites *)
    let node_bits_a= Array1.create int32 c_layout 1 in
    let _ = Array1.set node_bits_a 0 ( Int32.logxor
                                  (Int32.neg (Int32.of_int 1))
                                  ( Int32.shift_left (Int32.neg (Int32.of_int 1)) (Int32.to_int (Array1.get node_bits_a 0)))) in
   let  nodemask  =  Int32.shift_left  (Array1.get node_bits_a 0) 12 in (*  step is repeated here*)
    (* step bites *)
    let step_bits_a= Array1.create int32 c_layout 1 in
    let _ = Array1.set step_bits_a 0 ( Int32.logxor
                                  (Int32.neg (Int32.of_int 1))
                                  ( Int32.shift_left (Int32.neg (Int32.of_int 1)) (Int32.to_int (Array1.get step_bits_a 0)))) in
    let  stepmask  =  Int32.shift_left  (Array1.get node_bits_a 0) 12 in (*  step is repeated here *)
    let node_bits= Array1.get node_bits_a 0 in
    let step_bits= Array1.get step_bits_a 0 in
    {
        epoch     = Int64.of_int 1288834974657;  (* epoch_millis need not be called everytime *)
        node  =  node;
        step  =  Int64.of_int 0;(*I think  it is zero to start with*)
        mu        = mutex;
        nodemax   = Int64.of_int32 node_bits;
        nodemask  =  Int64.of_int32 nodemask;
        stepmask   =  Int64.of_int32 step_bits;
        time_shift       = Int64.add  (Int64.of_int32 node_bits)   (Int64.of_int32 step_bits);
        node_shift       =  (Int64.of_int32 step_bits);
    }


{% endhighlight %}

{% highlight ocaml %}

open Bitcask__Snowflake
let%expect_test _=
(* https://github.com/daypack-dev/timere/blob/main/examples/date_time.ml *)

let em = epoch_millis in
Fmt.pr "%a@." (Timedesc.Timestamp.pp  ()) em;
let timedesc = Timedesc.make_exn ~tz:(Timedesc.Time_zone.make_exn "UTC") ~year:2010 ~month:11 ~day:4 ~hour:1 ~minute:42 ~second:54 () in
let time_as_float = (Timedesc.Span.sub  (Timedesc.Timestamp.now()) (Timedesc.to_timestamp_single timedesc) ) in
Fmt.pr "%f\n"  (Timedesc.Timestamp.to_float_s time_as_float);
Fmt.pr "%f\n"  (Timedesc.to_timestamp_float_s_single timedesc);
Fmt.pr "%f"  (Timedesc.to_timestamp_float_s_single (Timedesc.now()));

[%expect {|
  2010 Nov 04 01:42:54 +00:00:00
  466753491.672000
  1288834974.000000
  1755588465.672007
  |}]


{% endhighlight %}
