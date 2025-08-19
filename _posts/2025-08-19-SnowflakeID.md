---
layout: post
title: Generate Snowflake ID
published: false
---

> Snowflake is a network service for generating unique ID numbers at high scale with some simple guarantees.
> I am trying to code this in OCaml.



# Experimenting with *Timedesc*

{% highlight ocaml %}
open Timedesc
open Bigarray



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
let epoch_millis =
let timedesc = Timedesc.make_exn ~tz:(Timedesc.Time_zone.make_exn "UTC") ~year:2010 ~month:11 ~day:4 ~hour:1 ~minute:42 ~second:54 () in
let ordinary_timestamp =
    (* since it is ordinary, we can get a single/unique timestamp out of it *)
    Timedesc.to_timestamp_single timedesc
  in
 Timedesc.to_timestamp_single timedesc


let create_snowflake_struct =
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
   let  stepmask  =  Int32.shift_left  (Array1.get node_bits_a 0) 12 in (*  step is repeated here*)

    let node_bits= Array1.get node_bits_a 0 in
    let step_bits= Array1.get step_bits_a 0 in
    {
        epoch     = Int64.of_int 1288834974657;  (* epoch_millis need not be called everytime *)
        node  =  Int64.of_int 10;
        step  =  Int64.of_int 12;
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
