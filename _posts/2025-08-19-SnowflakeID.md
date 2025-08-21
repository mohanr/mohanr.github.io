---
layout: post
title: Generate Snowflake ID
published: true
---

> Snowflake is a network service for generating unique ID numbers at high scale with some simple guarantees.
> I am trying to code this in OCaml.

> This is not finished yet. And the datetime utilities lack clear documentation. So writing tests
> for proper usage is hard.

# Experimenting with *Timedesc* and *Mtime*

The documentation for the time and date utilities is rather sketchy. So errors could be lurking in the code.
Moreover I use two libraries instead of one as I pick the easier and obvious parts of each API.
The following code is a test to understand the API better 

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

# Key functions

I am using *Bigarray* but it is a random choice.

{% highlight ocaml %}
open Timedesc
open Bigarray
open Eio.Std
open Int64
open Mtime.Span

exception Monotonic_clock of string

type id = ID of int64
type node = {
	mu  :  Eio.Mutex.t;
	epoch : int64;
	node :   int64;
	mutable step :   int64;
	mutable time :   int64;

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
  in ordinary_timestamp

let node  =  Int64.of_int 10
let step  =  Int64.of_int 12
let epoch = Int64.of_int 1288834974657 (* epoch_millis need not be called everytime *)

let monotonic_clock =
   let now = Mtime_clock.now () in
   (* 1 second delay in nanoseconds *)
   let ns =  mul (rem epoch (of_int 1000))  (of_int 1000000) in
   (* convert a nanoseconds delay into a time span *)
   let time_ns = of_uint64_ns ns in
   let s = div  epoch (of_int 1000) in
   let time_s = of_uint64_ns s in

   (* compute target time *)
   let monotonic_addtimes =
   (match (Mtime.add_span now (add time_s time_ns)) with
   | Some m ->  Mtime.sub_span now (of_uint64_ns (Mtime.to_uint64_ns m))
   | None -> raise (Monotonic_clock "Date/Time error") )                 (* Throw an error ! *)
   in
   (match monotonic_addtimes with
   | Some m ->  (Mtime.to_uint64_ns m)
   | None -> raise (Monotonic_clock "Date/Time error") )                 (* Throw an error ! *)



let create_snowflake_node node=
    let mutex = Eio.Mutex.create() in
    (* node bites *)
    let node_bits_a= Array1.create int32 c_layout 1 in
    let _ = Array1.set node_bits_a 0  Int32.(logxor
                                  (neg (of_int 1))
                                  ( shift_left (neg (of_int 1)) (to_int (Array1.get node_bits_a 0)))) in
   let  nodemask  =  Int32.shift_left  (Array1.get node_bits_a 0) 12 in (*  step is repeated here*)
    (* step bites *)
    let step_bits_a= Array1.create int32 c_layout 1 in
    let _ = Array1.set step_bits_a 0 Int32.( logxor
                                  (neg (of_int 1))
                                  ( shift_left (neg (of_int 1)) (to_int (Array1.get step_bits_a 0)))) in
    let  stepmask  =  Int32.shift_left  (Array1.get node_bits_a 0) 12 in (*  step is repeated here*)
    let epoch_node = monotonic_clock  in
    let node_bits= Array1.get node_bits_a 0 in
    let step_bits= Array1.get step_bits_a 0 in
    {
        epoch     = epoch_node;  (* epoch_millis need not be called everytime *)
        node  =  node;
        step  =  Int64.of_int 0;(*I think  it is zero to start with*)
        time =  Int64.of_int 0;(*I think  it is zero to start with*)
        mu        = mutex;
        nodemax   = Int64.of_int32 node_bits;
        nodemask  =  Int64.of_int32 nodemask;
        stepmask   =  Int64.of_int32 step_bits;
        time_shift       = Int64.add  (Int64.of_int32 node_bits)   (Int64.of_int32 step_bits);
        node_shift       =  (Int64.of_int32 step_bits);
    }


let generate node  =

   Eio.Mutex.use_rw ~protect:true node.mu (fun () ->
	let curr_time_timestamp = ref epoch_millis in
    let curr_time_millis  = ref (Int64.of_float (Timedesc.Timestamp.to_float_s epoch_millis)) in

	if Timedesc.Timestamp.compare  !curr_time_timestamp (Timedesc.Timestamp.of_float_s (Int64.to_float node.time)) == 0 then
		node.step <- Int64.logand (Int64.add node.step  (Int64.of_int 1))  node.stepmask;

		if Int64.(node.step == 0L) then(
			for _ = Int64.to_int !curr_time_millis to Int64.to_int node.time do (*Is it the same as <= operator ? *)
              let curr_time_millis  = ref (Int64.of_float (Timedesc.Timestamp.to_float_s epoch_millis)) in
	          curr_time_millis := !curr_time_millis;
            done;
		)
	else
		node.step <- Int64.of_int 0;

	node.time <- Int64.of_float (Timedesc.Timestamp.to_float_s !curr_time_timestamp);

	ID  (Int64.( logor (logor (shift_left !curr_time_millis  (to_int node.time_shift))
		(shift_left node.node  (to_int node.node_shift)))
		node.step))
	)
{% endhighlight %}

