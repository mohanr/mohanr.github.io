---
layout: post
title: Generate Snowflake ID
published: true
---

> Snowflake is a network service for generating unique ID numbers at high scale with some simple guarantees.
> I am trying to code this in OCaml.

> This is not finished yet because the tests are not sufficient. I am not looking at the individual bits that
> compose the Snowflake ID. And the datetime utilities lack clear documentation. So writing tests
> for proper usage is hard.
> But the tests shown below generate monotonically increasing Snowflake ID.

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


let epoch_millis ()=
  let timedesc = Timedesc.make_exn ~tz:(Timedesc.Time_zone.make_exn "UTC") ~year:2010 ~month:11 ~day:4 ~hour:1 ~minute:42 ~second:54 () in
  let ordinary_timestamp =
      (* since it is ordinary, we can get a single/unique timestamp out of it *)
      Timedesc.to_timestamp_single timedesc
  in ordinary_timestamp

let time_since () =
  let timedesc = Timedesc.make_exn ~tz:(Timedesc.Time_zone.make_exn "UTC") ~year:2010 ~month:11 ~day:4 ~hour:1 ~minute:42 ~second:54 () in
  let time_as_float =
      Timedesc.Span.sub (Timedesc.Timestamp.now()) ( Timedesc.to_timestamp_single timedesc)
  in Int64.of_float (Timedesc.Span.to_float_s time_as_float)
  (* Int64.of_float (Unix.gettimeofday()) *)


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
   (match (Mtime.Span.abs_diff  (Mtime.Span.of_uint64_ns (Mtime.to_uint64_ns now))  (add time_s time_ns)) with
   |  m ->  Mtime.add_span now m
   | _ -> raise (Monotonic_clock "Date/Time error") )                 (* Throw an error ! *)
   in
    (match  monotonic_addtimes with
      |Some m -> Mtime.Span.of_uint64_ns (Mtime.to_uint64_ns m)
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
        epoch     = Mtime.Span.to_uint64_ns epoch_node;
        node  =  node;
        step  =  0L;(*I think  it is zero to start with*)
        time =  0L;(*I think  it is zero to start with*)
        mu        = mutex;
        nodemax   = Int64.of_int32 node_bits;
        nodemask  =  Int64.of_int32 nodemask;

        stepmask   =  Int64.of_int32 step_bits;
        time_shift       = Int64.add  (Int64.of_int32 node_bits)   (Int64.of_int32 step_bits);
        node_shift       =  (Int64.of_int32 step_bits);
    }


let generate n  =

   Eio_main.run @@ fun env ->
   Eio.Switch.run @@ fun sw ->
   let clock = Eio.Stdenv.clock env in
   Fiber.fork ~sw (fun () ->
   Eio.Mutex.use_rw ~protect:true n.mu (fun () ->


       let rec loop_while_node idx node =
         let  milli  =  time_since () in
         if idx < 10 then(
         if  Int64.equal milli node.time then(
             node.step <- Int64.logand (Int64.add node.step  (Int64.of_int 1))  node.stepmask;

             Fmt.pr " node.step  %Ld\n"  node.step;
             if Int64.equal node.step 0L then(
                 let rec loop_while() =

                  let  millis  =  time_since () in
                  if Int64.compare millis node.time <= 0 then(
                     Eio.Time.sleep clock 0.00005;
                     loop_while() ;
                  )
                  else millis
                 in
                 let new_millis = loop_while() in
                 node.time <- new_millis;


             );
         )else(
             Fmt.pr " node.step is set to %Ld\n"  0L;
             node.step <- 0L;
             Fmt.pr " node.time is set from %Ld to %Ld\n" node.time milli;
             node.time <-   milli;

         );


         let id =
         ID  (Int64.( logor (logor (shift_left node.time  (to_int node.time_shift))
             (shift_left node.node  (to_int node.node_shift)))
             node.step)) in
           Fmt.pr "%Ld\n" (match id with | ID x -> x);
         loop_while_node (idx + 1) node;
        )
         in
         loop_while_node 0 n;
	)
	)
{% endhighlight %}

# Tests

{% highlight ocaml %}

open Bitcask__Snowflake

let%expect_test _=
(* https://github.com/daypack-dev/timere/blob/main/examples/date_time.ml *)

let em = epoch_millis () in
Fmt.pr "%a@." (Timedesc.Timestamp.pp  ()) em;
let timedesc = Timedesc.make_exn ~tz:(Timedesc.Time_zone.make_exn "UTC") ~year:2010 ~month:11 ~day:4 ~hour:1 ~minute:42 ~second:54 () in
let time_as_float = (Timedesc.Span.sub  (Timedesc.Timestamp.now()) (Timedesc.to_timestamp_single timedesc) ) in
Fmt.pr "%f\n"  (Timedesc.Timestamp.to_float_s time_as_float);
Fmt.pr "%f\n"  (Timedesc.to_timestamp_float_s_single timedesc);
Fmt.pr "%f"  (Timedesc.to_timestamp_float_s_single (Timedesc.now()));

[%expect {|
  2010 Nov 04 01:42:54 +00:00:00
  467179952.045944
  1288834974.000000
  1756014926.045950
  |}]

let%expect_test "Test Monotonic clock"=
      Printf.printf "%L" Int64.(to_int (Mtime.Span.to_uint64_ns monotonic_clock));
    (* [%expect {| 4912999253692 |}] *)
    [%expect {| 16753314274858 |}]

let%expect_test "Time Since"=
    let  millis  =  time_since () in
    Fmt.pr "%Ld" millis;
    [%expect {| 467179952 |}]

let%expect_test "Test Duplicate ID"=

	let node = create_snowflake_node (Int64.of_int 0) in
	(* for _ = 0 to 10 do *)

		let _id = generate node in
        ();
    (* done; *)
  [%expect {|
     node.step is set to 0
     node.time is set from 0 to 467179952
    467179952
     node.step  0
    467179953
     node.step  0
    467179954
     node.step  0
    467179955
     node.step  0
    467179956
     node.step  0
    467179957
     node.step  0
    467179958
     node.step  0
    467179959
     node.step  0
    467179960
     node.step  0
    467179961
    |}]


{% endhighlight %}
