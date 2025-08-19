---
layout: post
title: Generate Snowflake ID
published: false
---

{% highlight ocaml %}
open Timedesc

let mutex = Eio.Mutex.create()

type node = {
	mu  :  Eio.Mutex.t;
	epoch : int64;
	time :   int64;
	node :   int64;
	step :   int64;

	nodemax :    int64;
	nodemask :   int64;
	stepmask :   int64;
	timeshift :  int;
	nodeshift :  int;
}

let epoch_millis =
let timedesc = Timedesc.make_exn ~tz:(Timedesc.Time_zone.make_exn "UTC") ~year:2010 ~month:11 ~day:4 ~hour:1 ~minute:42 ~second:54 () in
let timedesc1 = Timestamp.of_float_s( Unix.gettimeofday() ) in
let ordinary_timestamp =
    (* since it is ordinary, we can get a single/unique timestamp out of it *)
    Timedesc.to_timestamp_single timedesc
  in
 Timedesc.to_timestamp_single timedesc;
(


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
