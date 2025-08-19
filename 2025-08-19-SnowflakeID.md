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
