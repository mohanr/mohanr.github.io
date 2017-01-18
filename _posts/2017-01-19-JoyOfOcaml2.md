
---
layout: post
title: Joy of OCaml - Part III
published: false
---
{% highlight OCaml %}

let locale = GtkMain.Main.init ()
;;
{% highlight OCaml %}
let drawrect area = 
let rec loop1 m =
  match m with
    | m when m < 5 ->
      (let rec loop x y n =
        match n with
        | n when n < 5 ->
          let x, y = x + 5, y + 5 in
          let width, height = 10,10 in
          let update_rect = Gdk.Rectangle.create ~x ~y ~width ~height  in
          area#misc#draw (Some update_rect);
          loop x y (n + 1)
        | n when n >= 5 -> loop1 (m + 1)
      in loop 0 0 0)
   (* when m >= 5 *)  
    | m when m >= 5 -> ()
in loop1 0
;;
{% endhighlight %}

{% highlight OCaml %}
let main () =
  let window = GWindow.window ~width:320 ~height:240 () in
  window#connect#destroy ~callback:GMain.Main.quit;
  let aspect_frame = GBin.aspect_frame 
    ~xalign:0.5 (* center x *)
    ~yalign:0.5 (* center y *)
    ~ratio:2.0	(* xsize/ysize = 2.0 *)
    ~obey_child:false (* ignore child's aspect *)
    ~packing:window#add () in

  let drawing_area = GMisc.drawing_area ~width:200 ~height:200 ~packing:aspect_frame#add () in
  let dr = drawrect drawing_area in
  window#show ();
 
  GMain.Main.main ()
;;
{% endhighlight %}

{% highlight OCaml %}
let _ = main ()
;;
{% endhighlight %}
