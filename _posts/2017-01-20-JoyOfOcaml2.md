---
layout: post
title: Joy of OCaml - Part III(Unfinished post)
published: true
---
In this installment of the series there is OCaml functional code that shows a 'Game of life' implementation. As usual I will explain the functional part of the code. I specifically point out
functional pieces because there is indispensable boilerplate code needed because I use _lablgtk_ which is a UI toolkit. And unfortunately this toolkit uses the Object-oriented parts of OCaml.

Did I mention that OCaml is a practical functional language ? Unlike Haskell it includes many imperative constructs and OO features. I do not explain the OO part of it because that is not the focus of this series.

{% highlight OCaml %}
let evolve area (backing:GDraw.pixmap ref) x y width height= 
          let update_rect = Gdk.Rectangle.create ~x ~y ~width ~height in
          !backing#set_foreground (`RGB (154*256, 205*256, 50*256));
          !backing#rectangle ~x ~y ~width ~height ();
          area#misc#draw (Some update_rect);
;;{% endhighlight %}

{% highlight OCaml %}

let drawrect area (backing:GDraw.pixmap ref)= 
let rec loop1 m y=
  match m with
    | m when m < 5 ->
      (let rec loop x  n =
        match n with
        | n when n < 5 ->
          let x = x + 20 in
          let width, height = 20,20 in
          evolve area backing x y width height;
          (*Printf.printf "%3d %3d\n" x y;*)
          loop x   (n + 1)
        | n when n >= 5 -> loop1 (m + 1) (y + 20)
      in loop 0  0)
   (* when m >= 5 *)  
    | m when m >= 5 ->  ()
in loop1 0 0
;;
{% endhighlight %}

{% highlight OCaml %}
(* Backing pixmap for drawing area *)
let backing = ref (GDraw.pixmap ~width:200 ~height:200 ())
{% endhighlight %}

{% highlight OCaml %}
(* Create a new backing pixmap of the appropriate size *)
let configure window backing ev =
  let width = GdkEvent.Configure.width ev in
  let height = GdkEvent.Configure.height ev in
  let pixmap = GDraw.pixmap ~width ~height ~window () in
  pixmap#set_foreground `WHITE;
  pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  backing := pixmap;
  true
;;
{% endhighlight %}

{% highlight OCaml %}
{(* Redraw the screen from the backing pixmap *)
let expose (drawing_area:GMisc.drawing_area) (backing:GDraw.pixmap ref) ev =
  let area = GdkEvent.Expose.area ev in
  let x = Gdk.Rectangle.x area in
  let y = Gdk.Rectangle.y area in
  let width = Gdk.Rectangle.width area in
  let height = Gdk.Rectangle.width area in
  let drawing =
    drawing_area#misc#realize ();
    new GDraw.drawable (drawing_area#misc#window)
  in
  drawing#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height !backing#pixmap;
  false
;;% endhighlight %}

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
    drawing_area#event#connect#expose ~callback:(expose drawing_area backing);
    drawing_area#event#connect#configure ~callback:(configure window backing);
    drawing_area#event#add [`EXPOSURE];
    window#show ();
    drawrect drawing_area backing;
 
  GMain.Main.main ()
;;
{% endhighlight %}

{% highlight OCaml %}
let _ = main ()
;;
{% endhighlight %}
