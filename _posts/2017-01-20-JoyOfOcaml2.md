---
layout: post
title: Joy of OCaml - Part III
published: true
---
In this installment of the series there is OCaml functional code that shows a 'Game of life' implementation. As usual I will explain the functional part of the code. I specifically point out
functional pieces because there is indispensable boilerplate code needed because I use _lablgtk_ which is a UI toolkit. And unfortunately this toolkit uses the Object-oriented parts of OCaml.

Did I mention that OCaml is a practical functional language ? Unlike Haskell it includes many imperative constructs and OO features. I do not explain the OO part of it because that is not the focus of this series.

{% highlight OCaml %}
(*1) Any live cell with fewer than two live neighbors dies, as if caused by underpopulation.
  2) Any live cell with more than three live neighbors dies, as if by overcrowding.
  3) Any live cell with two or three live neighbors lives on to the next generation.
  4) Any dead cell with exactly three live neighbors becomes a live cell.*)
  
  let locale = GtkMain.Main.init ()
;;
{% endhighlight %}

{% highlight OCaml %}
let displayrectangle area (backing:GDraw.pixmap ref) x y width height= 
          let update_rect = Gdk.Rectangle.create ~x ~y ~width ~height in
          !backing#set_foreground (`RGB (154*256, 205*256, 50*256));
          !backing#rectangle ~x ~y ~width ~height ();
          area#misc#draw (Some update_rect);
;;
{% endhighlight %}

{% highlight OCaml %}

let  triominoevolve area  (backing:GDraw.pixmap ref)  x y width height solid grid= 
   let rec loop i x y g=
     if i < 3 then
         let update_rect = Gdk.Rectangle.create ~x ~y ~width ~height in
          !backing#set_foreground (`RGB (154*256, 205*256, 50*256));
          !backing#rectangle ~x ~y ~width ~height ~filled:solid ();
          Printf.printf "Triomino %d %d \n" x y;
         area#misc#draw (Some update_rect);
         let newgrid =
         List.mapi (fun i el -> List.mapi ( fun i el1 ->
              if (x = el1.xc && y = el1.yc)
              then 
                ({ el1  with alive = true}
                 ) else el1) el ) g in

          loop ( i + 1) x (y + 20) newgrid
    else
       g
  in
  loop 0 (x + 40) (y + 40) grid;;
{% endhighlight %}

{% highlight OCaml %}
let tryleft  gf = 
 match  gf with
   | Some(gridzipper) -> (let offorongrid = left gridzipper in
                          match offorongrid with
                          | Some(newgridzipper) -> if (newgridzipper.focus.alive = true )
                              then 1 else 0
                          |  None -> 0 )
   | None -> 0
;;
{% endhighlight %}

{% highlight OCaml %}

let tryright  gf = 
 match  gf with
   | Some(gridzipper) -> (let offorongrid = right gridzipper in
                          match offorongrid with
                          | Some(newgridzipper) -> if (newgridzipper.focus.alive = true )
                              then 1 else 0
                          |  None -> 0 )
   | None -> 0
;;
{% endhighlight %}

{% highlight OCaml %}
let tryup  gf = 
 match  gf with
   | Some(gridzipper) -> (let offorongrid = up gridzipper in
                          match offorongrid with
                          | Some(newgridzipper) -> if (newgridzipper.focus.alive = true )
                              then 1 else 0
                          |  None -> 0 )
   | None -> 0
;;
{% endhighlight %}

{% highlight OCaml %}
let tryup  gf = 
 match  gf with
   | Some(gridzipper) -> (let offorongrid = up gridzipper in
                          match offorongrid with
                          | Some(newgridzipper) -> if (newgridzipper.focus.alive = true )
                              then 1 else 0
                          |  None -> 0 )
   | None -> 0
;;
{% endhighlight %}

{% highlight OCaml %}
let trydown  gf = 
 match  gf with
   | Some(gridzipper) -> (let offorongrid = down gridzipper in
                          match offorongrid with
                          | Some(newgridzipper) -> if (newgridzipper.focus.alive = true )
                              then 1 else 0
                          |  None -> 0 )
   | None -> 0
;;{% endhighlight %}

{% highlight OCaml %}
let evolve area (backing:GDraw.pixmap ref) x y width height= 
          let update_rect = Gdk.Rectangle.create ~x ~y ~width ~height in
          !backing#set_foreground (`RGB (154*256, 205*256, 50*256));
          !backing#rectangle ~x ~y ~width ~height ();
          area#misc#draw (Some update_rect);
;;{% endhighlight %}

{% highlight OCaml %}
let trycurrent  gf = 
 match  gf with
   | Some(gridzipper)  -> if (gridzipper.focus.alive = true )
                              then 1 else 0

   | None -> 0
;;
{% endhighlight %}

{% highlight OCaml %}
let drawrect area (backing:GDraw.pixmap ref) limit= 
let rec loop1 m y =
  match m with
    | m when m < limit ->
      (let rec loop x n =
        match n with
        | n when n < limit ->
          let x = x + 20 in
          let width, height = 20,20 in
          displayrectangle area backing x y width height;
          (*Printf.printf "%3d %3d\n" x y;*)
          loop x   (n + 1)
        | n when n >= limit -> loop1 (m + 1) (y + 20)
      in loop 0  0)
   (* when m >= limit *)  
    | m when m >= limit ->  ()
in loop1 0 0
;;
{% endhighlight %}

{% highlight OCaml %}
let neighbours grid =
let rec loop1 limit m g1 accum=
  match m with
    | m when m < limit ->
      (let rec loop  n g acc=
        match n with
        | n when n < limit ->
          let cellstatus = 
          acc @ [[trycurrent (gridfocus m n g1)] @ 
                 [tryleft (gridfocus m n g1)] @ 
                 [tryright (gridfocus m n g1)] @ 
                 [tryup (gridfocus m n g1)]@ 
                 [trydown (gridfocus m n g1)]] in
          loop  (n + 1) g cellstatus
        | n when n >= limit -> loop1  (List.length grid) (m + 1) g acc
      in loop 0  g1 accum)
   (* when m >= limit *)  
    | m when m >= limit -> accum
in loop1 (List.length grid) 0  grid []
;;
{% endhighlight %}

{% highlight OCaml %}
let drawgridrepresentation area (backing:GDraw.pixmap ref) grid =
let rec loop1 limit m y g1=
  match m with
    | m when m < limit ->
      (let rec loop x n g=
        match n with
        | n when n < limit ->
          let x = x + 20 in
          let width, height = 20,20 in
          displayrectangle area backing x y width height;
          (*Printf.printf "%3d %3d\n" x y;*)
          let gridmapi = 
          List.mapi (fun i el -> List.mapi ( fun i el1 ->
              if (n = el1.column && m = el1.row)
              then 
                ({ el1  with xc = x; yc = y}
                 ) else el1) el ) g in

          loop x   (n + 1) gridmapi
        | n when n >= limit -> loop1  (List.length grid) (m + 1) (y + 20) g
      in loop 0  0 g1)
   (* when m >= limit *)  
    | m when m >= limit ->  g1
in loop1 (List.length grid) 0 0 grid
;;
{% endhighlight %}

[lablgtk](http://lablgtk.forge.ocamlcore.org/) is the UI toolkit that was recommended to me. So most
of the code shown below is library code.

{% highlight OCaml %}
(* Backing pixmap for drawing area *)
let backing = ref (GDraw.pixmap ~width:200 ~height:200 ()){% endhighlight %}

{% highlight OCaml %}
((* Create a new backing pixmap of the appropriate size *)
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
(* Redraw the screen from the backing pixmap *)
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
;;{% endhighlight %}

{% highlight OCaml %}
let  printgrid grid =
 let rec loop g =
 match g with
   |hd :: tl ->
      (let rec loop1 g1 = 
       match g1 with
       |hd1::tl1 -> Printf.printf "[ %B x-%d y-%d ]" hd1.alive hd1.xc hd1.yc;loop1 tl1
       | [] -> Printf.printf "\n"; loop tl
       in loop1 hd)
   |[] -> Printf.printf "\n"
in loop grid
;;
{% endhighlight %}

{% highlight OCaml %}
let triominorepeat drawing_area grid= 
     triominoevolve drawing_area backing 20 0 20 20 true grid;
;;{% endhighlight %}

{% highlight OCaml %}
let main () =
 

  let window = GWindow.window ~width:320 ~height:240 ~position:`CENTER () in
  window#connect#destroy ~callback:GMain.Main.quit;
  
  let aspect_frame = GBin.aspect_frame 
    ~xalign:0.5 (* center x *)
    ~yalign:0.5 (* center y *)
    ~ratio:2.0	(* xsize/ysize = 2.0 *)
    ~obey_child:false (* ignore child's aspect *)
    ~packing:window#add () in

  let makegameoflifegrid = CCList.init 7 ( fun i -> (CCList.init 7 (fun j -> { alive = false; column = j;row = i;xc = 0;yc = 0 })) ) in
  let drawing_area = GMisc.drawing_area ~width:200 ~height:200 ~packing:aspect_frame#add () in
    drawing_area#event#connect#expose ~callback:(expose drawing_area backing);
    drawing_area#event#connect#configure ~callback:(configure window backing);
    drawing_area#event#add [`EXPOSURE];
    window#show ();
    let newgrid = drawgridrepresentation drawing_area backing makegameoflifegrid in 
    let newgrid1 = triominorepeat drawing_area newgrid in
      GMain.Main.main ();
    let results = neighbours newgrid1 in
       List.iteri  (fun i x -> 
                    ( Printf.printf "["; List.iter (fun  x1 -> 
                                           Printf.printf " %d " x1) x); 
                    if (( i mod 7 ) = 0) 
                    then 
                      Printf.printf "]\n"
                    else 
                      Printf.printf "]"  )  results;
    (*let rec play newgrid1 =

      play newgrid1 in
      play newgrid1
    printgrid newgrid1;*)

;;
let _ = main ()
;;
{% endhighlight %}
___
[ 0  0  0  0  0 ] [ 0  0  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ]

[ 0  0  0  0  0 ] [ 0  0  0  0  0 ][ 0  0  0  0  1 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ]

[ 0  0  0  0  0 ] [ 0  0  1  0  0 ][ 1  0  0  0  1 ][ 0  1  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ]

[ 0  0  0  0  0 ] [ 0  0  1  0  0 ][ 1  0  0  1  1 ][ 0  1  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ]

[ 0  0  0  0  0 ] [ 0  0  1  0  0 ][ 1  0  0  1  0 ][ 0  1  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ]

[ 0  0  0  0  0 ] [ 0  0  0  0  0 ][ 0  0  0  1  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ]

[ 0  0  0  0  0 ] [ 0  0  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ][ 0  0  0  0  0 ]

___
