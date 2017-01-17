---
layout: post
title: Joy of OCaml - Part II( Unfinished post )
published: true
---
In the second part I will explain some more language features and code. 

### _let_ keyword
We start with this keyword again as this tripped me up. Moreover one does not
proceed much further without understanding the semantics of this keyword.

{% highlight OCaml %}
(let table = Hashtbl.create 10 in
  
  Hashtbl.add table "functional" ["practical";"utilitarian"];
  Hashtbl.add table "side-effect" ["outcome";"consequence"];
 table ) 
;;
{% endhighlight %}

{% highlight OCaml %}
let choose_randomly_hashtbl table (word : string) =
  let n0 = Hashtbl.find  table word in
  let n1 = Random.int (List.length n0) in
  let n2 = List.nth n0 n1 in
  n2
;;
{% endhighlight %}

*choose_randomly_hashtbl* returns randomly one of the values from the list corresponding to the key . And we see that _n0_, _n1_ and _n2_ are used after they are defined one by one. This should give a clearer idea about _let_'s semantics.

### User-defined types

As the name implies a _UDF_ is a custom type defined by the user.

{% highlight OCaml %}
type x = cell list;;
{% endhighlight %}

### Record types

A record type is a step further. It identifies each constituent part of the type by a name identifier like this.


{% highlight OCaml %}
type cell = { alive : bool ; column : int ; row : int }
;;
{% endhighlight %}

#### Nested pattern matching Record types

I plan to write about pattern matching concepts in another installment of this series as that is deeper than what would fit in one paragraph.

{% highlight OCaml %}
   |  {gamegrid = line :: above},{gamegrid = below} -> (
{% endhighlight %}

The third line in the _up_ function shown below matches the _head(line)_ of the list and the _tail(above)_ of the list.
It is nested because the pattern which is nested inside a record type is matched. _gamegrid_ is the name identifier of a grid defined by this code. It is a grid made of a list of lists of _cell_ which is another record type.

{% highlight OCaml %}
type grid = {gamegrid : cell list list}
;;
{% endhighlight %}
#### Partial updates of Record types

### Basic containers

As I understand it these are library extensions. We have to install those libraries we require. So, for example, _CCList_ enables us to create a mxn list of lists easily. 

This is how I used it.

{% highlight OCaml %}
#require "containers";;
 let makegrid = CCList.init 2 ( fun i -> (CCList.init 2 (fun j -> { alive = true; column = j;row = i })) );;
{% endhighlight %}

This create a 2x2 grid which is a list of lists.`_List_ seems to be the workhorse of functional programming languages.

### More involved code

Let us use the record types and UDF's to code a grid using the _zipper_ pattern which is a functional way of coding a grid. The code should serve as a foundation for a _game of life_ representation in the future.

It should be  pointed out that none of these functions mutate any data structure. So there is no side-effect. As you see it takes some effort to shed our memory of imperative mutable code. It is hard at the beginning but seems quite natural now.

Some of the following functions are my OCaml port of existing Haskell code. Given that I am not an OCaml expert, this code is far more verbose than the equivalent Haskell code My Haskell chops were not enough and I turned to the IRC for much needed advice. 

The code focuses on a _cell_ like this picture shows. There are areas above,below, to the left and to the right. We move over this grid.
The focus is the green square.



![image-title-here](../images/myhanddrawn.tex.preview.pdf.png){:class="img-responsive"}


{% highlight OCaml %}
type cell = { alive : bool ; column : int ; row : int }
;;
{% endhighlight %}

{% highlight OCaml %}
type cell = { alive : bool ; column : int ; row : int }
;;
{% endhighlight %}

{% highlight OCaml %}
type grid = {gamegrid : cell list list}
;;
{% endhighlight %}

{% highlight OCaml %}
type gridzipper  =
             { above : grid
             ; below : grid
             ; left  : cell list
             ; right : cell list
             ; focus : cell }
{% endhighlight %}

{% highlight OCaml %}
let focuscell celllist n =
 let rec loop acc n l =
  match l,n with
    | hd :: tl,n when n > 0 -> loop (hd :: acc) (n - 1) tl
    | [],_  -> None
    | hd :: tl,0 -> Some (acc, hd, tl)
 in loop  [] n celllist
;;
{% endhighlight %}

{% highlight OCaml %}
let gridfocus x y g =
 let a = focuscell g x in
  match a with
    | Some(before, line , after) -> (
                                  let b = focuscell line y in
                                   match b with
                                   Some (left  , focus, right) -> 
                                                              ( 
                                                               
                                                               (let above =  { gamegrid = before } in
                                                               let below = { gamegrid = after} in
                                                                            Some(
                                                                            {  above
                                                                            ;  below
                                                                            ;  left
                                                                            ;  right
                                                                            ;  focus }
                                                                             )
                                                               )
                                                              )
                                     | None -> None
                                  )
    | None -> None
;;
{% endhighlight %}
### Move the focus inside the grid


![image-title-here](../images/myhanddrawn-move.tex.preview.png){:class="img-responsive"}

The _left_ function moves the focus to the left. Similarly the other functions shift the focus
to other grids cells.


{% highlight OCaml %}
let left g =
match g.left with
 [] -> None 
| hd::tl ->  let newgridzipper = { g  with focus = hd; left = tl; right = g.right @ [g.focus] } in
             Some(newgridzipper)
;;
{% endhighlight %}

{% highlight OCaml %}
let right g =
match g.left with
 [] -> None 
| hd::tl ->  let newgridzipper = { g  with focus = hd; left = [g.focus]; right =  tl } in
             Some(newgridzipper)
;;
{% endhighlight %}

{% highlight OCaml %}
(*pattern-matches on the list (of lists) , which should be non-empty, and introduces two bindings,
 line for the head, and above for the tail.*)
let up g =
 match g.above,g.below with
   |  {gamegrid = line :: above},{gamegrid = below} -> (
                          let below' =  (List.rev g.left) :: ([g.focus] @ g.right) :: below in
                          let a = focuscell line (List.length g.left) in
                          match a with
                           Some (left, focus, right) ->
                                                               let above =  { gamegrid = above } in
                                                               let below = { gamegrid = below'} in
                            { above
                            ; below
                            ; left
                            ; right
                            ; focus }
                          |None -> 
                            { above = g.above
                            ; below = g.below
                            ; left = g.left
                            ; right = g.right
                            ; focus = g.focus }
                         )
   |({gamegrid=[]},_) ->  { above = g.above
                            ; below = g.below
                            ; left = g.left
                            ; right = g.right
                            ; focus = g.focus }
;;

{% endhighlight %}

{% highlight OCaml %}
let down g = 
   match g.below,g.above with
   |  {gamegrid = line :: below},{gamegrid = above} -> (
                          let above' =  (List.rev g.left) :: ([g.focus] @ g.right) :: above in
                          let a = focuscell line (List.length g.left) in
                          match a with
                           Some (left, focus, right) ->
                                                               let above =  { gamegrid = above } in
                                                               let below = { gamegrid = above'} in
                            { above
                            ; below
                            ; left
                            ; right
                            ; focus }
                          |None -> 
                            { above = g.above
                            ; below = g.below
                            ; left = g.left
                            ; right = g.right
                            ; focus = g.focus }
                         )
   | ({gamegrid=[]},_)->    { above = g.above
                            ; below = g.below
                            ; left = g.left
                            ; right = g.right
                            ; focus = g.focus }

;;
{% endhighlight %}

{% highlight OCaml %}
#require "containers";;
 let makegrid = CCList.init 2 ( fun i -> (CCList.init 2 (fun j -> { alive = true; column = j;row = i })) );;
{% endhighlight %}

Function that tests if the focus stays within the grid. _None_ signifies an exceptional movement outside. 

{% highlight OCaml %}
let grid = makegrid in
  let gf = gridfocus 0 1 grid in
   match gf with
   | Some(gf) ->
                Printf.printf "Focus is on [ %B %d %d ]" gf.focus.alive gf.focus.column gf.focus.row;left gf
   | None -> Printf.printf "No focus";gf
;;
{% endhighlight %}

Another Function that tests _focuscell_, a lower-order function.

{% highlight OCaml %}
let grid = makegrid in
  let fc = focuscell grid 2 in
   match fc with
    | Some(before, line , after) -> Printf.printf "Focus is on [ %B %d %d ]" (List.nth line 0).alive (List.nth line 0).column (List.nth line 0).row
    | None -> Printf.printf "No focus"
;;
{% endhighlight %}

References :

1. Huet, Gerard (September 1997). "Functional Pearl: The Zipper"
