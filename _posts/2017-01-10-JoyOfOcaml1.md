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

let choose_randomly_hashtbl table (word : string) =
  let n0 = Hashtbl.find  table word in
  let n1 = Random.int (List.length n0) in
  let n2 = List.nth n0 n1 in
  n2
;;
{% endhighlight %}

*choose_randomly_hashtbl* returns randomly one of the values from the list corresponding to the key . And we see that _n0_, _n1_ and _n2_ are used after they are defined one by one. This should give a clearer idea about _let_'s semantics.

### Partial function application

### Record types

The following is my OCaml port of existing Haskell code. The code focuses on a _cell_ like this picture shows. There are areas above,below, to the left and to the right. We move over this grid.
The focus is the green square.

![image-title-here](../images/myhanddrawn.tex.preview.pdf.png){:class="img-responsive"}


{% highlight OCaml %}
type cell = { alive : bool }
;;
{% endhighlight %}

{% highlight OCaml %}
type cellzipper =    cell list *  cell *  cell list
;;
{% endhighlight %}

{% highlight OCaml %}
type grid = {gamegrid : cell list}
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
 in loop  [] 0 celllist
;;
{% endhighlight %}

{% highlight OCaml %}
let gridfocus x y g =
 let a = focuscell x g in
  match a with
    | Some(before, line , after) -> (
                          let b = focuscell y line in
                            match b with
                               Some (left  , focus, right) ->  
                                  let above =  { gamegrid = before } in
                                  let below = { gamegrid = after} in
                                                  {  above
                                                  ;  below
                                                  ;  left
                                                  ;  right
                                                  ;  focus }

                                  )
{% endhighlight %}

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
(*pattern-matches on the list (of lists) , which should be non-empty, and introduces two bindings, line for the head, and above for the tail.*)
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
                         )
;;
{% endhighlight %}


1. Huet, Gerard (September 1997). "Functional Pearl: The Zipper"
