---
layout: post
title: Joy of OCaml - Part I
published: true
---


### Imperative OCaml
This code checks if a List is sorted or not. 

{% highlight OCaml %}
let is_list_sorted l =
let n – ref true in
  for I = 0 to List.length l -1 do
      if ( I + 1 <= List.length l -1 then
         If( (String.compare (List.nth l i) (List.nth l (I + 1)) == -1 ||
              String.compare (List.nth l i) (List.nth l ( I + 1 )) == 0 ) 
          then n := true
          else n := false )
  done;
!n
;;
{% endhighlight %}

Even though OCaml has such constructs an aspiring functional programmer should be cautioned. It is all too easy to forget that we are learning about functions and hark back to an earlier style we are used to.

Let use write a more idiomatic OCaml function that does the same thing.

{% highlight OCaml %}
let is_list_sorter1 l =
let rec loop l1 =
  match l1 with
  } a :: (b :: _ as t ) -> if ( String.compare a b = -1 || 
                                String.compare a b = 0 )
                           then loop t
                           else false;
  | _ :: [] -> true
  | [] => true
in loop l
;;
{% endhighlight %}

