---
layout: post
title: Joy of OCaml - Part I
published: true
---


### Higher-order functions

####Example 1

A naïve way of writing functions to take ‘n’ elements from a list and to drop ‘n’ elements. This is not the Idiomatic OCaml style I have come across. Moreover the algorithmic complexity is off the scale as the length of the list is computed repeatedly.

{% highlight OCaml %}
let take n words =
  let rec loop i l1 = 
    if i = n
    then l1
    else
    if( n <= List.length words ) then
      loop (i + 1)  ( appendtolist l1 (List.nth words i) ) 
    else []
  in loop 0  []
;;
{% endhighlight %}

{% highlight OCaml %}
let drop n words =
  let rec loop i l1 = 
    if i >= List.length words
    then l1
    else
      loop (i + 1) (appendtolist l1 (List.nth words i))
  in loop n  []
;;
{% endhighlight %}

{% highlight OCaml %}
let rec appendtolist l a =
  match l with
  |[] -> [a]
  |h :: t -> (h :: appendtolist t a)
;;
{% endhighlight %}

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

