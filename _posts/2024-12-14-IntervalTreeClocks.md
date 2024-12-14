---
layout: post
title: Joy of OCaml - Part I
published: false
---

{% highlight ocaml %}


type 'a event = Empty | Leaf of 'a node | Node of 'a node
and 'a node = { value : 'a;
                left : 'a event;
                right : 'a event}
let change_r e =
         (match e.right with
              |Empty -> Empty
              |_ -> e.right
         )

let change_l e =
         (match e.left with
              |Empty -> Empty
              |_ -> e.left
         )

let copy e =
  match e with
  |Node n ->
	let el = n.left in
	let  er = n.right in
     Leaf { value = n.value; left = change_l { n with left = el }; right = change_r { n with right = er } }

  |Leaf l ->
	let el = l.left in
	let  er = l.right in
     Leaf { value = l.value; left = change_l { l with left = el }; right = change_r { l with right = er } }
  |Empty -> Empty

let rec join (e1 : 'a event) ( e2 : 'a event) : 'a event =

  match e1,e2 with
  | Node node1,Node node2->
			if node1.value > node2.value then
            begin
               join (copy e2) e1;
            end
            else
               let d = node2.value - node1.value in
               let n_left = join node1.left node2.left in
               let n_right = join node1.right node2.right in
                Node { value = node2.value + d; left = n_left; right = n_right }
  | Leaf _,Node _->
               let n = Node { value = 0; left = Empty; right = Empty } in
               join n e2;
  | Node _,Leaf _->
               let n = Node { value = 0; left = Empty; right = Empty } in
               join e1 n;
  | Leaf node1,Leaf node2->
                Node { value = max node1.value node2.value; left = node1.left; right = node1.right };
  | Empty,Empty -> Printf.printf "Event failure"; Empty;
  | Empty, _ ->
    e2
  | _, Empty -> e1
	(* 	e1.normalize(); *)

let drop val1 val2 =
 if val1 <= val2 then
   val2 - val1
 else
   val1

let normalize e1 =
  match e1 with
  | Node node->
              (match node.left,node.right with
               | Leaf node1,Leaf node2 when node1.value == node2.value ->
                Leaf { value = node.value + node1.value; left = node.left; right = node.right }
               | Node node1,Node node2 ->
                let mm = (min node1.value node2.value) in
                let n_l = Node { value = (drop node1.value mm); left = node1.left; right = node1.right } in
                let n_r = Node { value = drop node2.value mm; left = node2.left; right = node2.right } in
                Node { value = node.value + mm ; left = n_l; right = n_r }
               | Node node1, (Empty | Leaf _) ->
                Node { value = node.value; left = Node node1; right = node.right }
               | (Empty | Leaf _), Node node2 ->
                Node { value = node.value; left = node.left; right = Node node2 }
               | (Empty, Empty) ->
                Node { value = node.value; left = Empty; right = Empty }
               | (Empty, Leaf node ) ->
                Node { value = node.value; left = Empty; right = Leaf node } (* Case is not clear *)
               | (Leaf node, (Empty|Leaf _)) ->
                Node { value = node.value; left = Leaf node ; right = Leaf node } (* Case is not clear *)
              )
  | Leaf node ->
                Leaf { value = node.value ; left = node.left; right = node.right }
  | Empty -> Empty

{% endhighlight %}
