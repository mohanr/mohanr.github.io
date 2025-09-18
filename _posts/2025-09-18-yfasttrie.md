---
layout : post
title : yfasttrie
published : false
---

{% highlight ocaml %}
open Containers

module  Layermap = struct
  type t = int64
  let compare v v1 =
    Int64.compare v v1
end


module LayerMap = CCMap.Make(Layermap)

type node =
	 (* stores the left and right child of this node. *)
	children * parent
and
  parent = Parent of node
and
  children = Children of  node Array.t


(* https://reasonml.chat/t/help-to-create-a-tree-where-nodes-have-reference-to-parents/356/4 *)
 type xfasttrie = {
	layers : node LayerMap.t;
	root : node;
	number_of_items_in_trie : int64;
	bits : int;
    diff : int;
	min : node;
	max : node
}


{% endhighlight %}
