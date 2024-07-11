---
layout: post
title: Storage Engine
published: true
---
# Storage Engine

As I learn to implement a simple Database I post OCaml code to create Data Structures.
 
1. The OCaml code is raw even though it compiles and simple tests pass. I look at other source code
   written in Haskell, Rust, SML etc. to understand the Data Structures.
3. The code format and FP principles have to be reviewed and revised.
4. Eventually I hope the toy Database will have SQL parsers and optimizers.

## Log-Structured Merge

{% highlight OCaml %}

open Stdlib

let state = Random.State.make_self_init()

type 'v values =  Int
type 'k keys =  Int

type ('k, 'v) node =
  | Empty
  | Node of {
      keys : 'k;
      values : 'v;
      sibling : ('k, 'v) node;
      child : ('k, 'v) node;
    }
  | Head of
   { sibling :  ('k, 'v) node;
     child :  ('k, 'v) node
   }

type  ( 'k, 'v) skip_list  =
  { skpstate : Random.State.t;  (*Seed *)
    skpinternal :  ('k, 'v) node }

let ltekey (node  : ('k, 'v ) node) key : bool =
  match node with
   | Node {keys = k; _} -> k <= key
   | Empty  -> false
   | _  -> true

let ltkey (node  : ('k, 'v ) node) key : bool =
  match node with
   | Node {keys = k; _} -> k < key
   | Empty  -> false
   | _  -> true

let eqkey (node  : ('k, 'v ) node) key : bool =
  match node with
   | Node {keys = k; _} -> k == key
   | _  -> false

let empty_check (skp : ('k, 'v) skip_list) : bool =
  match skp.skpinternal with
  | Head { sibling = Empty; child = Empty } -> true
  | _ -> false

let create_empty_skiplist state : ('k, 'v) skip_list =
     let head = Head { sibling = Empty; child = Empty } in
     let st = state  in
     { skpstate = st ; skpinternal = head }

let rec addlevels k v (internal, levels ): ('k, 'v ) node =
  match levels with
        | levels when levels  > 0  -> create_level k v levels ( splitlevel internal)
        | _  -> internal
  and
    create_level k v levels (l, r) =
    match levels with
        | 1 ->  let node =  Node { keys = k; values = v; sibling = Empty; child = r} in
             Head { sibling = node ; child = r }
        | _ ->
              let head = Head { sibling = Empty; child = l } in
              let head1 = Head { sibling = Empty; child = r } in
              create_level k v (levels - 1) (head , head1)
  and
    splitlevel ( node : ('k, 'v) node ) :  ('k, 'v) node * ('k, 'v) node  =
      match node with
      | Head { sibling = _; child = _ } as head1 ->
        let head = Head { sibling = Empty; child = head1 } in
        (head, node)
       | ns ->
          let (next,r)  = splitlevel ns in
          (next ,  r)

let number (state  : Random.State.t) =
     let rec randomgen n (state  : Random.State.t) =
      let b = Random.State.bool state
      in if b then randomgen (n + 1)  state
           else (n,   state )
      in randomgen 0 state


let rec insert k v (skp : ('k, 'v) skip_list ) =
  let (rg, state') = number skp.skpstate in
  let skplist = addlevels k v (create_skiplist k v rg skp.skpinternal) in
  { skpstate = state'; skpinternal = skplist }
  and
    create_skiplist k v rg (node : ('k, 'v) node) :  ('k, 'v) node*int =

    match node with
    | Empty -> (Empty, rg)
    | Head _ -> (Empty, rg)
    | Node { keys = _; values = _; sibling = s; child = c } ->

      if ( ltkey s k ) then
        begin
          match (create_skiplist k v rg s) with
          | (a, _) ->
              (Node { keys = k; values = v; sibling = a; child = c }, -1)
        end
      else if ( ltekey s k ) then
        begin
             let ns = Node { keys = k; values = v; sibling = s; child = c } in
             (ns, -1)
        end
      else
        begin
         let (cn, w) = (create_skiplist k v rg c) in
         if  (w < 0) then
              (Node { keys = k; values = v; sibling = s; child = cn }, -1)
         else if  w == 0 then
              (Node { keys = k; values = v;
                      sibling =  Node {keys = k; values = v;sibling = s; child = c}; child = cn }, -1)
         else  (Node {keys = k; values = v;
                       sibling = Head {sibling = s; child = cn}; child =c}, w - 1)
        end

{% endhighlight %}
