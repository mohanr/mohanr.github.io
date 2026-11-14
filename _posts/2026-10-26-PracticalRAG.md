---
layout: post
title: Practical Retrieval Augmented Generation algorithms
published: true
---

A series devoted to the implementation of RAG foundational algorithms. This could be integrated
with an agentic harness I build separately.

I will most probably read other RAG implementation code in order to port other algorithms.

# tl;dr
1. The code will be gradually improved and  be committed to git finally. The version on this
   page is compiled but unfinished until I mention it here. Code is [here[(https://github.com/mohanr/agentic-harness/tree/main/lib/rag)
2. I found it hard to port the Rust iterator pattern to OCaml. So I may have introduced bugs.
3. The OCaml code is verbose now. It has to be refactored. It is not easy to understand
   the algorithm without comments whether it is Rust code or OCaml.
5. I have to research how to integrate my RAG code with an agentic harness I am building. That
   is a separate topic.

I set out to implement the _Leiden community detection_ algorithm initially. The [Rust](https://github.com/graspologic-org/graspologic-native/tree/main/packages) implemenation is my inspiration. As my code is OCaml there was no
direct way to port. In fact the Rust code uses many iterators like the following.


# Rust's iterators pose a problem

{% highlight rust %}

   pub fn neighbors_for(
        &self,
        id: CompactNodeId,
    ) -> NeighborIterator<'_> {
        let neighbor_range: Range<ConnectionId> = self.neighbor_range(id);
        let neighbor_start: ConnectionId = neighbor_range.start;
        NeighborIterator {
            compact_network: self,
            neighbor_range,
            current_neighbor: neighbor_start,
        }
    }
    
   impl Iterator for NeighborIterator<'_> {
    type Item = CompactNeighborItem;

    fn next(&mut self) -> Option<Self::Item> {
        if self.neighbor_range.contains(&self.current_neighbor) {
            let (neighbor_id, edge_weight) = self.compact_network.neighbors[self.current_neighbor];
            let item = CompactNeighborItem {
                connection_id: self.current_neighbor,
                id: neighbor_id,
                edge_weight,
                node_weight: self.compact_network.nodes[neighbor_id].0,
            };
            self.current_neighbor += 1;
            Some(item)
        } else {
            None
        }
    }
}

   pub fn total_edge_weight_per_node(&self) -> Vec<f64> {
        // when using modularity, this should return the exact same as node_weights.
        (0..self.nodes.len())
            .map(|node_id| {
                self.neighbors_for(node_id)
                    .map(|neighbor| neighbor.edge_weight)
                    .sum::<f64>()
            })
            .collect()
    }
 {% endhighlight%}

 I have elided many other parts of the code but the general idea is this. I was not clear about
 the way forward when I started to port this to OCaml. The Rust code uses a pattern that is powerful
 but it does seem that a simpler method would also suffice. I am not a Rust programmer. So my
 assessment could be wrong.

 But in any case I found some OCaml effect handler patterns to implement the same type of iterator pattern.

 # Key parts of the network creation code

 I have only tried to create a network by loading test data. No proper tests are executed. I found it hard
 to understand the Rust code to load and operate  a network, not to mention the difficulty of implementation
 of the Leiden community detection algorithm.

 # TODO Explain with code 

 
