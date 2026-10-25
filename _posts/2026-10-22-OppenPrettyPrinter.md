---
layout: post
title: Imperative pretty printer
published: false
---

The title means that there is already a functional pretty printer by *Wadler* but even though I use
OCaml the following code is based on Benjamin Pizza's Gutenberg library. The paper that shown the imperative
version is shown below.

# Pretty printer


![image-title-here](../images/Oppen.png){:width="10%"}{:class="img-responsive"}

{% highlight OCaml %}

let%expect_test "hello world"=
    let type1 = Alternative(Line, (Text " ")) in
    let formatted_doc = Append(Text "hello" , Append(type1 ,( Text "world"))) in
    Printf.printf "%s" (Layout.layout (Doc formatted_doc ));
    [%expect {|
      stack: ((Doc(Append(Text hello)(Append(Alternative Line(Text" "))(Text world)))))
      hello
      world
      |}]


{% endhighlight %}

![image-title-here](../images/Oppen1.png){:width="10%"}{:class="img-responsive"}
