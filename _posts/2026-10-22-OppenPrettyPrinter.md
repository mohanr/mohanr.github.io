---
layout: post
title: Static Analyzer
published: false
---

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
