---
layout: post
title: Tikz Notes
published: true
---

![image-title-here](../images/tikzeditor.png){:class="img-responsive"} 

Latex code that I use to draw various types of diagrams.

{% highlight OCaml %}

[auto, node distance=5cm]
\tikzstyle{block} = [draw, rectangle, minimum height=3em, minimum width=3em]
\tikzstyle{virtual} = [coordinate]

    % Place nodes
    \node [virtual]                 (input)     {};
    \node [virtual]                 (input1)     {};
    \node [rounded corners=5pt][name=t,block, right of=input]   (model)     {\(
        \pi_\theta (u | s )
		 \)};
    \node [virtual, right of=model] (output)    {};
    \node [virtual, right of=model] (feedback)  {};
    \node [rounded corners=5pt][name=e,block, below right of=input1]   (model1)     {Environment};
    \node [virtual, above left of=model1] (reward)    {};
    \node [name=i,virtual, below left] (input1)    {};
    \node [name=r, left of=model1] {\( reward \ r_t \)} (reward);
    % Connect nodes
    \draw [-] (model) -- node [name=y] 
	{\(action \
	   u_t \)}
	(output);
    \draw [->] (y) |- (feedback) |- (model1.-10);
    \draw [->] (model1.180) |- (r) |- (model.-180);
 

{% endhighlight %}

![image-title-here](../images/policy.png){:class="img-responsive"} 