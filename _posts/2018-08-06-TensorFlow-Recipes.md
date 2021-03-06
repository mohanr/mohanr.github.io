---
layout: post
title: TensorFlow Recipes
published: true
---

As part of my effort to engage with the Open-source community I started answering Stackoverflow questions. I started by asking
a few decent questions and then answered some of my own questions and accepted them. Gradually I understood that this community site is not only about answering but also asking good questions that others can answer.

That is how one builds one's [reputation](https://stackoverflow.com/help/whats-reputation). I believe a key skill one develops by answering is the ability to clearly understand what is being asked.

Here I've collected some Tensorflow recipes most of which are my answers to Stackoverflow questions. Not all though. Some are code
samples I built for myself to understand Tensorflow. I plan to add more explanations and some diagrams to make the code clearer.

The Tensorflow version that I use is 1.xx. I mention this because the framework is under constant development. 

# Table of contents
1. [Index one matrix based on another](#matrixindex)
2. [Roots of a polynomial by Halley's method](#Halleysmethod)
3. [How does _tf.scan_ work ?](#tfscan)
4. [How do you use TensorArray ?](#tensorarray)
5. [Initialize a variable by feeding a placeholder](#variableinitialization)

### How to get values of each row in a matrix according to the max and secondary value indexes which I got from another matrix ? <a name="matrixindex">

{% highlight Python %}

import tensorflow as tf

A = tf.Variable([[10,11,12],
     [13,14,15],
     [16,17,18]], name='A')

B = tf.Variable([[2,1],
     [0,2],
     [1,2]] , name='B')


sess = tf.Session()
sess.run(tf.global_variables_initializer())

indices =  sess.run(B)


incre = tf.Variable(0)
template = tf.Variable(tf.zeros([6,2],tf.int32))
sess.run(tf.global_variables_initializer())
row = tf.gather( indices , [0,1,2])

for i in range(0, row.get_shape()[0] ) :
    newrow = tf.gather(row, i)

    exprow1 = tf.concat([tf.constant([i]), newrow[1:]], axis=0)
    exprow2 = tf.concat([tf.constant([i]), newrow[:1]], axis=0)

    template = tf.scatter_update(template, incre, exprow1)
    template = tf.scatter_update(template, incre + 1, exprow2)
    with tf.control_dependencies([template]):
        incre = tf.assign(incre,incre + 2)

r = tf.gather_nd(A,template)
print(sess.run(template))

{% endhighlight %}

### Finding roots of a polynomial by Halley's method using tensorflow <a name="Halleysmethod">

{% highlight Python %}

import tensorflow as tf

h = tf.constant(.00000001, dtype='float64')
eps = tf.constant(.000001, dtype='float64')
b = tf.constant(2.0, tf.float64)

def f(x):
    return tf.subtract( tf.multiply(x , x ) , 2. )

def fp(x):
    return  tf.divide( tf.subtract( f(tf.add(x, h)) ,
                                    f(x)
                                  ) ,
                       h
                     )

def fpp(x):
    return tf.divide( tf.subtract( fp( tf.add(x , h)) ,
                                   fp(x)
                                 ),
                       h
                     )

def cond(i, x_new, x_prev):
    return tf.logical_and( i < 5,
                           tf.less_equal( tf.abs( tf.cast(tf.subtract( x_new ,
                                                                       x_prev),dtype='float64')),
                                          eps
                                        )
                         )

def body( i, x_new, x_prev ):
    fx = f( x_prev )
    fpx = fp( x_prev )
    x_new = tf.subtract( x_prev ,
                          tf.divide( b * fx * fpx  ,
                                     tf.subtract(b * fpx * fpx,
                                                 fx * fpp( x_prev )
                                                )
                                   )
                       )
    xnew = tf.Print(x_new, [x_new], message="The root is : ")

    with tf.control_dependencies([x_new,xnew]):
        x_prev = tf.identity(xnew)

    return [i + 1, xnew, x_prev ]

sess = tf.Session()
sess.run(tf.global_variables_initializer())


print( sess.run(tf.while_loop(cond, body, [1, b - fpp(b), b])) )

{% endhighlight %}

### How does _tf.scan_ work ? <a name="tfscan">

I will add more explanation in due time. But for now this code updates one row of a _tf.zeros(5,5)_ tensor. But _tf.scan_ is way more powerful than this.

{% highlight Python %}

import tensorflow as tf

input = tf.constant([3, 2, 4, 1, 0],tf.int16)

zeros = tf.Variable(tf.zeros(5,5),tf.int32)

def modify(zeros, x):
    row1, row2, row3, row4, row5 = zeros
    x_tensor = tf.convert_to_tensor([x])

    return [row1, x_tensor, row3, row4, row5]

update = tf.scan(modify, input, initializer=[zeros[0:1],
                                             zeros[1:2],
                                             zeros[2:3],
                                             zeros[4:5],
                                             zeros[4:5]])

with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())

    print(sess.run([update]))

{% endhighlight %}

The output is this.

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{font-family:Arial, sans-serif;font-size:14px;padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;border-color:black;}
.tg th{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;border-color:black;}
.tg .tg-0pky{border-color:inherit;text-align:left;vertical-align:top}
</style>
<table class="tg">
  <tr>
    <th class="tg-0pky">0</th>
    <th class="tg-0pky">0</th>
    <th class="tg-0pky">0</th>
    <th class="tg-0pky">0</th>
    <th class="tg-0pky">0</th>
  </tr>
  <tr>
    <td class="tg-0pky">3</td>
    <td class="tg-0pky">2</td>
    <td class="tg-0pky">4</td>
    <td class="tg-0pky">1</td>
    <td class="tg-0pky">0</td>
  </tr>
  <tr>
    <td class="tg-0pky">0</td>
    <td class="tg-0pky">0</td>
    <td class="tg-0pky">0</td>
    <td class="tg-0pky">0</td>
    <td class="tg-0pky">0</td>
  </tr>
  <tr>
    <td class="tg-0pky">0</td>
    <td class="tg-0pky">0</td>
    <td class="tg-0pky">0</td>
    <td class="tg-0pky">0</td>
    <td class="tg-0pky">0</td>
  </tr>
  <tr>
    <td class="tg-0pky">0</td>
    <td class="tg-0pky">0</td>
    <td class="tg-0pky">0</td>
    <td class="tg-0pky">0</td>
    <td class="tg-0pky">0</td>
  </tr>
</table>

### How do you use TensorArray ? <a name="tensorarray">

{% highlight Python %}

import numpy as np
import tensorflow as tf

a = tf.get_variable("a",[5,2],dtype=tf.float32,initializer=tf.constant_initializer(np.array([[1.5,0.2],[2.3,0.1],[1.3,0.2],[2.2,0.09],[4.4,0.8]],dtype=np.float32)))

rows = tf.shape(a)[0]
results = tf.TensorArray(dtype=tf.float32, size=0, dynamic_size=True,infer_shape=False)

init_state = (0, results)
condition = lambda i, _: i < rows
body = lambda i, results: (i + 1, results.write(i, a[i] ))
n, r = tf.while_loop(condition, body, init_state)
unstacked_result = r.stack()

# run the graph
with tf.Session() as sess:
    init = tf.initialize_all_variables()
    sess.run(init)
    # print the output of unstacked_result
    print(sess.run(unstacked_result))
{% endhighlight %}

### Can variable initialization depend on the fed value of a placeholder ? <a name="variableinitialization">
     
Even though we can initialize a variable that depends on a placeholder like this, independent variables like W will get initialized repeatedly. More checks are needed to initialize only variables like F which are dependent on placeholders. 

This is so because everytime a value is fed to the placeholder the dependent variable F will be initialized. But other independent variables need not be.

     
{% highlight Python %}
import tensorflow as tf

sess = tf.InteractiveSession()

X = tf.placeholder(tf.float32, name="X")

W = tf.Variable([1, 2, 1], dtype=tf.float32, name="weights")
W = tf.reshape(W, [1, 3])

var = tf.reshape([X*X,X,1],[3,1])
F = tf.get_variable('F', dtype=tf.float32, initializer=var)

init = tf.global_variables_initializer()
Y=tf.matmul(W,F)

for i in range(10):
    sess.run([init], feed_dict={X: i})
    print(sess.run(Y))

{% endhighlight %}

