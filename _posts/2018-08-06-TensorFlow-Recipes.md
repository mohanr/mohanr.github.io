---
layout: post
title: TensorFlow Recipes
published: true
---

As part of my effort to engage with the Open-source community I started answering Stackoverflow questions. I started by asking
a few decent questions and then answered some of my own questions and accepted them. Gradually I understood that this community site is not only about answering but also asking good questions that others can answer.

That is how one builds one's [reputation](https://stackoverflow.com/help/whats-reputation). I believe a key skill one develops by answering is the ability to clearly understand what is being asked.

Here I've collected some Tensorflow recipes some of which are my answers to Stackoverflow questions. Not all though. Some are code
samples I built for myself to understand Tensorflow. I plan to add more explanations and some diagrams to make the code clearer.

The Tensorflow version that I used was 1.10. I mention this because the framework is under constant development. 

### How to get values of each row in a matrix according to the max and secondary value indexes which I got from another matrix ?

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

### Finding roots of a polynomial by Halley's method using tensorflow 

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

### How does _tf.scan_ work ?

I will add more explanation is due time. But for now this code one row of a _tf.zeros(5,5)_ tensor. But this is way more powerful than this.

{% highlight Python %}

import tensorflow as tf

input = tf.constant([3, 2, 4, 1, 0],tf.int16)

zeros = tf.Variable(tf.zeros(5,5),tf.int32)

index = tf.Variable(0)

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

<table class="tg">
  <tr>
    <th class="tg-baqh" colspan="2">Some values</th>
  </tr>
  <tr>
    <td class="tg-baqh">0</td>
    <td class="tg-baqh">0</td>
    <td class="tg-baqh">0</td>
    <td class="tg-baqh">0</td>
    <td class="tg-baqh">0</td>
  </tr>
  <tr>
    <td class="tg-baqh">3</td>
    <td class="tg-baqh">2</td>
    <td class="tg-baqh">4</td>
    <td class="tg-baqh">1</td>
    <td class="tg-baqh">0</td>
  </tr>
  <tr>
    <td class="tg-baqh">0</td>
    <td class="tg-baqh">0</td>
    <td class="tg-baqh">0</td>
    <td class="tg-baqh">0</td>
    <td class="tg-baqh">0</td>
  </tr>
  <tr>
    <td class="tg-baqh">0</td>
    <td class="tg-baqh">0</td>
    <td class="tg-baqh">0</td>
    <td class="tg-baqh">0</td>
    <td class="tg-baqh">0</td>
  </tr>
  <tr>
    <td class="tg-baqh">0</td>
    <td class="tg-baqh">0</td>
    <td class="tg-baqh">0</td>
    <td class="tg-baqh">0</td>
    <td class="tg-baqh">0</td>
  </tr>
</table>
