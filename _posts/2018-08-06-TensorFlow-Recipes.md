---
layout: post
title: TensorFlow Recipes
published: true
---

### 

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
