---
layout: post
title: Data Streaming Algorithms
published: true
---

I hope to add details as I understand this field better. 
I  may also use multiple languages to code parts of algorithms.

# Sketching


# Hadamard Transform



{% highlight python %}
import tensorflow as tf

def hademard(M):
    i = tf.constant(1)
    loop_var = int(tf.math.log(tf.cast(tf.shape(M)[0],tf.float32))/tf.math.log(2.0))
    c = lambda i, d: tf.less_equal(i, tf.cond(loop_var < 1 , lambda: 1, lambda : loop_var))
    T = tf.constant([[1,1],[1,-1]],dtype=tf.float32)
    
    def logic(i, H1):
        operator_1 = tf.linalg.LinearOperatorFullMatrix(H1)
        operator_2 = tf.linalg.LinearOperatorFullMatrix(T/2)
        operator = tf.linalg.LinearOperatorKronecker([operator_1, operator_2])

        H1 = operator.to_dense()
        return tf.add(i, 1),H1

    H0 = tf.ones((1,1))
    i,H2 = tf.while_loop(  c,logic,[i, H0  ])
    H2 = tf.matmul(tf.matmul(H2, M), H2)

    return H2


#M = tf.ones((4,4),tf.float32)
#M = tf.constant([[2, 3], [2, 5]], dtype=tf.float32)
M = tf.linalg.band_part(tf.ones((4,4)), -1, 0)
print(f'Hademard {hademard(M)}')

{% endhighlight %}
