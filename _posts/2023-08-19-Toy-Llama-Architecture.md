---
layout: post
title: Llama Architecture(GPT)
published: true
---

# tl;dr
1. The architecture is described in [Llama: Open and Efficient Foundation Language Models](http://arxiv.org/pdf/2302.13971.pdf)
2. The code is ported from PyTorch to TensorFlow 2.x using various references. (e.g) https://nn.labml.ai/transformers/rope/index.html
   TensorFlow is very verbose and sometimes it poses difficulties as every line of PyTorch has to be ported.
4. In very few cases the code can be directly ported with minimal changes. But this situation isn't common.
5. The math supporting the algorithm is only partially understood. There are several research papers to read.
6. The RoPE embeddings need more insight and explanation.


# Batches

{% highlight python %}
def random_sample(text,block_size):
    rand = tf.random.uniform(shape=(batch_size,), minval=1, maxval=length - (block_size + 1),dtype=tf.int32)
    return [tf.strings.substr(text,i, block_size, unit='BYTE') for i in rand]

def draw_random_sample_batches(block_size):
        sample = random_sample(input,block_size)
        tf.map_fn(map_fn,tf.strings.bytes_split(sample))
        global samplelist
        X = tf.stack([inp[  : -1] for inp in samplelist])
        y = tf.stack([inp[ 1 :  ] for inp in samplelist])
        samplelist = []
        return X,y

{% endhighlight %}

# RMSNorm

{% highlight python %}

import numpy as np
import tensorflow as tf


class RMSNorm(tf.keras.Model):

    def __init__(self, layer_shape):
        super(RMSNorm, self).__init__()
        self.scale = tf.Variable(initial_value=np.ones(layer_shape), trainable=True,dtype=tf.float32)

    def call(self, x):
        normalized_mat, norm = tf.linalg.normalize(x, axis=(1, 2))
        # print(f'Normalize {norm}')
        rms = tf.multiply(norm ,
                             tf.pow(tf.cast(tf.size(x[0]),tf.float32),-0.5))
        r = tf.divide(x , rms )
        return tf.multiply(self.scale[:tf.shape(x)[1], :] , r)

{% endhighlight %}

# Rotary Positional Embeddings( RoPE)

Code used to test _RoPE_ embeddings

{% highlight python %}

from matplotlib import pyplot as plt
import tensorflow as tf
from Parameters import n_embd
from RotaryPositionalEmbeddings import RotaryPositionalEmbeddings


def rotaryPositionalEmbeddingsTest():
    K = 3
    rotary_emb = RotaryPositionalEmbeddings()
    R = rotary_emb.rotary_matrix(tf.pow(K,2),n_embd)
    fig, ax = plt.subplots(K, K, figsize=(K * 3, K * 4))

    for i in range(K):
        for j in range(K):
            ax[i, j].imshow(R[i * K + j, :, :])
            ax[i, j].set_title(f'rotation at {i * K + j}')
    # plt.show()
    plt.savefig("rotraryembeddings.png")

if __name__ == "__main__":
    rotaryPositionalEmbeddingsTest()

{% endhighlight %}

{% highlight python %}

import tensorflow as tf
import numpy as np

class RotaryPositionalEmbeddings(tf.keras.Model):

    def __init__(self):
        super(RotaryPositionalEmbeddings, self).__init__()

    def rotary_matrix( self,block_size, embedding_dim):
        R = tf.Variable(tf.zeros((block_size, embedding_dim, embedding_dim)))
        i = tf.constant(0)
        p_i = tf.constant(0)
        neg_2 = tf.constant(-2)
        emb = lambda i, d: tf.less(i, int(tf.divide(embedding_dim , 2) - 1))
        p = lambda p_i, d: tf.less(p_i, block_size )
        print(int(tf.divide(embedding_dim , 2)))
        def position(p_i, p_idx):
            def embedding(i, idx):
                theta = tf.pow(10000. , tf.divide(tf.multiply(neg_2 , tf.subtract(i , 1)) , embedding_dim))
                m_theta = tf.multiply(tf.cast(p_i,tf.float32) , tf.cast(theta,tf.float32))
                R[p_i, tf.multiply(2, i),tf.multiply(2, i)].assign(tf.cos(m_theta))
                # print(i, p_i, tf.multiply(2, i), tf.multiply(2, tf.add(i , 1)))
                R[p_i, tf.multiply(2, i), tf.multiply(2, tf.add(i , 1))].assign(- tf.sin(m_theta))
                R[p_i, tf.multiply(2, tf.add(i , 1)), tf.multiply(2, i)].assign(tf.sin(m_theta))
                R[p_i, tf.multiply(2, tf.add(i , 1)), tf.multiply(2, tf.add(i , 1))].assign(tf.cos(m_theta))

                return tf.add(i, 1), idx

            _, idx = tf.while_loop(emb, embedding, loop_vars=[i, embedding_dim])
            return tf.add(p_i, 1), p_idx


        _, idx = tf.while_loop(p, position, loop_vars=[p_i, block_size])
        return R
{% endhighlight %}

![image-title-here](../images/rotaryembeddings.png){:class="img-responsive"}


