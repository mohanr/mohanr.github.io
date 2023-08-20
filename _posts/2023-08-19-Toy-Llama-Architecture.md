---
layout: post
title: Llama Architecture(GPT)
published: false
---

# tl;dr
1. The architecture is described in [Llama: Open and Efficient Foundation Language Models](http://arxiv.org/pdf/2302.13971.pdf)
2. The code is ported from PyTorch to TensorFlow 2.x using various references. (e.g) https://nn.labml.ai/transformers/rope/index.html
3. In very few cases the code can be directly ported with minimal changes. But this situation isn't common.
4. The math supporting the algorithm is only partially understood. There are several research papers to read.


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

