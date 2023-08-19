---
layout: post
title: Llama Architecture(GPT)
published: false
---

# tl;dr
1. The architecture is described in [Llama: Open and Efficient Foundation Language Models](http://arxiv.org/pdf/2302.13971.pdf)


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
