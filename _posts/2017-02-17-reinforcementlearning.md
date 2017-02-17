---
layout: post
title: Reinforcement Learning
published: true
---

{% highlight Haskell %}
module RL where
import Control.Monad.State
import qualified Data.Map as Map

fun :: Map.Map String Int
fun = Map.empty

store :: String -> Int-> State (Map.Map String Int) ()
store row value = do
  fun <- get
  put (Map.insert row value fun)

retrieve :: String -> State (Map.Map String Int) (Maybe (Int))
retrieve roworcolumn = do
  fun <- get
  return (Map.lookup roworcolumn fun) 

main =  print (runState (do {store "row" 1; retrieve "row"}) fun) 
{% endhighlight %}
