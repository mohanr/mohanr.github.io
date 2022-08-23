---
layout: post
title: Functional Reactive Programming( FRP )
published: true
comment: true
---

## Introduction

There are details that are yet to be added to this post but this code works.

The code uses [reactive-banana](https://hackage.haskell.org/package/reactive-banana)

### What is functional reactive programming ?


###

{% highlight haskell %}

------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Data.IORef
import Data.Bool (bool)
import Data.IORef (newIORef, readIORef, writeIORef)
import Graphics.Gloss hiding (pictures)
import Reactive.Banana
import Reactive.Banana.Frameworks
import Graphics.Gloss.Interface.IO.Game( Event(..) )
import Graphics.Gloss.Interface.IO.Game( MouseButton(..) )
import Graphics.Gloss.Interface.IO.Game( KeyState( Down ) )
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Interface.IO.Game as Gloss (Event, playIO)



main = do

   picRef ← newIORef blank
   (eventHandler, event) ← newAddHandler

   sources <- makeSources
   network <- compile $ networkDescriptor picRef sources
   actuate network
   eventLoop sources

   Gloss.playIO
    (InWindow "Reactive-Banana Example" (320, 240) (800, 200))
    white
    30
    ()
    (\() -> readIORef picRef)
    (\ ev   _ → quit ev >> () <$ event ev)
    (\_ () -> pure ())
  where
    quit (EventKey (Char 's' )
                          _ _ _) = reactToKeyPress
    quit  _ = return ()

reactToKeyPress :: IO ()
reactToKeyPress = putStrLn "Key Pressed"

window :: Display
window = InWindow "Functional Reactive" (300, 300) (10, 10)
  

makeNewEvent :: MomentIO (Reactive.Banana.Event ())
makeNewEvent = do
  (enew, new) <- newEvent
  
  tid <- liftIO  $ do
    putStrLn "Fire new Event" 
    new ()

  return enew 

drawBoard :: Picture
drawBoard =
   Pictures $ [ translate x y $ rectangleWire 90 90| x<-[0,90..180], y<-[0,90..180] ] 


makeSources =  newAddHandler


type EventSource a = (AddHandler a, a -> IO ())



addHandler :: EventSource a -> AddHandler a
addHandler = fst

eventLoop :: EventSource ()  -> IO ()
eventLoop ( displayvalueevent)  =
  fire displayvalueevent ()

fire :: EventSource a -> a -> IO ()
fire = snd



networkDescriptor :: IORef Picture -> EventSource() -> MomentIO ()
networkDescriptor lastFrame  displayGlossEvent = do
  glossEvent <- fromAddHandler (addHandler displayGlossEvent )
  picture <- liftMoment (handleKeys displayGlossEvent )
  reactimate $ putStrLn . showValue <$> glossEvent
  changes picture >>= reactimate' . fmap (fmap (writeIORef lastFrame))
  valueBLater picture >>= liftIO . writeIORef lastFrame


showValue value = "Value is " ++ show value

handleKeys :: EventSource ()  -> Moment (Behavior Picture)
handleKeys glossEvent = do


    let picture = drawBoard

    return $ pure picture




{% endhighlight %}
