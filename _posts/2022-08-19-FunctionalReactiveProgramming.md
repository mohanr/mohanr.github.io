---
layout: post
title: Functional Reactive Programming( FRP )
published: true
comment: true
---

## Spacemacs

THe following are the keystrokes I needed to use Spacemacs editor for Haskell so that I could focus on the code
without too much distraction. More advanced customizations are possible but for now this suffices.

|KeyStroke     |                |                 |                            
|--------------|----------------|----------------|
|              | Windows        |        <       |
|==============|----------------|----------------|
| C-x b        |   Switch Buffer|        <       |
| SPC b x      |   Kill Buffer  |        <       |
| SPC w x      |   Kill Window  |        <       |
| C-x 2        |   Split Window |        <       |
| C-x k        |   Kill Buffer  |        <       |


| KeyStroke    |                |                |                
|--------------|----------------|----------------|
|              | Files          |        <       |
|==============|----------------|----------------|
| C-x f        |   Open File    |        <       |
| C-x C-s      |   Save File    |        <       |
| SPC w x      |   Kill Window  |        <       |


## Introduction

There are details that are yet to be added to this post but this code works. Since I am a Haskell novice
I can explain only part of the code. The program itself will be refactored as I understand it better.

The code uses [reactive-banana](https://hackage.haskell.org/package/reactive-banana)

### What is functional reactive programming ?


###

{% highlight haskell %}


------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

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
   let handleEvent e@(EventKey k Down _ _) = case k of
            (SpecialKey KeySpace) -> putStrLn "Space" 
            _                   -> putStrLn "Case"
       handleEvent e = event e

   Gloss.playIO
    (InWindow "Functional Reactive" (320, 240) (800, 200))
    white
    30
    ()
    (\() -> readIORef picRef)
    -- (\ ev   _ → quit ev >> () <$ handleEvent ev)
    (\ ev () -> handleEvent ev)
    (\_ () -> pure ())
  where
    quit (EventKey (Char 's' )
                          _ _ _) = reactToKeyPress
    quit  _ = return ()

reactToKeyPress :: IO ()
reactToKeyPress = putStrLn "Key Pressed"


makeNewEvent :: MomentIO (Reactive.Banana.Event ())
makeNewEvent = do
  (enew, new) <- newEvent
  
  tid <- liftIO  $ do
    putStrLn "Fire new Event" 
    new ()

  return enew 

drawBoard :: Picture
drawBoard =
   Pictures $ [ color violet $ translate x y $ rectangleWire 90 90| x<-[0,90..180], y<-[0,90..180] ] 


makeSources =  newAddHandler


type EventSource a = (AddHandler a, a -> IO ())



addHandler :: EventSource a -> AddHandler a
addHandler = fst

eventLoop :: EventSource ()  -> IO ()
eventLoop ( displayvalueevent)  = do
  putStrLn "Fired Event"
  fire displayvalueevent ()

fire :: EventSource a -> a -> IO ()
fire = snd



networkDescriptor :: IORef Picture -> EventSource() -> MomentIO ()
networkDescriptor lastFrame  displayGlossEvent = do
  glossEvent <- fromAddHandler (addHandler displayGlossEvent )
  reactimate $ putStrLn . showValue <$> glossEvent

  picture <- liftMoment (handleKeys displayGlossEvent )
  changes picture >>= reactimate' . fmap (fmap (writeIORef lastFrame))
  valueBLater picture >>= liftIO . writeIORef lastFrame




showValue value = "Value is " ++ show value

handleKeys :: EventSource ()  -> Moment (Behavior Picture)
handleKeys glossEvent = do


  let picture = drawBoard
  return $ pure picture





{% endhighlight %}
