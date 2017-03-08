---
layout: post
title: Reinforcement Learning(Unfinished post)
published: true
---

## Introduction





```haskell
module RL where
import Control.Monad.State
import qualified Data.Map as Map
import Control.Applicative
import Graphics.Gloss

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

getrow = do {store "row" 1; retrieve "row"}  
getcolumn = do {store "column" 1; retrieve "column"}  
getboardsize = do   
           let x = (runState getrow fun) in
             let y = (runState getcolumn fun) in
                (Just (*) <*> (fst x)  <*>  (fst y) )

 
```
```haskell
putmagicsquare = do { store "!" 2; store "2" 9;store "3" 4;
                      store "4" 7; store "5" 5;store "6" 4; 
                      store "6" 7; store "1" 5;store "8" 4; 
                    }
```
## Haskell Gloss 

While porting the code I realized that a visual representation is helpful. I used Haskell Gloss UI toolkit to draw the board and the pieces based on the _BoardState_

```haskell
translationaccumulator ::   [Int] -> [Int] -> [(Float,Float)] -> [Picture] -> [Picture]
translationaccumulator  [] _ _ ys = reverse ys
translationaccumulator  _ []  _ ys = reverse ys
translationaccumulator  (head1:xs1) (head:xs) angle  ys = let (a,b) = (angle !!(head - 1)) in
                                                            let (c,d) = (angle  !!(head1 - 1)) in
                                                              translationaccumulator xs1 xs angle ( ((translate a b) $
                                                                                                 drawx ) : ((translate c d) $
                                                                                                 drawo ):ys)


```

![image-title-here](../images/grid.PNG){:class="img-responsive"}

```haskell
drawBoard :: BoardState -> Picture
drawBoard (BoardState xloc oloc index)=
  Pictures $ [ translate x y $ rectangleWire 90 90| x<-[0,90..180], y<-[0,90..180] ] ++ (translationaccumulator xloc oloc [(0,180),(90,180),(180,180),(0,90),(90,90),(180,90),(0,0),(90,0),(180,0)] [])

drawx :: Picture
drawx = color green $ rotate 45 $
        pictures [rectangleWire 1 45, rectangleWire  45 1] 

drawo :: Picture
drawo = color rose $ thickCircle 25 2

 

```
 
```haskell
powersof2  :: [Int]  
powersof2  =  [ 2 ^ i | i <- [0..9]]

stateindex :: [Int] -> [Int] -> Int  
stateindex xloc oloc =  let powers = powersof2 in
                           foldl (+) 0 [  ( powers !!n) | n <- [0..(length xloc - 1)]]
 
```

The ReaderT Monad transformer for reading and writing to arrays.


```haskell
type ArrayAccess = ReaderT  (IOArray Int Int)  IO 
type ArrayWriteAccess = ReaderT  (IOArray Int Int)  IO() 

readvalue ::  Int -> ArrayAccess Int   
readvalue x    = do 
  a <- ask
  b <- liftIO( readArray a x);    
  return b

writevalue ::  Int -> Int -> ArrayWriteAccess   
writevalue x y   = do 
  a <- ask
  liftIO( writeArray a x y)    

-- Test array accesses
readfromarray = do { a <- createarray; liftIO (runReaderT (readvalue 1) a) }
writetoarray = do { a <- createarray; liftIO (runReaderT (writevalue 1 2) a) }
```
Haskell Enum to differentiate between players using X's and O's.

```haskell
data Player = X | O deriving Show
isX :: Player -> Bool
isX X = True
isX O = False 


```

### Calculate the next state in the board.


```haskell
append :: Int -> [Int] -> [Int]
append elem l = l ++ [elem]

readthevalue :: ( IOArray Int Int) -> Int -> IO Int
readthevalue a index =  liftIO (runReaderT (readvalue index ) a) 

writethevalue :: ( IOArray Int Int) -> Int -> Int -> IO ()
writethevalue a index value =  liftIO (runReaderT (writevalue index value) a) 
  
nextstate :: Player -> BoardState -> Int -> BoardState
nextstate  player (BoardState xloc oloc index) move= BoardState newx newo newindex where
  newx = if isX player then (append move xloc) else xloc
  newo = if isX player then (append move oloc) else oloc
  newindex = stateindex newx newo

magicnumber :: [Int]-> Int
magicnumber l = sum $ ([magicsquare !! (x-1) | x <- l])


newnextstate :: ( IOArray Int Int) -> BoardState-> IO ()
newnextstate  a ( BoardState xloc oloc index) =  do
  x <- readthevalue a index;
  if (x == 0)
  then if ((magicnumber xloc ) == 15)
       then (writethevalue a index 0)
       else if ((magicnumber oloc ) == 15)
            then (writethevalue a index 1)
            else pure ()
  else pure ()
```
Get a list of empty positions in the board.

```haskell
-- Returns a list of unplayed locations
possiblemoves :: BoardState -> [Int]
possiblemoves (BoardState xloc oloc index) =
  let xs =  [1,2,3,4,5,6,7,8,9] in
    (xs \\ xloc) \\ oloc

```

Select an empty position randomly

```haskell
--   "Returns one of the unplayed locations, selected at random"
randommove ::  BoardState -> IO Int
randommove state = 
  let possibles = possiblemoves state in
    case possibles of
      p ->   fmap (p !! ) $ randomRIO(0, length p - 1)
```
 
```haskell
main =  do print (runState getrow fun)
           let x = (runState getrow fun)
           let y = (runState getcolumn fun)

           let ms = (runState putmagicsquare fun)
           print (stateindex [1,2,3] [4,5,6])
           display (InWindow "Reinforcement Learning" (530,530) (220,220)) (greyN 0.5)  (drawBoard (BoardState [1,2,3] [4,5,6] 1))
           return ()

```
