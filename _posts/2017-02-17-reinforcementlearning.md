---
layout: post
title: Reinforcement Learning(Unfinished post)
published: true
---

## Introduction

{% highlight haskell %}fun :: Map.Map String Int
fun = Map.empty


store :: String -> Int-> State (Map.Map String Int) ()
store x value = do
  fun <- get
  put (Map.insert x value fun)

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
{% endhighlight %}

### Magic Square

{% highlight haskell %}
magicsquare :: [Int]
magicsquare = [2,9,4,7,5,4,7,5,4] 
{% endhighlight %}

### State of the board

{% highlight haskell %}
data BoardState = BoardState { xloc :: [Int],
                               oloc :: [Int],
                               index :: Int
                             }  deriving (Show)
{% endhighlight %}

### Haskell Gloss 


{% highlight haskell %}
translationaccumulator ::   [Int] -> [Int] -> [(Float,Float)] -> [Picture] -> [Picture]
translationaccumulator  [] _ _ ys = reverse ys
translationaccumulator  _ []  _ ys = reverse ys
translationaccumulator  (head1:xs1) (head:xs) angle  ys = let (a,b) = (angle !!(head - 1)) in
                                                            let (c,d) = (angle  !!(head1 - 1)) in
                                                              translationaccumulator xs1 xs angle ( ((translate a b) $
                                                                                                 drawx ) : ((translate c d) $
                                                                                                 drawo ):ys)

drawBoard :: BoardState -> Picture
drawBoard (BoardState xloc oloc index)=
  Pictures $ [ translate x y $ rectangleWire 90 90| x<-[0,90..180], y<-[0,90..180] ] ++ (translationaccumulator xloc oloc [(0,180),(90,180),(180,180),(0,90),(90,90),(180,90),(0,0),(90,0),(180,0)] [])

drawx :: Picture
drawx = color green $ rotate 45 $
        pictures [rectangleWire 1 45, rectangleWire  45 1] 

drawo :: Picture
drawo = color rose $ thickCircle 25 2
{% endhighlight %}

{% highlight haskell %}
powersof2  :: [Int]  
powersof2  =  [ 2 ^ i | i <- [0..8]]


createarray :: IO ( IOArray Int Double)
createarray =  do {
                       arr <- newArray (0,512*512) (-1.0);
                       return arr
                  }

addVal :: Int -> [Int] -> [Int]
addVal i [] = []
addVal i (x:xs) = x * 512: addVal i xs

stateindex :: [Int] -> [Int] -> Int
stateindex xloc oloc = sum (map (2^) xloc)
                       + sum [2^n | n <- (addVal 512 oloc)]
{% endhighlight %}

The ReaderT Monad transformer for reading and writing to arrays.

{% highlight haskell %}

type ArrayAccess = ReaderT  (IOArray Int Double)  IO 
type ArrayWriteAccess = ReaderT  (IOArray Int Double)  IO() 
readvalue ::  Int -> ArrayAccess  Double  
readvalue x    = do 
  a <- ask
  b <- liftIO( readArray a x);    
  return b
writevalue ::  Int -> Double -> ArrayWriteAccess   
writevalue x y   = do 
  a <- ask
  liftIO( writeArray a x y)    
-- Test array accesses
readfromarray = do { a <- createarray; liftIO (runReaderT (readvalue 1) a) }
writetoarray = do { a <- createarray; liftIO (runReaderT (writevalue 1 2) a) }
{% endhighlight %}

{% highlight haskell %}
data Player = X | O deriving Show
isX :: Player -> Bool
isX X = True
isX O = False 
{% endhighlight %}


### Calculate the next state in the board.


Get a list of empty positions in the board.

{% highlight haskell %}
-- Returns a list of unplayed locations
possiblemoves :: BoardState -> [Int]
possiblemoves (BoardState xloc oloc index) =
  let xs =  [1,2,3,4,5,6,7,8,9] in
    (xs \\ xloc) \\ oloc
{% endhighlight %}
```

Select an empty position randomly

{% highlight haskell %}
--   "Returns one of the unplayed locations, selected at random"
randommove ::  BoardState -> IO Int
randommove state = 
  let possibles = possiblemoves state in
    case possibles of
      p ->   fmap (p !! ) $ randomRIO(0, length p - 1)
{% endhighlight %}

### Greedy move

{% highlight haskell %}
greedymove ::  (String -> IO()) ->( IOArray Int Double) ->Player -> BoardState -> IO (Int,IOArray Int Double)
greedymove log a player state = 
  let possibles = possiblemoves state in
    case possibles of
      [] -> return (0, a)
      p  -> let bestvalue = -1.0 in
              let bestmove = 0 in
                choosebestmove a p bestvalue bestmove
                where
                  choosebestmove arr [] bestvalue1 bestmove1 = return (0,a)
                  choosebestmove arr (x:xs) bestvalue1 bestmove1 = do
                    (nv,b) <- nextvalue logs player x arr state
                    xvalue <-  catch (readthevalue b (ReinforcementLearning.index (nv)))(\(SomeException e) -> printf "Reading [%d} in greedy move" x >> print e >> throwIO e)
                    case compare bestvalue1 xvalue of
                      LT -> choosebestmove b xs xvalue x;
                      GT -> return (bestmove1,b)
                      EQ -> return (bestmove1,b)
  

{% endhighlight %}

### Abandoning the functional approach with this function

This is basically the original _Lisp_ converted line by line to Haskell. The Haskell programmers who I consulted dissuaded me from doing this but at this time my Haskell knowledge does not measure up to the task.

{% highlight haskell %}
gameplan :: (String -> IO()) ->( IOArray Int Double) -> BoardState -> BoardState -> IO (IOArray Int Double,BoardState,Double) 
gameplan log a state newstate = do 
  r1 <- randombetween;
  initialvalue <- readthevalue  a 0
  result <- (terminalstatep log a (ReinforcementLearning.index newstate));
    case result of
      True -> do
        b <- update a state newstate
        valueofnewstate <- catch (readthevalue b (ReinforcementLearning.index newstate)) (\(SomeException e) -> print e >> mapM_ (putStr . show) [ (ReinforcementLearning.index newstate)]>> throwIO e)
        log $ printf "Gameplan returns(True branch) %f\n " valueofnewstate
        return (b,newstate,valueofnewstate)
      False -> do
        rm <- randommove newstate
        (gm,c) <- greedymove log a O newstate
        log $ printf "Greedy Move is %d \n " gm
        valueofnewstate <-  catch (readthevalue c (ReinforcementLearning.index newstate)) (\(SomeException e) -> print e >> mapM_ (putStr . show) [ (ReinforcementLearning.index newstate)]>> throwIO e)
        -- if (gm == 0)
        --   then do
        --   return(c,newstate,valueofnewstate)
        --   else do
        (nv,d) <- nextvalue logs O (randomgreedy log r1 rm gm) c newstate
        d' <- if r1 < 0.01 then return d else update d state nv
        result1 <- (terminalstatep log d' (ReinforcementLearning.index nv));
        valueofnewstate1 <-  catch (readthevalue d' (ReinforcementLearning.index nv)) (\(SomeException e) -> print e >> mapM_ (putStr . show) [ (ReinforcementLearning.index nv)]>> throwIO e)
        if (length (possiblemoves nv) == 0)
          then
          return (d',nv,valueofnewstate1)
          else if result1
               then do
               log $ printf "Gameplan returns(False branch) %f\n " valueofnewstate1
               return (d',nv,valueofnewstate1)
               else do
               r <- randommove newstate
               (nv1,d1') <- nextvalue logs X r d' newstate
               gameplan log d1' newstate (nv1)
{% endhighlight %}

{% highlight haskell %}

playntimes :: IOArray Int Double -> (String -> IO()) ->Int -> IO (IOArray Int Double)
playntimes a log n = do writethevalue a 0 0.5
                        r <- (randommove (BoardState [] [] 0))
                        playtime  a (BoardState [] [] 0) (nextvalue logs X r a (BoardState [] [] 0)) n 0 r
                          where
                            playtime :: IOArray Int Double -> BoardState -> IO (BoardState,IOArray Int Double) -> Int -> Double -> Int -> IO (IOArray Int Double)
                            playtime newa s ns n acc r
                              | n == 0 = do logsresult $ printf "Played 100 times %f  %f"  acc (acc/100.0)
                                            return newa
                              | n > 0 = do
                                  (boardstate, b) <- ns 
                                  (newa, state, result )<- game logs s  boardstate b; 
                                  log $ printf "Game returns %f\n" result
                                  r1 <- randommove (BoardState [] [] 0)
                                  playtime newa (BoardState [] [] 0) (nextvalue logs X  r1 newa (BoardState [] [] 0)) (n - 1) (acc + result) r1
  
numruns :: IOArray Int Double ->Int -> Int -> Int -> IO()
numruns a n bins binsize  
  | n == 0 = printf "\nPlayed numruns times"
  | n > 0 = do
      arr <- newArray (0,bins) 0;
      b <- playrepeatedly a arr n bins binsize
      numruns b (n -1) bins binsize

playrepeatedly ::  IOArray Int Double ->IOArray Int Double -> Int -> Int -> Int -> IO(IOArray Int Double)
playrepeatedly a arr numrun numbins binsize = do 
 loop a 0 binsize
    where
      loop a i bs
        | i == numbins = let x = numrun
                             y = numbins
                             z = binsize in
                           loop1 a x 0 y z 
        | i < numbins = do
            v <- readthevalue arr i 
            writethevalue arr i (v+1)
            b <- playntimes a logs bs;
            loop b (i+1) bs
        where 
        loop1 a x j y z = if j < y
                              then do
                              fv <- readthevalue arr j
                              printf " Runs %f Final Value %f Binsize %d Numruns %d \n" (fv / fromIntegral( z * x)) fv z x
                              loop1 a x (j+1) y z
                              else
                              return a
{% endhighlight %}

{% highlight Haskell %}
main =  do
   p <- createarray
   ReinforcementLearning.numruns p 1 1 100
   return ()
{% endhighlight %}
