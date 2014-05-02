module Types.Strategies.Free where

import Control.Monad.Free
import qualified System.Random as R

-- Types

data Move
    = Betray
    | Cooperate
    deriving (Show, Eq)

flipMove Betray = Cooperate
flipMove Cooperate = Betray

charToMove :: Char -> Move
charToMove 'C' = Cooperate
charToMove _ = Betray

moveToString :: Move -> String
moveToString Cooperate = "C"
moveToString Betray = "D"

type Round = (Move, Move)

-- Strategy actions

data StrategyAction next
    = Set Move next
    | History (Round -> next)
    | Rounds (Int -> next)
    | Random (Float -> next)

instance Functor StrategyAction where
    fmap f (Set m next) = Set m (f next)
    fmap f (History c)  = History (f . c)
    fmap f (Rounds r)   = Rounds (f . r)
    fmap f (Random r)   = Random (f . r)

type Strategy a = Free StrategyAction a

liftF :: (Functor f) => f r -> Free f r
liftF command = Impure (fmap Pure command)

-- Combinators

history :: Strategy Round
history = liftF (History id)

random :: Strategy Float
random = liftF (Random id)

rounds :: Strategy Int
rounds = liftF (Rounds id)

play :: Move -> Strategy ()
play m = liftF (Set m ())

-- Derived Combinators

-- -- Deciding combinators
cooperate = play Cooperate
betray = play Betray
defaultMove = play
playFlipped move = play (flipMove move)

-- -- History combinators
getMyMove = history >>= return . fst
getTheirMove = history >>= return . snd

-- -- Repetition
repeatMyLastMove = do
    (myLast, oppLast) <- history
    play myLast
    return oppLast

repeatTheirLastMove = do
    (myLast, oppLast) <- history
    play oppLast
    return oppLast

-- -- Opposite day
flipAMove get = do
    t <- get
    playFlipped t
    return t

flipMyLastMove = flipAMove getMyMove
flipTheirLastMove = flipAMove getTheirMove

-- -- Conditional combinators
whenRand threshold a = do
    p <- random
    when (p < threshold) $ do
        a

whenBetrayedBy m a = when (m == Betray) a
whenCooperatedBy m a = when (m == Cooperate) a

-- Strategies

alwaysCooperate :: Strategy ()
alwaysCooperate = cooperate

titForTat :: Strategy Move
titForTat = do
    defaultMove Cooperate
    repeatTheirLastMove

titForTatRand :: Float -> Strategy ()
titForTatRand threshold = do
    theirLast <- titForTat
    whenRand threshold $ playFlipped theirLast

titFor2Tats :: Strategy ()
titFor2Tats = do
    defaultMove Cooperate
    theirLast <- repeatMyLastMove
    theirLastLast <- getTheirMove
    when (theirLast == theirLastLast) $ do
        play theirLast

flipper :: Strategy Move
flipper = do
    defaultMove Cooperate
    flipMyLastMove

spite :: Strategy ()
spite = do
    defaultMove Cooperate
    theirLast <- repeatMyLastMove
    whenBetrayedBy theirLast betray

pavlov :: Strategy ()
pavlov = do
    randomStrat 0.5
    (myLast,theirLast) <- history
    whenCooperatedBy theirLast (play myLast)
    whenBetrayedBy theirLast $ do
        whenBetrayedBy myLast cooperate

mistrust :: Strategy Move
mistrust = do
    defaultMove Betray
    repeatTheirLastMove

randomStrat :: Float -> Strategy ()
randomStrat threshold = do
    defaultMove Cooperate
    whenRand threshold betray

remorsefulProber :: Float -> Strategy ()
remorsefulProber threshold = do
  defaultMove Cooperate
  theirLast <- getTheirMove
  whenBetrayedBy theirLast betray
  whenCooperatedBy theirLast (randomStrat threshold)
  myLastLast <- getMyMove
  theirLastLastLast <- getTheirMove
  whenBetrayedBy theirLast $ do
    whenBetrayedBy myLastLast $ do
        whenCooperatedBy theirLastLastLast cooperate


-- Util

pchoose :: [(Float, a)] -> a -> Float -> a
pchoose [] d f = d
pchoose ((pa,a):ps) d p
    | p < pa    = a
    | otherwise = pchoose ps d (p-pa)

-- Evaluattion

playStratAction :: Move -> [Round] -> Int -> R.StdGen -> Strategy a -> IO ()
playStratAction d rs n s (Pure _) = putStrLn $ moveToString d
playStratAction d rs n s (Impure (Set m next)) = playStratAction m rs n s next
playStratAction d rs n s (Impure (Rounds f)) = playStratAction d rs n s (f n)
playStratAction d [] n s (Impure (History f)) = playStratAction d [] n s (Pure ())
playStratAction d (r:rs) n s (Impure (History f)) = playStratAction d rs n s (f r)
playStratAction d rs n s (Impure (Random f)) = do
    let (p, s') = R.randomR (0.0, 1.0) s
    playStratAction d rs n s' (f p)

playStrategy :: Strategy a -> IO ()
playStrategy s = do
    rgen <- R.newStdGen
    opponentsline <- getLine
    localline <- getLine
    let opp = map charToMove opponentsline
        local = map charToMove localline
        rounds = zip local opp
    playStratAction Cooperate rounds 1000 rgen s


