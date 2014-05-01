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

cooperate = play Cooperate
betray = play Betray

defaultMove = play

-- Strategies

alwaysCooperate :: Strategy ()
alwaysCooperate = cooperate

copyOpponent = do
    (_,t) <- history
    play t

titForTat :: Strategy ()
titForTat = do
    defaultMove Cooperate
    copyOpponent

titForTatRand :: Float -> Strategy ()
titForTatRand threshold = do
    defaultMove Cooperate
    (_,t) <- history
    play t
    p <- random
    when (p < threshold) $ do
        play (flipMove t)

titFor2Tats :: Strategy ()
titFor2Tats = do
    defaultMove Cooperate
    (myLastMove,opponent1) <- history
    play myLastMove
    (_,opponent2) <- history
    when (opponent1 == opponent2) $ do
        play opponent1

flipper :: Strategy ()
flipper = do
    defaultMove Cooperate
    (myLast,_) <- history
    play (flipMove myLast)

spite :: Strategy ()
spite = do
    defaultMove Cooperate
    (myLast,oppLast) <- history
    play myLast
    when (oppLast == Betray) $ do
        betray

pavlov :: Strategy ()
pavlov = do
    randomStrat 0.5
    (myLast,oppLast) <- history
    when (oppLast == Cooperate) $ do
        play myLast
    when (oppLast == Betray && myLast == Betray) $ do
        play Cooperate

mistrust :: Strategy ()
mistrust = do
    defaultMove Betray
    copyOpponent

randomStrat :: Float -> Strategy ()
randomStrat threshold = do
    defaultMove Cooperate
    p <- random
    when (p < 0.5) $ do
        betray

-- Util

pchoose :: [(Float, a)] -> a -> Float -> a
pchoose [] d f = d
pchoose ((pa,a):ps) d p
    | p < pa    = a
    | otherwise = pchoose ps d (p-pa)

-- Evaluattion

playStratAction :: Move -> [Round] -> Int -> R.StdGen -> Strategy () -> IO ()
playStratAction d rs n s (Pure ()) = putStrLn $ moveToString d
playStratAction d rs n s (Impure (Set m next)) = playStratAction m rs n s next
playStratAction d rs n s (Impure (Rounds f)) = playStratAction d rs n s (f n)
playStratAction d [] n s (Impure (History f)) = playStratAction d [] n s (Pure ())
playStratAction d (r:rs) n s (Impure (History f)) = playStratAction d rs n s (f r)
playStratAction d rs n s (Impure (Random f)) = do
    let (p, s') = R.randomR (0.0, 1.0) s
    playStratAction d rs n s' (f p)

playStrategy :: Strategy () -> IO ()
playStrategy s = do
    rgen <- R.newStdGen
    opponentsline <- getLine
    localline <- getLine
    let opp = map charToMove opponentsline
        local = map charToMove localline
        rounds = zip local opp
    playStratAction Cooperate rounds 1000 rgen s


