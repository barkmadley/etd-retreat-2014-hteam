module Types.Strategies.Free where

import Data.Monoid
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Monad.Free
import Control.Monad.RWS
import Control.Applicative
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

-- -- A monadic representation of the full set of information required to run a strategy action

runStratAction :: StrategyAction a -> MaybeT (RWS Int (Last Move) ([Round], R.StdGen)) a
runStratAction (Set move next) = tell (Last (Just move)) >> return next
runStratAction (Rounds f) = ask >>= return . f
runStratAction (History f) = do
    (rounds,gen) <- get
    case rounds of
        [] -> mzero
        (r:rs) -> do
            put (rs,gen)
            return $ f r
runStratAction (Random f) = do
    (rounds,gen) <- get
    let (p, gen') = R.randomR (0.0, 1.0) gen
    put (rounds, gen')
    return $ f p

-- -- run a strategy action fixing the random function to always return a certain probability

runDeterminisicStratAction :: Float -> StrategyAction a -> MaybeT (RWS Int (Last Move) [Round]) a
runDeterminisicStratAction _ (Set move next) = tell (Last (Just move)) >> return next
runDeterminisicStratAction _ (Rounds f) = ask >>= return . f
runDeterminisicStratAction p (Random f) = return $ f p
runDeterminisicStratAction _ (History f) = do
    rounds <- get
    case rounds of
        [] -> mzero
        (r:rs) -> do
            put rs
            return $ f r

-- -- run a strategy but ignore any requests for history

runFirstStratAction :: StrategyAction a -> MaybeT (RWS Int (Last Move) R.StdGen) a
runFirstStratAction (Set move next) = tell (Last (Just move)) >> return next
runFirstStratAction (Rounds f) = ask >>= return . f
runFirstStratAction (History f) = mzero
runFirstStratAction (Random f) = state (R.randomR (0.0, 1.0)) >>= return . f

-- -- a fully implemented recursive interpreter

playStrategy :: Move -> [Round] -> Int -> R.StdGen -> Strategy a -> IO ()
playStrategy d rs n s (Pure _) = putStrLn $ moveToString d
playStrategy d rs n s (Impure (Set m next)) = playStrategy m rs n s next
playStrategy d rs n s (Impure (Rounds f)) = playStrategy d rs n s (f n)
playStrategy d [] n s (Impure (History f)) = playStrategy d [] n s (Pure ())
playStrategy d (r:rs) n s (Impure (History f)) = playStrategy d rs n s (f r)
playStrategy d rs n s (Impure (Random f)) = do
    let (p, s') = R.randomR (0.0, 1.0) s
    playStrategy d rs n s' (f p)

-- -- run a strategy using the recursive interpreter above

runStrategyIO :: Strategy a -> IO ()
runStrategyIO s = do
    rgen <- R.newStdGen
    opponentsline <- getLine
    localline <- getLine
    let opp = map charToMove opponentsline
        local = map charToMove localline
        rounds = zip local opp
    playStrategy Cooperate rounds 1000 rgen s

-- -- run a strategy using a free combinator and the appropriate runRWS and runMaybeT functions

runStrategy :: Strategy a -> IO ()
runStrategy s = do
    rgen <- R.newStdGen
    opponentsline <- getLine
    localline <- getLine
    let opp = map charToMove opponentsline
        local = map charToMove localline
        rounds = zip local opp
        interpreter = induce runStratAction s
        state = (rounds,rgen)
        (result,state',lastMove) = runRWS (runMaybeT interpreter) 1000 state
        play = fromMaybe Cooperate (getLast lastMove)
    putStrLn $ moveToString play

-- -- run a strategy where the random probability is always set to 1.0

runStrategyP1 :: Strategy a -> IO ()
runStrategyP1 s = do
    opponentsline <- getLine
    localline <- getLine
    let opp = map charToMove opponentsline
        local = map charToMove localline
        rounds = zip local opp
        interpreter = induce (runDeterminisicStratAction 1.0) s
        state = rounds
        (result,state',lastMove) = runRWS (runMaybeT interpreter) 1000 state
        play = fromMaybe Cooperate (getLast lastMove)
    putStrLn $ moveToString play
