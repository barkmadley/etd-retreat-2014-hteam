module Types where

import Data.Monoid
import System.Random

import Control.Monad.State

data Move
    = Betray
    | Cooperate
    deriving (Show)

charToMove :: Char -> Move
charToMove 'C' = Cooperate
charToMove _ = Betray

moveToString :: Move -> String
moveToString Cooperate = "C"
moveToString Betray = "D"

instance Random Move where
    -- randomR :: RandomGen g => (a, a) -> g -> (a, g)
    randomR (lo,hi) g =
            (if rBool then Betray else Cooperate, g')
        where
            (rBool, g') = randomR (False,True) g
    -- random :: RandomGen g => g -> (a, g)
    random g = randomR (Betray,Cooperate) g

type Round = (Move, Move)

type StratF =
    [Round] -> State StdGen Move

data Strategy = S (String, StratF)
-- TODO:
-- newtype StrategyT f a = StrategyT ([Round] -> f a)

instance Eq Strategy where
    S (n1, _) == S (n2, _) = n1 == n2

instance Ord Strategy where
    compare (S (n1, _)) (S (n2, _)) = compare n1 n2

instance Show Strategy where
    show (S (n,_)) = n

newtype Score = Score Int
    deriving (Eq, Ord)

instance Show Score where
    show (Score i) = show i

instance Monoid Score where
    mempty = Score 0
    mappend (Score i) (Score j) = Score (i + j)

type Scores = (Score, Score)

s i = Score i

score :: Move -> Move -> Scores
score Betray Betray = (s 3, s 3)
score Betray Cooperate = (s 0,s 5)
score Cooperate Betray = (s 5,s 0)
score Cooperate Cooperate = (s 1,s 1)

move :: Strategy -> [Round] -> State StdGen Move
move (S (_, s)) = s


dilemmaMain :: Strategy -> IO ()
dilemmaMain s = do
    opponentsline <- getLine
    localline <- getLine
    let opp = map charToMove opponentsline
        local = map charToMove localline
        rounds = zip local opp
    gen <- newStdGen
    let myMove = evalState (move s rounds) gen
    putStrLn (moveToString myMove)

tftDefault v ((m,t):rs) = return t
tftDefault v _ = return v