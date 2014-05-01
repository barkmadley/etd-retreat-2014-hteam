module Main where

import Data.Monoid
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Function
import System.Random
import Control.Monad.State
import Control.Applicative
import Data.Tuple

import qualified Types.Strategies.Free as F

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

--newtype StrategyT f a = StrategyT ([Round] -> f a)

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

game :: Int -> (Strategy, Strategy) -> State StdGen Scores
game n (player1, player2) = game' n (player1, player2) [] (s 0, s 0)

game' :: Int -> (Strategy, Strategy) -> [Round] -> Scores -> State StdGen Scores
game' 0 (p1, p2) r s = return s
game' n (p1, p2) r s = do
    m1 <- move p1 r
    m2 <- move p2 (map swap r)
    game' (n-1) (p1,p2) ((m1, m2) : r) (score m1 m2 <> s)

pairs :: (Ord a, Eq a) => [a] -> [(a,a)]
pairs list =
    [ (a,b) | a <- list, b <- list, a /= b, a < b ]

runGame :: Int -> M.Map Strategy Score -> (Strategy, Strategy) -> State StdGen (M.Map Strategy Score)
runGame n scores (p1,p2) =
        fmap (\(p1score, p2score) ->
            M.insertWith (<>) p2 p2score $ M.insertWith (<>) p1 p1score scores)
            (game n (p1, p2))

championship :: Int -> [Strategy] -> State StdGen (M.Map Strategy Score)
championship n strategies =
    fmap (M.unionsWith (<>))
        (mapM (runGame n initialScore) games)
    where
        games = pairs strategies
        initialScore = M.fromList [(s, Score 0) | s <- strategies]

cooperateF _ = return Cooperate

cooperate :: Strategy
cooperate = S ("cooperate", cooperateF)

betrayF _ = return Betray

betray :: Strategy
betray = S ("betray", betrayF)

randomF _ = (StateT (return . random))

randomStrat :: Strategy
randomStrat = S ("random", randomF)

tftDefault v ((m,t):rs) = return t
tftDefault v _ = return v

tftF = tftDefault Cooperate

titfortat :: Strategy
titfortat = S("titfortat", tftF)

mistrustF = tftDefault Betray

mistrust :: Strategy
mistrust = S("mistrust", mistrustF)

spiteF ((Betray,_):rs) = return Betray
spiteF ((_, Betray):rs) = return Betray
spiteF _ = return Cooperate

spite :: Strategy
spite = S("spite", spiteF)

strats :: Int -> String -> StratF -> [Strategy]
strats n name f = [ S (name ++ " " ++ show x, f) | x <- [1..n] ]

ccbF rs = return $ head $ drop 4 (map fst rs ++ cycle [(Cooperate),(Cooperate),(Betray)])

ccb :: Strategy
ccb = S("ccb", ccbF)

bbcF rs = return $ head $ drop 4 (map fst rs ++ cycle [(Betray),(Betray),(Cooperate)])

bbc :: Strategy
bbc = S("bbc", bbcF)

pavlovF ((Cooperate, Cooperate):rs) = return Cooperate
pavlovF ((Betray, Cooperate):rs) = return Betray
pavlovF rs = randomF rs

pavlov :: Strategy
pavlov = S("pavlov", pavlovF)

testStrat :: Strategy -> [Round] -> IO ()
testStrat s r = do
    rgen <- newStdGen
    let m = evalState (move s r) rgen
    print m

strategies =
    concat
    [
        --strats 10 "spite" spiteF,
        --strats 10 "titfortat" tftF,
        --strats 10 "pavlov" pavlovF,
        --strats 10 "bbc" bbcF,
        --strats 10 "ccb" ccbF,
        strats 1 "random" randomF,
        strats 10 "mistrust" mistrustF,
        strats 10 "betray" betrayF,
        strats 10 "cooperate" cooperateF
    ]

--stratMain :: Strategy -> IO ()
--stratMain s = newStdGen >>= recurse s []
--    where
--        recurse :: Strategy -> [Round] -> StdGen -> IO ()
--        recurse s rs rgen = do
--            let (myMove, rgen') = runState (move s rs) rgen
--            print myMove
--            line <- getLine
--            --let opponentMove = read line
--            recurse s ((myMove,opponentMove):rs) rgen'


--main = stratMain pavlov

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

--main = dilemmaMain pavlov

main :: IO ()
main = do
    rgen <- newStdGen
    let scores = evalState (championship 1000 strategies) rgen
        scoresl = M.toList scores
        sorted = sortBy (compare `on` snd) scoresl
    mapM_ print sorted