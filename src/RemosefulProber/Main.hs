module Main where

import Types

import System.Random
import Control.Monad.State

choose :: [(Float, Move)] -> Move -> State StdGen Move
choose [] d = return Betray
choose xs d = (\f -> choose' f xs d) `fmap` (state (randomR (0.0,1.0)))
    where
        choose' :: Float -> [(Float, a)] -> a -> a
        choose' p [] d = d
        choose' p ((pa,a):ps) d
            | p < pa = a
            | otherwise = choose' (p - pa) ps d

f ((m,Betray):(Betray,_):(_,Cooperate):rs) = return Cooperate
f ((m,Betray):rs) = return Betray
f ((m,Cooperate):rs) =
    choose [(0.95, Cooperate)] Betray
f _ = return Cooperate

strat = S ("remorsefulprober 0.05", f)

testStrat :: Strategy -> [Round] -> IO ()
testStrat s r = do
    rgen <- newStdGen
    let m = evalState (move s r) rgen
    print m

--main = testStrat strat [(Cooperate, Cooperate)]

main = dilemmaMain strat

