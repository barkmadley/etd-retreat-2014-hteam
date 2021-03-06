module Main where

import Types

import System.Random
import Control.Monad.State

choose :: [(Float, Move)] -> Move -> State StdGen Move
choose xs d = (choose' xs d) `fmap` (state (randomR (0.0,1.0)))
    where
        choose' :: [(Float, a)] -> a -> Float -> a
        choose' [] d p = d
        choose' ((pa,a):ps) d p
            | p < pa = a
            | otherwise = choose' ps d (p - pa)

f ((_,Betray):(Betray,_):(_,Cooperate):rs) = return Cooperate
f ((_,Betray):rs) = return Betray
f ((_,Cooperate):rs) =
    choose [(0.95, Cooperate)] Betray
f _ = return Cooperate

strat = S ("remorsefulprober 0.05", f)

--main = testStrat strat [(Cooperate, Cooperate)]

main = dilemmaMain strat

