module Main where

import Types

tit42tats ((_,Betray):(_,Betray):rs) = return Betray
tit42tats ((_,Cooperate):(_,Cooperate):rs) = return Cooperate
tit42tats ((x,_):rs) = return x
tit42tats _ = return Cooperate

strat = S ("tit42tats", tit42tats)

main = dilemmaMain strat

