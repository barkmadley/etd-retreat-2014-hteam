module Main where

import Types

spiteF ((Betray,_):rs) = return Betray
spiteF ((_, Betray):rs) = return Betray
spiteF _ = return Cooperate

spite :: Strategy
spite = S("spite", spiteF)

main = dilemmaMain spite