module Main where

import Types

tftF = tftDefault Cooperate

titfortat :: Strategy
titfortat = S("titfortat", tftF)

main = dilemmaMain titfortat