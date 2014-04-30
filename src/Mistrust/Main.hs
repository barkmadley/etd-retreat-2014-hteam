module Main where

import Types

mistrustF = tftDefault Betray

mistrust :: Strategy
mistrust = S("mistrust", mistrustF)

main = dilemmaMain mistrust