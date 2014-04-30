module Main where

import Types
import System.Random
import Control.Monad.State

pavlovF ((Cooperate, Cooperate):rs) = return Cooperate
pavlovF ((Betray, Cooperate):rs) = return Betray
pavlovF rs = state random

pavlov :: Strategy
pavlov = S ("pavlov", pavlovF)

main = dilemmaMain pavlov

