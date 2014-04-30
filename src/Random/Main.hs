module Main where

import Types

import System.Random
import Control.Monad.State

randomF _ = state random

randomStrat :: Strategy
randomStrat = S ("random", randomF)

main = dilemmaMain randomStrat
