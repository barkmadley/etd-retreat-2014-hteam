module Main where

import Types

cooperateF _ = return Cooperate

cooperate :: Strategy
cooperate = S ("cooperate", cooperateF)

main = dilemmaMain cooperate