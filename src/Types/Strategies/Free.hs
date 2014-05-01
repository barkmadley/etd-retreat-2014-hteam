module Types.Strategies.Free where

import Data.Monoid
import Control.Monad.Free
import qualified System.Random as R

data StrategyAction next
    = Set Move next
    | History (Round -> next)
    | Rounds (Int -> next)
    | Random (Float -> next)

instance Functor StrategyAction where
    fmap f (Set m next) = Set m (f next)
    fmap f (History c)  = History (f . c)
    fmap f (Rounds r)   = Rounds (f . r)
    fmap f (Random r)   = Random (f . r)

type Strategy a = Free StrategyAction a

liftF :: (Functor f) => f r -> Free f r
liftF command = Impure (fmap Pure command)

-- Combinators

history :: Strategy Round
history = liftF (History id)

random :: Strategy Float
random = liftF (Random id)

rounds :: Strategy Int
rounds = liftF (Rounds id)

play :: Move -> Strategy ()
play m = liftF (Set m ())

-- Derived Combinators

cooperate = play Cooperate
betray = play Betray

-- Strategies

alwaysCooperate :: Strategy ()
alwaysCooperate = cooperate

titForTat :: Strategy ()
titForTat = do
    play Cooperate
    (_,t) <- history
    play t

mistrust :: Strategy ()
mistrust = do
    play Betray
    (_,t) <- history
    play t

randomStrat :: Strategy ()
randomStrat = do
    play Cooperate
    p <- random
    when (p < 0.5) $ do
        play Betray

-- Util

pchoose :: [(Float, a)] -> a -> Float -> a
pchoose [] d f = d
pchoose ((pa,a):ps) d p
    | p < pa    = a
    | otherwise = pchoose ps d (p-pa)


-- Evaluattion

playStratAction :: Move -> [Round] -> Int -> R.StdGen -> Strategy () -> IO ()
playStratAction d rs n s (Pure ()) = putStrLn $ moveToString d
playStratAction d rs n s (Impure (Set m next)) = playStratAction m rs n s next
playStratAction d rs n s (Impure (Rounds f)) = playStratAction d rs n s (f n)
playStratAction d [] n s (Impure (History f)) = playStratAction d [] n s (Pure ())
playStratAction d (r:rs) n s (Impure (History f)) = playStratAction d rs n s (f r)
playStratAction d rs n s (Impure (Random f)) = do
    let (p, s') = R.randomR (0.0, 1.0) s
    playStratAction d rs n s' (f p)

playStrategy :: Strategy () -> IO ()
playStrategy s = do
    rgen <- R.newStdGen
    opponentsline <- getLine
    localline <- getLine
    let opp = map charToMove opponentsline
        local = map charToMove localline
        rounds = zip local opp
    playStratAction Cooperate rounds 1000 rgen s


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

--instance Random Move where
--    -- randomR :: RandomGen g => (a, a) -> g -> (a, g)
--    randomR (lo,hi) g =
--            (if rBool then Betray else Cooperate, g')
--        where
--            (rBool, g') = randomR (False,True) g
--    -- random :: RandomGen g => g -> (a, g)
--    random g = randomR (Betray,Cooperate) g

type Round = (Move, Move)

--type StratF =
--    [Round] -> State StdGen Move

--data Strategy = S (String, StratF)
---- TODO:
---- newtype StrategyT f a = StrategyT ([Round] -> f a)

--instance Eq Strategy where
--    S (n1, _) == S (n2, _) = n1 == n2

--instance Ord Strategy where
--    compare (S (n1, _)) (S (n2, _)) = compare n1 n2

--instance Show Strategy where
--    show (S (n,_)) = n

--newtype Score = Score Int
--    deriving (Eq, Ord)

--instance Show Score where
--    show (Score i) = show i

--instance Monoid Score where
--    mempty = Score 0
--    mappend (Score i) (Score j) = Score (i + j)

--type Scores = (Score, Score)

--s i = Score i

--score :: Move -> Move -> Scores
--score Betray Betray = (s 3, s 3)
--score Betray Cooperate = (s 0,s 5)
--score Cooperate Betray = (s 5,s 0)
--score Cooperate Cooperate = (s 1,s 1)

--move :: Strategy -> [Round] -> State StdGen Move
--move (S (_, s)) = s


--dilemmaMain :: Strategy -> IO ()
--dilemmaMain s = do
--    opponentsline <- getLine
--    localline <- getLine
--    let opp = map charToMove opponentsline
--        local = map charToMove localline
--        rounds = zip local opp
--    gen <- newStdGen
--    let myMove = evalState (move s rounds) gen
--    putStrLn (moveToString myMove)

--tftDefault v ((m,t):rs) = return t
--tftDefault v _ = return v

