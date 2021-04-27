module DFA
( DFA(..)
, MyDFA(..)
, deltaToS_DFA
, deltaToS_DFA1
, acceptAtState
, accept
, crossTrans
{-
, MyDFAConfig(..)
, sequenceRun
-}
) where

-- make it a module

-- (Q, ∑, \delta, q0, F)
-- check format
-- ?


data DFA a b =
    ToDFA
    { states :: [a]
    , sigma :: [b]
    , transF :: DTrans
    , startIndex :: SimpleStartState
    , finalStates :: [a]
    } deriving (Show)




-- Q :: [String] = ["q1", "q2", "q3", "q4"]
-- ∑ :: [Char] = ['0', '1']
-- Set or List ?
-- ["q1", "q2", "q3", "q4"] ['0', '1'] [[1,2],[2,3],[3,4],[4,1]] 0 [2,4]
-- not implemented yet.
-- use S_DFA


-- S_DFA simpleDFA
-- Q :: [Int] = [1..n1]
-- ∑ :: [Int] = [0..n2]


type DTrans = [[Int]]
type SimpleState = Int
type SimpleFState = Int
type SimpleStartState = Int
type InputCode = Int
-- type Language = [String]
-- not having Language type until importing modules for Set
-- :t in ghci

data MyDFA =
    S_DFA
    { s_Q :: [SimpleState]
    , s_sigma :: [InputCode]
    , delta :: DTrans
    , q0 :: SimpleStartState
    , s_F :: [SimpleFState]
    } deriving (Show)


-- Example
-- *Main> S_DFA [1,2] [0,1] [[2,1],[1,2]] 1 [2]
-- S_DFA {s_Q = [1,2], s_sigma = [0,1], delta = [[2,1],[1,2]],
-- q0 = 1, s_F = [2]}


-- GNFA
-- >[q_start]  -e->  [1]  <]1
--                  | ^
--                  | |
--                  0 0
--                  | |
--                  v |
-- ([q_f])   <-e-   [2]  <]1

-- remove state [1]

-- >[q_start]  -e1*0
--                  |
--                  |
--                  |
--                 |
--                |
-- ([q_f]) <-e-  [2]  <]01*0

-- regExp = "1*0(01*0)*"

toS_DFA :: [String] -> MyDFA
toS_DFA s = S_DFA (read a1) (read a2) (read a3) (read a4) (read a5) where
    [a1, a2, a3, a4, a5] = [ a | a <- s]
-- pattern match
-- is the "take 5" necessary here?
-- now i guess it should be [String] -> Maybe MyDFA
{-
toS_DFA s =
    S_DFA
    { s_Q       = read $ s !! 0
    , s_sigma   = read $ s !! 1
    , delta     = read $ s !! 2
    , q0        = read $ s !! 3
    , s_F       = read $ s !! 4
    }
-}
-- Example
-- s = take 5 . words $ "[1,2] [0,1] [[2,1],[1,2]] 1 [2] [0,1,0,0,1]"
-- toS_DFA s


-- deltaToS_DFA get info from S_DFA's delta transition function
deltaToS_DFA :: DTrans -> SimpleStartState -> [SimpleFState] -> MyDFA
deltaToS_DFA d x0 f =
    S_DFA
    { s_Q = [1..n1]
    , s_sigma = [0..(n2-1)]
    , delta = d
    , q0 = x0
    , s_F = f
    } where
        n1 = length d
        n2 = length $ d!!0

deltaToS_DFA1 :: DTrans -> [SimpleFState] -> MyDFA
deltaToS_DFA1 d f = deltaToS_DFA d 1 f
{-
Example
(delta_1, fs_1) = ([[2,1],[1,2]], [2])
(delta_2, fs_2) = ([[1,2],[2,3],[3,3]], [2])
(d1, d2) = (deltaToS_DFA1 delta_1 fs_1, deltaToS_DFA1 delta_2 fs_2)
(d1, d2)
-}

-- check format ?

-- dToString :: MyDFA -> String

data MyDFAConfig =
    ToConfig
    { dfa :: MyDFA
    , currentState :: SimpleState
    , input :: [InputCode]
    }

instance Show MyDFAConfig where
    show c@ToConfig{ dfa = d, currentState = x, input = w} = unlines $
        [ show d
        , "CurrentState: "
        , markCurrentState' q f x
        , "ls-states:    "
        , markFStates q f
        , "input:"
        , show w
        ] where
            q = s_Q d
            f = s_F d


markCurrentState :: MyDFAConfig -> String
markCurrentState
    ToConfig
    { dfa = d
    , currentState = x
    , input = _
    }
    = markCurrentState' (s_Q d) (s_F d) x
-}
x <- Q
x <- [1..n]
markCurrentState' Q F x
putStrLn $ markCurrentState [1..5] [2,5] 2
       3
 1 [2] 3  4 [5]
when x > n
*Main> putStrLn $ markCurrentState' [1..5] [2,5] 13
               13
1 [2] 3  4 [5]
-}

markCurrentState' ::
    [SimpleState] -> [SimpleFState] -> SimpleState -> String
markCurrentState' q f x = concat
    [ replicate (length $ markFStates (take (x-1) q) []) ' '
    , addBrackets (elem x f) x
    ]

lsStates :: MyDFAConfig -> String
lsStates
    ToConfig
    { dfa = d
    , currentState = _
    , input = _
    }
    = markFStates (s_Q d) (s_F d)

markFStates :: [SimpleState] -> [SimpleFState] -> String
markFStates q f = concat $ zipWith addBrackets (forFStates q f) q

addBrackets :: Bool -> Int -> String
addBrackets x y
    | x = "[" ++ s ++ "]"
    | otherwise = " " ++ s ++ " "
    where
    s = show y

forFStates :: [SimpleState] -> [SimpleFState] -> [Bool]
-- forFStates [] _ = []
-- forFStates (x:xs) f = elem x f : forFStates xs f
forFStates q f = map (\x -> elem x f) q



initConfig :: MyDFA -> [InputCode] -> MyDFAConfig
initConfig d s =
    ToConfig
    { dfa = d
    , currentState = q0 d
    , input = s
    }

dChangeState :: DTrans -> InputCode -> SimpleState -> SimpleState
dChangeState d nc nq = (!!nc) . (!!(nq-1)) $ d

stepRun :: MyDFAConfig -> MyDFAConfig
stepRun c@ToConfig{ dfa = d, currentState = x, input = w@(s:ss)}
    | w == [] = c
    | otherwise = ToConfig
        { dfa = d
        , currentState = f x
        , input = ss
        }
        where f = dChangeState (delta d) s

sequenceRun :: MyDFAConfig -> Int -> MyDFAConfig
sequenceRun c@ToConfig{ dfa = d, currentState = x, input = []} _ = c
sequenceRun c n
    | n < 1 = c
    | otherwise = sequenceRun (stepRun c) (n-1)

dFinalAccept :: MyDFAConfig -> Bool
dFinalAccept c@ToConfig{ dfa = d, currentState = x, input = w}
    | w == [] = elem x (s_F d)
    | otherwise = dFinalAccept (stepRun c)
-- not sequenceRun c (length $ input d)

willFinallyAccept :: MyDFA -> [InputCode] -> Bool
d `willFinallyAccept` w = dFinalAccept $ initConfig d w




acceptAtState :: MyDFA -> SimpleState -> Bool
d `acceptAtState` x = elem x (s_F d)

accept :: MyDFA -> [InputCode] -> Bool
dfa `accept` w = dfa `acceptAtState` x
    where
    (d, x) = foldr f (dfa, q0 dfa) w
        where f s (d, x) = (d, dChangeState (delta d) s x)
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- acc@(d,x) :: b :: (MyDFA, Int)
{-
Example
(delta_1, fs_1, w) = ([[2,1],[1,2]], [2], [0,0,0,1,0,1,0,0,0,1,1,0])
testDFA = deltaToS_DFA1 delta_1 fs_1
testDFA `acceptAtState` 2
testDFA `accept` w
map (testDFA `accept`) [ drop n w | n <- [1..5]]
-}

-- wow, MyDFAConfig is not used in `accept`
-- im practising using foldr




showHistory :: [MyDFAConfig] -> String
-- showHistory cs = unlines . map ((!!2) . lines . show) $ cs
showHistory cs = unlines $ map markCurrentState cs

{-
Example
d = S_DFA [1,2] [0,1] [[2,1],[1,2]] 1 [2]
c = initConfig d [0,0,0,1,0,1,0,0,0,1,1,0]
putStrLn $ showHistory . map (sequenceRun c) $ [0..11]
-}


-- IO
-- read from args
-- read from files


-- operations for dfa

-- [(a,b) | a <- [1..2], b <- [1..3]]
-- d_1 = S_DFA [1,2,3] [0,1] [[2,1],[1,3],[1,3]] 1 [2]


-- ∑ d1 == ∑ d2
-- length (t1!!0) == length (t2!!0)
crossTrans :: DTrans -> DTrans -> DTrans
crossTrans t1 t2 = [ f stateList_1 stateList_2
    | stateList_1 <- t1
    , stateList_2 <- t2
    ] where
    f = zipWith (\x y -> nc * ( x - 1 ) + y) where
        nc = length t2
{-
A = Q1 x Q2
index of Aij in the mxn matrix is [ n*i+j | i <- [0..m-1], j <- [0..n-1]]
i = ( x - 1 ) here
-}

{-
Example
(delta_1, fs_1) = ([[2,1],[1,2]], [2])
(delta_2, fs_2) = ([[1,2],[2,3],[3,3]], [2])
(d1, d2) = (deltaToS_DFA1 delta_1 fs_1, deltaToS_DFA1 delta_2 fs_2)
crossTrans delta_1 delta_2
[[4,2],[5,3],[6,3],[1,5],[2,6],[3,6]]
-}


-- dIntersec :: MyDFA -> MyDFA -> MyDFA
-- dIntersec dfa1 dfa2 = deltaToS_DFA tfs f where
--     tfs = crossTrans (delta dfa1) (delta dfa2)
--     f =

-- dUnion :: MyDFA -> MyDFA -> MyDFA

-- dDiff :: MyDFA -> MyDFA -> MyDFA

-- test if a MyDFA recognize an empty set
-- dRecEmpty :: MyDFA -> Bool

-- dEq :: MyDFA -> MyDFA -> Bool
-- dEq dfa1 dfa2 = dRecEmpty $ dDiff dfa1 dfa2

-- dToGNFA :: MyDFA -> ?
-- After fininshing NFA part
-- dToRegExp :: MyDFA -> String


-- operations for regular expression
-- type regExp = String
-- newType regExp = String
-- use type or newType ?

-- union
-- star
-- concat
-- regular expression is closed under diff operation

-- to check whether a regExp is 'star-closed' is P

{-

dSimulate :: MyDFA -> [InputCode] -> IO()



f a b c d = y
can be written like this
f
    a
    b
    c
    d
    =
    y



recycle bin
-- import Data.Char(intToDigit)

getQ :: [String] -> [(Int, Char)]
getQ s = zip [0..] s


why we dont have this function?
feedArgs f [] = f
feedArgs f (x:xs) = feedArgs (f x) xs


| G_DFA
{ g_Q :: [String]
, g_sigma :: String
, delta :: [[Int]]
, q0 :: Int
, s_F :: [Int]
}


toS_DFA' :: String -> String -> String -> String -> String -> MyDFA
toS_DFA' a0 a1 a2 a3 a4 =
    S_DFA
    { s_Q       = read a0
    , s_sigma   = read a1
    , delta     = read a2
    , q0        = read a3
    , s_F       = read a4
    }

toS_DFA1 :: String -> String -> String -> String -> MyDFA
toS_DFA1 a0 a1 a2 a3 =
    S_DFA
    { s_Q       = read a0
    , s_sigma   = read a1
    , delta     = read a2
    , q0        = 1
    , s_F       = read a3
    }
    
-}
