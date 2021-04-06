-- import Data.Char(intToDigit)

-- (Q, ∑, \delta, q0, F)
-- check format
-- ?


-- Q :: [String] = ["q1", "q2", "q3", "q4"]
-- ∑ :: [Char] = ['0', '1']
-- Set or List ?
-- ["q1", "q2", "q3", "q4"] ['0', '1'] [[1,2],[2,3],[3,4],[4,1]] 0 [2,4]
-- not implemented yet.
-- use S_DFA


-- S_DFA simple DFA
-- Q :: [Int] = [1..n1] does not have a name::String for each state
-- ∑ :: [Int] = [0..n2] does not have a name::Char for each char


type DTrans = [[Int]]
type SimpleDFAState = Int
type SimpleDFAFState = Int

type InputCode = Int

data DFA =
    S_DFA
    { s_Q :: [SimpleDFAState]
    , s_sigma :: [InputCode]
    , delta :: DTrans
    , q0 :: SimpleDFAState
    , s_F :: [SimpleDFAState]
    } deriving (Show)


-- example
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

toS_DFA :: [String] -> DFA
toS_DFA s =
    S_DFA
    { s_Q       = read $ s !! 0
    , s_sigma   = read $ s !! 1
    , delta     = read $ s !! 2
    , q0        = read $ s !! 3
    , s_F       = read $ s !! 4
    }

-- dDeltaToS_DFA get info from S_DFA's delta transition function
dDeltaToS_DFA :: DTrans -> SimpleDFAState -> [SimpleDFAState] -> DFA
dDeltaToS_DFA d x0 f =
    S_DFA
    { s_Q = [1..n1]
    , s_sigma = [1..n2]
    , delta = d
    , q0 = x0
    , s_F = f
    } where
        n1 = length d
        n2 = length $ d!!0
dDeltaToS_DFA1 :: DTrans -> [SimpleDFAState] -> DFA
dDeltaToS_DFA1 d f = dDeltaToS_DFA d 1 f

-- check format ?


data Config =
    Config
    { dfa :: DFA
    , currentState :: SimpleDFAState
    , input :: [InputCode]
    }

instance Show Config where
    show Config{ dfa = d, currentState = x, input = s} = unlines $
        [ show d
        , "CurrentState: "
        , markCurrentState q f x
        , "ls-states:    "
        , markFStates q f
        , "input:"
        , show s
        ] where
            q = s_Q d
            f = s_F d


-- x <- Q
-- x <- [1..n]
-- markCurrentState Q F x
-- putStrLn $ markCurrentState [1..5] [2,5] 2
--        3
--  1 [2] 3  4 [5]
-- when x > n
-- *Main> putStrLn $ markCurrentState [1..5] [2,5] 13
--                13
-- 1 [2] 3  4 [5]


markCurrentState ::
    [SimpleDFAState] -> [SimpleDFAFState] -> SimpleDFAState -> String
markCurrentState q f x = concat
    [ replicate (length $ markFStates (take (x-1) q) []) ' '
    , addBrackets (elem x f) x
    ]



markFStates :: [SimpleDFAState] -> [SimpleDFAFState] -> String
markFStates q f = concat $ zipWith addBrackets (forFStates q f) q

addBrackets :: Bool -> Int -> String
addBrackets x y = if x
    then "[" ++ s ++ "]"
    else " " ++ s ++ " " where
        s = show y

forFStates :: [SimpleDFAState] -> [SimpleDFAFState] -> [Bool]
-- forFStates [] _ = []
-- forFStates (x:xs) f = elem x f : forFStates xs f
forFStates q f = map (\x -> elem x f) q



initConfig :: DFA -> [InputCode] -> Config
initConfig d s =
    Config
    { dfa = d
    , currentState = q0 d
    , input = s
    }

dChangeState :: DTrans -> SimpleDFAState -> InputCode -> SimpleDFAState
dChangeState d x y = (!!y) . (!!(x-1)) $ d

stepRun :: Config -> Config
stepRun Config{ dfa = d, currentState = x, input = []} =
    Config
        { dfa = d
        , currentState = x
        , input = []
        }
stepRun (Config d x (c:cs)) =
    Config
        { dfa = d
        , currentState = dChangeState (delta d) x c
        , input = cs
        }

sequenceRun :: Config -> Int -> Config
sequenceRun c@Config{ dfa = d, currentState = x, input = []} _ = c
sequenceRun c n
    | n < 1 = c
    | otherwise = sequenceRun (stepRun c) (n-1)

dConfigAccept :: Config -> Bool
dConfigAccept c@Config{ dfa = d, currentState = x, input = []} = elem x (s_F d)
dConfigAccept c = dConfigAccept (stepRun c)
-- not sequenceRun c (length $ input d)

dAccept :: DFA -> [InputCode] -> Bool
dAccept d cs = dConfigAccept $ initConfig d cs



showHistory :: [Config] -> String
showHistory cs = unlines . map ((!!2) . lines . show) $ cs

-- Example
-- d = S_DFA [1,2] [0,1] [[2,1],[1,2]] 1 [2]
-- c = initConfig d [0,0,0,1,0,1,0,0,0,1,1,0]
-- putStrLn $ showHistory . map (sequenceRun c) $ [0..11]


{-

dSimulate :: DFA -> [InputCode] -> IO()







recycle bin

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


toS_DFA' :: String -> String -> String -> String -> String -> DFA
toS_DFA' a0 a1 a2 a3 a4 =
    S_DFA
    { s_Q       = read a0
    , s_sigma   = read a1
    , delta     = read a2
    , q0        = read a3
    , s_F       = read a4
    }

toS_DFA1 :: String -> String -> String -> String -> DFA
toS_DFA1 a0 a1 a2 a3 =
    S_DFA
    { s_Q       = read a0
    , s_sigma   = read a1
    , delta     = read a2
    , q0        = 1
    , s_F       = read a3
    }
    
-}
