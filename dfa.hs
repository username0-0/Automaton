import Data.Char(intToDigit)

-- (Q, ∑, \delta, q0, F)
-- check format
-- ?


-- Q :: [String] = ["q1", "q2", "q3", "q4"]
-- ∑ :: [Char] = ['0', '1']
-- Set or List ?
-- ["q1", "q2", "q3", "q4"] ['0', '1'] [[1,2],[2,3],[3,4],[4,1]] 0 [2,4]
-- not implemented yet. use S_DFA


-- S_DFA simple DFA
-- Q :: [Int] = [1..n1] does not have a name::String for each state
-- ∑ :: [Int] = [0..n2] does not have a name::Char for each char


type DTrans = [[Int]]
type InChar = Int
type SimpleDFAState = Int

data DFA =
    S_DFA
    { s_Q :: [SimpleDFAState]
    , s_sigma :: [InChar]
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
    
dTransToS_DFA :: DTrans -> SimpleDFAState -> [SimpleDFAState] -> DFA
dTransToS_DFA d x0 f =
    S_DFA
    { s_Q = [1..n1]
    , s_sigma = [1..n2]
    , delta = d
    , q0 = x0
    , s_F = f
    } where
        n1 = length d
        n2 = length $ d!!0
dTransToS_DFA1 :: DTrans -> [SimpleDFAState] -> DFA
dTransToS_DFA1 d f = dTransToS_DFA d 1 f

-- check format ?


data Config =
    Config
    { dfa :: DFA
    , mark :: Int
    , input :: [InChar]
    }

instance Show Config where
    show Config{ dfa = d, mark = x, input = s} = unlines $
        [ show d
        , markCurrentState (s_Q d) (s_F d) x
        , show s
        ]


-- Config
-- 

-- markCurrentState Q F x
-- putStrLn $ markCurrentState [1..5] [2,5] 2
--        3
--  1 [2] 3  4 [5]

markCurrentState :: [Int] -> [Int] -> Int -> String
markCurrentState q f x = unlines
    [ concat
        [ replicate (length $ markFStates (take (x-1) q) []) ' '
        , addBrackets True x
        ]
    , b s
    ] where
        s = markFStates q f
        b y = y

-- markFStates Q F
markFStates :: [Int] -> [Int] -> String
markFStates q f = concat $ zipWith addBrackets (acceptable q f) q

addBrackets :: Bool -> Int -> String
addBrackets x y = if x
    then "[" ++ s ++ "]"
    else " " ++ s ++ " " where
        s = show y

-- acceptable Q F
acceptable :: [Int] -> [Int] -> [Bool]
-- acceptable [] _ = []
-- acceptable (x:xs) f = elem x f : acceptable xs f
acceptable q f = map (\x -> elem x f) q


    
dChangeState :: SimpleDFAState -> DTrans -> InChar -> SimpleDFAState
dChangeState x d y = (!!y) . (!!x) $ d

-- step :: Config -> Config




{-


getInput :: String -> String
getInput s = s!!5


getQ :: String -> [(Int, Char)]
getQ s = zip [0..] $ s

steps :: Config -> Int -> IO()
steps x 0 = show x
steps x n = do
    show x
    steps (step x) n-1

simulate :: Config -> IO()
simulate x@ =
        show x
        simulate . step $ x

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
