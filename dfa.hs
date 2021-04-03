import Data.Char(intToDigit)

-- (Q, ∑, \delta, q0, F)
-- ["q1", "q2", "q3", "q4"]

-- check format


-- S_DFA simple DFA
-- Q = [1..s_Q] does not have a name::String for each state
-- ∑ = [0..s_sigma] does not have a name::Char for each char
-- "2 2 [[2,2,2],[1,1,1]] 1 [2] [0,1,2,1,2,1,0,1,0,2]



data DFA =
    S_DFA
    { s_Q :: Int
    , s_sigma :: Int
    , delta :: [[Int]]
    , q0 :: Int
    , s_F :: [Int]
    } deriving (Show)

data Config =
    Config
    { dfa :: DFA
    , mark :: Int
    , stack :: [Int]
    }

instance Show Config where
    show Config{ dfa = d, mark = x, stack = s} = unlines $
        [ show d
        , (markCurrentState x (s_F d) [1..(s_Q d)])
        , show s
        ]

markCurrentState :: Int -> [Int] -> [Int] -> String
markCurrentState x f q = unlines
    [ concat
        [ replicate (length . markFStates [] $ take (x-1) q) ' '
        , addBrackets True x
        ]
    , b s
    ] where
        s = markFStates f q
        b y = y

-- markStates 2 [1..5] [2,5]
--        3
--  1 [2] ^  4 [5]


markFStates :: [Int] -> [Int] -> String
markFStates f q = concat $ zipWith addBrackets (acceptable f q) q

addBrackets :: Bool -> Int -> String
addBrackets x y = if x
    then "[" ++ s ++ "]"
    else " " ++ s ++ " " where
        s = show y

acceptable :: [Int] -> [Int] -> [Bool]
acceptable _ [] = []
acceptable q (x:xs) = elem x q : acceptable q xs


toS_DFA :: [String] -> DFA
toS_DFA s =
    S_DFA
    { s_Q       = read $ s !! 0
    , s_sigma   = read $ s !! 1
    , delta     = read $ s !! 2
    , q0        = read $ s !! 3
    , s_F       = read $ s !! 4
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


-}
