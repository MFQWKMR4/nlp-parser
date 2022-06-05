module CKY where

import Data.Array
import Data.Maybe
import GHC.Base (join)
import Debug.Trace

-- nonterminal symbol
-- terminal symbol
data Sym = NTS String | TS String deriving (Show, Eq)

-- parsed tree
data PT a = Z | Node a (PT a) (PT a) deriving (Show, Eq)

-- rewrite rule
type RL = (Sym, [Sym])
type Rules = [RL]

-- bottom up rewrite rules (sample case)
bottomUp :: [Sym] -> [Sym]
bottomUp [TS "a"] = [NTS "det"]
bottomUp [TS "the"] = [NTS "det"]
bottomUp [TS "desk"] = [NTS "n"]
bottomUp [TS "drawer"] = [NTS "n"]
bottomUp [TS "man"] = [NTS "n", NTS "v"]
bottomUp [TS "with"] = [NTS "prep"]
bottomUp [TS "broke"] = [NTS "v"]
bottomUp [NTS "NP", NTS "VP"] = [NTS "S"]
bottomUp [NTS "det", NTS "n"] = [NTS "NP"]
bottomUp [NTS "NP", NTS "PP"] = [NTS "NP"]
bottomUp [NTS "v", NTS "NP"] = [NTS "VP"]
bottomUp [NTS "VP", NTS "PP"] = [NTS "VP"]
bottomUp [NTS "prep", NTS "NP"] = [NTS "PP"]
bottomUp _ = []

type Sentence = [String]
-- example
sentence :: Sentence
sentence = ["the", "man", "broke", "a", "desk", "with", "a", "drawer"]

type Candidate = [PT Sym]
type Table = Array (Int, Int) Candidate
type Step = Sentence -> Table -> Table

applyRule :: ([Sym] -> [Sym]) -> [PT Sym] -> Candidate
applyRule rl [Node a tr1 tr2, Node b tr3 tr4] = case rl [a, b] of
    [] -> []
    li -> [ Node elem (Node a tr1 tr2) (Node b tr3 tr4) | elem <- li]
applyRule rl [Node a Z Z] = case rl [a] of
    [] -> []
    li -> [ Node elem Z Z | elem <- li]
applyRule rl x = error ("invalid argumants!!!" ++ show x)

rewrite :: [PT Sym] -> Candidate
rewrite = applyRule bottomUp

-- step1
step1 :: Step
step1 s arr = arr // [ ((i,i), rewrite [Node (TS word) Z Z]) | (i, word) <- zipWithIndex s]

-- step2
loop :: Int -> [(Int, [(Int, Int)])]
loop l = [(d, [ (i,i+d) | i <- [1..l-d] ]) | d <- [1..l-1] ]

running :: (Int, Int) -> [((Int, Int),(Int, Int))]
running (i,j) = [ ((i,x),(y,j)) | (x,y) <- zip [i..j-1] [i+1..j]]

makeCandidate :: Table -> (Int, Int) -> [Candidate]
makeCandidate arr (i,j) = [ rewrite [leftCandidate, rightCandidate] | ((a,b),(c,d)) <- running (i,j), leftCandidate <- arr ! (a,b), rightCandidate <- arr ! (c,d)]

miniStep :: Table -> (Int, [(Int, Int)]) -> Table
miniStep arr (d,li) = arr // [ ((i,j), join $ makeCandidate arr (i,j)) | (i,j) <- li]

step2 :: Step
step2 s arr = foldl miniStep arr (loop $ length s)

-- last
extract :: PT a -> Maybe a
extract (Node a tr1 tr2) = Just a
extract _ = Nothing

check :: Int -> Table -> Bool
check j arr = NTS "S" `elem` res where res = mapMaybe extract $ arr ! (1,j)

result :: Int -> Table -> IO ()
result j arr = if check j arr then print $ arr ! (1,j) else putStrLn "miss!"

-- main
ckyMethod :: Sentence -> IO ()
ckyMethod s = result j $ step2 s $ step1 s $ genArr j where j = length s

-- case1
case1 :: IO ()
case1 = ckyMethod sentence

-- helper
zipWithIndex:: [String] -> [(Int, String)]
zipWithIndex = zip [1..]

genArr :: Int -> Table
genArr l = array ((1,1),(l,l)) [((i,j), []) | i <- [1..l], j <- [1..l]]

