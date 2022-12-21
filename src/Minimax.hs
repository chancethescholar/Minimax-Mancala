module Minimax
where
import Debug.Trace
import Data.List
import Data.Ord
import Mancala
import qualified Data.Vector   as V
import Control.Parallel.Strategies

{-
--Test:
main :: IO()
main = do
    let board = Board $ V.fromList [6,6,6,6,6,6,0,6,6,6,6,6,6,0]
    let gs = MancalaGameState board Computer Player2
    --print (minimax gs False 0 8)
    --print (minimaxPar gs False 0 8) 
    print (alphabeta gs 0 8 (-1000) 1000)
-}

-- ghc -threaded -rtsopts -eventlog --make -main-is Minimax  Minimax.hs -package vector
-- time ./Minimax +RTS -ls -s
-- time ./Minimax +RTS -N2 -ls -s

minimax    :: (GameState a) => a -> Bool -> Int -> Int -> (Int, Maybe Int)
minimax gs _ depth depthlimit | depth == depthlimit || gameOver gs = (evaluate gs, Nothing)
minimax gs minimize depth depthlimit =
    let minOrMax = (if minimize then minimumBy else maximumBy) (comparing fst) 
        possibilities = (possibleMoves gs)
        scores = map fst $ map (\poss -> (minimax (makePossibility gs poss) (not minimize) (depth+1) depthlimit)) possibilities
        wrappedPossibilities = map Just possibilities 
        scorePossPairs = zip scores wrappedPossibilities in
    minOrMax scorePossPairs

minimaxPar    :: (GameState a) => a -> Bool -> Int -> Int -> (Int, Maybe Int)
minimaxPar gs _ depth depthlimit | depth == depthlimit || gameOver gs = (evaluate gs, Nothing)
minimaxPar gs minimize depth depthlimit =
    let minOrMax = (if minimize then minimumBy else maximumBy) (comparing fst) 
        possibilities = (possibleMoves gs)
        scores = (map fst $ map (\poss -> (minimaxPar (makePossibility gs poss) (not minimize) (depth+1) depthlimit)) possibilities) `using` parList rseq
        wrappedPossibilities = map Just possibilities 
        scorePossPairs = zip scores wrappedPossibilities in
    minOrMax scorePossPairs

{-
alphabeta   :: (GameState a) => a -> Int -> Int -> Int -> Int -> (Int, Maybe Int)
alphabeta gs _ _ _ _ | gameOver gs = (evaluate gs, Nothing)
alphabeta gs depth depthlimit _ _ | depth == depthlimit = (evaluate gs, Nothing)
alphabeta gs depth depthlimit alpha beta =
    alphabetafold possibilities alpha beta (-1)
    where   possibilities = possibleMoves gs
            alphabetafold [] a _ bestChild = (a, Just bestChild)
            alphabetafold (x:xs) a b bestChild =
                let child = makePossibility gs x
                    newAlpha = (if (isMaximizing child) then alphabetamax else alphabetamin) child (depth+1) depthlimit a b in
                if (newAlpha >= b)
                then (newAlpha, Just x)
                else alphabetafold xs (max a newAlpha) b (if newAlpha > a then x else bestChild)

alphabetamax    :: (GameState a) => a -> Int -> Int -> Int -> Int -> Int
alphabetamax gs _ _ _ _ | gameOver gs = evaluate gs
alphabetamax gs depth depthlimit _ _ | depth == depthlimit = evaluate gs
alphabetamax gs depth depthlimit alpha beta =
    alphabetaHelper gs possibilities alpha beta depth depthlimit
    where   possibilities = possibleMoves gs

alphabetamin    :: (GameState a) => a -> Int -> Int -> Int -> Int -> Int
alphabetamin gs _ _ _ _ | gameOver gs = evaluate gs
alphabetamin gs depth depthlimit _ _ | depth == depthlimit = evaluate gs
alphabetamin gs depth depthlimit alpha beta =
    alphabetaHelper gs possibilities alpha beta depth depthlimit
    where   possibilities = possibleMoves gs

alphabetaHelper :: (GameState a) => a -> [Int] -> Int -> Int -> Int -> Int -> Int
alphabetaHelper _ [] _ b _ _= b
alphabetaHelper gs (x:xs) a b depth depthlimit =
    let child = makePossibility gs x
        newBeta = (if (isMaximizing child) then alphabetamax else alphabetamin) child (depth+1) depthlimit a b in
    if (newBeta <= a)
        then newBeta
        else alphabetaHelper gs xs a (min b newBeta) depth depthlimit
-}