module Minimax
where
import Debug.Trace
import Data.List
import Data.Ord
import Mancala
import qualified Data.Vector   as V
import Control.Parallel.Strategies

{-
Test:
main :: IO()
main = do
    let board = Board $ V.fromList [1,2,7,4,0,1,32,1,0,2,1,1,2,18]
    let gs = MancalaGameState board Computer Player2
    print (minimax gs False 0 8)

-- ghc -threaded -rtsopts -eventlog --make -main-is Minimax  Minimax.hs -package vector
-- time ./Minimax +RTS -ls -s
-- time ./Minimax +RTS -N2 -ls -s
-}

minimax    :: (GameState a) => a -> Bool -> Int -> Int -> (Int, Maybe Int)
minimax gs _ depth depthlimit | depth == depthlimit || gameOver gs = (evaluate gs, Nothing)
minimax gs minimize depth depthlimit =
    let minOrMax = (if minimize then minimumBy else maximumBy) (comparing fst) 
        successors = (possibleMoves gs)
        scores = (map fst $ map (\succ -> (minimax (makeSuccessor gs succ) (not minimize) (depth+1) depthlimit)) successors) `using` parList rseq
        wrappedSuccessors = map Just successors 
        scoreSuccPairs = zip scores wrappedSuccessors in
    minOrMax scoreSuccPairs

{-
minimax    :: (GameState a) => a -> Bool -> Int -> Int -> (Int, Maybe Int)
minimax gs _ depth depthlimit | depth == depthlimit || gameOver gs = (evaluate gs, Nothing)
minimax gs minimize depth depthlimit =
    let minOrMax = (if minimize then minimumBy else maximumBy) (comparing fst)
        successors = (possibleMoves gs)
        scores = map fst $ map (\succ -> (minimax (makeSuccessor gs succ) (not minimize) (depth+1) depthlimit)) successors
        wrappedSuccessors = map Just successors
        scoreSuccPairs = zip scores wrappedSuccessors in
    minOrMax scoreSuccPairs
-}

{-minimax   :: (GameState a) => a -> Int -> Int -> Int -> Int -> (Int, Maybe Int)
minimax gs _ _ _ _ | gameOver gs = (evaluate gs, Nothing)
minimax gs depth depthlimit _ _ | depth == depthlimit = (evaluate gs, Nothing)
minimax gs depth depthlimit alpha beta =
    alphabetafold successors alpha beta (-1)
    where   successors = possibleMoves gs
            alphabetafold [] a _ bestChild = (a, Just bestChild)
            alphabetafold (x:xs) a b bestChild =
                let child = makeSuccessor gs x
                    newAlpha = (if (isMaximizing child) then alphabetamax else alphabetamin) child (depth+1) depthlimit a b in
                if (newAlpha >= b)
                then (newAlpha, Just x)
                else alphabetafold xs (max a newAlpha) b (if newAlpha > a then x else bestChild)
-} 
alphabetamax    :: (GameState a) => a -> Int -> Int -> Int -> Int -> Int
alphabetamax gs _ _ _ _ | gameOver gs = evaluate gs
alphabetamax gs depth depthlimit _ _ | depth == depthlimit = evaluate gs
alphabetamax gs depth depthlimit alpha beta =
    alphabetaHelper gs successors alpha beta depth depthlimit
    where   successors = possibleMoves gs

alphabetamin    :: (GameState a) => a -> Int -> Int -> Int -> Int -> Int
alphabetamin gs _ _ _ _ | gameOver gs = evaluate gs
alphabetamin gs depth depthlimit _ _ | depth == depthlimit = evaluate gs
alphabetamin gs depth depthlimit alpha beta =
    alphabetaHelper gs successors alpha beta depth depthlimit
    where   successors = possibleMoves gs

alphabetaHelper :: (GameState a) => a -> [Int] -> Int -> Int -> Int -> Int -> Int
alphabetaHelper _ [] _ b _ _= b
alphabetaHelper gs (x:xs) a b depth depthlimit =
    let child = makeSuccessor gs x
        newBeta = (if (isMaximizing child) then alphabetamax else alphabetamin) child (depth+1) depthlimit a b in
    if (newBeta <= a)
        then newBeta
        else alphabetaHelper gs xs a (min b newBeta) depth depthlimit