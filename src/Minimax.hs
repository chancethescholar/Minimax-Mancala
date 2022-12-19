module Minimax
where
import Debug.Trace
import Data.List
import Data.Ord

class GameState a where
    evaluate    :: a -> Int
    gameOver    :: a -> Bool
    possibleMoves    :: a -> [Int]
    makeSuccessor    :: a -> Int -> a
    isMaximizing    :: a -> Bool

{-
Test:
> gs = MancalaGameState initialBoard Computer Computer
> alphabeta gs False 0 8 (-1000) 1000
-}

minimax   :: (GameState a) => a -> Int -> Int -> Int -> Int -> (Int, Maybe Int)
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