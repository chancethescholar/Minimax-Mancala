module Minimax where
import           Data.List
import           Data.Ord
import           Debug.Trace
import qualified Data.Vector   as V

class GameState a where
    pointsLeft    :: a -> Int
    gameOver    :: a -> Bool
    possibleMoves    :: a -> [Int]
    startGameState    :: a -> Int -> a

data Player = Computer | Player
    deriving (Eq, Show)

data Board = Board (V.Vector Int)
    deriving (Show)

data MancalaGameState = MancalaGameState Board Player Player

initialBoard = Board $ V.fromList (take 14 (cycle [6, 6, 6, 6, 6, 6, 0]))
    
instance GameState MancalaGameState where
    pointsLeft _ = 80
    gameOver _ = False
    possibleMoves _ = [1,3,5]
    startGameState _ _= MancalaGameState initialBoard Computer Computer


--Test:
-- ghc -threaded -rtsopts -eventlog --make -main-is Minimax  Minimax.hs -package vector
-- time ./Minimax +RTS -ls -s
-- time ./Minimax +RTS -N2 -ls -s
main :: IO()
main = do
    let gs = MancalaGameState initialBoard Computer Computer
    print (minimax gs False 0 8)
    --(80,Just 5)


minimax :: (GameState a) => a -> Bool -> Int -> Int -> (Int, Maybe Int)
minimax gs _ depth depthlimit 
    | depth == depthlimit || gameOver gs = (pointsLeft gs, Nothing)
minimax gs minimize depth depthlimit =
    let
        minOrMax   = (if minimize then minimumBy else maximumBy) (comparing fst)
        successors = possibleMoves gs
        scores     = map fst $ map
            (\succ ->
                (minimax (startGameState gs succ)
                         (not minimize)
                         (depth + 1)
                         depthlimit
                )
            )
            successors
        wrappedSuccessors = map Just successors
        scoreSuccPairs    = zip scores wrappedSuccessors
    in
        minOrMax scoreSuccPairs
