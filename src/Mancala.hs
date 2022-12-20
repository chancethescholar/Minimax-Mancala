module Mancala where

import           Data.List
import qualified Data.Vector   as V
--import           Minimax

class GameState a where
    evaluate    :: a -> Int
    gameOver    :: a -> Bool
    possibleMoves    :: a -> [Int]
    makePossibility    :: a -> Int -> a
    isMaximizing    :: a -> Bool

data Player = Computer | Player2
    deriving (Eq, Show)

data Board = Board (V.Vector Int)
    deriving (Show)

initialBoard = Board $ V.fromList [6,6,6,6,6,6,0,6,6,6,6,6,6,0]

data MancalaGameState = MancalaGameState Board Player Player
    deriving Show
instance GameState MancalaGameState where
    evaluate (MancalaGameState board _ player) = getScore board player
    gameOver (MancalaGameState board _ _) = isGameOver board
    possibleMoves (MancalaGameState board player _) = getPossibleMoves board player
    makePossibility = distributeMarbles
    isMaximizing (MancalaGameState _ computer player) = computer == player

rowTotal :: Board -> Player -> Int
rowTotal (Board board) player =
    foldr (+) 0 $ map (board V.!) (currentPlayerRows player)

rowEmpty :: Board -> Player -> Bool
rowEmpty (Board board) player =
    null $ filter (\i -> (board V.! i) /= 0) (currentPlayerRows player)

mancalaTotal :: Board -> Player -> Int
mancalaTotal (Board board) player = board V.! (storePos player)

{- 
Game is over when both rows are empty 
Total 72 marbles are caught by players
-}
isGameOver :: Board -> Bool
isGameOver board 
    | totalPoints == 72 = True
    | otherwise = False
    where totalPoints = mancalaTotal board Computer + mancalaTotal board Player2

storePos :: Player -> Int
storePos p | p == Computer = 6 
           | otherwise = 13

getScore :: Board -> Player -> Int
getScore board player = (if (rowEmpty board Computer || rowEmpty board Player2) then 100 else 1) * (mancalaTotal board player - mancalaTotal board otherPlayer)
    where otherPlayer 
            | player == Computer = Player2
            | otherwise = Computer

getPossibleMoves :: Board -> Player -> [Int]
getPossibleMoves (Board board) player =
    filter (\i -> (board V.! i) /= 0) (currentPlayerRows player)

currentPlayerRows :: Player -> [Int]
currentPlayerRows player
                    | player == Computer = [0 .. 5]
                    | otherwise = [7 .. 12]

distributeMarbles :: MancalaGameState -> Int -> MancalaGameState
distributeMarbles (MancalaGameState (Board b) computer player) pos = (MancalaGameState finalNewBoard nextPlayer player)
  where
    count          = (b V.! pos)
    boardGetMarbles = Board (b V.// [(pos, 0)])
    (newBoard, nextPlayer) = placeStones boardGetMarbles computer (pos + 1) count
    finalNewBoard       = if (rowEmpty newBoard Computer || rowEmpty newBoard Player2) then endGameMove newBoard else newBoard

endGameMove :: Board -> Board
endGameMove (Board b) =
    Board
        $  b
        V.// (  [(6, (b V.! 6) + computerTotal), (13, (b V.! 13) + playerTotal)]
           ++ computerZeros
           ++ playerZeros
           )  where
    totalFunc = \l -> sum $ map (\i -> b V.! i) l
    computerTotal   = totalFunc (currentPlayerRows Computer)
    playerTotal   = totalFunc (currentPlayerRows Player2)
    zeroFunc  = map (\i -> (i, 0))
    computerZeros   = zeroFunc (currentPlayerRows Computer)
    playerZeros   = zeroFunc (currentPlayerRows Player2)

nextPos :: Player -> Int -> Int
nextPos player pos | (player == Computer && pos == 12) = 0
                   | (player == Player2 && pos == 5) = 7
                   | pos == 13 = 0
                   | otherwise = pos + 1

placeLastStone :: Board -> Player -> Int -> (Board, Player)
placeLastStone (Board board) player pos
    | board V.! pos == 0 && board V.! ((-) 12 pos) /= 0 && playerHole == player
    = (Board $ board V.// [(holeAcross, 0), (storePos player, newCount)], otherPlayer)
  where
    holeAcross      = (-) 12 pos
    holeAcrossCount = (board V.! holeAcross)
    newCount       = (mancalaTotal (Board board) player) + holeAcrossCount + 1
    otherPlayer    | player == Computer = Player2 
                   | otherwise = Computer
    playerHole | pos >= 0 && pos <= 6  = Computer
               | otherwise = Player2
placeLastStone (Board board) player pos =
    (newBoard, nextPlayer)
    where newBoard = Board $ board V.// [(pos, (board V.! pos) + 1)]
          otherPlayer    | player == Computer = Player2 
                         | otherwise = Computer
          nextPlayer     | pos == storePos player = player 
                         | otherwise = otherPlayer

takeReverse :: [Int] -> Int -> [Int]
takeReverse listUpdates count = takeReverse' listUpdates count []  
    where takeReverse' (x : _ ) 1     acc = x : acc
          takeReverse' (x : xs) count acc = takeReverse' xs (count - 1) (x : acc)

placeStones :: Board -> Player -> Int -> Int -> (Board, Player)
placeStones (Board b) player pos count = placeLastStone intermediateBrd player newPos  
    where allUpdates  = iterate (\i -> nextPos player i) pos
          currUpdates = takeReverse allUpdates count
          intermediateBrd = Board $ b V.// (map (\l -> (head l, (b V.! head l) + length l)) $ group . sort $ tail currUpdates)
          newPos = head currUpdates
