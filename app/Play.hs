module Main where
import           Data.List
import qualified Data.Vector                   as V
import           System.Exit
import           System.IO
import           Text.Printf

indent = replicate 8 ' '

data Player = Computer | Player
    deriving (Eq, Show)

data Board = Board (V.Vector Int)
    deriving (Show)

data MancalaGameState = MancalaGameState Board Player Player
    deriving Show

initialBoard = Board $ V.fromList (take 14 (cycle [6, 6, 6, 6, 6, 6, 0]))


printPlayer :: Player -> IO ()
printPlayer Computer = putStrLn "Computer's move: "
printPlayer Player = putStrLn "Player's move: "

letterRow :: [Char] -> String
letterRow chars = intercalate "  " $ map (\x -> [x]) chars

stringRow :: Board -> [Int] -> IO String
stringRow (Board b) xs = do
    lineStr <-
        return
            (foldl (\str n -> str ++ (printf "%3d" n)) "" (map (\i -> b V.! i) xs)
            )
    return lineStr

printTopRow :: Board -> IO ()
printTopRow b = do
    str <- stringRow b [12, 11 .. 7]
    putStrLn $ indent ++ "   " ++ letterRow ['A' .. 'F']
    putStrLn $ indent ++ "P" ++ str

printBottomRow :: Board -> IO ()
printBottomRow b = do
    str <- stringRow b [0 .. 5]
    putStrLn $ indent ++ " " ++ str ++ "  C"
    putStrLn $ indent ++ "   " ++ letterRow (reverse ['G' .. 'L'])


printMarbles :: Board -> IO ()
printMarbles (Board b) =
    putStrLn $ indent ++ (show $ b V.! 13) ++ (replicate 20 ' ') ++ (show $ b V.! 6)

printBoard :: Board -> IO ()
printBoard b = do
    printTopRow b
    printMarbles b
    printBottomRow b


printGameState :: MancalaGameState -> IO ()
printGameState (MancalaGameState b p _) = do
    printPlayer p
    printBoard b

startGameState = MancalaGameState initialBoard Computer Computer
main = do
    startGS <- return startGameState
    printGameState startGS
