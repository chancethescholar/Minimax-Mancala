module Main where
import           Data.List
import qualified Data.Vector                   as V
import           System.Exit
import           System.IO
import           Text.Printf

data Player = Computer | Player2
    deriving (Eq, Show)

data Board = Board (V.Vector Int)
    deriving (Show)

data MancalaGameState = MancalaGameState Board Player Player
    deriving Show

initialBoard = Board $ V.fromList (take 14 (cycle [6, 6, 6, 6, 6, 6, 0]))

getComputerMove :: String -> Int
getComputerMove "L" = 0
getComputerMove "K" = 1
getComputerMove "J" = 2
getComputerMove "I" = 3
getComputerMove "H" = 4
getComputerMove "G" = 5
--quit
getComputerMove "q" = 13
getComputerMove "Q" = 13 
getComputerMove _ = 14 --invalid

getPlayerMove :: String -> Int 
getPlayerMove "A" = 12
getPlayerMove "B" = 11
getPlayerMove "C" = 10
getPlayerMove "D" = 9
getPlayerMove "E" = 8
getPlayerMove "F" = 7
--quit
getPlayerMove "q" = 13
getPlayerMove "Q" = 13
getPlayerMove _ = 14 --invalid

getComputerLetter :: Int -> String
getComputerLetter 0 = "L"
getComputerLetter 1 = "K"
getComputerLetter 2 = "J"
getComputerLetter 3 = "I"
getComputerLetter 4 = "H"
getComputerLetter 5 = "G"
getComputerLetter _ = error "Invalid move"

getMove :: Player -> IO Int
getMove p = do
    str <- getLine
    let num = if p == Computer then getComputerMove str else getPlayerMove str
    case num of
        14 -> do
            putStr "Invalid Move. Try again: "
            hFlush stdout
            getMove p
        13 -> do
            putStrLn "Quitting."
            exitWith ExitSuccess
        num -> return num

printPlayer :: Player -> IO ()
printPlayer Computer = putStrLn "Computer's side: "
printPlayer Player2 = putStrLn "Your side: "

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
    putStrLn $ "        " ++ "   " ++ letterRow ['A' .. 'F']
    putStrLn $ "        " ++ "P" ++ str

printBottomRow :: Board -> IO ()
printBottomRow b = do
    str <- stringRow b [0 .. 5]
    putStrLn $ "        " ++ " " ++ str ++ "  C"
    putStrLn $ "        " ++ "   " ++ letterRow (reverse ['G' .. 'L'])


printMarbles :: Board -> IO ()
printMarbles (Board b) =
    putStrLn $ "        " ++ (show $ b V.! 13) ++ (replicate 20 ' ') ++ (show $ b V.! 6)

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
