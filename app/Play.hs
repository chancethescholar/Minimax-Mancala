module Main where
import           Data.List
import qualified Data.Vector                   as V
import           Mancala
import           Minimax
import           System.Exit
import           System.IO
import           Text.Printf

{-
Starting game board

    A  B  C  D  E  F
P   6  6  6  6  6  6
0                    0
    6  6  6  6  6  6  C
    L  K  J  I  H  G
-}

-- translate letters into numeric spaces
--valid computer moves
getComputerMove :: String -> Int
getComputerMove "L" = 0
getComputerMove "K" = 1
getComputerMove "J" = 2
getComputerMove "I" = 3
getComputerMove "H" = 4
getComputerMove "G" = 5
getComputerMove "q" = 13 --quit
getComputerMove "Q" = 13 --quit
getComputerMove _ = 14 --invalid input

--valid player moves
getPlayerMove :: String -> Int 
getPlayerMove "A" = 12
getPlayerMove "B" = 11
getPlayerMove "C" = 10
getPlayerMove "D" = 9
getPlayerMove "E" = 8
getPlayerMove "F" = 7
getPlayerMove "q" = 13 --quit
getPlayerMove "Q" = 13 --quit
getPlayerMove _ = 14 --invalid inpuy

-- translate numeric spaces into letters 
getComputerLetter :: Int -> String
getComputerLetter 0 = "L"
getComputerLetter 1 = "K"
getComputerLetter 2 = "J"
getComputerLetter 3 = "I"
getComputerLetter 4 = "H"
getComputerLetter 5 = "G"
getComputerLetter _ = error "Invalid move"

--input move, check if move is valid
getMove :: Player -> Board -> IO Int
getMove p (Board b) = do
    str <- getLine
    let move = if p == Computer then getComputerMove str else getPlayerMove str
    if (move == 14 || ((b V.! move) == 0 && move /= 13)) --invalid letter or hole that is empty
        then do
            putStr "Invalid Move. Try again: "
            hFlush stdout
            getMove p (Board b)
        else do
            if move == 13 --quit key
            then do
                putStrLn "Quitting."
                exitWith ExitSuccess
            else return move
                
--identify who's turn it is
printPlayer :: Player -> IO ()
printPlayer Computer = putStrLn "Computer: "
printPlayer Player2 = putStrLn "You: "

--print marbles in each hole
printMarbles :: Board -> [Int] -> IO String
printMarbles (Board b) xs = do
    lineStr <-
        return
            (foldl (\str n -> str ++ (printf "%3d" n)) "" (map (\i -> b V.! i) xs)
            )
    return lineStr

--print hole letters on top along with marbles
printTopRow :: Board -> IO ()
printTopRow b = do
    str <- printMarbles b [12, 11 .. 7]
    putStrLn $ "        " ++ "   " ++ "A  B  C  D  E  F"
    putStrLn $ "        " ++ "P" ++ str

--print hole letters on bottom along with marbles
printBottomRow :: Board -> IO ()
printBottomRow b = do
    str <- printMarbles b [0 .. 5]
    putStrLn $ "        " ++ " " ++ str ++ "  C"
    putStrLn $ "        " ++ "   " ++ "L  K  J  I  H  G"

--print both stores
printStores :: Board -> IO ()
printStores (Board b) =
    putStrLn $ "        " ++ (show $ b V.! 13) ++ (replicate 20 ' ') ++ (show $ b V.! 6)

--print board
printBoard :: Board -> IO ()
printBoard b = do
    printTopRow b
    printStores b
    printBottomRow b

--print board and get game input
printGameState :: MancalaGameState -> IO ()
printGameState (MancalaGameState b p _) = do
    printPlayer p
    printBoard b

applyMove gs move = return (distributeMarbles gs move) >>= playGame

humanMoveGS (MancalaGameState board player _) = do
    m <- getMove player board
    putStrLn ""
    return m

makeMoveGS :: MancalaGameState -> IO Int
makeMoveGS gs = do
    let (score, move) = minimax gs False 0 8
    case move of
        (Just x) -> do
            printf "Computer move: '%s'.\n\n"
                   (getComputerLetter x)
            return x
        Nothing -> error "Invalid move: Nothing"

playGame :: MancalaGameState -> IO()
playGame (MancalaGameState board computer player) | rowEmpty board Computer || rowEmpty board Player2 =
    let winString = case (evaluate (MancalaGameState board computer player)) of
            x | x > 0 -> "Winner is " ++ (show player)
            x | x < 0 -> "Winner is " ++ (show other)
            0         -> "Tie."
            where other | player == Computer = Player2
                            | otherwise = Computer
    in  (putStrLn $ "Game over. " ++ winString) >> printGameState (MancalaGameState board computer player)
playGame (MancalaGameState board Computer Computer) =
    printGameState (MancalaGameState board Computer Computer) >> putStrLn "Computer's turn" >> makeMoveGS (MancalaGameState board Computer Computer) >>= \m ->
        applyMove (MancalaGameState board Computer Computer) m
playGame (MancalaGameState board Player2 x) =
    printGameState (MancalaGameState board Player2 x)
        >>  putStr "Enter move: "
        >>  hFlush stdout
        >>  humanMoveGS (MancalaGameState board Player2 x)
        >>= \m -> applyMove (MancalaGameState board Player2 x) m

startGameState = MancalaGameState initialBoard Computer Computer
main = do
    startGS <- return startGameState
    playGame startGS
