-- Ariath S Gonzalez
module Main where

    import Board
    import System.IO

    -- Return a character representation of a player p.
    playerToChar :: Int -> Char
    playerToChar p
        | p == 1 = 'O'
        | p == 2 = 'X'
        | otherwise = '.'

    -- Read a 1-based pair of indices (x, y) for player p. 
    readXY :: [[Int]] -> Int -> IO(Int, Int)
    readXY bd p = do
        putStrLn (boardToStr playerToChar bd)
        putStrLn "Enter x and y (1-15, e.g., 8 10 | Enter -1 to quit):"
        line <- getLine
        let parsed = reads line :: [(Int, String)] in
            if length parsed == 0
            then readXY'
            -- x is not empty.
            else let (x, _) = head parsed in
                -- Quit.
                if x == -1
                then return (-1, -1)
                else if x > 0 && x <= size bd
                then let parsed = reads (tail(tail(line))) :: [(Int, String)] in
                    if length parsed == 0 
                    then readXY'
                    -- y is not empty.
                    else let (y, _) = head parsed in
                        if y == -1
                        then return (-1, -1)
                        -- Check if in range and position selected is empty
                        else if y > 0 && y <= size bd && isEmpty x y bd
                        then return (x, y)
                        else readXY'
                else readXY'
        where
            readXY' = do
                putStrLn "Invalid input!"
                readXY bd p

    -- Main function to play omok game between two human players.
    main :: IO ()
    main = do
        let bd = mkBoard 15
        main' bd

    main' :: [[Int]] -> IO()
    main' bd = do
        -- Player 1. 
        (x, y) <- readXY bd mkPlayer
        -- Check if player chose to quit.
        if x == -1 then do
            putStrLn "Quitting game..."
            return ()
        else do
            let ackMove = mark x y bd mkPlayer 
            putStrLn (boardToStr playerToChar ackMove)
            if isWonBy ackMove mkPlayer then do
                putStrLn "O has Won!"
                return ()
            else do
                -- Player 2. 
                (oX, oY) <- readXY ackMove mkOpponent
                -- Check if player chose to quit.
                if oX == -1 then do
                    putStrLn "Quitting game..."
                    return ()
                else do
                    let move = mark oX oY ackMove mkOpponent 
                    putStrLn (boardToStr playerToChar move)
                    if isWonBy move mkOpponent then do
                        putStrLn "X has Won!"
                        return ()
                    -- Check draw.
                    else if isDraw move then do
                        putStrLn "Draw!"
                        return ()
                    else do
                        main' move
