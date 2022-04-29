-- Ariath S Gonzalez
module Board where

    -- Creating a board and accessing its elements. --

    -- Create board of size n * n.
    mkBoard :: Int -> [[Int]]
    mkBoard n = [[0 | x <- [1 .. n]] | y <- [1 .. n]]

    -- Player marked as 1.
    mkPlayer :: Int
    mkPlayer = 1

    -- Opponent marked as 2.
    mkOpponent :: Int
    mkOpponent = 2

    -- Get the size of the board.
    size :: [[Int]] -> Int
    size bd = length bd

    -- Return row at indicated y-index.
    row :: Int -> [[Int]] -> [Int]
    row _ [[]] = []
    row y (h : t)
        | y == 1 = h
        | otherwise = row (y - 1) t

    -- Return column at indicated x-index.
    col :: Int -> [[Int]] -> [Int]
    -- Idx takes in x and row, effectively traversing each row until position of column reached. 
    col x bd = [idx x h | h <- bd]
        where idx y (h : t)
                | y == 1 = h
                | otherwise = idx (y - 1) t

    -- Return diagonal at indicated (x, y) place.
    diagonal :: Int -> Int -> [[Int]] -> [Int]
    diagonal x y bd
        | (y - 1) < size bd && (x - 1) < size bd = (row y bd) !! (x - 1) : diagonal (x + 1) (y + 1) bd
        | otherwise = []

    -- Checking places and placing stones. --

    -- Mark a place (x, y) in a board by a player p.
    mark :: Int -> Int -> [[Int]] -> Int -> [[Int]]
    mark x y (h : t) p
        | x == 1 = (markRow y h p) : t
        | otherwise = h : mark (x - 1) y t p

    -- Place stone at the given row/col.
    markRow :: Int -> [Int] -> Int -> [Int]
    markRow n (h : t) p
        | n == 1 = p : t
        | otherwise = h : markRow (n - 1) t p

    -- Check if a place (x, y) of a board bd is unmarked.
    isEmpty :: Int -> Int -> [[Int]] -> Bool
    isEmpty x y bd = if ((row x bd) !! (y - 1)) == 0 then True else False

    -- Check if a place (x, y) of a board bd have a stone placed.
    isMarked :: Int -> Int -> [[Int]] -> Bool
    isMarked x y bd = if isEmpty x y bd then False else True

    -- Check if a place (x, y) of a board bd have a stone placed by a player p.
    isMarkedBy :: Int -> Int -> [[Int]] -> Int -> Bool
    isMarkedBy x y bd p = if ((row x bd) !! (y - 1)) == p then True else False

    -- Return the player of the stone placed on a place (x, y) of a board bd. (Assuming not empty)
    marker :: Int -> Int -> [[Int]] -> Int
    marker x y bd = (row x bd) !! (y - 1)

    -- Determining the game outcomes. --

    -- Check if board fully marked by stones or not. 
    isFull :: [[Int]] -> Bool
    isFull bd = length (filter (\x -> x == 0) (concat bd)) == 0

    -- isWonBy bd p
    isWonBy :: [[Int]] -> Int -> Bool
    isWonBy bd p
        | elem True ([hasWinSeq (row n bd) p | n <- [1 .. size bd]]) = True -- Horizontal
        | elem True ([hasWinSeq (col n bd) p | n <- [1 .. size bd]]) = True -- Vertical
        | elem True ([hasWinSeq (diagonal n 1 bd) p | n <- [1 .. size bd]]) = True -- Top Diagonal
        | elem True ([hasWinSeq (diagonal 1 n bd) p | n <- [1 .. size bd]]) = True -- Bottom Diagonal
        | elem True ([hasWinSeq (diagonal n 1 (reverse bd)) p | n <- [1 .. size bd]]) = True -- Reverse Diagonal
        | elem True ([hasWinSeq (diagonal 1 n (reverse bd)) p | n <- [1 .. size bd]]) = True -- Reverse Diagonal
        | otherwise = False

    -- Check if given row/col has a win sequence.
    hasWinSeq :: [Int] -> Int -> Bool
    hasWinSeq [] _ = False
    hasWinSeq (h : t) p
        | h == p && length t >= 4 && length ([n | n <- take 4 t, n == p]) == 4 = True
        | otherwise = hasWinSeq t p 

    -- Check if the game ended in a draw. 
    isDraw :: [[Int]] -> Bool
    isDraw bd = isFull bd

    -- Check if the game ended.
    isGameOver :: [[Int]] -> Bool
    isGameOver bd
        | isWonBy bd mkPlayer = True
        | isWonBy bd mkOpponent = True
        | isDraw bd = True
        | otherwise = False

    -- Return a string representation of a board bd.
    boardToStr :: (Int -> Char) -> [[Int]] -> String
    boardToStr playerToChar bd = h1 ++ h2 ++ rows
        where 
            h1 = "x " ++ concat([show (mod i 10 ) ++ " "| i <- [1 .. size bd]]) ++ "\n"
            h2 = "y " ++ concat(["-" | i <- [1 .. ((size bd) * 2)]]) ++ "\n"
            rows = concat([show(mod i 10) ++ "|" ++ rowToStr playerToChar (row i bd) | i <- [1 .. size bd]])

    -- Return a string representation of a row.
    rowToStr :: (Int -> Char) -> [Int] -> String
    rowToStr _ [] = "\n"
    rowToStr f (h : t) = [f h] ++ " " ++ rowToStr f t