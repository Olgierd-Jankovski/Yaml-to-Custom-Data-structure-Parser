{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Lib1(
    State(..), emptyState, gameStart, render, mkCheck, toggle, hint
) where

import Types

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State = State{
        doc :: [(String, Document)],
        path :: [String],
        -- now create some fields for the hints
        doc2 :: [(String, Document)],
        -- and the last part.. lets create some fields for the toggle
        doc3 :: [(String, Document)]
} deriving (Show, Eq)

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State [] [] [] []

-- IMPLEMENT sh
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart state (DMap doc) = State doc (path state) [] []
gameStart state (DList doc) = State (zip (map show [0..]) doc) (path state) [] []
gameStart state (DInteger doc) = State [("value", DInteger doc)] (path state) [] []
gameStart state (DString doc) = State [("value", DString doc)] (path state) [] []
gameStart state (DNull) = State [("value", DNull)] (path state) [] []

-- IMPLEMENT
-- renders your game board
render :: State  -> String
render state = "NUMBER OF HINTS: "++ (getHints state) ++ " \nHINT ROWS   :" ++ show (printHint state "row") ++ " \nHINT COLUMNS:" ++ show (printHint state "col")  ++ " \nNUMBER OF OCCUPIED COLUMNS:" ++ endl ++ "   "++ (renderColumns state) ++ endl ++ repeat1 10 state current_row_index 
-- we need to write a function renderRowIndex which will render the row with the current index

getHints :: State -> String
getHints state = show (getInteger (getDoc state "number_of_hints"))

getDoc :: State -> String -> Document
getDoc state key = snd (head (filter (\x -> fst x == key) (doc state)))

getInteger :: Document -> Int
getInteger (DInteger x) = x

repeat1 :: Int -> State ->  Int -> String
repeat1 0 _ _ = "" ++ endl
repeat1 n state current_row_index = (renderRowIndex state current_row_index) ++ (renderCurrentAmountOfRows (dec(current_row_index)) state) ++ endl ++ (repeat1 (n-1) state (current_row_index+1))

renderCurrentAmountOfRows :: Int -> State -> String
-- after the board row is printed, we need to print on the right the amount of occupied cells in that row
-- to get that data we need to search for the key "occupied_rows" in the State
-- then we need to get the value of that key, which is a list of integers
renderCurrentAmountOfRows index state = renderCurrentAmountOfRows' (index, doc state)

renderCurrentAmountOfRows' :: (Int, [(String, Document)]) -> String
renderCurrentAmountOfRows' (_, []) = ""
-- the result was 2 5 0 2 1 0 0 0 0 0... but has to be 1 1 4 2 2 2 2 2 0 4
-- the index is the row index, the value is the number of occupied cells in that row
-- it didnt print the 1st index because it was 0, so we need to add 1 to the index in the line below
renderCurrentAmountOfRows' (index, (key, value):xs) = if key == "occupied_rows" then  renderCurrentAmountOfRows''(index, value) ++ (renderCurrentAmountOfRows' (index, xs)) else renderCurrentAmountOfRows' (index, xs)

renderCurrentAmountOfRows'' :: (Int, Document) -> String
renderCurrentAmountOfRows'' (index, (DList list)) = show (getInteger (list !! index))

current_row_index :: Int
current_row_index = 1

-- create a inc function
inc :: Int -> Int
inc x = x + 1

dec :: Int -> Int
dec x = x - 1

renderRowIndex :: State -> Int -> String
renderRowIndex state current_row_index = (show (dec current_row_index))++ printDot ++ " " ++ (renderRow state current_row_index)

renderRow :: State -> Int -> String
renderRow state current_row_index = renderRow' state current_row_index 0

renderRow' :: State -> Int -> Int -> String
renderRow' state current_row_index current_column_index = if current_column_index == 10 then "" else (renderCell state current_row_index current_column_index) ++ (renderRow' state current_row_index (inc current_column_index))

renderCell :: State -> Int -> Int -> String
renderCell state current_row_index current_column_index = if (isOccupied state current_row_index current_column_index) then "X " else ". "

isOccupied :: State -> Int -> Int -> Bool 
--  k time to edit
-- it should check does the state doc3, toggle data, contain the current_row_index and current_column_index
-- if it does, then the cell is occupied
-- it contains the data like this: doc3 = [("3",DInteger 5),("3",DInteger 5)]}
-- the key is the col number, and the value is the row number
isOccupied state current_row_index current_column_index = isOccupied' (current_row_index - 1, current_column_index, doc3 state)

isOccupied' :: (Int, Int, [(String, Document)]) -> Bool
isOccupied' (_, _, []) = False

isOccupied' (current_row_index, current_column_index, (key, value):xs) = if (current_row_index == getInteger value) && (current_column_index == (read key :: Int)) then True else isOccupied' (current_row_index, current_column_index, xs)
--EDIT *****************************
-- IMPORTANT, isOccupied should be further modified to check if the cell is occupied by a ship or a hit
-- in general, when we type x and y indexes, the row index is bigger by one because of the row index on the left
-- to fix this we need to subtract 1 from the row index in the line below


renderColumns :: State -> String 
renderColumns state = renderColumns' (doc state)

renderColumns' :: [(String, Document)] -> String
renderColumns' [] = ""
renderColumns' ((key, value):xs) = if key == "occupied_cols" then renderColumns'' value else renderColumns' xs

renderColumns'' :: Document -> String
renderColumns'' (DList doc) = renderColumns''' doc

renderColumns''' :: [Document] -> String
renderColumns''' [] = ""
renderColumns''' (x:xs) = (renderColumns'''' x) ++ " " ++ (renderColumns''' xs)

renderColumns'''' :: Document -> String
renderColumns'''' (DInteger doc) = returnString doc

endl :: [Char]
endl = "\n"

printDot :: String
printDot = "."

returnString :: Int -> String
returnString num = show(num)

-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check

mkCheck state = Check (mkCheck' (doc3 state))

mkCheck' :: [(String, Document)] -> [Coord]
mkCheck' [] = []
mkCheck' ((key, value):xs) = (mkCheck'' (key, value)) : (mkCheck' xs)

mkCheck'' :: (String, Document) -> Coord
mkCheck'' (key, value) = Coord (read key :: Int) (getInteger value)

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
--toggle state tokens = State (doc state) (path state) (doc2 state) (doc3 state ++ (toggle' tokens))
--toggle' :: [String] -> [(String, Document)]
--toggle' [] = []
--toggle' (x:y:xs) = (x, DInteger (read y :: Int)) : (toggle' xs)
-- the toggle above works well, but we need the action of untoggle
-- if we toggle the same cell twice, it should untoggle it

toggle s [] = s
toggle s (x:y:xs) = toggle (toggle' s (x,y)) xs

toggle' :: State -> (String, String) -> State
toggle' s (x,y) = if (isToggled s (x,y)) then untoggle s (x,y) else toggle'' s (x,y)

toggle'' :: State -> (String, String) -> State
toggle'' s (x,y) = State (doc s) (path s) (doc2 s) (doc3 s ++ [(x, DInteger (read y :: Int))])

isToggled :: State -> (String, String) -> Bool
isToggled s (x,y) = isToggled' s (x,y) (doc3 s)

isToggled' :: State -> (String, String) -> [(String, Document)] -> Bool
isToggled' _ (_,_) [] = False
isToggled' s (x,y) ((key, value):xs) = if (x == key) && (y == (show (getInteger value))) then True else isToggled' s (x,y) xs

untoggle :: State -> (String, String) -> State
untoggle s (x,y) = State (doc s) (path s) (doc2 s) (untoggle' s (x,y) (doc3 s))

untoggle' :: State -> (String, String) -> [(String, Document)] -> [(String, Document)]
untoggle' _ (_,_) [] = []
untoggle' s (x,y) ((key, value):xs) = if (x == key) && (y == (show (getInteger value))) then xs else (key, value) : (untoggle' s (x,y) xs)



-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
-- this should save in doc2 field
hint (State doc path _ toggle) (DMap list) = State doc path list toggle

printHint :: State -> String -> [Int]
printHint state key=getHint (doc2 state) key

getHint :: [(String, Document)] -> String -> [Int]
getHint [] _=[]
getHint ((_, DMap list):xs) sear= merge (getHint list sear) (getHint xs sear) 
getHint ((_, DList _):xs) sear= getHint xs sear
getHint ((key, DInteger int):xs) sear= if key==sear then merge [int] (getHint xs sear) else getHint xs sear
getHint ((_, DString _):xs) sear= getHint xs sear
getHint ((_, DNull):xs) sear=getHint xs sear

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs