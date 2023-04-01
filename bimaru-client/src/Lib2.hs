{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document (..), Check (Check), Coord (Coord) )
import Lib1 (State(..))

--These are our requirements for changing project
--Now project does not build out of the box, it requires you to implement some ToDocument instance(s). Implement the instances
--"renderDocument" converts any Document to yaml. Indeed, the specification is quite big and output might vary, so if game server understands your yaml request, then your "renderDocument" is good enough. Here is the example how Check entity in translated to yaml.
--Now "error" is not allowed: gameStart and hint function return Left on error (but functionality remains the same)
--You have to implement a bunch of tests to show your implemented functions work correctly. Please cover all constructors of Document in your tests.

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
-- hint FUNCTION checks if the hints are integers and are not negative numbers
    -- but does it check all of the hints? no, it does not (OLDER validation function)
    -- we should check all of the hints, not just the first one
hint (State idoc ipath _ toggle) (DMap list) =
-- it should call the function, that will check all of the hints
-- and if it is not a number, then it should return Left "error"
    case (checkHints list) of -- we put this argument into function: checkHints, (NEWER validation)
        Left err -> Left err
        Right _ -> Right (State idoc ipath list toggle)

checkHints :: [(String, Document)] -> Either String [(String, Document)]
-- here we are checking this: [("head",DMap [("col",DInteger 8),("row",DInteger 2)]),("tail",DMap [("head",DMap [("col",DInteger 8),("row",DInteger 3)]),("tail",DMap [("head",DMap [("col",DInteger 8),("row",DInteger 4)]),("tail",DMap [("head",DMap [("col",DInteger 8),("row",DInteger 5)]),("tail",DNull)])])])])]
-- we should check, if it is a number and if it is not negative
-- if it is not a number, then we should return Left "error"
checkHints [] = Right []
checkHints ((_, DMap list):xs) = 
    case (checkHints list) of
        Left err -> Left err
        Right _ -> checkHints xs
checkHints ((_, DInteger i):xs) =
    if i < 0 then Left "error" else checkHints xs
checkHints ((_, DNull):xs) = checkHints xs
checkHints ((_, _):_) = Left "error"


-- lets implement gameStart
-- IMPLEMENT
-- Adds game start data to the game state
-- Errors are reported via Either but not error
gameStart :: State -> Document -> Either String State
--gameStart state (DMap doc) = Right $ State doc (path state) [] []
--gameStart state (DList doc) = Right $ State (zip (map show [0..]) doc) (path state) [] []
--gameStart state (DInteger doc) = Right $ State [("value", DInteger doc)] (path state) [] []
--gameStart state (DString doc) = Right $ State [("value", DString doc)] (path state) [] []
--gameStart state (DNull) = Right $ State [("value", DNull)] (path state) [] []
-- we get this input:
-- State {doc = [("number_of_hints",DInteger 10),("occupied_cols",DList [DInteger 3,DInteger 3,DInteger 0,DInteger 0,DInteger 3,DInteger 0,DInteger 5,DInteger 0,DInteger 4,DInteger 2]),("occupied_rows",DList [DInteger 1,DInteger 
--1,DInteger 4,DInteger 2,DInteger 2,DInteger 2,DInteger 2,DInteger 2,DInteger 0,DInteger 4]),("game_setup_id",DString "9cfda689-fee4-4a87-b82b-49dd379f3cad")], path = [], doc2 = [], doc3 = []}
-- lets check if we got number_of_hints escpecially 10 and occupied_cols DList, which consists of integers and are not negative
gameStart state (DMap idoc) =
    case (checkGameStart idoc) of
        Left err -> Left err
        Right _ -> Right $ State idoc (path state) [] []

gameStart state (DList idoc) = 
    case (checkGameStartList idoc) of
        Left err -> Left err
        Right _ -> Right $ State (zip (map show [0..]) idoc) (path state) [] []

gameStart state (DInteger idoc) =
    case (checkGameStartInteger idoc) of
        Left err -> Left err
        Right _ -> Right $ State [("value", DInteger idoc)] (path state) [] []

gameStart state (DString idoc) =
    case (checkGameStartString idoc) of
        Left err -> Left err
        Right _ -> Right $ State [("value", DString idoc)] (path state) [] []

gameStart state (DNull) =
    case (checkGameStartNull) of
        Left err -> Left err
        Right _ -> Right $ State [("value", DNull)] (path state) [] []



checkGameStartList :: [Document] -> Either String [Document]
checkGameStartList [] = Right []
checkGameStartList ((DInteger i):xs) =
    if i < 0 then Left "error1" else checkGameStartList xs
checkGameStartList ((DNull):xs) = checkGameStartList xs
checkGameStartList ((DMap _):xs) = checkGameStartList xs
checkGameStartList ((DList _):xs) = checkGameStartList xs
checkGameStartList ((DString _):xs) = checkGameStartList xs

checkGameStartInteger :: Int -> Either String Int
--checkGameStartInteger i = Right i
checkGameStartInteger i = if i < 0 then Left "error" else Right i

checkGameStartString :: String -> Either String String
checkGameStartString s = Right s

checkGameStartNull :: Either String ()
checkGameStartNull = Right ()

-- this function checks 
checkGameStart :: [(String, Document)] -> Either String [(String, Document)]
checkGameStart [] = Right []
checkGameStart ((_, DMap list):xs) = 
    case (checkGameStart list) of
        Left err -> Left err
        Right _ -> checkGameStart xs
checkGameStart ((_, DInteger i):xs) =
    if i < 0 then Left "error!" else checkGameStart xs
checkGameStart ((_, DNull):xs) = checkGameStart xs
checkGameStart ((_, DList list):xs) = -- we need to check does it contain the DInteger array, which does not contain negative numbers
    if (checkGameStartDIntegerList list) then checkGameStart xs else Left "error"
checkGameStart ((_, DString _):xs) = checkGameStart xs

checkGameStartDIntegerList :: [Document] -> Bool
checkGameStartDIntegerList [] = True
checkGameStartDIntegerList ((DInteger i):xs) = if i < 0 then False else checkGameStartDIntegerList xs
checkGameStartDIntegerList ((DNull):xs) = checkGameStartDIntegerList xs
checkGameStartDIntegerList ((DMap _):xs) = checkGameStartDIntegerList xs
checkGameStartDIntegerList ((DList _):xs) = checkGameStartDIntegerList xs
checkGameStartDIntegerList ((DString _):xs) = checkGameStartDIntegerList xs



instance ToDocument Check where
    toDocument :: Check -> Document
    toDocument (Check coords) = 
        DMap [("coords", DList (map toDocument coords))] 

-- lets create ToDocument instance, but this should be only one instance, not two

instance ToDocument Coord where
    toDocument :: Coord -> Document
    toDocument (Coord col row) = 
        DMap [("col", DInteger col), ("row", DInteger row)]


renderDocument :: Document -> String
renderDocument (DList []) = "[]"
renderDocument (DMap []) = "{}"
renderDocument (DList list) = "---\n" ++ renderDocumentDList list 0
renderDocument (DMap list) = "---\n" ++ renderDocumentDMap list 0
renderDocument (DInteger value) = show value
renderDocument (DString value) = -- it should be seperated with singly quoted string
    "'" ++ value ++ "'"
renderDocument (DNull) = "null"

renderDocumentDList :: [Document] -> Int -> String -- list first element:
renderDocumentDList [] _ = ""
renderDocumentDList ((DInteger i):xs) depth = "- " ++ (show i) ++ "\n" ++ renderDocumentDList2 xs depth
renderDocumentDList ((DString s):xs) depth = "- " ++ "'" ++ s ++ "'" ++ "\n" ++ renderDocumentDList2 xs depth
renderDocumentDList ((DNull):xs) depth = "- null\n" ++ renderDocumentDList2 xs depth
renderDocumentDList ((DList []):xs) depth = "- []\n" ++ renderDocumentDList2 xs depth
renderDocumentDList ((DList s):xs) depth = "- " ++ renderDocumentDList s (depth + 1)  ++ renderDocumentDList2 xs depth
renderDocumentDList ((DMap []):xs) depth = "- {}\n" ++ renderDocumentDList2 xs depth
renderDocumentDList ((DMap s):xs) depth = "- " ++ renderDocumentDMap s (depth + 1)  ++ renderDocumentDList2 xs depth

renderDocumentDList2 :: [Document] -> Int -> String -- list later elements:
renderDocumentDList2 [] _ = ""
renderDocumentDList2 ((DInteger i):xs) depth = (replicate (depth * 2) ' ') ++ "- " ++ (show i) ++ "\n" ++ renderDocumentDList2 xs depth
renderDocumentDList2 ((DString s):xs) depth = (replicate (depth * 2) ' ') ++ "- " ++ "'" ++ s ++ "'" ++ "\n" ++ renderDocumentDList2 xs depth
renderDocumentDList2 ((DNull):xs) depth = (replicate (depth * 2) ' ') ++ "- null\n" ++ renderDocumentDList2 xs depth
renderDocumentDList2 ((DList []):xs) depth = (replicate (depth * 2) ' ') ++ "- []\n" ++ renderDocumentDList2 xs depth
renderDocumentDList2 ((DList s):xs) depth = (replicate (depth * 2) ' ') ++ "- " ++ renderDocumentDList s (depth + 1)  ++ renderDocumentDList2 xs depth
renderDocumentDList2 ((DMap []):xs) depth = (replicate (depth * 2) ' ') ++ "- {}\n" ++ renderDocumentDList2 xs depth
renderDocumentDList2 ((DMap s):xs) depth = (replicate (depth * 2) ' ') ++"- " ++ renderDocumentDMap s (depth + 1)  ++ renderDocumentDList2 xs depth

-- if it recieves: DMap []
-- then it should return: " - []\n"


renderDocumentDMap :: [(String, Document)] -> Int -> String -- Dmap first element:
renderDocumentDMap [] _ = ""
renderDocumentDMap ((key, DInteger i):xs) depth =  key ++ (if key=="" then "" else ": ") ++ (show i) ++ "\n" ++ renderDocumentDMap2 xs depth
renderDocumentDMap ((key, DString s):xs) depth = key ++ (if key=="" then "" else ": ") ++ if s=="" then "''\n" ++ renderDocumentDMap2 xs depth else "'" ++ s ++ "'" ++ "\n" ++ renderDocumentDMap2 xs depth
renderDocumentDMap ((key, DNull):xs) depth = key ++ (if key=="" then "null\n" else ": null\n") ++ renderDocumentDMap2 xs depth
renderDocumentDMap ((key, DList []):xs) depth = key ++ (if key=="" then "" else ": ") ++ "[]\n" ++ renderDocumentDMap2 xs depth
renderDocumentDMap ((key, DList s):xs) depth = key ++ (if key=="" then "\n" else ":\n") ++ renderDocumentDList2 s (depth) ++ renderDocumentDMap2 xs depth
renderDocumentDMap ((key, DMap []):xs) depth = key ++ (if key=="" then "" else ": ") ++ "{}\n" ++ renderDocumentDMap2 xs depth
renderDocumentDMap ((key, DMap s):xs) depth = key ++ (if key=="" then "\n" else ":\n") ++ renderDocumentDMap2 s (depth + 1) ++ renderDocumentDMap2 xs depth

renderDocumentDMap2 :: [(String, Document)] -> Int -> String -- Dmap later elements:
renderDocumentDMap2 [] _ = ""
renderDocumentDMap2 ((key, DInteger i):xs) depth = (replicate (depth * 2) ' ') ++ (if key=="" then "" else key ++ ": ") ++ (show i) ++ "\n" ++ renderDocumentDMap2 xs depth
renderDocumentDMap2 ((key, DString s):xs) depth = (replicate (depth * 2) ' ') ++ (if key=="" then "" else key ++ ": ") ++ if s=="" then "''\n"  ++ renderDocumentDMap2 xs depth else "'" ++ s ++ "'" ++ "\n" ++ renderDocumentDMap2 xs depth
renderDocumentDMap2 ((key, DNull):xs) depth = (replicate (depth * 2) ' ') ++ (if key=="" then "null\n" else key ++ ": null\n") ++ renderDocumentDMap2 xs depth
renderDocumentDMap2 ((key, DList []):xs) depth = (replicate (depth * 2) ' ') ++ (if key=="" then "" else key ++ ": ") ++ "[]\n" ++ renderDocumentDMap2 xs depth
renderDocumentDMap2 ((key, DList s):xs) depth = (replicate (depth * 2) ' ') ++ (if key=="" then "\n" else key ++ ":\n") ++ renderDocumentDList2 s (depth) ++ renderDocumentDMap2 xs depth
renderDocumentDMap2 ((key, DMap []):xs) depth = (replicate (depth * 2) ' ') ++ (if key=="" then "" else key ++ ": ") ++ "{}\n" ++ renderDocumentDMap2 xs depth
renderDocumentDMap2 ((key, DMap s):xs) depth = (replicate (depth * 2) ' ') ++ (if key=="" then "\n" else key ++ ":\n") ++ renderDocumentDMap2 s (depth + 1) ++ renderDocumentDMap2 xs depth
