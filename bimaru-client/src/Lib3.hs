{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Lib3(hint, Lib3.gameStart, parseDocument, GameStart, Hint) where

import Types ( Document(..), FromDocument, fromDocument, Coord(..) )
import Lib1 (State(..), gameStart)
import GHC.Generics ((:+:)(R1))
import qualified Data.Char
import Data.Char
import Control.Applicative
import Data.Yaml as Y ( encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat)
import Data.String.Conversions

-- I suddently hide all the wanrings, how to see them? I want to see them
-- lets analyse reduntant? is it warning? or error? or just a message?
-- now its time to fill the requirements for 3rd task
--These are the requirements:
-- 1.Now project does not build out of the box, it requires you to implement some FromDocument instance(s). Implement the instances. Note, that signatures of "hint" and "gameStart" have changed (but the way they work remains the same)
-- 2.Function "parseDocument" converts yaml (String) to Document. This function must be capable to parse any yaml representable by Document ADT, not only Documents used during the game. Left is used to report parse errors. The parse errors must be meaningful.
-- 3.Implement tests
-- 4.Create BNF (extend BNF if not enough) for yaml grammar your parser can accept. It can be just a txt file in your repo.
-- 5.The parser can be a monadic or applicative, it can use methods of Functor but using monad transformers or state monad (applicative) in any form is forbidden.

instance FromDocument GameStart where
    fromDocument :: Document -> Either String GameStart
-- it recieves this input: DMap [("number_of_hints", DInteger 10),("occupied_cols", DList [DInteger 3,DInteger 3,DInteger 0,DInteger 0,DInteger 3,DInteger 0,DInteger 5,DInteger 0,DInteger 4,DInteger 2]),("occupied_rows",DList [DInteger 1,DInteger 1,DInteger 4,DInteger 2,DInteger 2,DInteger 2,DInteger 2,DInteger 2,DInteger 0,DInteger 4]),("game_setup_id",DString "9cfda689-fee4-4a87-b82b-49dd379f3cad")]
    fromDocument initDocument = 
        case initDocument of
            DMap doc -> Right (GameStart (getInteger (getDoc doc "number_of_hints")) (getIntegerList (getDoc doc "occupied_cols")) (getIntegerList (getDoc doc "occupied_rows")) (getString (getDoc doc "game_setup_id")))
            _ -> Left "Not a DMap, invalidit initialization document"
        where
            getDoc :: [(String, Document)] -> String -> Document
            getDoc doc key = snd (head (filter (\x -> fst x == key) doc))
            getInteger :: Document -> Int
            getInteger (DInteger x) = x
            getInteger _ = 0
            getIntegerList :: Document -> [Int]
            getIntegerList (DList x) = map getInteger x
            getIntegerList _ = []
            getString :: Document -> String
            getString (DString x) = x
            getString _ = []
            
{-
(DMap [("number_of_hints", DInteger 10),
("occupied_cols", DList [DInteger 3, DInteger 3, DInteger 0, DInteger 0, DInteger 3, DInteger 0, DInteger 5, DInteger 0, DInteger 4, DInteger 2]),
("occupied_rows", DList [DInteger 1, DInteger 1, DInteger 4, DInteger 2, DInteger 2, DInteger 2, DInteger 2, DInteger 2, DInteger 0, DInteger 4]), 
("game_setup_id", DString "9cfda689-fee4-4a87-b82b-49dd379f3cad")])
-}

instance FromDocument Hint where
    fromDocument :: Document -> Either String Hint
    fromDocument (DMap [hints]) = 
        case snd hints of
            DMap hintDocument -> Right
                Hint
                { 
                    Lib3.retrievedHints = getHints (DMap hintDocument)
                }
            _ -> Left "Wrong hint document retrieved!"
    fromDocument _ = Left "Wrong initialisation format retrieved!"

getHints :: Document -> [Coord]
getHints (DMap ((_, DMap ((_, DInteger _col) : (_, DInteger _row) : _)) : xs : _)) = Coord {col = _col, row = _row} : getHints (snd xs)
getHints DNull = []
getHints _ = []

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument -- GameStart is a type
data GameStart = GameStart { 
    numberOfHints :: Int,
    occupiedCols :: [Int],
    occupiedRows :: [Int],
    gameSetupId :: String
} deriving (Show, Eq)

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed

gameStart :: State -> GameStart -> State
gameStart (State doc path doc2 doc3) gameStart = State (doc ++ [("number_of_hints", DInteger (numberOfHints gameStart)), ("occupied_cols", DList (map DInteger (occupiedCols gameStart))), ("occupied_rows", DList (map DInteger (occupiedRows gameStart))), ("game_setup_id", DString (gameSetupId gameStart))]) path doc2 doc3


-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data Hint = Hint {
    retrievedHints :: [Coord]
} deriving Show


-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
hint :: State -> Hint -> State
-- we are saving hints in doc2 field
hint (State doc path _ doc3) hint = -- overwrite the existing data
    State doc path ([("coords", DMap (map (\x -> ("head", DMap [("col", DInteger (col x)), ("row", DInteger (row x))])) (retrievedHints hint)))]) doc3

-- ANARCHY STARTS HERE
-- DO NOT CHANGE ANYTHING BELOW THIS LINE
-- UNLESS YOU ARE ABSOLUTELY SURE WHAT YOU ARE DOING
newtype Parser a = Parser { runParser :: String -> [(a, String)] }

result :: a -> Parser a
result v = Parser (\inp -> [(v, inp)])
-- runParser (result 3) "abc" = [(3, "abc")]

zero :: Parser a
zero = Parser (\_ -> []) -- zero does this: it takes a string and returns an empty list
-- runParser zero "abc" = []

sat :: (Char -> Bool) -> Parser Char
sat p = Parser parseIfSat
    where
        parseIfSat [] = []
        parseIfSat (x:xs) = if p x then [(x, xs)] else []

instance Monad Parser where -- we are getting error: no
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p>>= f = Parser $ \inp ->
        concat [runParser (f v) inp' | (v, inp') <- runParser p inp]
    return = result

instance Functor Parser where
    fmap f p = p >>= \v -> return (f v)

instance Applicative Parser where
    pure = return
    pf <*> pv = pf >>= \f -> pv >>= \v -> return (f v)

char :: Char -> Parser Char
char x = sat (== x)

-- runParser (char 'a') "abc" = [('a', "bc")]

digit :: Parser Char
digit = sat Data.Char.isDigit

class (Monad m) => MonadPlus m where 
    pzero :: m a

instance Lib3.MonadPlus Parser where
    pzero = zero

or' :: Parser a -> Parser a -> Parser a
-- pattern match
p `or'` q = Parser $ \inp -> case runParser p inp of
    [] -> runParser q inp
    [(v, out)] -> [(v, out)]
    (x:xs) -> (x:xs)
-- runParser (char 'a' `or'` char 'b') "abc" = [('a', "bc")]
-- runParser (char 'a' `or'` char 'b') "bbc" = [('b', "bc")]
-- runParser (char 'a' `or'` char 'b') "cbc" = []

instance Alternative Parser where
    empty = zero
    (<|>) = or'

-- runParser alphanum "abc" = [('a', "bc")]
-- runParser letter "abc" = [('a', "bc")]
-- runParser letter "123" = []

string :: String -> Parser String
string "" = result ""
string (x:xs) =
    char x >> string xs >> result (x:xs)
-- runParser (string "abc") "abc" = [("abc", "")]

many' :: Parser a -> Parser [a]
many' p = do
    x <- p -- apply p to the input
    xs <- many' p -- apply many' p to the input
    return (x:xs) -- return the result of applying p to the input
    <|> return [] -- or return an empty list
-- runParser (many' digit) "123abc" = [("123", "abc")]
-- runParser (many' $ char 'a') "aaaabc" = [("aaaa", "bc")]

many1 :: Parser a -> Parser [a]
many1 p = do
    x <- p
    xs <- many' p
    return (x:xs)


nat :: Parser Int
nat = do
    many1 digit >>= eval
    where
        eval xs = result $ foldl1 op [digitToInt x | x <- xs]
        op x y = 10 * x + y
-- runParser nat "123" = [(123, "")]
-- runParser nat "abc" = []

spaces :: Parser ()
spaces = void $ many' $ sat isSpace

space :: Parser () -- delete one space or new line character
space = void $ char ' ' <|> char '\n'


endl :: Parser ()
endl = void $ many' $ char '\n'

void :: Parser a -> Parser ()
void p = p >> result ()
-- runParser spaces "  abc" = [((),"abc")]

token :: Parser a -> Parser a
token p = do
    spaces
    x <- p
    spaces
    return x

allowWsToken :: Parser a -> Parser a
allowWsToken p = do
    x <- p
    spaces
    return x
    

prefixToken :: Parser a -> Parser a
prefixToken p = do
    spaces
    x <- p
    return x



fullWsToken :: Parser a -> Parser a
fullWsToken p = do
    x <- p
    return x

charToken :: Char -> Parser Char -- it gets a char and returns a parser that parses that char
charToken = token <$> char -- token <$> char is the same as token (char c)
-- Console: runParser (charToken 'a') "a" = [('a', "")]
-- Console: runParser (charToken 'a') " a" = [('a', "")]
-- Console: runParser (charToken 'a') " a " = [('a', "")]
-- Console: runParser (charToken 'a') " a b" = [('a', "b")]

-- runParser document "abc" = []
document :: Parser Document
document = do
    spaces
    x <- value
    spaces
    return x

value :: Parser Document
value = do
    _ <- charToken '{' -- it checks for a '{' and then parses a map
    x <- map' -- it parses a map
    _ <- charToken '}' -- it checks for a '}' and then returns the map
    return x -- it returns the map
    <|> do
        _ <- charToken '['
        x <- list'
        _ <- charToken ']'
        return x
    <|> do
-- use a functtion to parse a string
        x <- string'
        return x
    <|> do
        x <- integer
        return x
    <|> do
        x <- null'
        return x

map' :: Parser Document
map' = do
--    x <- pair -- 
 --   xs <- many' (charToken ',' >> pair)
  --  return $ DMap (x:xs)
-- so, check is it empty or not
    x <- pair
    xs <- many' (charToken ',' >> pair)
    return $ DMap (x:xs)
    <|> do
        return $ DMap []

pair :: Parser (String, Document)
pair = do
    key <- fromDocumentToString <$> string'
    _ <- charToken ':'
    value <- value
    return (key, value)

list' :: Parser Document
list' = do
 --   x <- value
 --   xs <- many' (charToken ',' >> value)
 --   return $ DList (x:xs)
 -- check is it empty or not
    x <- value
    xs <- many' (charToken ',' >> value)
    return $ DList (x:xs)
    <|> do
        return $ DList []

string' :: Parser Document
string' = do
    _ <- charPrefixToken '"'
    x <- many (sat (/= '"'))
    _ <- charPrefixToken '"'
    return $ DString x

parseDocument :: String -> Either String Document
parseDocument s = 
    case runParser document (fromYamlStringToJsonString s) of
        [(x, "")] -> Right x
        _ -> Left "Error in parsing: parseDocument, invalid input"

-- fromYamlStringToJsonString "abc" = "\"abc\""
fromYamlStringToJsonString :: String -> String
fromYamlStringToJsonString s =
    case runParser toJson s of
        [(x, "")] -> x
        _ -> error "Error in yaml to json string conversion"


fromDocumentToString :: Document -> String
fromDocumentToString (DString s) = s
fromDocumentToString _ = error "Error in parsing: fromDocumentToString, invalid input"


integer :: Parser Document
-- it should be able to parse number, negative or not negative
integer = do
    _ <- charPrefixToken '-'
    y <- nat
    return $ DInteger (-y)
    <|> do
        x <- nat
        return $ DInteger x

null' :: Parser Document
null' = do
    _ <- stringToken "null"
    return DNull

--runParser (stringToken "abc") "  abc" = [("abc", "")]
stringToken :: String -> Parser String
stringToken = token . string

allowWsStringToken :: String -> Parser String
allowWsStringToken = allowWsToken . string

charFullWsToken :: Char -> Parser Char
charFullWsToken = fullWsToken <$> char

fullWsStringToken :: String -> Parser String
fullWsStringToken = fullWsToken . string

prefixStringToken :: String -> Parser String
prefixStringToken = prefixToken . string


toJson :: Parser String
toJson = do
    -- input can start with ---, remote it, if it exists
    _ <- prefixStringToken "---"
    endl
    toJson
    <|> do
    x <- value'
    spaces
    return $ x

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse a (x:xs) = x : a : intersperse a xs

parseYamlDstring :: Parser String
-- return a string until it sees a new line or ":" symbol, remove them
parseYamlDstring = 
    do
        x <- many (noneOf ":\n")
        return ("\"" ++ x ++ "\"")

        
noneOf :: String -> Parser Char
noneOf s = sat (`Lib3.notElem` s)

notElem :: Eq a => a -> [a] -> Bool
notElem _ [] = True
notElem x (y:ys) = x /= y && Lib3.notElem x ys

    -- console: runParser (charPrefixToken 'a') " a b" = [("a", " b")]
charPrefixToken :: Char -> Parser Char
charPrefixToken = prefixToken <$> char 

-- console: runParser parseYamlPossibleNegativeInteger "-1" = [(-1, "")]
parseYamlPossibleNegativeInteger :: Parser String
{-parseYamlPossibleNegativeInteger = do
    x <- charPrefixToken '-'
    y <- nat
    return $ x : show y
    <|> do
        x <- nat
        return $ show x -}
parseYamlPossibleNegativeInteger = do
    x <- charPrefixToken '-'
    y <- natToEndl
    return $ x : show y
    <|> do
        x <- natToEndl
        return $ show x

natToEndl :: Parser Int
natToEndl = 
    let temp = noneOf ("\n") in
    do
        x <- many1 temp
        if(Lib3.isDigit x) then return (read x) else pzero

isDigit :: String -> Bool
-- check is it digit and this should not contain a "-" character
isDigit str = -- if it contains a -, then return false
    do 
        if (head str == '-') then False else if (head str == ' ') then False else
            case (reads str) :: [(Double, String)] of
                [(_, "")] -> True
                _ -> False

parseYamlDnull :: Parser String
parseYamlDnull = do
    _ <- stringToken "null"
    return "null"

-- console: runParser parseYamlEmptyDmap "{}" = [({}, "")]
parseYamlEmptyDmap :: Parser String
parseYamlEmptyDmap = do
    _ <- fullWsStringToken "{}"
    return "{}"

-- console: runParser parseYamlEmptyDlist "[]" = [([], "")]
parseYamlEmptyDlist :: Parser String
parseYamlEmptyDlist = do
    _ <- fullWsStringToken "[]"
    return "[]"

parseYamlDoubleQuotedString :: Parser String
parseYamlDoubleQuotedString = do
    _ <- charToken '"'
    x <- many' (sat (/= '"'))
    _ <- charPrefixToken '"'
    return $ "\"" ++ x ++ "\""

value' :: Parser String
value' = do
    x <- parseYamlEmptyDmap
    return $ x
    <|> do
    x <- parseYamlEmptyDlist
    return $ x
    <|> do
    x <- parseYamlPossibleNegativeInteger
    return $ x
    <|> do
    x <- parseYamlDlist 0
    return $ x
    <|> do
    x <- parseYamlDmap 0
    return $ x
    <|> do
    x <- parseYamlDnull
    return $ x
    <|> do
    x <- parseYamlSinglyQuotedString
    return $ x
    <|> do
    x <- parseYamlDoubleQuotedString
    return $ x
    <|> do
    x <- parseYamlDstring
    return $ x

-- console: runParser parseYamlDmap "\"abc\": 123" = [("abc: 123", "")]
-- console: runParser parseYamlDmap "\"game_setup_id\": 93cfda689-fee4-4a87-b82b-49dd379f3cad\n"
parseYamlDmap ::Int -> Parser String
parseYamlDmap depth = do
    x <- parseYamlDmap' depth
    return $ "{" ++ x ++ "}"

-- test parseYamlDmap' in console: runParser parseYamlDmap' "\"abc\": 123" = [("abc: 123", "")]
parseYamlDmap' :: Int -> Parser String
parseYamlDmap' depth = do
    x <- parseYamlDmapElement depth
    --xs <- many' (parseYamlDmapElement)
    xs <- many' (do
        _ <- fullWsStringToken (buildPreliminaryTokenForExpansion (depth))
        parseYamlDmapElement depth)
    return $ Prelude.concat (intersperse "," (x:xs))


-- test parseYamlDmapElement in console: runParser parseYamlDmapElement "\"abc\": 123" = [("abc: 123", "")]
parseYamlDmapElement :: Int -> Parser String
parseYamlDmapElement depth = do
    {-x <- parseYamlDstring
    _ <- charFullWsToken ':'
    spaces
    y <- parseYamlDmapElement' (depth + 1)
    endl
    return $ x ++ ":" ++ y-}
    -- the previous code works good, but we need to check keys with:
    -- singly quoted string, double quoted strig, parseYamlDstring
    x <- parseYamlSinglyQuotedString
    _ <- charFullWsToken ':'
    spaces
    y <- parseYamlDmapElement' (depth + 1)
    endl
    return $ x ++ ":" ++ y
    <|> do
    x <- parseYamlDoubleQuotedString
    _ <- charFullWsToken ':'
    spaces
    y <- parseYamlDmapElement' (depth + 1)
    endl
    return $ x ++ ":" ++ y
    <|> do
    x <- parseYamlDstring
    _ <- charFullWsToken ':'
    spaces
    y <- parseYamlDmapElement' (depth + 1)
    endl
    return $ x ++ ":" ++ y


-- test parseYamlDmapElement' in console: runParser parseYamlDmapElement' "123" = [("123", "")]
parseYamlDmapElement' ::Int -> Parser String
parseYamlDmapElement' depth =  do
    x <- parseYamlEmptyDmap
    return $ x
    <|> do
    x <- parseYamlEmptyDlist
    return $ x 
    <|> do
    x <- parseYamlPossibleNegativeInteger
    return $ x
    <|> do
    x <- parseYamlDlist (depth -1)
    return $ x
    <|> do
        x <- parseYamlDmap depth
        return $ x
    <|> do
        x <- parseYamlDnull
        return $ x
    <|> do
    x <- parseYamlSinglyQuotedString
    return $ x
    <|> do
    x <- parseYamlDoubleQuotedString
    return $ x
    <|> do
    x <- parseYamlDstring
    return $ x
parseYamlDlist :: Int -> Parser String
parseYamlDlist depth = do
    x <- parseYamlDlist' depth
    return $ "[" ++ x ++ "]"

-- console: runParser parseYamlDlist "- 1 - 2 - 3" = [("1,2,3", "")]
parseYamlDlist' :: Int -> Parser String
parseYamlDlist' depth = do
    x <- parseYamlDlistElement depth
    xs <- many' (do
        _ <- allowWsStringToken (buildPreliminaryTokenForExpansion (depth))
        parseYamlDlistElement depth)
    return $ concat (intersperse "," (x:xs))

parseYamlDlistElement :: Int -> Parser String
parseYamlDlistElement depth = do
    _ <- charPrefixToken '-'
    space
    x <- parseYamlDlistElement' (depth + 1)
    endl
    return $ x 

parseYamlSinglyQuotedString :: Parser String
parseYamlSinglyQuotedString = do
    _ <- charPrefixToken '\''
    x <- many' (sat (/= '\''))
    _ <- charPrefixToken '\''
    return $ "\"" ++ x ++ "\""

parseYamlDlistElement' :: Int -> Parser String
parseYamlDlistElement' depth = do
    x <- parseYamlEmptyDmap
    return $ x
    <|> do
    x <- parseYamlEmptyDlist
    return $ x 
    <|> do
    x <- parseYamlPossibleNegativeInteger
    return $ x
    <|> do
    x <- parseYamlDlist (depth)
    return $ x
    <|> do
        x <- parseYamlDmap depth
        return $ x
    <|> do
        x <- parseYamlDnull
        return $ x
    <|> do
    x <- parseYamlSinglyQuotedString
    return $ x
    <|> do
    x <- parseYamlDoubleQuotedString
    return $ x
    <|> do
    x <- parseYamlDstring
    return $ x

buildPreliminaryTokenForExpansion :: Int -> String
buildPreliminaryTokenForExpansion times = concat (replicate times "  ")

------------------Results:
--fromStringToDocument :: String -> Document - is out final function
-- fromStringToDocument "123" = DInteger 123
-- fromStringToDocument "null" = DNull
-- fromStringToDocument "abc" = DString "abc"
-- fromStringToDocument "- 1\n- 2\n- 3" = DList [DInteger 1,DInteger 2,DInteger 3]
-- fromStringToDocument "abc: 123" = DMap [("abc",DInteger 123)]
-- fromStringToDocument "abc: - 1\n- 2\n- 3" = DMap [("abc",DList [DInteger 1,DInteger 2,DInteger 3])]
-- fromStringToDocument "abc: 123\ndef: 456" = DMap [("abc",DInteger 123),("def",DInteger 456)]
-- fromStringToDocument "game_setup_id: 9cfda689-fee4-4a87-b82b-49dd379f3cad\nnumber_of_hints: 10\n occupied_rows:\n- 1\n- 4\n- 2\n- 2\n- 2\n- 2\n- 2\n- 4\n- 4\noccupied_cols:\n- 3\n- 3\n- 0\n- 0\n- 3\n- 0\n- 5\n- 0\n- 4\n- 2\n"
-- and ALL the nested tests... OK
{-
DMap [("kko",DMap [("jTk",DMap [("ofb",DString "c n")])]),("vIp",DMap [("AtV",DMap [("bUu",DMap [("tl",DMap [("E",DList [DInteger (-2)]),("Slc",DString "4n ")])]),("xGe",DList [DMap [("I",DList [DList [DList [DInteger (-1),DList [DString ""]],DList [DString "",DList [DString " i",DInteger 3,DInteger 2],DInteger 1]]]),("Nw",DInteger 2),("fsh",DList [DInteger (-3),DString "",DString " 3"])],DInteger 3]),("zAb",DList [DMap [("r",DString "")],DList [DMap [("NO",DList []),("e",DMap [("AhP",DString ""),("M",DList [DInteger (-3),DString "tq",DList []]),("duX",DList [DList [DString " ",DString "g ",DList []]])])]]])]),("WPf",DList [DInteger (-1)])])] 




DMap [("YkIeYy",DString ""),("mX",DString "Fp 6g62 TQ9WEG"),("ujM",DString "NY4 1d9 HH G"),("vEOZby",DString "Bl4 ")]
-- server render output:"mX: Fp 6g62 TQ9WEG\nujM: NY4 1d9 HH G\nYkIeYy: ''\nvEOZby: 'Bl4 '\n"
-- our  render output:"---\nYkIeYy: ''\nmX: 'Fp 6g62 TQ9WEG'\nujM: 'NY4 1d9 HH G'\nvEOZby: 'Bl4 '\n"
-}

-- Data.Yaml.encode (toJSON (DString ""))