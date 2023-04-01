{-# OPTIONS_GHC -Wno-unused-imports #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat)

import Lib2 (renderDocument, gameStart, hint)
import Lib3 (parseDocument)
import Types (Document(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  fromYamlTests,
  gameStartTests,
  hintTests,
  properties])

properties :: TestTree
properties = testGroup "Properties" [golden, dogfood]

friendlyEncode :: Document -> String
friendlyEncode doc = cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions) doc)


golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
      \doc -> parseDocument (friendlyEncode doc) == Right doc
  ]

dogfood :: TestTree
dogfood = testGroup "Eating your own dogfood"
  [  
    testProperty "parseDocument (renderDocument doc) == doc" $
      \doc -> parseDocument (renderDocument doc) == Right doc
  ]

fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml"
  [   testCase "null" $
        parseDocument "null" @?= Right DNull
    , testCase "int" $
        parseDocument "5" @?= Right (DInteger 5) 
    , testCase "negInt" $
        parseDocument "-16" @?=Right (DInteger (-16)) 
    , testCase "string" $
        parseDocument "test" @?=Right (DString "test") 
    , testCase "empString" $
        parseDocument "" @?=Right (DString "") 
    , testCase "list string" $
        parseDocument "- string" @?=Right (DList [DString "string"])
    , testCase "list Int" $
        parseDocument "- 987" @?=Right (DList [DInteger 987])
    , testCase "list neg Int" $
        parseDocument "- -53" @?=Right (DList [DInteger (-53)])
    , testCase "list null" $
        parseDocument "- null" @?=Right (DList [DNull])
    , testCase "list of ints" $
        parseDocument listOfInts2 @?=Right (DList [DInteger 5,DInteger 6,DInteger (-7)])
    , testCase "listOflists" $
        parseDocument listOflists @?=Right (DList [DList [DString "test",DInteger 6,DList [DNull]],DInteger 4])
    , testCase "listOflists2" $
        parseDocument listOflists2 @?=Right (DList [DList [DInteger 4,DList [DInteger 5,DInteger 6],DList [DInteger 5]]])
    , testCase "dmap string" $
        parseDocument "test: string" @?=Right (DMap [("test",DString "string")])
    , testCase "dmap Int" $
        parseDocument "test: 5" @?=Right (DMap [("test",DInteger 5)])
    , testCase "dmap neg Int" $
        parseDocument "test: -1" @?=Right (DMap [("test",DInteger (-1))])
    , testCase "dmap null" $
        parseDocument "test: null" @?=Right (DMap [("test",DNull)])
    , testCase "dmap Of Dmap" $
        parseDocument dmapOfDmap @?=Right (DMap [("test",DMap [("test2",DMap [("test3",DInteger 4),("test3",DInteger 6)]),("test2",DString "string")]),("test2",DNull)])
    , testCase "dmap Of Lists" $
        parseDocument dmapOfLists @?=Right (DMap [("test4",DMap [("test5",DInteger 6),("test6",DList [DList [DInteger 4,DList [DInteger 5,DInteger 6],DList [DInteger 5]]])]),("test3",DNull),("test2",DNull)])
        
  ]

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "null"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "5"
    , testCase "list of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

listOfInts :: String
listOfInts = unlines [
      "---"
    , "- 5"
    , "- 6"
  ]

listOfInts2 :: String
listOfInts2 = unlines [
      "---"
    , "- 5"
    , "- 6"
    , "- -7"
  ]

listOflists :: String
listOflists = unlines [
      "---"
    , "- - test"
    , "  - 6"
    , "  - - null"
    , "- 4"
  ]

listOflists2 :: String
listOflists2 = unlines [
      "- - 4"
    , "  - - 5"
    , "    - 6"
    , "  - - 5"
  ]
  
dmapOfDmap :: String
dmapOfDmap = unlines [
      "---"
    , "test:"
    , "  test2:"
    , "    test3: 4"
    , "    test3: 6"
    , "  test2: string"
    , "test2: null"
  ]

dmapOfLists :: String
dmapOfLists = unlines [
    "test4:"
    ,"  test5: 6"
    ,"  test6:"
    ,"  - - 4"
    ,"    - - 5"
    ,"      - 6"
    ,"    - - 5"
    ,"test3: null"
    ,"test2: null"
  ]

gameStartTests :: TestTree
gameStartTests = testGroup "Test start document" []

hintTests :: TestTree
hintTests = testGroup "Test hint document" []