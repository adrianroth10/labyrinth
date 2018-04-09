module Main where

import Test.HUnit
import World

main :: IO ()
main = do
  runTestTT tests
  return ()


tests = TestList [TestLabel "parseMap" testparseMap]
                  --TestLabel "parseEvents", testparseEvents,
                  --TestLabel "parsePlayer", testparsePlayer,
                  --TestLabel "parseTiles", testparseTiles]

--testing map parsing
testparseMap = TestList
                   [testparseMapH "just 1" parseMapIn1 parseMapOut1,
                    testparseMapH "nothing 1" parseMapIn2 parseMapOut2,
                    testparseMapH "nothing 2" parseMapIn3 parseMapOut3]
testparseMapH ref dataIn dataOut =
                   TestCase $ assertEqual ref dataOut (parseWorld dataIn)

parseMapIn1 = "Map 1\n3\n0 10 11\n20 20 20\n30 30 30\nEND"
parseMapOut1 = Just $ [(Map 1, MapContent (3, [Start, Free 1, Free 2,
                                               Wall 1, Wall 1, Wall 1,
                                               Event 1, Event 1, Event 1]))]
parseMapIn2 = "Map 1\n3\n10 10 11\n20 20 20\n30 30 30\nEND"
parseMapOut2 = Nothing
parseMapIn3 = "Map 1\n3\n10\n20 20 20\n30 30 30\nEND"
parseMapOut3 = Nothing
