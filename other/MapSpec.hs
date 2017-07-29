module MapSpec (tests) where

import Test.HUnit
import Map

tests = TestList [TestLabel "parseMap" testparseMap]

--testing map parsing
testparseMap = TestList
                   [testparseMapH "just 1" parseMapIn1 parseMapOut1,
                    testparseMapH "just 2" parseMapIn2 parseMapOut2,
                    testparseMapH "nothing 1" parseMapIn3 parseMapOut3,
                    testparseMapH "nothing 2" parseMapIn4 parseMapOut4]
testparseMapH ref dataIn dataOut =
                   TestCase $ assertEqual ref dataOut (parseMap dataIn)

parseMapIn1 = "3\n3\n0 1 2\n3 4 5\n6 7 8"
parseMapOut1 = Just $ [[Free, Start, End],
                       [Wall, Event 1, Event 2],
                       [Event 3, Event 4, Event 5]]
parseMapIn2 = "3\n3\n0 1 2\n3 4 5\n6 7 8\n Hej hej hallo"
parseMapOut2 = Just $ [[Free, Start, End],
                       [Wall, Event 1, Event 2],
                       [Event 3, Event 4, Event 5]]
parseMapIn3 = "3\n3\n0 1 i\n3 4 5\n6 7 8"
parseMapOut3 = Nothing
parseMapIn4 = "3\n4\n0 1 2\n3 4 5\n6 7 8"
parseMapOut4 = Nothing
