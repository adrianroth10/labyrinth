module MapSpec where

import Test.HUnit
import Map

tests = TestList [TestLabel "tile" testtile,
                  TestLabel "prepend" testprepend,
                  TestLabel "content" testcontent,
                  TestLabel "matrixify" testmatrixify,
                  TestLabel "parseMap" testparseMap]

--testing tile
testtile = TestList [testtileH "Free" 0 Free ,
                     testtileH "Start" 1 Start ,
                     testtileH "End" 2 End ,
                     testtileH "Wall" 3 Wall ,
                     testtileH "Event 1" 4 (Event 1),
                     testtileH "Event 2" 5 (Event 2)]
testtileH ref dataIn dataOut =
                        TestCase $ assertEqual ref dataOut (tile dataIn)

--testing prepend
prependIn = (3, [Free, End, Free, Event 1])
prependOut = [Wall, Free, End, Free, Event 1]
testprepend = TestCase $ assertEqual "prepend"
                                     prependOut
                                     (prepend prependIn)

--testing content
contentIn = "0 1 2 3\n 10 9 8 \n 5 \n 3 4 5"
contentOut = Just ([Free, Start, End, Wall, Event 7, Event 6, Event 5,
                     Event 2, Wall, Event 1, Event 2], "")
testcontent = TestCase $ assertEqual "content"
                                     contentOut
                                     (content contentIn)
--testing matrixify
matrixifyIn = Just (((2, 2), [Free, Start, End, Wall]), "")
matrixifyOut = Just $ Map [[Free, Start], [End, Wall]]
testmatrixify = TestCase $ assertEqual "matrixify just"
                                       matrixifyOut
                                       (matrixify matrixifyIn)

--testing parseMap
testparseMap = TestList
                    [testparseMapH "just 1" parseMapIn1 parseMapOut1,
                     testparseMapH "just 2" parseMapIn2 parseMapOut2,
                     testparseMapH "nothing 1" parseMapIn3 parseMapOut3,
                     testparseMapH "nothing 2" parseMapIn4 parseMapOut4]
testparseMapH ref dataIn dataOut =
                    TestCase $ assertEqual ref dataOut (parseMap dataIn)
parseMapIn1 = "3\n3\n0 1 2\n3 4 5\n6 7 8"
parseMapOut1 = Just $ Map [[Free, Start, End],
                    [Wall, Event 1, Event 2],
                    [Event 3, Event 4, Event 5]]
parseMapIn2 = "3\n3\n0 1 2\n3 4 5\n6 7 8\n Hej hej hallo"
parseMapOut2 = Just $ Map [[Free, Start, End],
                    [Wall, Event 1, Event 2],
                    [Event 3, Event 4, Event 5]]
parseMapIn3 = "3\n3\n0 1 i\n3 4 5\n6 7 8"
parseMapOut3 = Nothing
parseMapIn4 = "3\n4\n0 1 2\n3 4 5\n6 7 8"
parseMapOut4 = Nothing
