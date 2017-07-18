module MapSpec where

import Test.HUnit
import Map

tests = TestList [TestLabel "tile" testtile,
                  TestLabel "prepend" testprepend,
                  TestLabel "content" testcontent,
                  TestLabel "matrixify" testmatrixify,
                  TestLabel "parseMap" testparseMap]

--testing tile
testtile = TestList [testtile1,
                     testtile2,
                     testtile3,
                     testtile4,
                     testtile5,
                     testtile6]
testtile1 = TestCase $ assertEqual "Free" Free (tile 0)
testtile2 = TestCase $ assertEqual "Start" Start (tile 1)
testtile3 = TestCase $ assertEqual "End" End (tile 2)
testtile4 = TestCase $ assertEqual "Wall" Wall (tile 3)
testtile5 = TestCase $ assertEqual "Event 1" (Event 1) (tile 4)
testtile6 = TestCase $ assertEqual "Event 2" (Event 2) (tile 5)

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
parseMapIn = "3\n3\n0 1 2\n3 4 5\n6 7 8"
parseMapOut = Just $ Map [[Free, Start, End],
                    [Wall, Event 1, Event 2],
                    [Event 3, Event 4, Event 5]]
testparseMap = TestCase $ assertEqual "parseMap"
                                       parseMapOut
                                       (parseMap parseMapIn)
