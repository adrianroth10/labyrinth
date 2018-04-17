{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Test.HUnit
--import World
--import System.Exit
--
--main :: IO ()
--main = do
--      cs@(Counts _ _ errs fails) <- runTestTT tests
--      putStrLn (showCounts cs)
--      if errs > 0 || fails > 0
--          then exitFailure
--          else exitSuccess
--
--
--tests = TestList [TestLabel "parseMap" testparseMap]
--                    --TestLabel "parseEvents", testparseEvents,
--                    --TestLabel "parsePlayer", testparsePlayer,
--                    --TestLabel "parseTiles", testparseTiles]
--
--  --testing map parsing
--testparseMap = TestCase $ do
--    i <- readFile "map.json"
--    assertEqual "Just" parseMapOut (parseWorld i)
--
--parseMapOut = Just [(Map 1, MapContent (3, [Start, Free 1, Free 2,
--                                            Wall 1, Wall 1, Wall 1,
--                                            Event 1, Event 1, Event 1]))]

main ::
