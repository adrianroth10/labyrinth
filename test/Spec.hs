import Test.HUnit
import qualified MapSpec 

tests = TestList [TestLabel "MapTests" MapSpec.tests]

main :: IO Counts
main = runTestTT tests
