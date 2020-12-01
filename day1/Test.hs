import Data.Maybe (mapMaybe)
import Text.Read ( readMaybe )

import Test.Tasty
import Test.Tasty.HUnit

import Day1 (triProd, cartProd, star, star2)

main :: IO ()
main = do
    file <- readFile "input.dat"
    let numbers = mapMaybe readMaybe (lines file)
    defaultMain (day1Tests numbers)

day1Tests :: [Int] -> TestTree
day1Tests numbers = testGroup "Day 1 tests" [
        testCase "Star 1" $ star (cartProd numbers numbers) @?= Just 988771,
        testCase "Star 2" $ star2 (triProd numbers) @?= Just 171933104
    ]