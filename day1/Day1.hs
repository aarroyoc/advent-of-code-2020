module Day1 where

import Data.Maybe (mapMaybe)
import Text.Read ( readMaybe )

main :: IO ()
main = do
    file <- readFile "input.dat"
    let numbers = mapMaybe readMaybe (lines file)
    let pairs = cartProd numbers numbers
    let result = star pairs
    printResult result
    let tris = triProd numbers
    let result2 = star2 tris
    printResult result2


printResult :: Maybe Int -> IO ()
printResult Nothing = putStrLn "No hay solución"
printResult (Just x) = putStrLn ("Solución: " ++ show x)

cartProd :: [Int] -> [Int] -> [(Int, Int)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

star :: [(Int, Int)] -> Maybe Int
star [] = Nothing
star ((x, y):xs) = if x + y == 2020 then Just (x * y) else star xs

triProd :: [Int] -> [(Int, Int, Int)]
triProd xs = [(x, y, z) | x <- xs, y <- xs, z <- xs]

star2 :: [(Int, Int, Int)] -> Maybe Int
star2 [] = Nothing
star2 ((x, y, z):xs) = if x + y + z == 2020 then Just (x * y * z) else star2 xs