-- Matei Bianca - Larisa -> 322CB
module Tema1 (
        solveSimple,
        solveCosts
        ) where

import Data.Array
solveSimple :: (Int, [(Int, Int, Int)]) -> Maybe ([Int], Int)

first (a, b, c) = a
second (a, b, c) = b
third (a, b, c) = c

solveSimple (n, []) = Nothing
solveSimple (n, (a, b, c):xs) =
                                let bounds = ((1,1), (n,n))
                                    list = snd (n, (a, b, c):xs)

                                    distances = listArray bounds [ lungime list i j | (i, j) <- range bounds]
                                    cost = listArray ((0,1,1), (n,n,n)) [ findCost k i j | k <- [0..n], i <- [1..n], j <- [1..n]]
                                    path = listArray ((0,1,1), (n,n,n)) [ findPath k i j | k <- [0..n], i <- [1..n], j <- [1..n]]

                                    lungime list i j
                                      | list == [] = 10000
                                      | i == j = 0
                                      | i > j = 10000
                                      | i == first (head list) && j == second (head list) = third (head list)
                                      | otherwise = lungime (tail list) i j

                                    findCost k i j
                                      | k == 0 = distances ! (i, j)
                                      | otherwise = min (cost ! (k-1, i, j)) ((cost ! (k-1, i, k)) + (cost ! (k-1, k, j)))

                                    findPath k i j
                                      | k == 0 = []
                                      | cost ! (k-1, i, j) < (cost ! (k-1, i, k)) + (cost ! (k-1, k, j)) = path ! (k-1, i, j)
                                      | otherwise = (path ! (k-1, i, k)) ++ [k] ++ (path ! (k-1, k, j))

                                in if cost ! (n, 1, n) == 10000 then Nothing
                                  else Just(path ! (n, 1, n), cost ! (n, 1, n))

solveCosts :: (Int, Int, [Int], [(Int, Int, Int)]) -> Maybe ([(Int, Int)], Int)
solveCosts = undefined
