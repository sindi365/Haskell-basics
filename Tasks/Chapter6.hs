-- Chapter 6: Practical and Homework Tasks

-- 1. Recursive factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

main1 :: IO ()
main1 = print (factorial 5) -- Output: 120

-- 2. Recursive Fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main2 :: IO ()
main2 = print (fibonacci 7) -- Output: 13

-- 3. Sum of list using foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0

main3 :: IO ()
main3 = print (sumList [1,2,3,4,5]) -- Output: 15

-- 4. Product of list using foldl
productList :: [Int] -> Int
productList = foldl (*) 1

main4 :: IO ()
main4 = print (productList [1,2,3,4]) -- Output: 24

-- 5. Reverse list using recursion
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

main5 :: IO ()
main5 = print (reverseList [1,2,3,4]) -- Output: [4,3,2,1]

-- 6. zipWith' function
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

main6 :: IO ()
main6 = print (zipWith' (+) [1,2,3] [4,5,6]) -- Output: [5,7,9]

-- 7. takeWhile' function
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x       = x : takeWhile' p xs
    | otherwise = []

main7 :: IO ()
main7 = do
  print (takeWhile' (<3) [1,2,3,4,1,2,3,4]) -- Output: [1,2]
  print (takeWhile' (<9) [1,2,3])           -- Output: [1,2,3]
  print (takeWhile' (<0) [1,2,3])           -- Output: []

-- Master main to run all
main :: IO ()
main = do
  putStrLn "1. Factorial of 5:"

