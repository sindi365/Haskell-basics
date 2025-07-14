-- Chapter 7: Practical and Homework Tasks

-- HC7T1 & HC7T2: Color with Eq, Ord, and Enum instances
data Color = Red | Green | Blue deriving (Show, Eq, Enum, Ord)

-- HC7T3: compareValues function with Eq and Ord
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y
  | x >= y    = x
  | otherwise = y

-- HC7T4: Shape with Show and Read
data Shape = Circle Double | Rectangle Double Double
    deriving (Show, Read)

-- HC7T5: squareArea function with Num constraint
squareArea :: Num a => a -> a
squareArea side = side * side

-- Homework Q4: Type Signatures
f1 :: (Fractional a, Show a) => a -> a -> String -> String
f1 x y z = show (x / y) ++ z

f2 :: (Eq a, Bounded a, Enum a) => a -> a
f2 x = if x == maxBound then minBound else succ x

-- Main to run everything
main :: IO ()
main = do
    -- HC7T1: Eq Example
    putStrLn "HC7T1: Color Eq Examples"
    print (Red == Red)   -- True
    print (Red == Blue)  -- False

    -- HC7T2: Ord Example
    putStrLn "\nHC7T2: Color Ord Examples"
    print (Red < Green)  -- True
    print (Green < Blue) -- True
    print (Blue < Red)   -- False

    -- HC7T3: compareValues
    putStrLn "\nHC7T3: compareValues Examples"
    print (compareValues 5 10)      -- 10
    print (compareValues 'a' 'z')   -- 'z'

    -- HC7T4: Shape Show and Read
    putStrLn "\nHC7T4: Shape Show and Read"
    let circle = Circle 5.0
    let rectangle = Rectangle 3.0 4.0
    print circle
    print rectangle
    print (read "Circle 7.5" :: Shape)
    print (read "Rectangle 3.0 6.0" :: Shape)

    -- HC7T5: squareArea
    putStrLn "\nHC7T5: squareArea Examples"
    print (squareArea 4)    -- 16
    print (squareArea 5.5)  -- 30.25

    -- Homework Q3: Enum
    putStrLn "\nHomework Q3: Enum Examples"
    print (succ 'A')          -- 'B'
    print (pred 5)            -- 4
    print ([1..5])            -- [1,2,3,4,5]
    print ([Red .. Blue])     -- [Red,Green,Blue]

    -- Homework Q4: Type Signatures
    putStrLn "\nHomework Q4: f1 and f2 Examples"
    print (f1 6 3 " result")   -- "2.0 result"
    print (f2 (5 :: Int))      -- 6
    print (f2 (maxBound :: Int)) -- minBound
