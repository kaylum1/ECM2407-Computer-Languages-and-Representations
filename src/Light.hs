import Data.Char (digitToInt)
import Data.List (nub)

-- QUESTION 1.1  MAKING A GENERATOR X TO LIST ALL TUPLES
generator1 :: [(Int, Int, Int, Int)]
generator1 = [(hr, mn, dy, mt) | hr <- [0 .. 23], mn <- [0 .. 59], mt <- [1 .. 12], dy <- [1 .. daysInMonth mt]]
  where
    daysInMonth mt
      | mt `elem` [1, 3, 5, 7, 8, 10, 12] = 31
      | mt `elem` [4, 6, 9, 11] = 30
      | otherwise = 28


-- Test the function using x_tester1 and x_generator1
x_generator1 :: Int
x_generator1 =
  length [t | t <- ts, t `elem` g]
  where
    g = generator1
    ts =
      [ (2, 15, 14, 11),
        (4, 31, 27, 9),
        (6, 47, 10, 8),
        (9, 3, 23, 6),
        (11, 19, 6, 5),
        (13, 35, 19, 3),
        (15, 51, 2, 2),
        (18, 6, 16, 12),
        (20, 22, 29, 10),
        (22, 38, 11, 9)
      ]

-- QUESTION 1.2 MAKING A FUNCTION THAT RETUNS CORRECT TUPLES

-- (helper) to count all segments---------------------------------------------------
digitToValue :: Int -> Int
digitToValue 0 = 6
digitToValue 1 = 2
digitToValue 2 = 5
digitToValue 3 = 5
digitToValue 4 = 4
digitToValue 5 = 5
digitToValue 6 = 6
digitToValue 7 = 3
digitToValue 8 = 7
digitToValue 9 = 6
digitToValue _ = 0

-- create tuple to be able to turn into segments
digits :: Int -> [Int]
digits n
  | n < 10 = [0, n]
  | otherwise = map digitToInt (show n)

-- adds all the segments
sumValues :: (Int, Int, Int, Int) -> Int
sumValues (hr, mn, dy, mt) = sum (map digitToValue (digits hr)) + sum (map digitToValue (digits mn)) + sum (map digitToValue (digits dy)) + sum (map digitToValue (digits mt))




-- Function to check if a number is prime-------------------------------------------
prime :: Int -> Bool
prime n = not . factorisable 2 $ n
factorisable :: Int -> Int -> Bool
factorisable f n
  | f * f <= n = n `mod` f == 0 || factorisable (f + 1) n
  | otherwise = False



-- funtion to check  where display digits are all differnet---------------------------
flatten :: (Int, Int, Int, Int) -> [Int]
flatten (a, b, c, d) = [a, b, c, d]

myNub :: Eq a => [a] -> [a]
myNub [] = []
myNub (x : xs) = x : myNub (filter (/= x) xs)

allUnique :: Eq a => [a] -> Bool
allUnique xs = length xs == length (myNub xs)

allUniqueInTuple :: (Int, Int, Int, Int) -> Bool
allUniqueInTuple = allUnique . flatten



-- where magic bring a and b toegther ------------------------------------------------
magic :: (Int, Int, Int, Int) -> Bool
magic (hr, mn, dy, mt) =
  allUniqueInTuple (hr, mn, dy, mt)
    && prime (sumValues (hr, mn, dy, mt))




-- checks for question 1.2 tuples --------------------------------------------------
-- adds minute to tuple

addMin :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
addMin (hr, mn, dy, mt)
  | mn == 59 = (hr + 1, 00, dy, mt) -- increments hour by 1 and makes minutes 00 if given tuple's minutes are 59
  | mn == 59 && hr == 23 = (00, 00, dy + 1, mt) -- increments day by one is hour is 23 and minutes is 59
  | otherwise = (hr, mn + 1, dy, mt) -- increments minutes by 1 if minutes < 59





tester1 :: (Int, Int, Int, Int) -> Bool
tester1 (hr, mn, dy, mt) =
  magic (hr, mn, dy, mt) -- tuple is magic
    && magic (hr, mn, dy + 1, mt) -- tuple exactly one day later is magic
    && sumValues (addMin (hr, mn, dy + 1, mt)) == (sumValues (hr, mn, dy, mt) + sumValues (hr, mn, dy + 1, mt)) `div` 2



x_tester1 :: Int
x_tester1 =
  length [t | t <- ts, tester1 t]
  where
    ts =
      [ (6, 59, 17, 24),
        (6, 59, 17, 34),
        (6, 59, 27, 14),
        (6, 59, 27, 41),
        (8, 59, 12, 46),
        (16, 59, 7, 24),
        (16, 59, 7, 42),
        (16, 59, 7, 43),
        (16, 59, 27, 40),
        (18, 59, 2, 46)
      ]

main :: IO () 
main =
  -- print (x_generator1)
  print (x_tester1)
  -- print (filter tester1 generator1)

