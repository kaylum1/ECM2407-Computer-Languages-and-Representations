import Data.Char (digitToInt)
-- import Data.List (nub, sort)
import qualified Data.Set as Set
import Data.List

-- QUESTION 2.1 GENERATING THE TUPLES (N1, N2, N3, N4, N5)------------------
threeDigits :: [Int]
threeDigits = [123 .. 987]

-- function returns true if all digit are non-zero
allDigitsNonZero :: [Int] -> Bool
allDigitsNonZero digits = all (/= 0) (concatMap (map digitToInt . show) digits)


-- this function returns true if first digit n1 =/= n2
twoDigitsOverlap :: [Int] -> Bool
twoDigitsOverlap [n1, n2, _, _, _] = head (show n1) /= head (show n2)


generator2 :: [(String, String, String, String, String)]
generator2 = [(n1, n2, n3, n4, n5) 
  | n1 <- map show threeDigits
  , n2 <- map tail (permutations n1) 
  , n3 <- permutations n1, n4 <- map tail (permutations n1)
  , n5 <- permutations n1
  , allDigitsNonZero [read n1, read n2, read n3, read n4, read n5]
  , twoDigitsOverlap [read n1, read n2, read n3, read n4, read n5]]


x_generator2 :: Int
x_generator2 =
  length [t | t <- ts, t `elem` g]
  where
    g = generator2
    ts =
      [ ("123", "21", "123", "12", "123"),
        ("162", "26", "261", "12", "621"),
        ("219", "19", "912", "21", "291"),
        ("329", "92", "932", "32", "239"),
        ("439", "94", "394", "43", "394"),
        ("549", "95", "945", "95", "945"),
        ("568", "68", "586", "56", "586"),
        ("769", "67", "679", "97", "796"),
        ("879", "79", "897", "98", "789"),
        ("987", "79", "789", "79", "789")
      ]


-- QUESTION 2.2 GENERATING THE TUPLES WHERE UNDER VALUE 2000----------------

tester2 :: (String, String, String, String, String) -> Bool
tester2 (n1, n2, n3, n4, n5) =
  condition1 && condition2 && condition3
  where
    condition1 = read n1 - read n2 == read n3
    condition2 = read n3 - read n4 == read n5
    condition3 = read n1 + read n3 + read n5 < 2000


x_tester2 :: Int 
x_tester2 =
  length [t | t <- ts, tester2 t] 
  where
  ts =
    [ ("138","01","137","50","87")
    , ("143","01","142","52","90")
    , ("171","02","169","79","90") 
    , ("152","03","149","54","95") 
    , ("159","04","155","61","94") 
    , ("161","05","156","63","93") 
    , ("182","06","176","80","96")
    , ("151","07","144","57","87") 
    , ("165","08","157","64","93") 
    , ("174","09","165","71","94") ]


main :: IO ()
main = print (x_generator2)
--main = print (x_tester)
-- main = print ("starting", filter tester2 generator2)

