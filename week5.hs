import Data.Text.Internal.Builder.Int.Digits (digits)
--USER DEFINED DATA TYPES - WEEK 5
--UDT 1
data Day= Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Ord,Eq,Show,Enum)

weekend :: Day -> Bool
weekend d = d==Fri || d==Sat || d==Sun

--UDT 2
data Shape = Circle Double |Rectangle Double Double|Square Double|Triangle Double Double
  deriving (Eq,Show)

area:: Shape -> Double
area (Circle r)=3.14*r*r
area (Rectangle l b)= l*b
area (Square s)=s*s
area (Triangle b h)= 0.5*b*h

instance Ord Shape where
  (<=) :: Shape -> Shape -> Bool
  s1<=s2 = area s1 <= area s2 

--Random code to extract digits of a number and back to int
extract ::Int -> [Int]
extract 0=[]
extract n = extract (div n 10) ++ [mod n 10]
-- *OR using foldr
digitsList :: Int -> [Int]
digitsList n = foldr (\x acc -> acc ++ [x]) [] (go n) where
  go 0 = []
  go m = mod m 10 : go (div m 10)

backtoint1 :: [Int] -> Int
backtoint1 [] = 0
backtoint1 (x:xs) =foldl (\y acc -> y*10 + acc) 0 (x:xs)

backtoint2 :: [Int] -> Int
backtoint2 [] = 0 
backtoint2 (x:xs) = go (reverse (x:xs)) where
  go [] = 0
  go (y:ys) = y + 10 * go ys

