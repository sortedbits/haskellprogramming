
-- implement enumFromTo, do not use ..

eftBool :: Bool -> Bool -> [Bool]
eftBool b1 b2
  | b1 < b2   = [b1, b2]
  | b1 == b2  = [b1]
  | otherwise = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd o1 o2
  | o1 < o2   = o1 : eftOrd (succ o1) o2
  | o1 == o2  = [o1]
  | otherwise = []
    
eftInt :: Int -> Int -> [Int]
eftInt i1 i2
  | i1 < i2   = i1 : eftInt (succ i1) i2
  | i1 == i2  = [i1]
  | otherwise = []

eftChar :: Char -> Char -> [Char]
eftChar c1 c2
  | c1 < c2   = c1 : eftChar (succ c1) c2
  | c1 == c2  = [c1]
  | otherwise = []

-- alternatively

eftPoly :: (Ord a, Enum a) => a -> a -> [a]
eftPoly x1 x2
  | x1 < x2   = x1 : eftPoly (succ x1) x2
  | x1 == x2  = [x1]
  | otherwise = []

eftBool' :: Bool -> Bool -> [Bool] 
eftBool' b1 b2 = eftPoly b1 b2

eftInt' :: Int -> Int -> [Int]
eftInt' i1 i2 = eftPoly i1 i2

eftChar' :: Char -> Char -> [Char]
eftChar' c1 c2 = eftPoly c1 c2
