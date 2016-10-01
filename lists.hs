
-- EnumFromTo

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


-- Thy Fearful Symmetry

-- implement string tokenizer using takeWhile and dropWhile

myWords :: [Char] -> [[Char]]
myWords []         = []
myWords (' ' : xs) = myWords xs
myWords xs         = (takeWhile notSpace xs) : (myWords $ dropWhile notSpace xs)
  where notSpace = (/= ' ')

myLines :: [Char] -> [[Char]]
myLines []         = []
myLines ('\n' : xs) = myLines xs
myLines xs         = (takeWhile notNl xs) : (myLines $ dropWhile notNl xs)
  where notNl= (/= '\n')

-- alternatively

tokenize :: Char -> [Char] -> [[Char]]
tokenize sep [] = []
tokenize sep xs
  | z == sep  = tokenize sep zs
  | otherwise = (takeWhile notSep xs) : (tokenize sep  $ dropWhile notSep xs)
  where
    (z : zs) = xs
    notSep =  (/= sep)


myWords' xs = tokenize ' ' xs

myLines' xs = tokenize '\n' xs


-- Comprehend Thy Lists

mySqr = [x^2 | x <- [1..5]]

c1 = [x | x <- mySqr, rem x 2 == 0]

c2 = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]

c3 = take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]


-- Square Cube
