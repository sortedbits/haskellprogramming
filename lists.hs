import Data.Char
import Data.Bool

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

myCube = [y^3 | y <- [1..5]]

sqrCube = [(x, y) | x <- mySqr, y <- myCube]

sqrCube' = [(x, y) | x <- mySqr, y <- myCube, x <50, y < 50]

lenSqrCube' = length sqrCube'


-- Bottom Madness
-- bottom?

bm1 = [x^y | x <- [1..5], y <- [2, undefined]]
-- y
bm2 = take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
-- n
bm3 = sum [1, undefined, 3]
-- y
bm4 = length [1, 2, undefined]
-- n
bm5 = length $ [1, 2, 3] ++ undefined
-- y
bm6 = take 1 $ filter even [1, 2, 3, undefined]
-- n
bm7 = take 1 $ filter even [1, 3, undefined]
-- y
bm8 = take 1 $ filter odd [1, 3, undefined]
-- n
bm9 = take 2 $ filter odd [1, 3, undefined]
-- n
bm10 = take 3 $ filter odd [1, 3, undefined]
-- y


-- nf, whnf?

-- [1, 2, 3, 4, 5]
-- nf (and whnf)
-- 1 : 2 : 3 : 4 : _
-- whnf
-- enumFromTo 1 10
-- none
-- length [1, 2, 3, 4, 5]
-- none
-- sum (enumFromTo 1 10)
-- none
-- ['a'..'m'] ++ ['n'..'z']
-- none
-- (_, 'b')
-- whnf


-- Data.Char

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (x : xs) = toUpper x : xs
    
allUpper :: [Char] -> [Char]
allUpper [] = []
allUpper (x : xs) = toUpper x : allUpper xs

firstUpper :: [Char] -> Char
firstUpper xs = toUpper $ head xs 

firstUpper' :: [Char] -> Char
firstUpper' xs = toUpper . head $ xs

firstUpperPF = toUpper . head


-- More Bottoms
-- bottom?

b11 = take 1 $ map (+1) [undefined, 2, 3]
-- y
b12 = take 1 $ map (+1) [1, undefined, 3]
-- n
b13 = take 2 $ map (+1) [1, undefined, 3]
-- y

itIsMystery :: [Char] -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs
-- itIsMystery "hello" = [False, True, False, False, True]

r1 = map (^2) [1..10]
-- [1,4,9,16,25,36,49,64,81,100]

r2 = map minimum [[1..10], [10..20], [20..30]]
-- [1,10,20]

r3 = map sum [[1..5], [1..5], [1..5]]
-- [15,15,15]

-- function bool not in Data.Bool 
bool :: a -> a -> Bool -> a
bool x y p = if p then y else x

myAbs :: (Num a, Ord a) => [a] -> [a]
myAbs xs = map (\x -> bool x (-x) (x < 0)) xs 


-- Filtering

multOfThree :: (Integral a) => [a] -> [a]
multOfThree xs = [x | x <- xs,  (rem x 3) == 0]

lenMultOfThreePF :: (Integral a) => [a] -> Int
lenMultOfThreePF = length . multOfThree

noArticles :: [Char] -> [[Char]]
noArticles = filter (\x -> x /= "a" && x /= "an") . words


-- Zipping

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys 

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys

myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (\x y -> (x, y))


-- My Standard Functions

-- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = if x == False then False else myAnd xs

-- direct recursion, using (&&)
myAnd' :: [Bool] -> Bool
myAnd' []     = True
myAnd' (x:xs) = x && myAnd' xs

myOr :: [Bool] -> Bool
myOr []       = False
myOr (x : xs) = if x then True else myOr xs

myOr' :: [Bool] -> Bool
myOr' []       = False
myOr' (x : xs) = x ||  myOr' xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f []       = False
myAny f (x : xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x []       = False
myElem x (y : ys) = (x == y) || myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x ys = myAny (== x) ys

myReverse :: [a] -> [a]
myReverse []       = []
myReverse (x : xs) = myReverse xs ++ (x : [])

squish :: [[a]] -> [a]
squish xs = go xs []
  where
    go :: [[a]] -> [a] -> [a]
    go [] zs       = zs
    go (x : xs) zs = x ++ go xs []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f []       = []
squishMap f (x : xs) = (f x) ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain xss = squishMap id xss 

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [] = error "empty list"
myMaximumBy f (x : xs) = go f xs x 
  where go :: (a -> a -> Ordering) -> [a] -> a -> a
        go f [] z       = z
        go f (x : xs) z
          | f x z == GT = go f xs x
          | otherwise   = go f xs z

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [] = error "empty list"
myMinimumBy f (x : xs) = go f xs x
  where go :: (a -> a -> Ordering) -> [a] -> a -> a
        go f [] z       = z
        go f (x : xs) z
          | f x z == LT = go f xs x
          | otherwise   = go f xs z 

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs
