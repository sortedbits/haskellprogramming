
-- myFoldr and myFoldl definitions

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z []       = z
myFoldr f z (x : xs) = f x (myFoldr f z xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f z []     = z
myFoldl f z (x : xs) = myFoldl f (f z x) xs

-- f = (\x y -> concat ["(", x, "+", y, ")"])
-- myFoldr f "0" (map show [1..5])
