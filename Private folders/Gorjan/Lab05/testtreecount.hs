module Test where

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)
example1 = T 1 [T 2 [], T 3 [T 3 []],T 3 []]

countTree :: Tree a -> Int
countTree (T _ xs) = 1 + sum(map countTree xs)
--countTree (T _ xs) = 1 + (foldr (+) 0 (map countTree xs))

depth :: Tree a -> Int
depth (T _ []) = 0
depth (T _ ts) = foldl max 0 (map depth ts) + 1

collect :: Tree a -> [a]
collect (T a xs) = a : concat (map collect xs)

mapT :: (a->b) -> Tree a -> Tree b
mapT f (T x xs) = T (f x) (map (mapT f) xs)


------------------

foldT :: (a -> [b] -> b) -> Tree a -> b
foldT f (T x ts) = f x (map (foldT f) ts)

countTree' :: Tree a -> Int
countTree' t = foldT (\_ ns -> (sum ns) + 1) t

depth' :: Tree a -> Int
depth' = foldT (\_ ds -> if null ds then 0 else (maximum ds + 1))

collect' :: Tree a -> [a]
collect' = foldT (\x lists -> x : concat lists)

mapT' :: (a->b) -> Tree a -> Tree b
mapT' f t = foldT (\x ts -> T (f x) ts) t