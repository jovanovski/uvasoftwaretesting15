module Btree

where
import Data.List
import Test.QuickCheck

main :: IO String
main = return "compiled"

data Btree a = Leaf | B a (Btree a) (Btree a) deriving (Eq,Show)

-- QuickCheck tree generator
instance Arbitrary a => Arbitrary (Btree a) where
    arbitrary = sized gtree
 
gtree 0 = return Leaf
gtree n = do
        x <- arbitrary 
        t1 <- subtree
        t2 <- subtree
        return (B x t1 t2)
        where subtree = gtree (n `div` 2)


--- Examples ----
-----------------
example1 = makeT [5,6,7,1,2,3,13,12,10,9,4,11]
example2 = makeT [13,12,10,11]

dict1 = makeT [("ma", "vesna"), ("mb", "vesna"), ("jo", "gorjan"), ("od", "pero"), ("tr", "trpo")]
-----------------


-- Make a tree from a list
makeT :: Ord a => [a] -> Btree a
makeT = foldr insertT Leaf

-- Insert a value into a tree
insertT :: Ord a => a -> Btree a -> Btree a
insertT x Leaf = B x Leaf Leaf
insertT x (B y left right) 
  | x < y     = B y (insertT x left) right
  | otherwise = B y left (insertT x right) 

-- Depth of tree
depth :: Btree a -> Integer
depth Leaf = 0
depth (B _ left right) = (maximum (map depth [left, right])) + 1

-- Weight of tree
weight ::  Btree a -> Int
weight Leaf = 0
weight (B _ left right) = weight left + weight right + 1

-- Leaf count of tree
leafCount ::  Btree a -> Int
leafCount Leaf = 1
leafCount (B _ left right) = leafCount left + leafCount right

-- Find a node by value, and return the node's tree structure
findNode :: Ord a => a -> Btree a -> Btree a
findNode x (B y Leaf Leaf) = if x==y then B y Leaf Leaf else error "not found"
findNode x (B y left right) | x == y = B y left right
							| x < y = findNode x left
							| otherwise = findNode x right

-- Find minimum value from in given binary tree
findMinVal :: Ord a => Btree a -> a
findMinVal (B y Leaf _) = y
findMinVal (B _ left _) = findMinVal left

-- Find maximum value in given binary tree
findMaxVal :: Ord a => Btree a -> a
findMaxVal (B y _ Leaf) = y
findMaxVal (B _ _ right) = findMaxVal right

-- Replace one value with another in given binary tree
replaceVal :: Ord a => a -> a -> Btree a -> Btree a
replaceVal search replace (B y Leaf Leaf) = if search == y then B replace Leaf Leaf else error "not found"
replaceVal search replace (B y left right) | search == y = B replace left right
							| search < y = B y (replaceVal search replace left) right
							| otherwise = B y left (replaceVal search replace right) 

-- Delete node from binary tree with given value
deleteNode :: Ord a => a -> Btree a -> Btree a
deleteNode search (B x left@(B y _ _) Leaf) = if search == y then B x Leaf Leaf 
												else if search < x then B x (deleteNode search left) Leaf
												else error "not found"
deleteNode search (B x Leaf right@(B y _ _)) = if search == y then B x Leaf Leaf 
												else if search > x then B x Leaf (deleteNode search right)
												else error "not found"
deleteNode search (B x left@(B y _ _) right@(B z _ _)) = if search == y then B x Leaf right else if search == z then B x left Leaf else if search < x then B x (deleteNode search left) right else B x left (deleteNode search right)

-- This is the magic function, remove a node from a binary tree with a given value
removeNode :: Ord a => a -> Btree a -> Btree a
removeNode x tree = let 
						node@(B y left right) = findNode x tree
						minNodeRight = if right /= Leaf then findMinVal right else y
						maxNodeLeft = if left /= Leaf then findMaxVal left else y
					in
						if right == Leaf && left /= Leaf then replaceVal x maxNodeLeft (removeNode maxNodeLeft tree)
							else if right /= Leaf && left == Leaf then replaceVal x minNodeRight (removeNode minNodeRight tree)
							else if left == Leaf && right == Leaf then deleteNode minNodeRight tree 
							else replaceVal y minNodeRight (removeNode minNodeRight tree)

-- Map over a tree
mapT :: (a -> b) -> Btree a -> Btree b
mapT f Leaf = Leaf
mapT f (B x left right) = B (f x) (mapT f left) (mapT f right)

-- Fold over a tree
foldT :: (a -> [b] -> b) -> Btree a -> b
foldT f (B x Leaf Leaf) = f x []
foldT f (B x Leaf right) = f x (map (foldT f) [right])
foldT f (B x left Leaf) = f x (map (foldT f) [left])
foldT f (B x left right) = f x (map (foldT f) [left, right])


-- Collect nodes inorder
inOrder :: Btree a -> [a]
inOrder Leaf = []
inOrder (B x left right) = inOrder left ++ [x] ++ inOrder right

-- Collect nodes preorder
preOrder :: Btree a -> [a]
preOrder Leaf = []
preOrder (B x left right) = [x] ++ (preOrder left) ++ (preOrder right)

-- Collect nodes postorder
postOrder :: Btree a -> [a]
postOrder Leaf = []
postOrder (B x left right) = (postOrder left) ++ (postOrder right) ++ [x]

-- Collect values from speficied level in tree  (left to right)
getFromLevel :: Integer -> Integer -> Btree a -> [a]
getFromLevel _ _ Leaf = []
getFromLevel target start (B x left right) = if target == start then [x] else (getFromLevel target (start+1) left) ++ (getFromLevel target (start+1) right)

-- Collect values from speficied level in tree  (right to left)
getFromLevel' :: Integer -> Integer -> Btree a -> [a]
getFromLevel' _ _ Leaf = []
getFromLevel' target start (B x left right) = if target == start then [x] else (getFromLevel' target (start+1) right) ++ (getFromLevel' target (start+1) left)

-- Collect top down (left to right)
topDown :: Btree a -> [a]
topDown tree = concat (map (\x -> getFromLevel x 0 tree) [0..(depth tree)])

-- Collect bottom up (left to right)
bottomUp :: Btree a -> [a]
bottomUp tree = concat (map (\x -> getFromLevel x 0 tree) (reverse [0..(depth tree)]))

-- Collect nodes rev inorder
inOrderRev :: Btree a -> [a]
inOrderRev Leaf = []
inOrderRev (B x left right) = inOrderRev right ++ [x] ++ inOrderRev left

-- Sort list using tree
sortInOrder :: Ord a => [a] -> [a]
sortInOrder xs = inOrder $ makeT xs

-- Test sort list
treeProperty :: Eq a => Btree a -> Bool
treeProperty t = inOrder t == reverse (inOrderRev t)


type Dict = Btree (String,String) 

lemma, info :: (String,String) -> String
lemma (x,_) = x
info  (_,y) = y 

-- Dictionary lookup
lookUp :: String -> Dict -> [String]
lookUp _ Leaf = []
lookUp search (B item left right) 	| search == lemma item = [(info item)] 
									| search < lemma item = lookUp search left
									| otherwise = lookUp search right
-- Dictionary insert
insertLemma :: (String,String) -> Dict -> Dict
insertLemma item Leaf = B item Leaf Leaf
insertLemma item (B x left right) 	| (lemma item) < (lemma x) = B x (insertLemma item left) right
									| otherwise = B x left (insertLemma item right)

-- Dictionary is ordered
ordered :: Dict -> Bool
ordered dic = 	let 
					inord = inOrder dic
				in 
					inord == sort (inord)