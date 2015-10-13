


module Project where

import X07
import SVG07

___ = error "nezadefinovana funkcia"

--inicializacia typov--

data Bt a = E
          | Nd a (Bt a) (Bt a)
        deriving (Show,Eq)

data Dir = L | R deriving (Show)  
type Directions a = [(Dir, a, Bt a)]                
{-  Smer, Root, Sibling-}                
                
type Zipper a = (Bt a, Directions a)
type Zips a = [Zipper a]
type Forest a = [Bt a]
{- Aktualny srom, (smer, sibling) -}

--------------------------------------------------------------------------------
------------------------------realizacia ZIPPER---------------------------------
--------------------------------------------------------------------------------

--MOVE LEFT--
left :: (Bt a, Directions a) -> (Bt a, Directions a) -- Zipper a -> Zipper a
left (Nd v l r, ds) = (l, ((L, v, r):ds) )

--MOVE RIGHT
right :: (Bt a, Directions a) -> (Bt a, Directions a)  
right (Nd v l r, ds) = (r, ((R, v, l):ds) )

--MOVE UP--
up :: (Bt a, Directions a) -> (Bt a, Directions a)
up (t, []) = (t,[])
up (t, (L, v, r):ds) = (Nd v t r, ds)
up (t, (R, v, l):ds) = (Nd v l t, ds)

goUp :: Zips a -> Zips a
goUp ((t,[]):zs) = ((t, []):zs)
goUp ((t,ds):zs) = goUp ((up (t, ds)):(t,ds):zs)

goUpChange :: Ord a => a -> a-> Zips a -> Zips a
goUpChange x y ((t,[]):zs) = ((t, []):zs)
goUpChange x y ((E,ds):zs) = goUpChange x y ((up (E,ds)):((E,ds):zs))
goUpChange x y (z@((Nd v l r),ds):zs) 
            | x == v = goUp ((up ((Nd y l r), ds)):(((Nd y l r),ds):zs))
            | otherwise = goUpChange x y ((up z):(z:zs))
            
--REBUILD TREE--
rebZip :: (Bt a, Directions a) -> (Bt a, Directions a) 
rebZip (t,[]) = (t,[])  
rebZip z@(E, ds) = rebZip (up z)
rebZip z@(Nd v l r, ds) = rebZip (up z)
--rebZip (E,[(L, 'c', E)])
--rebZip (Nd 'x' E E,[(L,'b',Nd 'd' E E),(L,'a',Nd 'c' (Nd 'e' (Nd 'g' E E) E) (Nd 'f' E E))])

--GET TREE--
getTree :: (Bt a, Directions a) -> Bt a 
getTree (t,[]) = t   
getTree z@(t, ds) = getTree (rebZip z)


--GET ROOT--
getRoot :: (Bt a, Directions a) -> a -> a 
getRoot (E,ds) val = val  
getRoot (Nd v _ _, ds) val = v
--getRoot (Nd 'x' E E,[(L,'b',Nd 'd' E E),(L,'a',Nd 'c' (Nd 'e' (Nd 'g' E E) E) (Nd 'f' E E))]) 'L'


--GET TREE AND CURRENT ROOT--
getTreeRoot :: Zipper a -> a -> (Bt a, a)
getTreeRoot z@(E, ds) val = (getTree z, val) 
getTreeRoot z@(Nd v l r, ds) val = (getTree z, v) 
--getTreeRoot (Nd 'x' E E,[(L,'b',Nd 'd' E E),(L,'a',Nd 'c' (Nd 'e' (Nd 'g' E E) E) (Nd 'f' E E))]) 'L

--------------------------------------------------------------------------------
----------------------------------ZIPS------------------------------------------
--------------------------------------------------------------------------------
--MAKE FOREST FROM ZIPS--
mkForest :: Zips a -> Forest a -> Forest a  
mkForest [] f = f
mkForest (z:zs) f = mkForest zs ((getTree z):f) --  mkForest zs ((getTree $ rebZip z):f)

--mkForest [(Nd 'a' E E,[]), (Nd 'd' E E,[(L,'k',E)])]

--MAKE ZIPS FROM ZIPPER--
mkZips :: Zipper a -> Zips a
mkZips z = z:[]

--ADD ZIPPER TO ZIPS--
addToZips :: Zipper a -> Zips a -> Zips a
addToZips z zs = (z:zs) 

countZips :: Zips a -> Int
countZips [] = 0
countZips (z:zs) = 1 + countZips zs

--------------------------------------------------------------------------------
---------------------------------TREE METHODS-----------------------------------
--------------------------------------------------------------------------------

--ADD ROOT TO EMPTY TREE--
addRoot :: a -> Zipper a
addRoot x = ((Nd x E E), [])

--GET ROOT--
root :: Bt a -> a -> a
root E val = val
root (Nd v l r) val = v

--CHANGE ROOT--
changeRoot :: a -> Zipper a -> Zipper a
changeRoot x (E, ds) = (Nd x E E, ds)
changeRoot x (Nd v l r, ds) = (Nd x l r, ds)

--IS EMPTY--
isEmpty :: Zipper a -> Bool
isEmpty (E,[]) = True
isEmpty (E, ds) = False
isEmpty ((Nd v l r), _) = False

--IS LEAF--
isLeaf :: Bt a -> Bool
isLeaf E = True
isLeaf (Nd _ _ _) = False

--ADD NODE--
addNode :: Ord a => a -> Zipper a -> Zipper a
addNode x z@(E,ds) = rebZip (changeRoot x z)
addNode x z@(Nd v l r, ds)  
                | x == v = rebZip ((Nd v l r),ds)
	              | x  < v = addNode x (left z)
                | x  > v = addNode x (right z)
                
--ADD NODE IN ZIPS AND CREATE LIST OF PREVIOUS TREES - HISTORY--                
addNodeZ :: Ord a => a -> Zips a -> Zips a
addNodeZ x ((E,ds):zs) = rebalance (goUp [changeRoot x (E,ds)])++((E,ds):zs)
addNodeZ x (z@(Nd v l r, ds):zs)  
                | x == v = goUp (z:zs)
	            | x  < v = rebalance $ addNodeZ x ((left z):z:zs)
                | x  > v = rebalance $ addNodeZ x ((right z):z:zs)                
--addNodeZ 1 [(Nd 10 (Nd 4 E E) (Nd 8 E E),[])]
--doTest $ addNodeZ 2 [(Nd 10 (Nd 4 (Nd 2 E E) E) (Nd 18 E E),[])]

--CREATE FROM LIST, REMEMBER PREVIOUS TREES--
fillTree :: Ord a => [a] -> Zips a -> Zips a
fillTree [] zs = zs
fillTree (x:xs) [] = fillTree xs ((addRoot x):[])
fillTree (x:xs) ((t,ds):zs) = fillTree xs (addNodeZ x ((t,ds):zs))

{--}
------------------------------------DELETE NODE---------------------------------
-- DELETE NODE --
deleteB :: (Ord a) => a -> Zips a -> Zips a
deleteB x ((E,[]):zs) = ((E,[]):zs)
deleteB x ((E,ds):zs) = deleteB x (goUp ((E,ds):zs))
deleteB x zps@(((Nd v l r),ds):zs)  
	| x == v = rebalance $ goUp $ (deleteRoot zps)
	| x  < v = deleteB x [left ((Nd v l r),ds)]++zps
	| x  > v = deleteB x [right ((Nd v l r),ds)]++zps
--deleteRoot [( (Nd 8 (Nd 5 E (Nd 3 (Nd 2 E E) E) )E), [])]


deleteRoot :: (Ord a) => Zips a -> Zips a 
--deleteRoot (((Nd v E r),ds):zs) = [(r,ds)]++[right ((Nd v E r),ds)]++(((Nd v E r),ds):zs)
deleteRoot (z@((Nd v l E),ds):zs) = (goUpChange v v' [(t,dsCurr)])++(((Nd v' l' r'),ds'):zs')
    where
         ((t,dsCurr):((Nd v' l' r'),ds'):zs') = maxElement [left  z]++(z:zs)
deleteRoot (z@((Nd v l r),ds):zs) = (goUpChange v v' [(t,dsCurr)])++(((Nd v' l' r'),ds'):zs')
	where 
		((t,dsCurr):((Nd v' l' r'),ds'):zs') = minElement [right z]++(z:zs)


--doTest $ deleteB 20 [((Nd 10 (Nd 2 E E) (Nd 20 E (Nd 15 (Nd 11 E E)E) )),[])]
--doTest $ deleteB 20 [(Nd 20 E (Nd 15 (Nd 11 E E) (Nd 18 E E)),[(R,10,Nd 2 E E)])]++[]

{-!!moznost rozdelit rightMinElement na 2 fcie!!-}

--FIND MIN FROM RIGHT BRANCH--
minElement :: (Ord a) => Zips a -> Zips a
minElement (z@((Nd v E E),ds):zs) = [(E,ds)]++(z:zs)  --najde a vymaze
minElement (z@((Nd v E r),ds):zs) = [(r,ds)]++(z:zs)
minElement (z@((Nd v l E),ds):zs) = minElement [left z]++(z:zs)
minElement (z@((Nd v l r),ds):zs) = minElement [left z]++(z:zs)

--rightMinElement [((Nd 8 (Nd 5 (Nd 3 E E) E) E),[])]
--let zz = [(Nd 5 E (Nd 3 (Nd 2 (Nd 1 E E) E) E),[(L,8,E)]), ((Nd 8 (Nd 5 E (Nd 3 (Nd 2 (Nd 1 E E) E) E) )E),[])]
--let zz1 = [(Nd 5 (Nd 3 (Nd 2 E E) E) E,[(R,8,E)]), ((Nd 8 E (Nd 5 (Nd 3 (Nd 2 E E)E) E)),[])]


--FIND MAX FROM LEFT BRANCH--
maxElement :: (Ord a) => Zips a -> Zips a
maxElement (z@((Nd v E E),ds):zs) = [(E,ds)]++(z:zs)
maxElement (z@((Nd v l E),ds):zs) = [(l,ds)]++(z:zs)
maxElement (z@((Nd v E r),ds):zs) = maxElement [right z]++(z:zs)
maxElement (z@((Nd v l r),ds):zs) = maxElement [right z]++(z:zs)

--------------------------------------------------------------------------------

------------------------------REBELANCE TREE------------------------------------
rebalance :: Zips a -> Zips a
rebalance ((E,[]):zs) = ((E,[]):zs) 
rebalance ((E,ds):zs) = rebalance $ goUp ((E,ds):zs)
rebalance (z@((Nd v l r),ds):zs)
     | abs (td) < 2 = (((Nd v l r),ds):zs) 
     | td >= 2 && ld == -1 = goUp ((rotateRight ((Nd v t r),ds))++[z]++zs)
     | td >= 2 && ld /= -1 = goUp ((rotateRight ((Nd v l r),ds))++zs)
     | td >= -2 && rd == 1 = (goUp ((rotateLeft ((Nd v t'' r),ds))++[z]++zs))   
     | td >= -2 && rd /= 1 = goUp ((rotateLeft ((Nd v l r),ds))++zs)
     | otherwise = (((Nd v l r),ds):zs)
     where      
       td  = diff (Nd v l r)
       ld = diff l
       rd = diff r
       ((t,ds'):zs') = rotateLeft ( left z)
       ((t'',ds''):zs'') = rotateRight ( right z)

--rebalance [((Nd 8 (Nd 5 (Nd 3 (Nd 2 E E) E) E) E),[])]
--rebalance [((Nd 8 (Nd 5 E (Nd 3 E E)) E),[])]

--ROTATE TREE TO LEFT--
rotateLeft :: Zipper a -> Zips a
rotateLeft (E,ds) = [(E,ds)]
rotateLeft z@((Nd v l (Nd w r t)),ds) = (((Nd w (Nd v l r) t),ds):(left((Nd w (Nd v l r) t),ds)):z:[])

--rotateLeft ((Nd v l (Nd w r t)),[]) = (((Nd w (Nd v l r) t),[]):(left((Nd w (Nd v l r) t),[])):((Nd v l (Nd w r t)),[]):[])
{-
ds = ((Nd v1 l1 (Nd w r1 t)), [L,v,r]) =     (left ((Nd w (Nd v l1 r1) t), [L,v,r]) ):((Nd v1 l1 (Nd w r1 t)), [L,v,r])
-}                                                                                                                      

--ROTATE TREE TO RIGHT--
rotateRight :: Zipper a -> Zips a
rotateRight (E,ds) = [(E,ds)]
rotateRight z@((Nd v (Nd w l t) r),ds) = (((Nd w l (Nd v t r)),ds):(right((Nd w l (Nd v t r)),ds)):z:[])

--HEIGHT--
height :: Bt a -> Int
height E = 0
height (Nd v l r) = 1 + max (height l) (height r)

--DIFFERENCE BETWEEN HEIGHT OF BRANCHES--       
diff :: (Bt a) -> Int
diff E = 0
diff (Nd v l r) = ((height l) - (height r))



--------------------------------------------------------------------------------
------------------------------------SVG-----------------------------------------
--------------------------------------------------------------------------------


lenList :: [a] -> Int
lenList [] = 0
lenList (x:xs) = 1 + lenList xs

line :: Double -> Double -> Double -> Double -> X
line x1 y1 x2 y2 = xee "line" [("x1",show(x1)),("y1",show(y1)), ("x2",show(x2)), ("y2",show(y2))]

-- ADD STRING TO NAME OF SVG FILE --
makeName :: String -> String
makeName s = "tree"++s++".svg"       

-- MAKE XML ELEMENTS FROM ZIPPER --      
mkZipperX :: (Show a, Ord a) => Double -> Double -> Zipper a -> a -> (Double, Double, [X])
mkZipperX d shift z@(E, ds) eval = drawBt d shift (getTree z) (getRoot z eval)
mkZipperX d shift z@(Nd v l r, ds) eval = drawBt d shift (getTree z) (getRoot z eval) 

-- CREATE ONE XML DOC -- 
mkZipsX :: (Show a, Ord a) => Double -> Zips a -> a -> (Double, Double, [X]) -> (Double, Double, [X])
mkZipsX d [] eval (w,h,xs) = (w,h,xs)
mkZipsX d (z:zs) eval (w,h,[]) = mkZipsX d zs eval (mkZipperX d w z eval)
mkZipsX d (z:zs) eval (w,h,xss) = mkZipsX d zs eval (w+w1, (max h1 h), xss++xs)
          where
          (w1, h1, xs) = mkZipperX d w z eval

-- FINAL VISUALISATION --
svgZips:: (Show a, Ord a) => Double -> Zips a -> Int -> a -> IO ()
svgZips d zs n eval = writeSvgFile (makeName (show n)) (svg w (h+(d/2)) xs)
              where
              (w, h, xs) = mkZipsX d (reverse zs) eval (0,0,[])
--svgZips 50 [(Nd 2 E E,[(L,4,E),(L,10,Nd 8 E E)]),(E,[(L,4,E),(L,10,Nd 8 E E)]),(Nd 4 E E,[(L,10,Nd 8 E E)]),(Nd 10 (Nd 4 E E) (Nd 8 E E),[])] 2 0

-- MERGE TWO SVG TO ONE --
svgMerge :: (Double, Double, [X]) -> (Double, Double, [X]) -> (Double, Double, [X])
svgMerge (w1, h1, xs) (w,h,xss) = (w+w1, h+h1, xss++xs)       
      
maxx :: Double -> Double -> Double
maxx a b
        | a >= b = a
        | otherwise = b

-- TEXT ELEMENT WITH ATRRIBUTE --
textatt :: Double -> Double -> String -> String -> String -> X
textatt x y s an av = xa "text-anchor" "middle" (text x y s)

-- EMPTY ELEMENT --
xEmpty :: X
xEmpty = xe "" [] []
            
-- DRAW BT --
drawBt :: (Show a, Ord a) => Double -> Double -> Bt a -> a -> (Double,Double,[X])
drawBt d shift E node = (d, d, crc)
    where
    crc = [translate (0) (d/2) [stroke "black" [fill "white" [circle ((d/2)+shift) (d/2) (d/4)]]]]
    txtE = [translate (d/2) 0 [(textatt 0 d "E" "text-anchor" "middle")]]
drawBt d shift (Nd ch l r) node 
            | node /=  ch = (w, h, xs)
            | node == ch = (w, h, xs')
                where
                    {- doplňte rekurzívne volania -}
                    (wl, hl, ls) = drawBt d shift l node
                    (wr, hr, rs) = drawBt d shift r node
                    {- výpočet šírky a výšky -}
                    w = wl+wr
                    h = maxx (hl+d) (hr+d)
                    {- konštrukciu obrázku vnútorného uzla -}
                    xs = lnL++lnR++xss
                    xs' = lnL++lnR++xss'
                    lnL = [stroke "black" [line ((w/2)+shift) d (((w/2)-wr/2)+shift) (d+d)]] 
                    lnR = [stroke "black" [line ((w/2)+shift) d (((wl)+wr/2)+shift) (d+d)]]
                    xss = crc++[(textatt ((w/2)+shift) d (show ch) "text-anchor" "middle")]++[translate 0 d ls]++[translate ((wl)) d rs]
                        where
                        crc = [translate 0 0 [stroke "black" [fill "white" [circle ((w/2)+shift) (d) (d/4)]]]]
                    xss' = crc++[(textatt ((w/2)+shift) d (show ch) "text-anchor" "middle")]++[translate 0 d ls]++[translate ((wl)) d rs]
                        where
                        crc = [translate 0 0 [stroke "red" [fill "white" [circle ((w/2)+shift) (d) (d/4)]]]]

 
-- DRAW ZIPS --
doTest :: (Ord a, Show a, Num a) => Zips a -> IO()
doTest zs = svgZips 70 zs 3 0

doTestC :: Zips Char -> IO()
doTestC zs = svgZips 70 zs 3 'a'
--------------------------------------------------------------------------------
---------------------------------My Trees---------------------------------------
--------------------------------------------------------------------------------
freeTree :: Bt Char  
freeTree =   
    Nd 'P'  
        (Nd 'O'  
            (Nd 'L'  
                (Nd 'N' E E)  
                (Nd 'T' E E)  
            )  
            (Nd 'Y'  
                (Nd 'S' E E)  
                (Nd 'A' E E)  
            )  
        )  
        (Nd 'L'  
            (Nd 'W'  
                (Nd 'C' E E)  
                (Nd 'R' E E)  
            )  
            (Nd 'A'  
                (Nd 'A' E E)  
                (Nd 'C' E E)  
            )  
        )  
        
myTree = Nd 'a' (Nd 'b' E (Nd 'd' E E)) (Nd 'c' (Nd 'e' (Nd 'g' E E) E) (Nd 'f' E E))