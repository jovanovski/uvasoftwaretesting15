module CSI where

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && (not (a && b))
   
boys = [Matthew, Peter, Jack, Arnold, Carl]

says :: Boy -> Boy -> Bool
says Matthew b = not (b `elem` [Carl, Matthew])
says Peter b = b `elem` [Matthew, Jack]
says Jack n = (not (says Peter n)) && (not (says Matthew n))
says Arnold n = (says Peter n) `xor` (says Matthew n)
says Carl n = not (says Arnold n)

accusers :: Boy -> [Boy]
accusers a = filter (\b -> (says b a)) boys

guilty, honest :: [Boy]
guilty = [a | a <- boys, length (accusers a) == 3] 
honest = foldr (++) [] (map (\a -> filter (\b -> says b a) boys) guilty)
