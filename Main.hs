import Homework3
import Prelude hiding (filter, map)


-- Homework4        

numPreds :: [(a -> Bool)] -> a -> Int
numPreds plist x = foldl (\ acc p -> if (p x) then acc+1 else acc) 0  plist

filterMapAnd :: [a -> Bool] -> a -> ([a -> Bool], [a -> Bool])
filterMapAnd flist a = (ft,ff)
                       where f = map (\ q -> (q,(q a))) flist
                             ft = map fst $ filter (\ z -> snd z) f
                             ff = map fst $ filter (\ z -> not (snd z)) f

--  head (fst (filterMapAnd [odd, even, (>5), (>4), (>1)] 6)) 7

-- findIndices :: (a -> Bool) -> [a] -> [Int]



-- Homework5

map            :: (a -> b) -> [a] -> [b]
map f []       = []
map f (x:xs)   = foldr (\y ys -> (f y):ys) [] xs

elem :: (Eq a) => a -> [a] -> Bool
elem x  =  foldl (\ s x -> if x == x then True else s) False

maximum :: (Ord a) => [a] -> a  
maximum = foldr1 (\x acc -> if x > acc then x else acc)  

filter :: (a -> Bool) -> [a] -> [a]  
filter p = foldr (\x acc -> if p x then x : acc else acc) []  