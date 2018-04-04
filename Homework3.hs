module Homework3(stringIter,compose, iter) where

    import Prelude hiding (foldl, foldr, length, reverse, elem, maximum)
    import Data.Monoid

    foldl :: (b -> a -> b) -> b -> [a] -> b
    foldl fun init list =
        case list of
            [] -> init
            (head:tail) -> foldl fun (fun init head) tail

    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr fun init list =
        case list of
            [] -> init
            (head:tail) -> fun head (foldr fun init tail)

    fold :: (a -> b -> b) -> b -> [a] -> b                
    fold op init [] = init
    fold op init (x:xs) = x `op` fold op init xs

    mysum :: [Integer] -> Integer
    mysum = foldr (+) 0

    length :: [a] -> Integer
    length = foldr (\_ n -> 1 + n) 0

    append:: [a] -> [a] -> [a]
    append xs ys = foldr (\x y -> x:y) ys xs 

    reverse:: [a] -> [a]
    reverse xs = foldr (\ x y -> y ++ [x]) [] xs

    stringIter :: [Int] -> String
    stringIter xs = foldr go "+" xs
        where go x b = ('+' : replicate x '-') ++ b

    iter :: Int -> (a -> a) -> (a -> a)    
    -- iter :: a -> (a -> a) -> Int -> a    
    iter n f = foldr (.) id (replicate n f)

    compose :: [a -> a] -> (a -> a)
    compose fs = appEndo $ mconcat $ map Endo fs 

    longCollatz :: Int -> Integer -> Int
    longCollatz count 1 = count
    longCollatz count n = longCollatz (count+1) $ if n `mod` 2 == 0 then n `div` 2 else 3*n+1
    
