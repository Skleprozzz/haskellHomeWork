{-# LANGUAGE GADTs #-}

import Prelude hiding (Monoid)

-- Примеры функций высших порядков

--[] ++ l2 = l2
--(h : t) ++ l2 = h : (t ++ l2)

append' l1 l2 = foldr (\h p -> h : p) l2 l1

map' f l = foldr (\h p -> f h : p) [] l

-- Напишите:
-- map через foldr
-- elem через foldr
-- maximum через foldl1 и max
-- filter через foldr

maximum' l = foldl1 max l

-- mapAnd возвращает True, если все предикаты из списка
-- истинны на аргументе
mapAnd :: [a -> Bool] -> a -> Bool
mapAnd ps x = foldr (\p b -> p x && b) True ps
 
-- Объясните следующую эквивалентную реализацию
mapAnd' :: [a -> Bool] -> a -> Bool
mapAnd' ps x = and $ map ($ x) ps

-- Функции для преобразования значений в строчки и обратно

-- :t show
-- :t read
-- read "54"
-- read "54" + 1
-- read "54" :: Integer

-- Алгебраические типы данных (algebraic datatypes)

-- data Bool = False | True

data Nat = Zero | Succ Nat deriving (Show)

eqNat :: Nat -> Nat -> Bool
eqNat Zero Zero = True
eqNat (Succ x) (Succ y) = eqNat x y
eqNat _ _ = False

add :: Nat -> Nat -> Nat
add Zero y = y
add (Succ x) y = Succ (add x y)

two :: Nat
two = Succ (Succ Zero)

three :: Nat
three = Succ (Succ (Succ Zero))

-- Каждый тип имеет конструкторы, служащие для создания значений этого типа,
-- и деструкторы, служащие для анализа значений

-- False и True -- конструкторы типа Bool

-- Типы и конструкторы пишутся с большой буквы.

-- Иногда типы и конструкторы имеют одинаковое имя

data SameName = SameName

data Day = Day Int

-- Здесь SameName :: SameName
-- Пустой кортеж () :: ()

-- Конструкторы могут использоваться как образцы в определении функции
-- Так реализуются деструкторы

not' :: Bool -> Bool
not' True = False
not' False = True

-- Еще пример (аналог enum в С и Java)

data Piece = Pawn | Rook | Knight | Bishop | Queen | King

-- Конструктор с аргументами: точка в двумерном пространстве

data Point = Pt Float Float

-- Pt :: Float -> Float -> Point

px :: Point -> Float
px (Pt x _) = x

-- Именованные поля

data Point' = Pt' {px' :: Float, py' :: Float}

-- Автоматически генерируются проекторы px' :: Point' -> Float и py' :: Point' -> Float

-- Попытка напечатать значение типа Point ведет к ошибке
-- Можно добавить deriving (Eq, Show) к объявлению типа

data Point'' a = Pt'' a a deriving (Eq, Show)

-- Тип Maybe в Prelude

lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' key [] = Nothing
lookup' key ((k,v) : dict)
  | key == k = Just v
  | otherwise = lookup' key dict

-- Рекурсивные (индуктивные) типы

data List a = Nil | Cons a (List a) deriving Show

-- Cons 'a' (Cons 'b' (Cons 'c' Nil)) :: List Char

-- Если команде ghci дать опцию -XGADTs в командной строке или добавить
-- первую строчку, как этом файле, то можно записывать алгебраические типы
-- в следующем виде

data List' a where
  Nil' :: List' a
  Cons' :: a -> List' a -> List' a

data Tree a = Empty | Leaf a | Branches (Tree a) (Tree a) deriving Show

-- Деревья с произвольным количеством детей в каждом узле

-- data Tree a = Node a [Tree a]

-- Как написать деревья со счетным числом детей?

data InfTree a where
  Leaf' :: a -> InfTree a
  Children :: Integer -> InfTree a

infixl 6 #

class Eq a => Monoid a where
  (#) :: a -> a -> a
  e :: a

instance Eq Nat where
  Zero == Zero = True
  Succ x == Succ y = x == y
  _ == _ = False

instance Monoid Bool where
  e = False
-- операцию можно определить одним из следующих образов
--  (#) = (||)
  x # y = x || y
 
instance Eq a => Monoid [a] where
  e = []
  (#) = (++)
 
double :: Monoid a => a -> a
double x = x # x
 
instance Monoid Nat where
  e = Zero
  (#) = add
 
-- Local Variables:
-- haskell-program-name: "ghci -XGADTs"
-- End:
