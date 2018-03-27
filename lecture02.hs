-- Рекомендуемая документация
-- Tour of the Haskell Syntax.htm - хорошая шпаргалка по синтаксису
-- Язык и библиотеки Haskell 98, модули Prelude и List (на русском)
-- Более современная документация по Prelude и List (на английском)
-- https://hackage.haskell.org/package/base-4.7.0.2/docs/Prelude.html
-- https://hackage.haskell.org/package/base-4.7.0.2/docs/Data-List.html
-- Ссылки на материалы выше: https://nngufall2016fp.wordpress.com/about-2/

-- Команда для печати типа возвращаемых выражений
-- :set +t

-- Если нужно переопределить функции из Prelude
import Prelude hiding ((^), (^^), even, odd)

-- Теперь можно переопределить функцию из Prelude
even n = (mod n 2) == 0

-- Локальные определения

solve a b c
  | d < 0 = []
  | otherwise = [(-b + sqrt d)/2/a, (-b - sqrt d)/2/a]
    where
      d = b * b - 4 * a * c

solve1 a b c =
  let d = b * b - 4 * a * c in
    if d < 0 then [] else [(-b + sqrt d)/2/a, (-b - sqrt d)/2/a]

-- Рекурсия

-- Обращение списка
-- Нехвостовая рекурсия, квадратичная сложность
reverse' [] = []
reverse' (x : xs) = reverse xs ++ [x]

addToEnd :: a -> [a] -> [a]
addToEnd x [] = [x]
addToEnd x (y : ys) = y : addToEnd x ys

reverse'' [] = []
reverse'' (x : xs) = addToEnd x (reverse'' xs)

-- Хвостовая рекурсия, линейная сложность
reverse2 :: [a] -> [a] -> [a]
reverse2 [] ys = ys
reverse2 (x : xs) ys = reverse2 xs (x : ys)

rev xs = reverse2 xs []

-- Следующие определения выглядят похоже,
-- но есть большая разница в поведении и в сложности.
-- append (конкатенация списков, или ++)
foo1 [] acc = acc
foo1 (x : xs) acc = x : foo1 xs acc

-- reverse
foo2 [] acc = acc
foo2 (x : xs) acc = foo2 xs (x : acc)

-- факториал с нехвостовой рекурсией
fact 0 = 1
fact n = n * fact (n - 1)

-- факториал с хвостовой рекурсией
fact1 0 m = m
fact1 n m = fact1 (n - 1) (n * m)

fact2 n = fact1 n 1

-- Ленивые вычисления

-- const в Prelude

myConst x = \ y -> x
myConst' x y = x
myConst'' = \ x y -> x
myConst''' = \ x -> \ y -> x

-- undefined - это функция без аргументов, вызывающая исключение

-- Следующие вызовы не вызывают исключение, потому что ненужные
-- подвыражения не вычисляются.
-- const "hello" undefined
-- length [undefined]
-- let f (_, _) = 1 in f (undefined, 2)

-- Бесконечный список из элемента x
myRepeat :: a -> [a]
myRepeat x = x : myRepeat x

-- take n l возвращает первые n элементов списка l
-- take 5 (myRepeat 1)

-- replicate через repeat
myReplicate n x = take n (myRepeat x)

naturals = naturalsFrom 1
naturalsFrom n = n : naturalsFrom (n + 1)

factorial n = product (take n naturals)

-- еще один вариант
-- factorial n = product [1..n]

-- Следующее определение правильно с математической точки зрения
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]
-- или
-- fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Что будет, если функции дать бесконечный список?
-- 1. Вернет конечный результат. Пример: take
-- 2. Вернет бесконечный список, с которым можно работать, если не вычислять его целиком.
--    Пример: take 3 (drop 2 naturals)
-- 3. Не остановится. Пример: length naturals

-- Вычисление выражения в интепретаторе вызывает функцию show,
-- которая преобразовывает полученное значение в строку.
-- Значение анализируется целиком, поэтому вычисление в интепретаторе (at the prompt)
-- энергичное, а не ленивое.

-- Арифметические последовательности

-- [1 .. 10] -> [1,2,3,4,5,6,7,8,9,10]
-- [1, 3 .. 10] -> [1,3,5,7,9]
-- [10, 9 .. 1] -> [10,9,8,7,6,5,4,3,2,1]
-- [1 ..] -> [1,2,3,4,5,6,7,8,9,10,...]

-- Генераторы (или замыкания) списков (list comprehension)
-- [x^2 | x <- [1..10]]
-- [ (x, y) | x <- [1..3], y <- "ab"]
-- [выражение | образец <- список, образец <- список, ... , условие, условие, ... ]
qsort [] = []
qsort (x : xs) =
  qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, y > x]
