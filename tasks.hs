{-# LANGUAGE DeriveFunctor #-}

import Data.Function
import Data.Foldable
import Data.Monoid
import Control.Applicative
import Control.Monad.Random
import Data.List (group, genericLength)
--import Data.PQueue.Prio.Min (MinPQueue)
--import qualified Data.PQueue.Prio.Min as P

--эта-редукция 
--a1
-- a1 b c = (+) b c
-- a2 :: Int -> Int -> Int
-- a2 b = (+) b
-- a3 :: Int -> Int -> Int
-- a3 = (+)

--найти последний элемент списка
f1 :: [a] -> a
f1 [] = error "err"
f1 [a] = a
f1 (a:t) = f1 t

--предпоследний
f2 :: [a] -> a
f2 [] = error "err"
f2 [a] = error "err"
f2 [a, b] = a
f2 (a:t) = f2 t

--k-й элемент
f3 :: Int -> [a] -> a
f3 n [] = error "err"
f3 1 (a:t) = a
f3 n (a:t) = f3 (n - 1) t

--количество элементов в списке
f4 :: [a] -> Int
f4 [] = 0
f4 (a:t) = f4 t + 1

--перевернуть список
f5 :: [a] -> [a]
f5 [] = []
f5 (a:t) = f5 t ++ [a] --сложить два списка
--оно медленное, потом сделаем быстрее

--проверить,является ли списко палиндром
f6' :: Eq a => [a] -> Bool --eq делается для того, чтобы а было сравниваемое
f6' a = reverse a == a

f6 :: Eq a => [a] -> Bool
f6 [] = True
f6 [_] = True
f6 a = head a == last a && f6 (tail $ init a) --tail отбрасывает первый элемент, init - последний

--сделать из списка со вложенными списками один список
f7 :: [[a]] -> [a]
f7 [] = []
f7 (a:t) = a ++ f7 t 

--убрать дубликаты из списка
f8 :: Eq a => [a] -> [a]
f8 [] = []
f8 (a:t)
  | foldr (||) False (map (== a) t) = f8 t
  | otherwise = a : f8 t -- или any (map (a ==) t) то же самое

f8' :: Eq a => [a] -> [a]
f8' l = f [] l
  where f clear [] = clear
        f clear (a:t)
          | foldr (||) False (map (== a) clear) = f clear t
          | otherwise = f (a:clear) t

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [a] = [a]
qsort (h:t) = qsort [ x | x <- t, x <= h ] ++ [h] ++ qsort [ x | x <- t, x > h ]

--список. в нем есть дубликаты. подряд идущие дубликаты упаковать в список
--[a, a, b, a, a, b, b, c] -> [ 'aa', 'b', 'aa', 'bb', 'c']
f9 :: Eq a => [a] -> [[a]]
f9 a = f [] a
  where f a [] = [a]
        f [] (h:t) = f [h] t
        f a@(h':_) (h:t)
          | h == h' = f (h:a) t
          | otherwise = a : f [] t

f10 :: Eq a => [a] -> [(Int, a)] 
f10 = map (\(h:t) -> (length t + 1, h)) . f9

-- map :: (a -> b) -> [a] -> [b]

-- (a -> b) -> c
-- a -> b -> c

-- a -> (b -> c)
-- a -> b -> c

--[a] -> [Either a (Int, a)] если элемент встретился 1 раз, то писать только его, иначе ...

f11 :: Eq a => [a] -> [Either a (Int, a)]
f11 = map f . f10
  where f (1, a) = Left a
        f pair = Right pair

-- раскодировать f11 назад в полную строчку
-- uncurry :: (a -> b -> c) -> ((a, b) -> c)
-- replicate :: Int -> a -> [a]
f12 :: [(Int, a)] -> [a]
f12 = concat . map (uncurry replicate) -- concatMap

--написать 11 без 10 и 9 и без промежуточных внутренних списков
f13 :: Eq a => [a] -> [Either a (Int, a)]  -- и так понятно встретился
f13 = f11

--взять список и повторить каждый элемент
f14 :: [a] -> [a]
f14 [] = []
f14 (h:t) = h:h:f14 t

-- f14 = f15 2
f15 :: Int -> [a] -> [a]
f15 n [] = []
f15 n (h:t) = replicate n h ++ f15 n t

-- выкинуть каждый n-й элемент
f16 :: Int -> [a] -> [a]
f16 n s = f n s
  where f _ [] = []
        f 1 (h:t) = f n t 
        f n' (h:t) = h : f (n' - 1) t

--разделить список на 2 части : первые n и все остальные
f17 :: Int -> [a] -> ([a], [a])
f17 _ [] = ([], [])
f17 0 a = ([], a)
f17 n (h:t) = let (a, b) = f17 (n - 1) t
              in (h:a, b)

--два числа даются, надо вытащить все элементы между первым и вторым включительно считая с 1
f18 :: Int -> Int -> [a] -> [a]
f18 _ _ [] = []
f18 n m _ | n >= m = []
f18 1 n (h:t) = h : f18 1 (n - 1) t
f18 n m (h:t) = f18 (n - 1) (m - 1) t



-- [1,2,3,4] << 2 === [3,4,1,2]
f19 :: Int -> [a] -> [a]
f19 _ [] = []
f19 0 l = l
f19 n (h:t) = f19 (n - 1) $ t ++ [h]

--удалить k-й
f20 :: Int -> [a] -> [a]
f20 _ [] = []
f20 1 (h:t) = t
f20 k (h:t) = h : f20 (k - 1) t

--вставить на место k
f21 :: Int -> a -> [a] -> [a]
f21 i a [] = [a]
f21 1 h t = h:t
f21 n i (h:t) = h : f21 (n - 1) i t

--для любых перечислимых n и k делает список от n до k
f22 :: (Ord a, Enum a) => a -> a -> [a]
f22 n m
  | n > m = []
  | otherwise = n : f22 (succ n) m

-- rnd :: Isemechko -> (Float, Isemechko)
--дан список, взять из него n случайных элементов
f23 :: RandomGen g => g -> Int -> [a] -> ([a], g)
f23 g 0 a = (a, g)
f23 g n a = let (i, g') = randomR (0, length a - 1) g
                (es, g'') = f23 g' (n - 1) a
             in (a !! i : es, g'')

f23' :: RandomGen g => Int -> [a] -> Rand g [a]
f23' 0 a = return a
f23' n a = do
  i <- getRandomR (0, length a - 1)
  es <- f23' (n - 1) a
  return $ a !! i : es -- !! - взять n-й элемент

--вернуть список из n разных лучайных числ от 1 до m
f24 :: RandomGen g => Int -> Int -> Rand g [Int]
f24 n m = f n [1..m] 
  where f 0 _ = return []
        f _ [] = fail "error"
        f n a = do
          i <- getRandomR (1, length a)
          es <- f (n - 1) (f20 i a)
          return $ f3 i a : es

--комбинации разных из n по k 
f26 :: Int -> [a] -> [[a]]
f26 0 _ = []
f26 n a | n > length a = []
f26 1 a = map pure a
f26 n (h:t) = map (h:) (f26 (n - 1) t) ++ f26 n t

--перестановки хуев
insert =  f21
f27' :: [a] -> [[a]]
f27' [] = []
f27' [a] = [[a]]
f27' (h:t) = concat $ map (\x -> map (\n -> insert n h x) [1..length t + 1]) $ f27' t


--cдеалть нижнее без перестановок
nub = f8
f27'' :: [a] -> [Int] -> [[[a]]]
f27'' a b = map (f27''' a) $ nub $ f27' b

--делает выборки по сколько в [Int]
f27''' :: [a] -> [Int] -> [[a]]
f27''' [] [] = []
f27''' a (h:t) = let (n, m) = splitAt h a
                 in n : f27''' m t

--список а, список b из чисел, сумма которых равна длине а. надо разбить а на все позможные комбинации выборок по числам во втором списке
--то есть [1 1 1 1 1 1 1] [2 3 2] = [[1 1] [111] [11]] =[[111] [11] [11]]
f27 :: [a] -> [Int] -> [[[a]]]
f27 a b = concat $ map (\x -> f27'' x b) $ f27' a --concat вместо join

qsortBy :: (a -> a -> Ordering) -> [a] -> [a]
qsortBy f [] = []
qsortBy f [a] = [a]
qsortBy f (h:t) = qsortBy f [ x | x <- t, f x h /= GT ] ++ [h] ++ qsortBy f [ x | x <- t, f x h == GT ]

f28 :: [[a]] -> [[a]]
f28 = qsortBy (compare `on` length)

--aritmetic

--определить, простое ли число
f31 :: Integer -> Bool
f31 n = all (/= 0) $ map (n `mod`) [2..floor (sqrt $ fromIntegral n)]

--решето Эратосфена
f31' :: [Integer]
f31' = 1 : f [2..]
  where f (h:t) = h : f (filter (\x -> x `mod` h /= 0) t)

--найти наибольший общий делитель
f32 :: Integer -> Integer -> Integer
f32 n 0 = n
f32 0 m = m 
f32 n m = f32 m $ n `mod` m

f33 :: Integer -> Integer -> Bool
f33 n m = f32 n m == 1

f34 :: Integer -> Integer
f34 1 = 1
f34 n = genericLength $ filter (f33 n) [1..n]

--найти из произведения каких простых чисел состоит число
f35 :: Integer -> [Integer]
f35 1 = []
f35 n = let (_:h:_) = filter (\x -> n `mod` x == 0) f31'
        in h : f35 (n `div` h)

-- \(h:t) -> 1

type Length = Integer

f36 :: Integer -> [(Integer, Length)]
f36 a = map (\x@(h:_) -> (h, genericLength x)) $ group $ f35 a
--f36 = map (\x@(h:_) -> (h, length x)) . group . f35

f37 :: Integer -> Integer
f37 a =  foldr1 (*) $ map (\(p, m) -> (p - 1) * p ^ (m - 1)) $ f36 a

f39 :: Integer -> Integer -> [Integer]
f39 a b = takeWhile (<= b) $ dropWhile (< a) f31'

f40 :: Integer -> (Integer, Integer)
f40 a = let primeList = takeWhile (<= a) f31'
            x:_ = filter (\x -> (a - x) `elem` primeList) primeList
        --in head $ filter ((`elem` primeList) . snd) $ map (\x -> (x, a - x)) primeList
        in (x, a - x)

f41 :: Integer -> Integer -> [(Integer, (Integer, Integer))]
f41 a b = map (\x -> (x, f40 x)) $ filter even [a..b]

f46 :: (Bool -> Bool -> Bool) -> [((Bool, Bool), Bool)]
f46 f = [ ((a, b), f a b) | a <- [True, False], b <- [True, False] ]

type Input = [Bool]

f48 :: Integer -> (Input -> Bool) -> [(Input, Bool)]
--f48 a f = [Input] -> (Input -> Bool) -> [(Input, Bool)]
f48 a f = map (\x -> (x, f x)) $ dick a
  where dick :: Integer -> [Input]
        dick 0 = []
        dick 1 = [[True], [False]]
        --dick a = concat $ map (\x -> [True:x, False:x]) $ dick (a - 1) или
        dick a = let r = dick (a - 1)
                 in map (True:) r ++ map (False:) r
--[a] -> (a -> b) -> [(a, b)]

f49 :: Integer -> [String]
f49 0 = []
f49 1 = ["0", "1"]
f49 n = let k = f49 (n - 1)
        in map ('0':) k ++ map ('1':) (reverse k)

type Code = String

data HTree a = HLeaf a | HNode (HTree a) (HTree a)
              deriving (Show, Eq)

instance Functor HTree where
  fmap f (HLeaf a) = HLeaf (f a)
  fmap f (HNode a b) = HNode (fmap f a) (fmap f b)

--Huffman
--f50 :: [(Int, a)] -> [(Code, a)]
--f50 = dick . P.fromList . map (\(w, a) -> (w, HLeaf ("", a)))
--  where dick :: MinPQueue Int (HTree (Code, a)) -> [(Code, a)]
--        dick p = case P.size p of
--          0 -> []
        --   1 -> case snd $ P.findMin p of
        --     HLeaf (_, a) -> [("1", a)]
        --     t -> cock t
        --   _ -> let ((w1, a), p') = P.deleteFindMin p
        --            ((w2, b), p'') = P.deleteFindMin p'
        --            c = HNode (womb '0' a) (womb '1' b)
        --        in dick $ P.insert (w1 + w2) c p''
        -- cock :: HTree a -> [a]
        -- cock (HLeaf a) = [a]
        -- cock (HNode a b) = (cock a) ++ (cock b)
        -- womb :: Char -> HTree (Code, a) -> HTree (Code, a)
        -- womb c t = fmap (\(code, a) -> (c:code, a)) t

data Tree a = Null | Node (Tree a) a (Tree a)
              deriving (Show, Eq, Functor)

instance Foldable Tree where
  -- foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f Null = mempty
  foldMap f (Node a h b) = foldMap f a <> f h <> foldMap f b -- <> сложение элементов моноида

f54 :: Integer -> [Tree ()]
f54 0 = [Null]
f54 n
  | odd n = let cs = f54 n'
            in buildTree cs cs
  | even n = let cs1 = f54 n'
                 cs2 = f54 (n' + 1)
              in buildTree cs1 cs2 ++ buildTree cs2 cs1 
  where n' = (n - 1) `div` 2  

buildTree :: [Tree ()] -> [Tree ()] -> [Tree ()]
--[ ((a, b), f a b) | a <- [True, False], b <- [True, False] ]
buildTree a b = [ Node x () y | x <- a, y <- b ]

f55 :: Tree a -> Bool
f55 Null = True
f55 (Node a _ b)  = isMirror a b

isMirror :: Tree a -> Tree a -> Bool
isMirror Null Null = True
isMirror (Node a _ b) (Node c _ d) =  isMirror a d && isMirror b c

--это была f57
addElem :: Ord a => a -> Tree a -> Tree a
addElem a Null = Node Null a Null
addElem e (Node a h b) 
  | e < h = Node (addElem e a) h b
  | otherwise = Node a h (addElem e b)   

f58 :: Integer -> [Tree ()]
f58 a = filter f55 $ f54 a

f59 :: Integer -> [Tree ()]
f59 0 = [Null]
f59 n
  | n == 1 = same
  | otherwise = same ++ buildKitem c' c'' ++ buildKitem c'' c'
  where c' = item (n - 1)
        c'' = item (n - 2)
        same = buildKitem c' c'

item :: Integer -> [Tree ()]
item 0 = [Null]
item n
  | n == 1 = same
  | otherwise = same ++ buildKitem tf tm ++ buildKitem tm tf 
  where tf = item (n - 1)
        tm = helper (n - 2)
        same = buildKitem tf tf

helper :: Integer -> [Tree ()]
helper n = concatMap item [0..n] 

f60 :: Integer -> Integer -> [Tree ()]
f60 hite emaunt = filter ((== emaunt) . fromIntegral . length) $ f59 hite 

f61 :: Tree a -> Integer
f61 Null = 0
f61 (Node Null _ Null) = 1
f61 (Node l e r) = f61 l + f61 r

f61A :: Tree a -> [a]
f61A Null = []
f61A (Node Null a Null) = [a]
f61A (Node l e r) = f61A l ++ f61A r

f62A :: Integer -> Tree a -> [a]
f62A _ Null = []
f62 1 (Node _ a _) = [a]
f62 n (Node l a r) = f62 (n - 1) l ++ f62 (n - 1) r







