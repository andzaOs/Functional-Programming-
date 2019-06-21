{-- Prüfung 2019-06-21 --}

data BTp a b = Nil | Node a b (BTp a b) (BTp a b)

requal :: (Eq b) => BTp a b -> BTp b c -> Bool
requal Nil Nil                           = True
requal (Node _ b t1 t2) (Node c _ t3 t4) = (b == c) && requal t1 t3 && requal t2 t4
requal _ _                               = False

filterbt :: (a -> Bool) -> BTPlus a b -> BTPlus c a -> [a]
filterbt _ Nil Nil              = []
filterbt f (Node a _ t1 t2) Nil = [a | f a] ++ filterbt f t1 Nil ++ filterbt f t2 Nil
filterbt f Nil (Node _ b t1 t2) = [b | f b] ++ filterbt f Nil t1 ++ filterbt f Nil t2
filterbt f t1 t2                = filterbt f t1 Nil ++ filterbt f Nil t2

{-- Prüfung 2010-03-04 

data BTree a = Nil | Node Integer a (BTree a) (BTree a)

aufsteigend :: BTree a -> Bool
aufsteigend Nil = True
aufsteigend (Node a _ l r) = aufsteigend' a l && aufsteigend' a r

aufsteigend' :: Integer -> BTree a -> Bool
aufsteigend' _ Nil  = True
aufsteigend' a (Node b _ l r) = if a > b then aufsteigend' b l && aufsteigend' b r else False


b :: (BTree a) -> Integer -> [a]
b Nil _ = []
b (Node x c left right) y
  | x > y = c : (b left y) ++ (b right y)
  | otherwise = (b left y) ++ (b right y)
--}

{-- Prüfung 2010-01-21 

data TTree a = Leaf Integer a |	Node Integer a (TTree a) (TTree a) (TTree a)

wiederholungsfrei :: TTree a -> Bool
wiederholungsfrei (Leaf _ _)            = True
wiederholungsfrei (Node a _ t1 t2 t3)   = wiederholungsfrei' a t1 && wiederholungsfrei' a t2 && wiederholungsfrei' a t3 && wiederholungsfrei t1 && wiederholungsfrei t2 && wiederholungsfrei t3

wiederholungsfrei' :: Integer -> TTree a -> Bool
wiederholungsfrei' a (Leaf b _)             = a /= b
wiederholungsfrei' a (Node b _ t1 t2 t3)    = a /= b && wiederholungsfrei' a t1 && wiederholungsfrei' a t2 && wiederholungsfrei' a t3


alle_von_bis :: TTree a -> Integer -> Integer -> [(Integer,a)];
alle_von_bis t v b = if(v>b) then [] else takeWhile (\(x,y) -> x>=v && x<=b) (toList t)

toList :: TTree a -> [(Integer, a)]
toList (Node i j l m r) = [(i,j)] ++ toList r ++ toList m ++ toList l
toList (Leaf i j) = [(i,j)]
--}

{-- Prüfung 2011-06-09 

data TTree a = Nil | Node a (TTree a) (TTree a) (TTree a)

alleGroesser :: (Ord a) =>TTree a -> Bool
alleGroesser Nil    = True
alleGroesser (Node a t1 t2 t3) = alleGroesser' a t1 && alleGroesser' a t2 && alleGroesser' a t3 && alleGroesser t1 && alleGroesser t2 && alleGroesser t3

alleGroesser' :: (Ord a) => a -> TTree a -> Bool
alleGroesser' a Nil                 = True
alleGroesser' a (Node b t1 t2 t3)   = if a < b then alleGroesser' a t1 && alleGroesser' a t2 && alleGroesser' a t3 else False


groesserAls :: Ord a => (TTree a) -> a -> [a]
groesserAls Nil _ = []
groesserAls (Node b t1 t2 t3) a = [b | b > a] ++ groesserAls t1 a ++ groesserAls t2 a ++ groesserAls t3 a

--}

{-- Prüfung 2011-01-20

data BTree a b = Nil | Node a b (BTree a b) (BTree a b)

wiederholungsfrei :: (Eq a) => BTree a b -> Bool
wiederholungsfrei Nil               = True
wiederholungsfrei (Node a b l r)    = (wiederholungsfrei' a l r) && (wiederholungsfrei l) && (wiederholungsfrei r)

wiederholungsfrei' :: (Eq a) => a -> BTree a b -> BTree a b -> Bool
wiederholungsfrei' _ Nil Nil                                = True
wiederholungsfrei' a (Node a1 _ l1 r1)  Nil                 = if a /= a1 then True else False
wiederholungsfrei' a Nil  (Node a2 _ l2 r2)                 = if a /= a2 then True else False
wiederholungsfrei' a (Node a1 _ l1 r1) (Node a2 _ l2 r2)    = if a /= a1 && a /= a2 then True else False

--}

{-- Prüfung 2011-01-20

data TTree a = 	Nil | Node a (TTree a) (TTree a) (TTree a) deriving Show


geordnet :: Ord a => TTree a -> Bool
geordnet Nil    = True
geordnet (Node a t1 t2 t3) = geordnet' a t1 && geordnet' a t2 && geordnet' a t3 && geordnet t1 && geordnet t2 && geordnet t3


geordnet' :: Ord a => a -> TTree a -> Bool
geordnet' _ Nil = True
geordnet' a (Node b t1 t2 t3)   =  if a < b then geordnet' a t1 && geordnet' a t2 && geordnet' a t3 else False


alle_groesser :: Ord a => TTree a -> a -> [a]
alle_groesser Nil _ = []
alle_groesser (Node a t1 t2 t3) b = [a|a>b] ++ (alle_groesser t1 b) ++ (alle_groesser t2 b) ++ (alle_groesser t3 b)

--}

{-- Prüfung Prüfung 2012-03-02

data TTree a = Nil | Leaf a | Node (TTree a) (TTree a) (TTree a)

geordnet :: Ord a => TTree a -> Bool
geordnet Nil = True
geordnet (Leaf a) = True
geordnet (Node x y z) = and [help y x, help z y, geordnet x, geordnet y, geordnet z]

help :: Ord a => TTree a -> TTree a -> Bool
help _ Nil = True
help Nil _ = True
help a b 
	| flatten a == [] || flatten b == [] = True
	| otherwise = minimum (flatten a) > maximum (flatten b)

flatten :: TTree a -> [a]
flatten Nil = []
flatten (Leaf a) = [a]
flatten (Node x y z) = flatten x ++ flatten y ++ flatten z



test = and [geordnet tree1 == True, geordnet tree2 == False, geordnet tree3 == False, geordnet tree4 == True]

tree1 = (Node (Leaf 1) (Leaf 2) (Leaf 3))
tree2 = (Node (Node (Leaf 1) (Leaf 2) (Leaf 3)) (Leaf 2) Nil)
tree3 = (Node (Node (Leaf 1) (Leaf 2) (Node (Leaf 5) (Leaf 4) Nil)) (Leaf 10) Nil)
tree4 = (Node (Node (Leaf 1) (Leaf 2) (Node (Leaf 4) (Leaf 9) Nil)) (Leaf 10) Nil)

zipT :: TTree a -> TTree b -> TTree (a,b)
zipT (Node t11 t12 t13) (Node t21 t22 t23) = Node (zipT t11 t21) (zipT t12 t22) (zipT t13 t23)
zipT (Leaf v1) (Leaf v2) = Leaf (v1,v2)
zipT _ _ = Nil

--}



{-- Prüfung 2012-01-19

data BTree a = Nil | Node a (BTree a) (BTree a)

kdif :: (Eq a) => BTree a -> Bool
kdif Nil = True
kdif (Node a l r) = count a l == 0 && count a r == 0 && kdif l && kdif r

count :: (Eq a) => a -> BTree a -> Integer
count _ Nil = 0
count a (Node b l r) = (if a == b then 1 else 0) + count a l + count a r

filtert :: (a -> Bool) -> BTree a -> [a]
filtert _ Nil   = []
filtert f (Node a l r) = [a | f a] ++ filtert f l ++ filtert f r

--}

{-- Prüfung 2013-04-26

data BTree a b = Nil | Node (BTree a b) (BTree a b) | Leaf a b

beq :: Eq b => BTree a b -> BTree b a -> Bool
beq (Node t1a t1b) (Node t2a t2b) = beq t1a t2a && beq t1b t2b
beq (Leaf _ b) (Leaf a _) = a == b
beq Nil Nil = True
beq _ _ = False


filterbt :: (a -> Bool) -> (BTree b a) -> (BTree a c) -> [a]
filterbt _ Nil Nil = []
filterbt f (Leaf _ b) t2 = [b|f b] ++ filterbt f Nil t2
filterbt f t1 (Leaf a _) = [a|f a] ++ filterbt f t1 Nil
filterbt f (Node t1a t1b) t2 = (filterbt f t1a Nil) ++ (filterbt f t1b Nil) ++ (filterbt f Nil t2)
filterbt f t1 (Node t2a t2b) = (filterbt f Nil t2a) ++ (filterbt f Nil t2b) ++ (filterbt f t1 Nil)

 --}

{-- Prüfung 2013-03-01 

data Bp a b =   Node a b (Bp a b) (Bp a b) | Nil

erq :: (Eq a) => Bp a b -> Bp b a -> Bool
erq Nil Nil = True
erq (Node a1 _ l1 r1) (Node _ b2 l2 r2) = a1 == b2 && erq l1 l2 && erq r1 r2
erq _ _     = False


filterb :: (b -> Bool) -> Bp a b  -> [a]
filterb _ Nil = []
filterb f (Node a b l r) = [a | f b] ++ filterb l ++ filterb r

--}

{-- Prüfung 2013-01-17

data BTplus a = Nil | Node a (BTplus a) (BTplus a)

sym :: (Eq a) => BTplus a -> BTplus b -> Bool
sym Nil Nil = True
sym (Node a1 l1 r1) (Node a2 l2 r2) = sym l1 l2 && sym r1 r2 && notEqual a1 l1 && notEqual a1 r1
sym _ _ = False

notEqual ::  (Eq a) => a -> BTplus a -> Bool
notEqual _ Nil = True
notEqual a (Node b l r) = a/=b && notEqual a l && notEqual a r

filtert :: (BTplus a -> Bool) -> BTplus a -> [BTplus a]
filtert f Nil = []
filtert f (Node _ n1 n2) = [n1 | f n1] ++ [n2 | f n2] ++ filtert f n1 ++ filtert f n2

--}

{-- Prüfung 2014-03-07

import Data.List 

data BTplus a = Nil | Node a (BTplus a) (BTplus a)

flatten :: BTplus a -> [a]
flatten Nil = []
flatten (Node a a1 a2)  = [a] ++ flatten a1 ++ flatten a2 

		
sym :: (Eq a) => BTplus a -> BTplus b -> Bool
sym Nil Nil = True
sym _ Nil = False
sym Nil _ = False
sym n@(Node a a1 a2) (Node _ b1 b2) = (sym a1 b1) && (sym a2 b2) && (nub f) == f where f = flatten n

--}

{-- Prüfung 2014-03-07 

data TTree a b c = Nil | Node a b c (TTree a b c) (TTree a b c) (TTree a b c)


rrt :: (Eq b , Eq c) => TTree (TTree a b c) b c -> Bool
rrt (Node (Node _ b' c' Nil Nil Nil) b c n1 n2 n3) = (b' == b) && (c' == c) && (rrt n1) && (rrt n2) && (rrt n3)
rrt Nil = True
rrt _ = False


filtertt :: (a -> b -> Bool) -> TTree a b c -> [TTree a b c]
filtertt _ Nil = []
filtertt f n@(Node a b c t1 t2 t2) = [n|f a b] ++ filtertt t1 ++ filtertt t2 ++ filtertt t3

--}

{-- Prüfung 2014-01-16 


data BTPlus a b = Node a b (BTPlus a b) (BTPlus a b) | Nil deriving Show


vergleiche :: (Eq a) => (BTPlus a b) -> (BTPlus c a) -> Bool
vergleiche Nil Nil  = True
vergleiche (Node a _ l1 r1) (Node _ d l2 r2) = a == d && vergleiche l1 l2 && vergleiche r1 r2
vergleiche _ _      = False


isEqual :: [Char] -> Bool
isEqual a = a /= "f"


filterbt :: (a -> Bool) -> BTPlus a b -> BTPlus c a -> [a]
filterbt _ Nil Nil              = []
filterbt f (Node a _ t1 t2) Nil = [a | f a] ++ filterbt f t1 Nil ++ filterbt f t2 Nil
filterbt f Nil (Node _ b t1 t2) = [b | f b] ++ filterbt f Nil t1 ++ filterbt f Nil t2
filterbt f t1 t2                = filterbt f t1 Nil ++ filterbt f Nil t2

--}

{-- Prüfung 2015-03-06

data TTree a = Nil | Leaf a | Node (TTree a) (TTree a) (TTree a)

geordnet :: Ord a => TTree a -> Bool
geordnet (Node (Leaf a) (Leaf b) (Leaf c)) = a < b && b < c
geordnet (Node n1 n2 n3)                   = geordnet n1 && geordnet n2 && geordnet n3
geordnet _                                 = False

zipT :: TTree a -> TTree b -> TTree (a,b)
zipT (Leaf a) (Leaf b)               = Leaf (a,b)
zipT (Node a1 b1 c1) (Node a2 b2 c2) = Node (zipT a1 a2) (zipT b1 b2) (zipT c1 c2)
zipT _ _                             = Nil

 --}

{-- Prüfung 2015-01-15 


data BTree a = Nil | Node a (BTree a) (BTree a)


jdif :: Eq a => BTree a -> Bool
jdif Nil = True 
jdif (Node a l r) = jdif'' a l && jdif'' a r && jdif l && jdif r

jdif' :: Eq a => a -> BTree a -> Bool
jdif' _ Nil = True
jdif' a (Node b l r)
 | a == b       = False
 | otherwise    = True


jdif'' :: Eq a => a -> BTree a -> Bool
jdif'' _ Nil = True
jdif'' a (Node b l r)
 | a == b       = False
 | otherwise    = jdif'' a l && jdif'' a r


 filtert :: (a -> Bool) -> BTree a -> [a]
 filtert _ Nil = []
 filtert f n@(Node a l r) = [a | f a] ++ filtert l ++ filtert r

 --}



{-- Prüfung 2016-03-04

data BTree a = Nil | Node a (BTree a) (BTree a)

jdif :: Eq a => BTree a -> Bool
jdif Nil = True
jdif (Node a l r) = count a l < 2 && count a r < 2 && jdif l && jdif r


count :: Eq a => a -> BTree a -> Integer
count _ Nil = 0
count a (Node b l r) = (if a == b then 1 else 0) + (count a l) + (count a r)

--}

{-- Prüfung 2018-03-02 

data BTree a = Node a (BTree a) (BTree a) | Nil


juniq :: Eq a => BTree a -> Bool
juniq Nil = True
juniq (Node a l r) = ((count a l) + (count a r) < 2) && (juniq l) && (juniq r)

count :: Eq a => a -> BTree a -> Integer
count _ Nil = 0
count a (Node b l r) = (if a == b then 1 else 0) + (count a l) + (count a r)

trx :: (BTree a -> Bool) -> BTree a -> [(a, BTree a, BTree a)]
trx _ Nil = []
trx f n@(Node a l r) = [(a,l,r)|f n] ++ (trx f l) ++ (trx f r)

--}


{-- Prüfung 2018-01-18 


data BTPlus a b = Nil | Node a b (BTPlus a b) (BTPlus a b) deriving (Eq, Show)

requal :: (Eq b) => BTPlus a b -> BTPlus b c -> Bool
requal Nil Nil  = True
requal (Node a b l1 r1) (Node c d l2 r2) = b == c && requal l1 l2 && requal r1 r2
requal _ _      = False

isEqual :: [Char] -> Bool
isEqual a = a /= "f"

filterbt :: (a -> Bool) -> BTPlus a b -> BTPlus c a -> [a]
filterbt _ Nil Nil = []
filterbt f (Node a _ t1 t2) Nil = [a | f a] ++ filterbt f t1 Nil ++ filterbt f t2 Nil
filterbt f Nil (Node _ b t1 t2) = [b | f b] ++ filterbt f Nil t1 ++ filterbt f Nil t2
filterbt f t1 t2                = filterbt f t1 Nil ++ filterbt f Nil t2

--}



