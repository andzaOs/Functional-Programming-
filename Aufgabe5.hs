newtype IN_0 		= IN_0 Integer deriving (Eq,Ord,Show)
type Matrioid     	= [[Integer]]
type Typ          	= (IN_0,IN_0)
data Matrix       	= Matrix Typ Matrioid deriving (Eq,Show)
data Determinante 	= Determinante_ist Integer | Determinante_ist_undefiniert deriving (Eq,Show)
newtype Zeichenreihe 	= Z String deriving (Eq,Show)
newtype Tripel 		= T (IN_0,Zeichenreihe,Matrix) deriving (Eq,Show)
newtype Liste 		= L [Tripel] deriving (Eq,Show)


-- Aufgabe 1


class (Eq a, Ord a, Show a, Num a) => FFGB a where
 fac :: a -> a           -- Fakultaetsfunktion
 fib :: a -> a           -- Fibonacci-Funktion
 ggt :: a -> a -> a      -- Groesster-gemeinsamer-Teiler-Funktion
 binom :: a -> a -> a    -- Binomialkoeffizientenfunktion
 div_ffgb :: a -> a -> a -- Ganzzahlige Divisionsfunktion (ohne Protoimpl.)

instance FFGB Int where
 fac n
  | n == 0 = 1
  | sonst  = n * fac (n-1) where sonst = True
 fib n
  | n == 0 = 0
  | n == 1 = 1
  | True   = fib (n-1) + fib (n-2)
 ggt m n
  | m == n = n
  | m > n  = ggt (m-n) n
  | m < n  = ggt m (n-m)
 binom n k = div_ffgb (fac n) (fac k * fac (n-k))
 div_ffgb n m 
  | m > 0	= n `div` m
  | True	= -1
 

instance FFGB Integer where
 fac n
  | n == 0 	= 1
  | n > 0	= n * fac (n-1)
  | sonst  	= -1 
  where sonst = True
 fib n
  | n == 0	= 0
  | n == 1 	= 1
  | n > 1   	= fib (n-1) + fib (n-2)
  | True	= -1
 ggt m n
  | m <= 0 || n <= 0	= -1
  | m == n 		= n
  | m > n  		= ggt (m-n) n
  | m < n  		= ggt m (n-m)
 binom n k 
  | n <= 0 || k < 0	= -1
  | True		= div_ffgb (fac n) (fac k * fac (n-k))
 div_ffgb n m 
  | m > 0	= n `div` m
  | True	= -1


instance FFGB IN_0 where    
 fac (IN_0 n)
  | n < 0 = (IN_0 n)
  | n == 0 = (IN_0 1)
  | sonst = IN_0 (n * fac (n-1)) where sonst = True
 fib (IN_0 n)
  | n < 0 = (IN_0 n)
  | n == 0 = (IN_0 0)
  | n == 1 = IN_0 1
  | True = IN_0 (fib (n-1) + fib (n-2))
 ggt (IN_0 m) (IN_0 n)
  | m < 0  = IN_0 m
  | n < 0 = IN_0 n
  | m == n = IN_0 n
  | m > n = IN_0 (ggt (m-n) n)
  | m < n = IN_0 (ggt m (n-m))
 div_ffgb (IN_0 n) (IN_0 m)
  | n < 0 = IN_0 (-1)
  | m < 0 = IN_0 (-1)
  | m > 0 = IN_0 (n `div` m)
  | otherwise = IN_0 (-1)
 binom (IN_0 n) (IN_0 k)
  | n<0 || k<0 = IN_0 (-99)
  | k > n = IN_0 (-99)
  | otherwise = IN_0 (div_ffgb (fac n) (fac k * fac (n-k)))



---------------------------------------------------------------------------------------------------------------------



-- Aufgabe 2

class Quadratisch a where
 ist_quadratisch :: a -> Bool

instance Quadratisch Matrix where
 ist_quadratisch = ist_quadratisch_m


ist_quadratisch_m :: Matrix -> Bool
ist_quadratisch_m (Matrix (n,m) l)
 | isValidType (n,m) && isValidMatrioid l  && isValidRowNumber n (length l)	= True
 | otherwise									= False


isValidType :: Typ -> Bool
isValidType ((IN_0 n),(IN_0 m)) = n >= 1 && n == m

isValidRowNumber :: IN_0 -> Int -> Bool
isValidRowNumber (IN_0 n) m = n == (fromIntegral m)


isValidMatrioid :: Matrioid -> Bool
isValidMatrioid l = isValidMatrioid' l (length l)


isValidMatrioid' :: Matrioid -> Int -> Bool
isValidMatrioid' [] n	= True
isValidMatrioid' (l:ls) n
 | (length l) == n	= isValidMatrioid' ls n
 | otherwise		= False

----------------------------------------------------------------------------------------------------------------------


-- Aufgabe 3

determinante :: Matrix -> Determinante
determinante (Matrix (m, n) l)
 | ist_quadratisch_m (Matrix (m, n) l)	= (Determinante_ist (determinant l))
 | otherwise				= Determinante_ist_undefiniert


determinant :: Matrioid -> Integer
determinant [[x]] = x
determinant mat =
 sum [s*x*(determinant (getRest i mat)) | i <- [0..n-1], let x = (head mat) !! i
                                                             s = (-1)^i]
 where n = length $ head mat

getRest :: Int -> Matrioid -> Matrioid
getRest i mat = removeCols i (tail mat)

removeCols :: Int -> Matrioid -> Matrioid
removeCols _ [] = []
removeCols i (r:rs) = [r !! j | j <- [0..n-1], j /= i] : removeCols i rs
 where n = length r



----------------------------------------------------------------------------------------------------------------------


-- Aufgabe 4

is_square :: Integer -> Bool
is_square n = sq * sq == n
    where sq = floor $ sqrt $ (fromIntegral n::Double)

instance Quadratisch IN_0 where
 ist_quadratisch (IN_0 n) = (istgueltig (IN_0 n)) && (is_square n)


instance Quadratisch Zeichenreihe where
 ist_quadratisch = quadratischZeichen

quadratischZeichen :: Zeichenreihe -> Bool
quadratischZeichen (Z "")               = True
quadratischZeichen (Z (s:[]))			= True
quadratischZeichen (Z s)
 | (repeatWord word repeatNr) == s	= True
 | otherwise				= False
 where  word = konkatenation s []
        lw = length word
        repeatNr = if (lw > 0) then ((length s) `quot` lw) else 1


repeatWord :: String -> Int -> String
repeatWord s 1	= s
repeatWord s n 	= s ++ repeatWord s (n-1)


konkatenation :: String -> String -> String
konkatenation [] _	= []
konkatenation (x:xs) []	= konkatenation xs [x]
konkatenation (x:xs) s
 | isInList x s	== False	= konkatenation xs (s ++ [x])
 | otherwise			= s

isInList :: Char -> String -> Bool
isInList a []	= False
isInList a (x:xs)
 | a == x	= True
 | otherwise	= False


instance Quadratisch Tripel where
 ist_quadratisch (T (n,z,m)) = ist_quadratisch n && ist_quadratisch z && ist_quadratisch m


instance Quadratisch Liste where
 ist_quadratisch (L [])		= True
 ist_quadratisch (L (l:ls))
  | ist_quadratisch l		= ist_quadratisch (L ls)
  | otherwise			= False
 
--------------------------------------------------------------------------------------------------------------------


istgueltig :: IN_0 -> Bool
istgueltig (IN_0 m)
 | m >= 0 	= True
 | otherwise	= False


nmal :: IN_0 -> IN_0 -> IN_0
nmal (IN_0 m) (IN_0 n)
 | m >= 0 && n >= 0	= IN_0 (n*m)
 | otherwise		= IN_0 0


instance Num IN_0 where
    fromInteger x
        |x>=0 = IN_0 x
        |otherwise = IN_0 0
        
    (IN_0 x) + (IN_0 y)
        |x >= 0 && y >= 0 = IN_0 (x+y)
        |otherwise = IN_0 0
        
    (IN_0 x) - (IN_0 y)
        |x >= 0 && y >= 0 = IN_0 (maximum[0,x-y])
        |otherwise = IN_0 0
        
    (IN_0 x) * (IN_0 y)
        |x >= 0 && y >= 0 = IN_0 (x*y)
        |otherwise = IN_0 0    
        
    abs (IN_0 x)
        |x>=0 = (IN_0 x)
        |otherwise = IN_0 0
        
    negate (IN_0 x) = IN_0 0
        
    signum (IN_0 x)
        |x>0 = IN_0 1
        |otherwise = IN_0 0





