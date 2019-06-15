newtype IN_0 = IN_0 Integer
data Antwort3 = Ja | Nein | Teilsteils deriving (Eq,Ord,Show)
type A3 = Antwort3
newtype Nat_Liste = NL [IN_0] deriving (Eq,Show)

-- Aufgabe 1

getOkt :: Integer -> Integer -> Integer
getOkt n d
 | n > 0 	= (n `mod` 8)*d + (getOkt (n `div` 8) d*10)
 | otherwise	= 0

showIN :: IN_0 -> String
showIN (IN_0 m)
 | istgueltig (IN_0 m) 	= show (getOkt m 1)
 | otherwise		= "Nicht gueltig!"


istgueltig :: IN_0 -> Bool
istgueltig (IN_0 m)
 | m >= 0 	= True
 | otherwise	= False

instance Show IN_0 where
 show = showIN


-- Aufgabe 2 

class Gueltig a where
 ist_gueltig :: a -> A3


instance Gueltig IN_0 where
 ist_gueltig (IN_0 m)
  | istgueltig (IN_0 m)	= Ja
  | otherwise		= Nein

-- Aufgabe 3

instance Gueltig Int where
 ist_gueltig n	= Ja

instance Gueltig Integer where
 ist_gueltig n	= Ja

instance Gueltig Double where
 ist_gueltig n	= Nein


instance Gueltig Float where
 ist_gueltig n
  | n - m == 0.0		= Ja
  | otherwise			= Nein
  where m = fromIntegral (truncate (n)) :: Float


-- Aufgabe 4

instance Eq IN_0 where
  (IN_0 x) == (IN_0 y)
      | x >= 0 && y >= 0  = x == y
      | True              = False

  (IN_0 x ) /= (IN_0 y)
      | x >= 0 && y >= 0  = x /= y
      | True              = False
  
instance Ord IN_0 where
  (IN_0 x) <= (IN_0 y)
      | x >= 0 && y >= 0  = x <= y
      | True              = False
      
  (IN_0 x) >= (IN_0 y)
      | x >= 0 && y >= 0  = x >= y
      | True              = False
      
  (IN_0 x) > (IN_0 y)
      | x >= 0 && y >= 0  = x > y
      | True              = False
   
  (IN_0 x) < (IN_0 y)
      | x >= 0 && y >= 0  = x < y
      | True         = False
      
      
instance Enum IN_0 where
  succ (IN_0 x)
      | (x+1) >= 0  = IN_0 (x+1)
      | True        = IN_0 0
      
  pred (IN_0 x)
      | (x - 1) >= 0  = IN_0 (x-1)
      | True          = IN_0 0        
  
  toEnum x                                             
      | x >= 0        = IN_0 (toInteger x)
      | True          = IN_0 0
      
  fromEnum (IN_0 x)
      | x >= 0  = (fromIntegral x)
      | True    = (fromIntegral 0)
      
  enumFrom (IN_0 x) 
      | x < 0   = map toEnum [0..]
      | x >= 0  = map toEnum [(fromIntegral x)..]
      
  enumFromTo (IN_0 x) (IN_0 y)
      | x < 0   = map toEnum [0..(fromIntegral y)]
      | x >= 0  = map toEnum [(fromIntegral x)..(fromIntegral y)]
      
  enumFromThen (IN_0 x) (IN_0 y)
      | x < 0   = map toEnum [0,(fromIntegral y)..]
      | x >= 0  = map toEnum [(fromIntegral x),(fromIntegral y)..]
  
  enumFromThenTo (IN_0 x) (IN_0 y) (IN_0 z)
      | x < 0   = map toEnum [0,(fromIntegral y)..(fromIntegral z)]
      | x >= 0  = map toEnum [(fromIntegral x),(fromIntegral y)..(fromIntegral z)]
      
instance Num IN_0 where
  fromInteger x
      | x >= 0  = IN_0 x
      | True    = IN_0 0
      
  (IN_0 x) + (IN_0 y)
      | x >= 0 && y >= 0  = IN_0 (x+y)
      | True = IN_0 0
      
  (IN_0 x) - (IN_0 y)
      | x >= 0 && y >= 0  = IN_0 (maximum[0,x-y])
      | True              = IN_0 0
      
  (IN_0 x) * (IN_0 y)
      | x >= 0 && y >= 0  = IN_0 (x*y)
      | True              = IN_0 0    
      
  abs (IN_0 x)
      | x>=0              = (IN_0 x)
      | True              = IN_0 0
      
  negate (IN_0 x) = IN_0 0
      
  signum (IN_0 x)
      | x >0              = IN_0 1
      | True              = IN_0 0



-- Aufgabe 5


instance Gueltig Nat_Liste where
 ist_gueltig (NL l)
  | countJa l' == length l'		= Ja
  | countNein l' == length l'		= Nein
  | otherwise				= Teilsteils
  where l' = gueltigValues l


countJa :: [Antwort3] -> Int
countJa []	= 0
countJa (x:xs)	
 | x == Ja	= 1 + countJa xs
 | otherwise	= countJa xs

countNein :: [Antwort3] -> Int
countNein []	= 0
countNein (x:xs)	
 | x == Nein	= 1 + countNein xs
 | otherwise	= countNein xs

gueltigValues :: [IN_0] -> [Antwort3]
gueltigValues []	= []
gueltigValues (x:xs)
 | istgueltig x		= [Ja] ++ (gueltigValues xs) 
 | otherwise		= [Nein] ++ (gueltigValues xs)


-- Aufgabe 6

summe_integer :: Nat_Liste -> Integer
summe_integer (NL l)
 | ist_gueltig (NL l) == Ja	= sumNatListInteger l
 | otherwise			= (-1)

sumNatListInteger :: [IN_0] -> Integer
sumNatListInteger []		= 0
sumNatListInteger (x:xs)	= (getInteger x) + (sumNatListInteger xs)

getInteger :: IN_0 -> Integer
getInteger (IN_0 x) = x


summe_int :: Nat_Liste -> Int
summe_int (NL l)
 | ist_gueltig (NL l) == Ja	= sumNatListInt l
 | otherwise			= (-1)

sumNatListInt :: [IN_0] -> Int
sumNatListInt []		= 0
sumNatListInt (x:xs)	= (getInt x) + (sumNatListInt xs)

getInt :: IN_0 -> Int
getInt (IN_0 x) = fromIntegral (x) :: Int


tripel_finder :: Nat_Liste -> IN_0 -> [Nat_Liste]
tripel_finder (NL []) _			= []
tripel_finder (NL (x:[])) _		= []
tripel_finder (NL (x:y:[])) _		= []
tripel_finder (NL l) n 
 | ist_gueltig (NL l) /= Ja || ist_gueltig n /= Ja	= []
 | otherwise	= (tripel_finder' x y (NL xs) n) ++ (tripel_finder (NL (y:xs)) n) 
 where (x:y:xs) = quicksort l


tripel_finder' :: IN_0 -> IN_0 -> Nat_Liste -> IN_0 -> [Nat_Liste]
tripel_finder' x y (NL []) n	= []
tripel_finder' x y (NL (z:xs)) n
 | summeRichtig x y z n	= [NL [x, y, z]] ++ tripel_finder' x y (NL xs) n
 | otherwise		= tripel_finder' x y (NL xs) n


elementFrequenz :: Nat_Liste -> IN_0 -> Integer
elementFrequenz (NL []) _	= 0
elementFrequenz (NL (x:xs)) n
 | ngleich x n			= 1 + (elementFrequenz (NL xs) n)
 | otherwise			= elementFrequenz (NL xs) n


kleiner :: IN_0 -> IN_0 -> Bool
kleiner (IN_0 m) (IN_0 n)
 | m >= 0 && n >= 0	= m < n
 | otherwise		= False

summeRichtig :: IN_0 -> IN_0 -> IN_0 -> IN_0 -> Bool
summeRichtig x y z n
 | sumXYZ == n	= True
 | otherwise	= False
 where sumXYZ = nplus (nplus x y) z

ngleich :: IN_0 -> IN_0 -> Bool
ngleich (IN_0 m) (IN_0 n)
 | m >= 0 && n >= 0	= m == n
 | otherwise		= False

klgleich :: IN_0 -> IN_0 -> Bool
klgleich (IN_0 m) (IN_0 n)
 | positive_parameters	= m <= n
 | otherwise			= False
 where positive_parameters = istgueltig (IN_0 m) && istgueltig (IN_0 n)

nplus :: IN_0 -> IN_0 -> IN_0
nplus (IN_0 m) (IN_0 n)
 |  m >= 0 && n >= 0	= IN_0 (m + n)
 | otherwise		= IN_0 0

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
     let smallerSorted = quicksort [a | a <- xs, a <= x]  
         biggerSorted = quicksort [a | a <- xs, a > x]  
     in  smallerSorted ++ [x] ++ biggerSorted 

groesser :: IN_0 -> IN_0 -> Bool
groesser (IN_0 m) (IN_0 n)
 | m >= 0 && n >= 0	= m > n
 | otherwise		= False


nminus :: IN_0 -> IN_0 -> IN_0
nminus (IN_0 m) (IN_0 n)
 | m >= 0 && n >= 0 = IN_0 (maximum [0, m - n])
 | otherwise 		= IN_0 0

-- Test Funktionen


validDouble :: Double -> A3
validDouble n = ist_gueltig n

validInteger :: Integer -> A3
validInteger n = ist_gueltig n

validInt :: Int -> A3
validInt n = ist_gueltig n

validFloat :: Float -> A3
validFloat n = ist_gueltig n



