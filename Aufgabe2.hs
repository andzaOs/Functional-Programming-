type Nat1 = Integer


-- Aufgabe 1

echt_quer_max :: Nat1 -> Nat1
echt_quer_max n
 | n >= 1	= getEchteQuerzah [n, n-1..2]
 | otherwise	= 1

quer_alle :: Nat1 -> Nat1 -> [Nat1]
quer_alle m n
 | m >= 1 && n >= 1	= getAlleQuerzahlen [m..n] []
 | otherwise		= []

getAlleQuerzahlen :: [Nat1] -> [Nat1] -> [Nat1]
getAlleQuerzahlen [] ys			= ys
getAlleQuerzahlen (x:xs) ys
 | (isQuerzahl x)			= getAlleQuerzahlen xs zs
 | otherwise				= getAlleQuerzahlen xs ys
 where zs = [x] ++ ys

getEchteQuerzah :: [Nat1] -> Nat1
getEchteQuerzah []			= 1
getEchteQuerzah (x:xs)
 | (isQuerzahl x) && not (isPrime x)	= x
 | otherwise				= getEchteQuerzah xs

isQuerzahl :: Nat1 -> Bool
isQuerzahl n
 | (sum_of_elements (digits n)) == (sum_of_elements (primeFactors n))	= True
 | otherwise								= False

isPrime k = null [ x | x <- [2..k - 1], k `mod`x  == 0]


primeFactors :: Nat1 -> [Nat1]
primeFactors 1 = []
primeFactors n
  | factors == []  = [n]
  | otherwise = factors ++ primeFactors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

sum_of_elements :: [Nat1] -> Nat1
sum_of_elements []	= 0
sum_of_elements (0:xs)	= sum_of_elements xs
sum_of_elements (x:xs) 
 | len digs == 1	= x + sum_of_elements xs
 | len digs > 1		= (sum_of_elements digs) + (sum_of_elements xs)
 where digs = digits x

len [] = 0
len (h:t) = 1 + len t

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]


--Aufgabe 2

type Expandierte_Zeichenfolge = String
type Komprimierte_Zeichenfolge = String

komp :: Expandierte_Zeichenfolge -> Komprimierte_Zeichenfolge
komp xs = x xs 1
    
expnd :: Komprimierte_Zeichenfolge -> Expandierte_Zeichenfolge
expnd xs = if isValid xs then expnd' 0 xs else xs
        
expnd' :: Int -> String -> String 
expnd' n a
    | (length a) == 0 							= ""
    | (length a) == 1 && n == 0 					= ""
    | (a !! 0) == '0' 							= [] ++ (expnd' n (1 `drop` a))
    | ((a !! 0) `elem` ['1','2'..'9'] && n==0) 				= expnd' (isDigit a 0) (1 `drop` a)
    | ((a !! 0) `elem` ['1','2'..'9'] && (n>0) && (length a > 0)) 	= expnd' n (1 `drop` a)
    | n > 0 								= taken a n ++ expnd' 0 (1 `drop` a)

isDigit :: String -> Int -> Int
isDigit s d
 | ((s !! d) `elem` ['0','1'..'9']) && d < ((length s) - 1) 	= isDigit (s) (d+1)
 | otherwise 							= read (d `take` s) :: Int

taken :: String -> Int -> String
taken s n = take n(repeat(head s))
   
isValid::String->Bool
isValid s
 | (p == "") && (l == 0)	= True                        
 | (p == "") && (l /= 0)	= False
 | l == p'			= False
 | otherwise			= isValid((p' + 1) `drop` s)
 where p = takeWhile (`elem` "0123456789") s
       p'= length p
       l = length s

x :: Expandierte_Zeichenfolge -> Int -> Komprimierte_Zeichenfolge
x xs n
    | length xs == 0 = ""
    | length xs == 1 = show n ++ xs
    | length [a | a <- xs, a `elem` ['1','2'..'9']] > 0 = xs
    | ((xs !! 0) == '0') = x(1 `drop` xs) n
    | ((xs !! 0) == (xs !! 1)) && ((xs !! 0) /= '0') = x (1 `drop` xs) (n+1)
    | ((xs !! 0) /= (xs !! 1)) && ((xs !! 0) /= '0') = show n ++ [head xs] ++ x (1 `drop` xs) 1
    | otherwise = []


-- Aufgabe 3


aufteilen2 :: String -> [(String,String)]
aufteilen2 ""		= [("","")]
aufteilen2 s
 | t == ""	= [("",s),(s,"")]
 | otherwise	= aufteilen3 h t [("",s)]
 where t = tail s
       h = [head s]

aufteilen3:: String -> String -> [(String,String)] -> [(String,String)]
aufteilen3 h t cs
 | t == ""	= cs'
 | otherwise	= aufteilen3 h' t' cs'
 where h' = h ++ [(head t)]
       t' = tail t
       cs' = cs ++ [(h,t)]


--Aufgabe 4

type Mine = [Nat1]
type Goldnugget = Nat1
type Katzengoldnugget = Nat1
type Ausbeute = ([Goldnugget],[Katzengoldnugget])

schuerfe :: Mine -> Ausbeute
schuerfe m = (g,k)
          where k = [k' | k' <- reverse (m),((isGoldnugget k' 1 1) && (not (isSquare k')))];
                g = [g' | g' <- m,((isGoldnugget g' 1 1) && (isSquare g'))]

isSquare :: Integer -> Bool
isSquare n = m*m == n where m = floor $ sqrt (fromIntegral n)

isGoldnugget :: Integer -> Integer -> Integer -> Bool
isGoldnugget x y z
             | x == y ^ 2 + z ^ 2 = True
             | (z < ((floor $ sqrt (fromIntegral x)))) = isGoldnugget x y (z+1)
             | ((y < (floor $ sqrt (fromIntegral x)))) && ((z==(floor $ sqrt (fromIntegral x)))) = isGoldnugget x (y + 1) 1
             | otherwise = False


