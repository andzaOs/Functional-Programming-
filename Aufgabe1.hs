type Nat1 = Integer


h :: Integer -> Integer
h n
 | n < 0 	= n
 | n >= 0 	= add n


add :: Integer -> Integer
add 0 = 1
add n = (add (n-1))*(summ n n)

summ :: Integer -> Integer -> Integer
summ n e
 | e == 0	= 1
 | e > 0	= n^e + summ n (e-1)

klassifiziere :: Nat1 -> String
klassifiziere n
 | n >= 1	= func n
 | otherwise	= error "n must be greater than 0"	

func :: Nat1 -> String
func n
 | (isQuerzahl n) && (isPrime n)		= "unecht quer"
 | (isQuerzahl n) && (isPrime n) == False	= "echt quer"
 | otherwise					= "nicht quer"

isQuerzahl :: Integer -> Bool
isQuerzahl n
 | (sum_of_elements (digits n)) == (sum_of_elements (primeFactors n))	= True
 | otherwise								= False

isPrime k = null [ x | x <- [2..k - 1], k `mod`x  == 0]


primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n
  | factors == []  = [n]
  | otherwise = factors ++ primeFactors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

sum_of_elements :: [Integer] -> Integer
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





