-- Aufgabe6

type Nat1 = Integer
data Variable = X | Y deriving (Eq,Ord,Enum,Show)
type Variablen = Variable
type Zustand = (Variablen -> Nat1)
type Zustandsmenge = Zustand
type Sigma = Zustandsmenge
type Zustandstransformator = (Sigma -> Sigma)
type Spur = [Sigma]

-- Aufgabe 6.1
map_2 :: (a -> b) -> (a -> b) -> [a] -> [b]
map_2 f g [] = []
map_2 f g [x] = [f x]
map_2 f g (x : y : xs) = f x : g y : map_2 f g xs


-- Aufgabe 6.2
map_n :: [(a -> b)] -> [a] -> [b]
map_n lf ln =  map_n' lf lf ln


map_n' :: [(a -> b)] -> [(a -> b)] -> [a] -> [b]
map_n' lf lf' []    = []
map_n' lf [] ln     = map_n lf ln
map_n' lf (f:fs) (x:xs) = f x :  map_n' lf fs xs


-- Aufgabe 6.3


wellenpfeil :: Sigma -> Sigma
wellenpfeil f
 | (f X) == (f Y)   = f
 | (f X) > (f Y)     = sigmaX
 | (f X) < (f Y)     = sigmaY
 where
    sigmaX :: (Variablen -> Nat1)
    sigmaX X = f X - f Y
    sigmaX Y = f Y
    sigmaY :: (Variablen -> Nat1)
    sigmaY Y = f Y - f X
    sigmaY X = f X


-- Aufgabe 6.4

ggt :: Zustandstransformator
ggt f
 | (f X) == (f Y)   = f
 | True             = ggt (wellenpfeil f)


-- Aufgabe 6.5

ggt_spur :: Sigma -> Spur
ggt_spur f
 | (f X) == (f Y)   = [f]
 | True             = f : ggt_spur (wellenpfeil f)


anfangszustand1 :: Sigma
anfangszustand1 = \z -> if z==X then 18 else 12

az1 = anfangszustand1

endzustand1 :: Sigma
endzustand1 = \z -> 6

ez1 = endzustand1


anfangszustand2 :: Sigma
anfangszustand2 = \z -> if z==X then 20 else 35

az2 = anfangszustand2

endzustand2 :: Sigma
endzustand2 = \z -> 5

ez2 = endzustand2



-- Aufgabe 6.6


zeige_spur :: Spur -> String
zeige_spur []       = ""
zeige_spur (f:[])   = "[(x<-" ++ show (f X) ++ ",y<-" ++ show (f Y) ++ ")]"
zeige_spur (f:fs)   = "[(x<-" ++ show (f X) ++ ",y<-" ++ show (f Y) ++ ") " ++ (zeige_spur' fs)

zeige_spur' :: Spur -> String
zeige_spur' []   = "]"
zeige_spur' (f:[])   = "-w-> (x<-" ++ show (f X) ++ ",y<-" ++ show (f Y) ++ ")]"
zeige_spur' (f:fs)   = "-w-> (x<-" ++ show (f X) ++ ",y<-" ++ show (f Y) ++ ") " ++ (zeige_spur' fs)



----------------------------------------------------


fac n
 | n == 0 = 1
 | sonst  = n * fac (n-1) where sonst = True

fib n
 | n == 0 = 0
 | n == 1 = 1
 | True   = fib (n-1) + fib (n-2)

binom n k = div_ffgb (fac n) (fac k * fac (n-k))

div_ffgb n m 
 | m > 0	= n `div` m
 | True	= -1
