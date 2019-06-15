> type Nat0             = Int
> type Zett             = Int
> type Terrain          = [Zett]
> type Schluchtenbreite = Nat0
> type Schluchtenliste  = [Terrain]
> type Nat1 = Int
> type Lauflaenge = Nat1
> type Zeichen = Char
> type Expandierte_Zeichenfolge = String
> type Komprimierte_Zeichenfolge = String
> newtype IN_0 = IN_0 Integer deriving Show
> newtype Xp_Zf' = Xp String deriving Show
> newtype Kp_Zf' = Kp [(Lauflaenge,Zeichen)] deriving Show


-- Aufgabe1

> schluchtenfinder :: Terrain -> Schluchtenbreite -> Schluchtenliste
> schluchtenfinder [] sb	= []
> schluchtenfinder t sb = getSchluchtenliste (schluchtenfinder' (tail (quicksort t)) sb (head (quicksort t)) (head (quicksort t))) [] sb

> schluchtenfinder' :: Terrain -> Schluchtenbreite -> Zett -> Zett -> Terrain
> schluchtenfinder' [] 0 m p	= [m]
> schluchtenfinder' [] n m p	= if (p + n + 1) /= m then [] else [m]
> schluchtenfinder' (t:ts) n m p
>  | (t == nextZett) || (t /= nextZett && (m == prevZett || n == 0))	= [m] ++ schluchtenfinder' ts n t m
>  | otherwise								= schluchtenfinder' ts n t m
>  where nextZett = n + m + 1
>        prevZett = p + n + 1


> getSchluchtenliste :: Terrain -> Terrain -> Schluchtenbreite -> Schluchtenliste
> getSchluchtenliste [] [] n	= []
> getSchluchtenliste [] ts' n	= [ts']
> getSchluchtenliste (t:[]) ts' n	= [ts' ++ [t]]
> getSchluchtenliste (t:ts) ts' n
>  | (t + n + 1 == head ts) || (n == 0 && t == (head ts)) = getSchluchtenliste ts ts'' n	
>  | otherwise						= [ts''] ++ getSchluchtenliste ts [] n
>  where ts'' = ts' ++ [t]


> quicksort :: (Ord a) => [a] -> [a]  
> quicksort [] = []  
> quicksort (x:xs) =   
>     let smallerSorted = quicksort [a | a <- xs, a <= x]  
>         biggerSorted = quicksort [a | a <- xs, a > x]  
>     in  smallerSorted ++ [x] ++ biggerSorted 


-- Aufgabe 2

> nplus :: IN_0 -> IN_0 -> IN_0
> nplus (IN_0 m) (IN_0 n)
>  | m >= 0 && n >= 0	= IN_0 (n + m)
>  | otherwise		= IN_0 0

> nminus :: IN_0 -> IN_0 -> IN_0
> nminus (IN_0 m) (IN_0 n)
>  | m >= 0 && n >= 0 	= IN_0 (maximum [0, m - n])
>  | otherwise 		= IN_0 0

> nmal :: IN_0 -> IN_0 -> IN_0
> nmal (IN_0 m) (IN_0 n)
>  | m >= 0 && n >= 0	= IN_0 (n*m)
>  | otherwise		= IN_0 0

> ndurch :: IN_0 -> IN_0 -> IN_0
> ndurch (IN_0 m) (IN_0 n)
>  | m >= 0 && n >= 1	= IN_0 (m `div` n)
>  | otherwise		= IN_0 0

> ngleich :: IN_0 -> IN_0 -> Bool
> ngleich (IN_0 m) (IN_0 n)
>  | m >= 0 && n >= 0	= m == n
>  | otherwise		= False

> nungleich :: IN_0 -> IN_0 -> Bool
> nungleich (IN_0 m) (IN_0 n)
>  | m >= 0 && n >= 0	= m /= n
>  | otherwise		= False

> groesser :: IN_0 -> IN_0 -> Bool
> groesser (IN_0 m) (IN_0 n)
>  | m >= 0 && n >= 0	= m > n
>  | otherwise		= False

> kleiner :: IN_0 -> IN_0 -> Bool
> kleiner (IN_0 m) (IN_0 n)
>  | m >= 0 && n >= 0	= m < n
>  | otherwise		= False

> grgleich :: IN_0 -> IN_0 -> Bool
> grgleich (IN_0 m) (IN_0 n)
>  | istgueltig (IN_0 m) && istgueltig (IN_0 n)	= m >= n
>  | otherwise					= False

> klgleich :: IN_0 -> IN_0 -> Bool
> klgleich (IN_0 m) (IN_0 n)
>  | positive_parameters	= m <= n
>  | otherwise			= False
>  where positive_parameters = istgueltig (IN_0 m) && istgueltig (IN_0 n)

> istgueltig :: IN_0 -> Bool
> istgueltig (IN_0 m)
>  | m > 0	= True
>  | otherwise	= False

-- Aufgabe 3

> type Xp_Zf = String
> type Kp_Zf = String
> 
> opt_komp :: Xp_Zf -> Kp_Zf
> opt_komp [] 		= []
> opt_komp m
>  | validOpt m == False		= m 
>  | length fstPart <= 2 	= fstPart ++ (opt_komp sndParts)
>  | True 			= (show (length fstPart)) ++ [(head fstPart)] ++ (opt_komp sndParts)     
>  where parts 	= span (==(head m)) m 
>	 fstPart = fst parts 
>	 sndParts = snd parts                              


> validOpt :: Xp_Zf -> Bool
> validOpt s = length (takeWhile (`notElem` "0123456789") s) == (length s)    


> opt_expnd :: Kp_Zf -> Xp_Zf
> opt_expnd s     
>  | validExp s		= expnd s
>  | True 		= s


> expnd ::Kp_Zf -> Xp_Zf 
> expnd s  
>  | s == "" 	= ""
>  | n == "" 	= c : expnd(tail s)
>  | sonst 	= (replicate (read n) c) ++ (expnd (drop ((length n) + 1) s))
>  where n = takeWhile (`elem` "0123456789") s
>        c = (s!!(length n))
>	 sonst = True

> validExp :: Kp_Zf -> Bool
> validExp s
>  | s == "" 					= True
>  | (length c > 2) || (length n == length s)	= False
>  | n == "" 					= validExp (drop (length c) s)
>  | (read n == 1) 				= False
>  | sonst 					= validExp (drop ((length n) + 1) s)
>  where c = takeWhile (`elem` [head s]) (takeWhile (`notElem` "0123456789") s)
> 	 n = takeWhile (`elem` "0123456789") s       
>	 sonst = True


-- Aufgabe 4


> komp :: Expandierte_Zeichenfolge -> Komprimierte_Zeichenfolge
> komp xs = x xs 1

> x :: Expandierte_Zeichenfolge -> Int -> Komprimierte_Zeichenfolge
> x xs n
>  | length xs == 0 = ""
>  | length xs == 1 = show n ++ xs
>  | length [a | a <- xs, a `elem` ['1','2'..'9']] > 0 = xs
>  | ((xs !! 0) == '0') = x(1 `drop` xs) n
>  | ((xs !! 0) == (xs !! 1)) && ((xs !! 0) /= '0') = x (1 `drop` xs) (n+1)
>  | ((xs !! 0) /= (xs !! 1)) && ((xs !! 0) /= '0') = show n ++ [head xs] ++ x (1 `drop` xs) 1
>  | otherwise = []


> komp' :: Xp_Zf' -> Kp_Zf'
> komp' a
>  | isValidXp s 	= getKp(transformXp (komp s))
>  | True 		= getKp (getWrong s)
>   where s = auspacken a


> isValidXp::String -> Bool
> isValidXp s = length(takeWhile (`notElem` "0123456789") s) == (length s)

> getKp :: [(Lauflaenge, Zeichen)] -> Kp_Zf'
> getKp a = Kp a

> getWrong :: String -> [(Lauflaenge, Zeichen)]
> getWrong s
>  | (length s) == (length a) 	=  []
>  | True 			= [(1, s !! (length a))]
>   where a = takeWhile (`notElem` "0123456789") s

> transformXp :: Kp_Zf -> [(Lauflaenge,Zeichen)]
> transformXp s
>  | n == "" 	= []
>  |True 	= [((read n), c)] ++ transformXp (drop ((length n) + 1) s)
>     where n = takeWhile (`elem` "0123456789") s
>           c = (s !! (length n))

> expnd' :: Kp_Zf' -> Xp_Zf'
> expnd' a
>  | validKp s = einpacken (expnd s)
>  | True = einpacken s 
>  where t = getList a
>        s = transformKp t

> getList:: Kp_Zf' -> [(Lauflaenge,Zeichen)]
> getList (Kp a) = a

> transformKp :: [(Lauflaenge,Zeichen)] -> Kp_Zf
> transformKp [] 	= []
> transformKp k		= show (fst h) ++ [snd h] ++ transformKp (tail k) where h = head k

> validKp :: Komprimierte_Zeichenfolge -> Bool
> validKp s
>  | length s == 0 				= True                        
>  | (length n == length s) || (n == "") 	= False
>  | True 					= validKp (drop ((length n) + 1) s)
>  where n = takeWhile (`elem` "0123456789") s   


-- Aufgabe 5

> einpacken :: Xp_Zf -> Xp_Zf'
> einpacken a = Xp a

> auspacken :: Xp_Zf' -> Xp_Zf
> auspacken (Xp a) = a

> komp'' :: Xp_Zf -> Kp_Zf'
> komp'' s = komp' (einpacken s)

> expnd'' :: Kp_Zf' -> Xp_Zf
> expnd'' x = auspacken (expnd' x) 


-- Aufgabe 6

> aufteilen3 :: String -> [(String,String,String)]
> aufteilen3 s = zip3 (first s 0) (second s 0) (third s 0)

> first :: String -> Int -> [String]
> first s n
>  | n <= length s 	= take (length s + 1 - n) (repeat (take n s)) ++ first s (n + 1)
>  | True 		= []
    
> second :: String -> Int -> [String]
> second "" _	= [""]
> second s 0	= (second s 1) ++ (second (tail s) 0)
> second s n 	= (second (init s) n) ++ [s]
    
> third :: String -> Int -> [String]
> third "" _	= [""]
> third s 0	= (third s 1) ++ (third (tail s) 0)
> third s n	= s : (third (tail s) n)




