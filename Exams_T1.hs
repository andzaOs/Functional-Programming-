{-- Prüfung 2012-01-19 --}

mnr = [0,1,2,3,4,5,6]  :: [Int]
name = "Max Mustermann" :: String
kzn = "e53X" :: String


t1 = ( "p1",[head mnr],[(head.tail) mnr],((take 2 ). words . (\_ -> name)) 9);
-- ("p1",[0],[1],["Max","Mustermann"]) :: ([Char],[Int],[Int],[[Char]])


t4 = ( [[i*2]|i<-mnr], [i|i<-mnr,i>3] )
-- ([[0],[2],[4],[6],[8],[10],[12]],[4,5,6]) :: ([[Int]],[Int])

t5 = [[j | j <- [1..i]] | i <- mnr, i < 9]
-- [[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[1,2,3,4,5,6]] :: [[Int]]

ml n l = drop 5 l ++ take n l
t6 = ( ml 2 mnr, [(i,j)|i<- ml 2 mnr, j <- ml 2 mnr, j<i] )
-- ([5,6,0,1],[(5,0),(5,1),(6,5),(6,0),(6,1),(1,0)]) :: ([Int],[(Int,Int)])

p (a:b:l) xs ys = p l (a:b:xs) (b:ys)
p _ xs ys = (sum xs, product ys)
t7 = (p [mnr!!5, mnr!!6] [] [], p mnr [] [])

-- ((11,6),(15,15)) :: ((Int,Int),(Int,Int))



{-- Prüfung 2013-04-26 

mnr = [ 1 , 2 , 3 , 4 , 5 , 6 , 7 ] {- Matrikelnummer -}
name = "Mustermann Max"             {- Name -}
knz = "E 033 534"                   {- Studienkennzahl -}

t1 = ("p1",(length.words)name, (take 3 .(let no name= name; in no))"NO");
-- ("p1", 2, "NO") :: ([Char],Int,[Char])


t4 = (( take 4 . drop 2) [[i-2]|i<-mnr], take 3 [i|i<-mnr,i<3] );
-- ([[1],[2],[3],[4]], [1,2]) :: ([[Integer]],[Integer])

t5 = take 4 [[j|j<-[5..i]]|i<-mnr];
-- [[],[],[],[]] ::[[Integer]]

ml n m = (take 5 . reverse . drop n)m;
t6 = (ml 3 mnr, [(i,j)|j<- ml 3 mnr, i<- ml 3 mnr, i<j] );
-- ([7,6,5,4],[(6,7),(5,7),(4,7),(5,6),(4,6),(4,5)]) :: ([Integer],[(Integer,Integer)])

p (a:b:l) | a <= b = p (b:l);
p (a:l) = p l + a + p l;
p _ = 1;
t7 = ( (p.drop 4)mnr , p mnr);
-- (9,9) :: (Integer,Integer)

--}


{-- Prüfung 2013-03-01

mnr = [0,1,2,3,4,5,6]  :: [Int]
name = "Max Mustermann" :: String
kzn = "e53X" :: String
x = 2


t1 = ("p1",(length.take 2)mnr, (take 2.(let no name = name;in no))"No");
-- ("p1", 2, "No") :: ([Char], Int, [Char])


t4 = ( [(i*2,i)|i<-mnr], [i>2|i<-mnr] )
-- ([(0,0),(2,1),(4,2),(6,3),(8,4),(10,5),(12,6)],[False,False,False,True,True,True,True]) :: ([(Int,Int)],[Bool])

t5 = [[j|j<-[i..5]]|i<-mnr,i>1]
-- [[2,3,4,5],[3,4,5],[4,5],[5],[]] :: [[Int]]

ml n l =[l!!i|i<-[5,n,4],i < length l];
t6=(ml 3 mnr,[(i,j)|i<-ml 3 mnr,j<- ml 3 mnr,i>j]);
-- ([5,3,4],[(5,3),(5,4),(4,3)]) :: ([Int],[(Int,Int)]


p (e:t) = p t + e + p t
p _ = 0;
t7 = (p [mnr!!5, mnr!!6], p mnr)
-- (17,642) :: (Int,Int)

--}


{-- Prüfung 2013-01-17

mnr = [0,1,2,3,4,5,6]  :: [Int]
name = "Max Mustermann" :: String
kzn = "e53X" :: String

t1 = ("p1",(take 4.tail)mnr, (take 3.words.(let no n= name;in no))"No");
-- ("p1", [1,2], ["Max", "Musterman"]) :: ([Char], [Int], [[Char]])


t4 = [[i+2]|i<-mnr,i+2<5] ++ [[i|i<-mnr,3<i,i>4]];
-- [[2],[3],[4],[5,6]] :: [[Int]] 

t5 = [[2..i]|i<-[i|i<-mnr,i>2],i<9];
-- [[2,3],[2,3,4],[2,3,4,5],[2,3,4,5,6]] :: [[Int]]


ml n l = drop 5 l ++ take n l
t6 = ( ml 2 mnr, take 5[(i,j)|i<- ml 2 mnr, j<- ml 2 mnr, j>=i ]);
-- ([5,6,0,1],[(5,5),(5,6),(6,6),(0,5),(0,6)]) :: ([Int],[(Int,Int)])


p (a:b:l) xs ys = p l (a:b:xs) (b:ys);
p _ xs ys = (sum xs, product ys);
t7 = ( p (drop 4 mnr) [1] [], p mnr [1] []);
-- ((10, 6),(16, 15)) :: ((Int,Int),(Int,Int))

 --}


{-- Prüfung 2014-03-07

mnr = [0,2,0,0,3,3,1] :: [Integer]; {- Matrikelnummer -}
name = "Smith John" :: String; {- Familienname, Vorname(n) -}
knz = "999" :: String; {- Kennzahl -}

t1 = (take 2 . words . reverse . \x -> name++x)"!"
-- ["!nhoJ", "htimS"] :: [[Char]]

t4 = ( (drop 2 . take 4) [(i,i>2)|i<-mnr], take 3 [i|i<-mnr,i>2] );
-- ([(0,False),(0,False)],[3,3]) :: ([(Integer,Bool)],[Integer])

t5 = drop 3 [[j|j<-[i..3]]|i<-mnr];
-- [[0,1,2,3],[3],[3],[1,2,3]] :: [[Integer]]

ml = (drop 3 . reverse . tail);
t6 = ( ml mnr, take 5 [(j,i)|i<- ml mnr, j<- ml mnr, j>i] );
-- ([0,0,2],[(2,0),(2,0)]) :: ([Integer],[(Integer,Integer)])

p (a:b:l) | a <= b = p (b:l);
p (a:l) = p l + a + p l;
p [e] = 10000;
p [] = 0;
t7 = ( (p.drop 4)mnr , p mnr);
-- (5,12) : (Integer,Integer)

--}


{-- Prüfung 2014-01-16
mnr = [ 1 , 2 , 3 , 4 , 5 , 6 , 7 ] {- Matrikelnummer -}
name = "Max Mustermann" {- Name -}
knz = "E 033 4711" {- Studienkennzahl -}

t1 = ("p1", (drop 9.show) mnr, (head.words.(let no n = name; in no))"No");
-- ("p1", "5,6,7]", "Max") :: ([Char], [Char], [Char])

t4 = ((drop 4) [[i-2]|i<-mnr], [i|i<-mnr, i>3]);
-- ([[3], [4], [5]], [4,5,6,7]) :: ([[Integer]], [Integer])

t5 = take 4 [[i|j<-[i..5]]|i<-mnr];
-- [[1,1,1,1,1], [2,2,2,2], [3,3,3], [4,4]] :: [[Integer]]

ml = (tail.tail.reverse.tail);
t6 = (ml mnr, take 5 [(i,j)|i<-ml mnr, j<-ml mnr, j>=i]);
-- ([5,4,3,2], [(5,5),(4,5),(4,4),(3,5),(3,4)]) :: ([Integer], [(Integer, Integer)])

p o (a:l) (b:m) n = o a b : p o l m n;
p o _ _ n = n;
t7 = p (+) (reverse mnr) mnr [11];
-- [8,8,8,8,8,8,8,11] :: [Integer]

--}


{-- Prüfung 2015-03-06 

mnr = [ 1 , 2 , 3 , 4 , 5 , 6 , 7 ] {- Matrikelnummer -}
name = "Max Mustermann" {- Name -}
knz = "E 033 4711" {- Studienkennzahl -}

t1 = ("p2", unlines((:) ((head.words) name)["!"]));
{-- ("p2", "Max\n!\n") :: ([Char], [Char]) --}

t4 = [[i+2]|i<-mnr,i+2<5] ++ [[i|i<-mnr,3<i,i>4]];
{-- [[3], [4], [5,6,7]] :: [[Integer]] --}

t5 = take 4 [[j|j<-[5..i]]|i<-mnr]
{-- [[], [], [], []] :: [[Integer]]--}

tls xs = case xs of _:ys  -> ys:tls ys; _-> []
t6 = ((tls.take 3) mnr, take 5[(i,j)|(i:j:_)<-tls mnr, i<j]);
{-- ([[2,3],[3],[]], [(2,3), (3,4), (4,5), (5,6), (6,7)]) :: ([[Integer]], [(Integer,Integer)]) --}


p (a:b:l) | a <= b = p (b:l);
p (a:l) = p l + a + p l;
p [e] = 10000;
p [] = 0;
t7 = ( (p.drop 4)mnr , p mnr);
{-- (7, 7) :: (Integer, Integer)--}

--}


{-- Prüfung 2015-01-15 

mnr = [ 7, 6, 5, 4, 3, 2, 1 ] {- Matrikelnummer -}
name = "Max Mustermann" {- Name -}
knz = "E 033 4711" {- Studienkennzahl -}


t1 = ("p1", (reverse . take 2 . words . \_ -> name)"!");
{-- ("p1", ["Mustermann","Max"]) :: ([Char], [[Char]]) --}

t4 = [[i+2]|i<-mnr,i+2<5] ++ [[i|i<-mnr,3<i,i>4]];
{-- [[4], [3], [7,6,5]] :: [[Integer]] --}

t5 = take 4 [[j|j<-[5..i]]|i<-mnr]
{-- [[5,6,7], [5,6], [5], []] :: [[Integer]] --}

tls xs = xs : case xs of _:ys  -> tls ys; _-> []
t6 = ((tls.take 2) mnr, take 5[(i,j)|(i:j:_)<-tls mnr, i<j]);

{-- ([[7,6], [6], []], []) :: ([[Integer]], [(Integer, Integer)]) --}


p (a:b:l) xs ys = p l (a:b:xs) (b:ys);
p _ xs ys = (sum ys, product xs);

t7 = ( p [mnr!!5, mnr!!6] [] [], p mnr [] []);
{-- ((1, 2), (12, 5040)) :: ((Integer, Integer), (Integer, Integer))--}

--}


{-- Prüfung 2016-03-04 

mnr = [ 1 , 2 , 3 , 4 , 5 , 6 , 7 ] {- Matrikelnummer -}
name = "Mustermann, Max" {- Name -}
knz = "E 033 4711" {- Studienkennzahl -}

t1 = ("p1", (take 3.reverse.show) mnr)
{-- ("p1", "]7,") :: ([Char], [Char]) --}

t4 = ([i|i<-mnr,i>2], 2)
{-- ([3,4,5,6,7], 2) :: ([Integer], Integer) --}

tls xs = xs : case xs of _:ys  -> tls ys; _-> []
t6 = ((tls.take 3) mnr, take 5 [j|(0:j:_)<-tls mnr]);

{-- ([[1,2,3], [2,3], [3], []], []) :: ([[Integer]], [Integer]) --}

p (a:l) = p l + a + p l;
p (a:b:l) | a <= b = p (b:l);
p [e] = 10000;
p [] = 0;

t7 = ( (p.drop 4)mnr , p mnr);
{-- (45, 769) :: (Integer, Integer) --}

--}


{-- Prüfung 2016-01-14 

mnr = [ 1 , 2 , 3 , 4 , 5 , 6 , 7 ] {- Matrikelnummer -}
name = "Mustermann Max"             {- Name -}
knz = "E 033 534"                   {- Studienkennzahl -}

t1 = ["p1", (drop 9.show) mnr, (head.words.(let no n = name; in no))"No"]
{-- ["p1", "5 , 6 , 7 ]", "Mustermann"] :: [[Char]] --}

t4 = ((drop 4) [[i-2] | i <- mnr], [i | i <- mnr, i > 3])
{-- ([[3], [4], [5]], [4,5,6,7]) :: ([[Integer]], [Integer]) --}

t5 = take 4 [[i | j<- [i..5]] | i <- mnr]
{--  [[1,1,1,1,1], [2,2,2,2], [3,3,3], [4,4]] :: [[Integer]] --}

ml _ = (tail.tail.reverse.tail)
t6 = (ml 2 mnr, take 5 [(i,j) | i <- ml 2 mnr, j <- ml 2 mnr, j >= i])
{-- ([5,4,3,2], [(5,5), (4, 5), (4, 4), (3, 5), (3, 4)]) :: ([Integer], [(Integer, Integer)]) --}

p o (a:l) (b:m) n = o a b : p o l m n
p o _ _ n = n
t7 = p (+) (reverse mnr) mnr [11]
{-- [8, 8, 8, 8, 8, 8, 11] :: [Integer] --}

--}


{-- Prüfung 2017-01-19

mnr = [1,2,3,4,5,6,7] :: [Integer]; {-Matrikelnummer-}
name = "Mustermann, Max" :: String; {- Familienname, Vorname(n) -}
knz = "E033 534" :: String {- Kennzahl -}

t1 = ("p1", (take 2.reverse.show)mnr, (head.words.(\n o ->o:name)'-')'+');
{- ("p1", "[7", "+Mustermann,") :: ([Char], [Char], [Char]) --}

t4 = ( [i|i<-mnr, i>2], [(i, i`mod`3)|i<-mnr] );
{- ([3,4,5,6,7], [(1, 1), (2, 2), (3, 0), (4, 1), (5, 2), (6, 0), (7, 1)]) :: ([Integer], [(Integer, Integer)] --}

t5 = [(sum([i..5]),i)|i<-(tail mnr)];
{-- [(14,2), (12,3), (9,4), (5,5), (0,6), (0,7)] :: [(Integer, Integer)] --}

tls xs = xs: case xs of _:ys -> tls ys; _ -> [];
t6 = ((tls.take 3)mnr, take 5[j|0:j:_<-tls mnr]);
{--([[1,2,3], [2,3], [3], []], []) :: ([[Integer]], [Integer])--}

p (a:l) = p l + a + p l;
p (a:b:l) | a <= b = p (b:l);
p [e] = 200000;
p _ = 0;
t7 = ( (p.drop 4)mnr, p mnr);
{-- (45, 769) :: (Integer, Integer)--}
--}


{-- Prüfung 2018-03-02 

mnr = tail [0,1,2,3,4,5,6,7] :: [Integer]; {- Matrikelnummer -}
name = "Mustermann, Max" :: String; {- Familienname, Vorname(n) -}
knz = "E033 534" :: String {- Kennzahl -}

t1 = ("p1", (take 2.reverse.show)mnr, (head.words.(\n o ->o:name)'-')'+');
{- ("p1", "[7", "+Mustermann,") :: ([Char], [Char], [Char]) --}

t4 = ( [i|i<-mnr, i>3], [(i, i`mod`3)|i<-mnr] );
{- ([4,5,6,7],[(1,1),(2,2),(3,0),(4,1),(5,2),(6,0),(7,1)]) :: ([Integer], [(Integer, Integer)]) -}


t5 = [(sum([i..4]),i)|i<-(tail mnr)];
{- [(9,2),(7,3),(4,4),(0,5),(0,6),(0,7)] :: [(Integer, Integer)] -}

tls xs = xs: case xs of _:ys -> tls ys; _ -> [];
t6 = ((tls.take 3)mnr, take 5[j|1:j:_<-tls mnr]);
{- ([[1,2,3],[2,3],[3],[]],[2]) :: ([Integer]) -}

p (a:l) = a + p l + a + p l;
p (a:b:l) | a <= b = p (b:l);
p [e] = 200000;
p _ = 0;
t7 = ( (p.drop 4)mnr, p mnr);
{- (90,1538) :: (Integer, Integer) -}

--}


{-- Prüfung 2018-01-18 

mnr = tail[ 0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 ]::[Integer] {- Matrikelnummer -}
name = "Mustermann, Max"::String {- Familienname, Vorname(n) -}
knz = "E 033 4711" {- Studienkennzahl - falls mehrere Studien-}

t1 = ["p1", (drop 9.show)mnr, (head.words.(let no n=name;in no))"No"]
{- ["p1","5,6,7]","Mustermann,"] :: [[Char]]-}

t4 = ( drop 4 [[i-2]|i<-mnr], take 3 [i|i<-mnr,i>3])
{- ([[3],[4],[5]],[4,5,6]) :: ([[Integer]],[Integer]) -}

t5 = take 4 [[i|j<-[i..5]]|i<-mnr]
{- [[1,1,1,1,1],[2,2,2,2],[3,3,3],[4,4]] :: [[Integer]] -}

ml _ = (tail.tail.reverse.tail)
t6 = ( ml 1 mnr, take 5[(i,j)|i<- ml 2 mnr,j<-ml 3 mnr, j>=i ])
{- ([5,4,3,2],[(5,5),(4,5),(4,4),(3,5),(3,4)]) :: ([Integer],[(Integer,Integer)]) -}

p o (a:l) (b:m) n = o a b : p o l m n
p o _ _ n = n
t7 = p (+) (reverse mnr) mnr [11]
{- [8,8,8,8,8,8,8,11] :: [Integer] -}

--}


{-- Prüfung 2019-01-17 

mnr = tail [0,1,2,3,4,5,6,7] :: [Integer]
name = "Mustermann Max" :: String

t1 = ("p1", (drop 9.show)mnr, (take 3.zip name.tail)name)

{- t1 = ("p1", "5,6,7]", [('M','u'),('u','s'),('s','t')] :: ([Char], [Char], [(Char, Char)]) -}

t4 = (drop 4 [[i-1] | i<-mnr], take 3 [i | i<-mnr, i>4])
{- t4 = ([[4],[5],[6]],[5,6,7]) :: ([[Integer]], [Integer]) -}


t5 = take 4 [[i|j<-[i..5]] | i<-mnr]
{- t5 = [[1,1,1,1,1], [2,2,2,2], [3,3,3], [4,4]] :: [[Integer]] -}

tls _ = (tail.reverse.tail.tail)
t6 = (tls 1 mnr, take 5 [(i,j)| i <- tls 2 mnr, j <- tls 3 mnr, j<i])
{- t6 = ([6,5,4,3],[(6,5),(6,4),(6,3),(5,4),(5,3)]) :: ([Integer], [(Integer, Integer)])  -}

p o (b:m) (a:l) n = o a b : p o l m n
p _ _ _ n = n

t7 = p (+) (reverse mnr) mnr [12]

{- t7 = [8,8,8,8,8,8,8,12] :: [Integer] -}

--}


