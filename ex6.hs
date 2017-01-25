{-	Alunos: Guilherme Saulo Alves
			Mateus Lopes Teixeira			
	Exercicios Capitulo 13	
	Obs: Algumas respostas estão em comentario
-}

-- 13.7 	Qual é o tipo da função compare x y = size x <= size y ?

compare :: (Ord a, Ord b) => a -> b -> Bool
compare x y = size x <= size y

{- 13.18  	Os seguintes pares de tipos podem se unificar? 
			Se sim, dê uma unificação geral para eles, se não, explique por que eles não conseguem unificar. 
			1 - (Int -> b)	 e	( a -> Bool)
			2 - (Int, a, a)    e  (a, a, [Bool])
-}

-- 1 - (Int -> b) e	( a -> Bool) unifica para (Int -> Bool)
-- 2 - (Int, a, a) e (a, a, [Bool]) não podem se unificar

-- 13.19	Mostre que nós podemos unificar (a,[a]) com (b,c) para dar (Bool, [Bool]).

f :: (a,[a]) -> (b,c) -> (Bool, [Bool])

-- 13.21	Repita a questao anterior para a função f :: (a,[a]) -> a

f :: (a, [a]) -> a 
-- Pode ser aplicada para (2, [3]), (2,[]), mas não para (2,[True])

-- 13.22	Dê o tipo de f [] [] se f tem tipo f :: [a] -> [b] -> a -> b. Qual o tipo da função h dada pela definição h x = f x x ?

f :: [a] -> [b] -> a -> b 
f [] [] :: a -> b 

h :: [a] -> (a -> a)
h x = f x x

-- Pois o tipo b sera uniﬁcado com 'a' e 'f' esta parcialmente aplicado na deﬁnicação de h.

{- 13.26	Dê o tipo de cada uma das equações condicionais que se seguem e discuta o tipo da função que, juntos, elas definem.

			merge (x:xs) (y:ys) 
			  | x<y         = x : merge xs (y:ys)
			  | x==y        = x : merge xs ys
			  | otherwise   = y : merge (x:xs) ys
			merge (x:xs) []    = (x:xs)
			merge []    (y:ys) = (y:ys)
			merge []    []     = []	
-}

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) 
	| x<y         = x : merge xs (y:ys)
	| x==y        = x : merge xs ys
	| otherwise   = y : merge (x:xs) ys

merge :: [a] -> [b] -> [a]
merge (x:xs) [] = (x:xs)

merge :: [a] -> [b] -> [b]
merge [] (y:ys) = (y:ys)

merge :: [a] -> [b] -> [c]
merge [] [] = []

-- Logo o tipo da função merge é merge :: Ord a => [a] -> [a] -> [a]