--Aluno: Guilherme Saulo Alves
--Aluno: Mateus Lopes Teixeira

--Exercicio 9.5: Prova para todos os xs e ys finitos que 
-- soma (xs ++ ys) = sum xs + sum ys (sum++)

Para o caso base, temos:
    sum ([] ++ ys) = sum [] + sum ys,
e pela (++.1)
    sum ys = 0 + sum ys.
2- Para o caso geral, temos:
    sum ((x:xs) ++ ys) = sum (x:xs) + sum ys
e pela (++.2)
    sum (x : (xs ++ ys)) = sum (x:xs) + sum ys
por (sum.2)
    x + sum (xs ++ ys) = x + sum xs + sum ys
e pela hipotese indutiva:
    x + sum xs + sum ys = x + sum xs + sum ys

	
--Exercicio 9.8: Mostre para todas as listas de inteiros finitas xs e ys que
-- elem z (xs ++ ys) = elem z xs || elem z ys

(elem.1) elem z []     = False
(elem.2) elem z (x:xs) = z == x || elem z xs

Para o caso base, nos temos
    elem z ([] ++ ys) = elem z [] || elem z ys.
e por (++.1) and (elem.1),
    elem z ys = False || elem z ys.
Para o caso geral,
    elem z ((x:xs) ++ ys) = elem z (x:xs) || elem z ys.
e por (++.2),
    elem z (x : (xs ++ ys)) = elem z (x:xs) || elem z ys.
e por (elem.2),
    z == x || elem z (xs ++ ys) = z == x || elem z xs || elem z ys.
E pela hipotese indutiva,
    z == x || elem z xs || elem z ys = z == x || elem z xs || elem z ys.	

	
--Exercicio 9.9: Mostre para todas as listas de inteiros finitas ps que
-- zip(fst (unzip ps)) (snd(unzip ps)) = ps

(zip.1) zip (x:xs) (y:ys) = (x,y) : zip xs ys
(zip.2) zip _ _ = []

(unzip.1) unzip [] = ([],[])
(unzip.2) unzip ((x,y):ps) = (x:xs,y:ys)
            where (xs,ys)  = unzip ps

(fst.1) fst (x,y) = x
(snd.1) snd (x,y) = y

Para o caso base,
    zip (fst (unzip [])) (snd (unzip [])) = [].
Por (unzip.1),
    zip (fst ([],[])) (snd ([],[])) = [].
Por (fst.1) and (snd.1),
    zip [] [] = [].
Por (zip.2)
    [] = [].
Para o caso geral,
    zip (fst (unzip ((x,y):(xs,ys)))) (snd (unzip ((x,y):(xs,ys)))) = ((x,y):(xs,ys)).
Por (unzip.2),
    zip (fst (x:xs,y:ys)) (snd (x:xs,y:ys)) = ((x,y):(xs,ys)).
Por (fst.1) and (snd.1),
    zip (x:xs) (xs:ys) = ((x,y):(xs,ys)).
E por (zip.1),
    (x,y):(xs,ys) = (x,y):(xs,ys).
----
    Sob quais condicoes em xs e ys é o caso em que
        unzip(zip xs ys) = (xs,ys)
    quando unzip é definido por (unzip.1) e (unzip.2)?

Entao deve ser o caso em que
    xs ≠ [],
e
    ys ≠ [],
ou que
    xs = ys = []. 

Pelo ultimo caso,
    unzip (zip [] []) = ([],[]).
Por (zip.2),
    unzip [] = ([],[]),
e (unzip.1),
    ([],[]) = ([],[]).

Pelo primeiro caso,
    unzip (zip (x:xs) (y:ys)) = ((x:xs),(y:ys)).
Por (zip.1),
    unzip ((x,y) : (zip xs ys)) = ((x:xs),(y:ys)).

--Exercicio 9.13: Usando a funcao
-- facAux :: Integer -> Integer -> Integer
-- facAux 0 p = p
-- facAux n p = facAux (n-1) (n*p)
-- podemos definir fac2 n

fac n = fac2 n

facAux :: Int -> Int -> Int
facAux 0 p = p                  (fAux.1)
facAux n p = facAux (n-1) (n*p) (fAux.2)

fac2 :: Int -> Int
fac2 n = facAux n 1             (fac2.1)

fac :: Int -> Int
fac 0 = 1                       (fac.1)
fac n = n * fac (n-1)           (fac.2)

fac 0 = fac2   0                (base)
1     = fac2   0                (fac.1)
1     = facAux 0 1              (fac2.1)
1     =          1              (fAux.1)

fac n         = fac2   n        (ind)
n * fac (n-1) = fac2   n        (fac.2)
n * fac (n-1) = facAux n 1      (fac2.1)
n * fac (n-1) = facAux n 1      (fac2.1)