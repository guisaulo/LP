-- Aluno: Guilherme Saulo
-- Aluno: Mateus Lopes

import Data.List

--Ex 10.6 Dê definições de funções para fazer uma lista de inteiros, ns, e  
-- a) Retorna uma lista com os quadrados dos inteiros em ns;

quadrado :: [Integer] -> [Integer]
quadrado x = map (^2) x

-- b) Retorna a soma dos quadrados dos itens ns; 
somaDosQuadrados :: [Integer] -> Integer
somaDosQuadrados x = sum (quadrado x)

-- c) Verifique se todos os itens da lista são maiores do que zero.
maiorQueZero :: [Integer] -> Bool
maiorQueZero xs = and (map (>0) xs)

-- Ex 10.7 Usando as funções já definidas, sempre que possível, escreva as definições de funções para:
-- a) Dar o valor mínimo de uma função f em entradas de 0 a n; 

valorMinimo :: (Integer -> Integer) -> Integer -> Integer
valorMinimo f n = minimum (map f [0..n])

-- b) Testar se os valores de f de entrada 0 para n são todos iguais;
iguais :: (Integer -> Integer) -> Integer -> Bool
iguais f n = todosIguais (map f [0..n])
    where todosIguais []      = True
          todosIguais (x:xs)  = and (map (==x) xs)

-- c) Testar se todos os valores de f de entrada 0 para n é maior do que zero;
maioresQueZero :: (Integer->Integer) -> Integer -> Bool
maioresQueZero f n = ehMaior (map f [0..n])
    where ehMaior []  = True
          ehMaior x   = and (map (>0) x)

-- d) Verificar se os valores f 0, f 1 para f n estão em ordem crescente.
ordemCrescente :: (Integer->Integer) -> Integer -> Bool
ordemCrescente f n = (map f [0..n]) == sort (map f [0..n])

-- Ex 10.15 Para os efeitos deste exercício você deve usar foldr nas funçoes definidas unZip, last e init, exemplos: 
-- last "Greggery Pecari" = "y" '
-- init "Greggery Pecari" = "Greggery Pecari"...

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' xs = (foldr (:) [] (map fst xs), foldr (:) [] (map snd xs) )
 
last':: [a] -> a
last' n = foldr1 obtemSegundo n
      where obtemSegundo a b = b
 
init':: [a] -> [a]
init' ls = take ((length ls)-1) (foldr (:) [] ls)
