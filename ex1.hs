module Ex1 where

import Pictures

-- Alunos: Guilherme Saulo e Mateus Lopes

-- Exercicio 3.20) 	Escreva definição de função que retorna a média de tres inteiros: 
--					averageThree:: Integer -> Integer -> Integer -> Float 
-- 					Usando averageThree, defina função howManyAboveAverage :: Integer -> Integer -> Integer -> Integer, 
--					que retorna quantos valores da entrada são maiores que a média

averageThree :: Integer -> Integer -> Integer -> Float
averageThree a b c = fromInteger(a + b + c) / 3

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage d e f = toInteger((fromEnum(fromInteger d > averageThree d e f)) + fromEnum(fromInteger e > averageThree d e f) + fromEnum(fromInteger f > averageThree d e f))

	
-- Exercicio 4.30) 	Escreva uma definição recursiva de função: chessBoard :: Integer -> Picture 
-- 					definida de modo que chessBoard n = ... chessBoard (n-1) ...
-- 					Usar printPicture(chessBoard n) para imprimir corretamente

blackLine, whiteLine:: Integer -> Picture

blackLine n
	| n==1	= black
	| n>1	= black `beside` whiteLine (n-1)

whiteLine n
	| n==1  = white
	| n>1  = white `beside` blackLine (n-1)

blackChess, whiteChess:: Integer -> Integer -> Picture

blackChess n m
	| n==1	 = blackLine m
	| n>1	 = blackLine m `above` whiteChess(n-1) m

whiteChess n m
	| n==1	 = whiteLine m
	| n>1	 = whiteLine m `above` blackChess(n-1) m
	
chessBoard :: Integer -> Picture

chessBoard n = blackChess n n
	
-- Exercicio 4.32) 	Se n é par, 2^n = 2^(2*k) = (2^k)^2
--					Se n é ímpar, 2^n = 2^(2*k+1) = 2^k * 2
--					Defina função de exponenciação baseado nisso.

funcAux :: Integer -> Integer
pot2 :: Integer -> Integer

funcAux n
	|n==0 = 1
	|otherwise = 4 * funcAux(n-1)
	
pot2 n
	|even n==True = funcAux(n)
	|otherwise = 2 * funcAux(n)
	