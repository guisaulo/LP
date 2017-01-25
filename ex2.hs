module Ex2 where
import Data.Char  -- para função toLower(para minuscula)


--	Alunos: Guilherme Saulo Alves e Mateus Lopes  
-- Data: 08/03/2014										  

--	Exercício 5.7	Add an Extra constructor to Shape for triangles, and extend the functions isRound, area and perimeter to include triangles

data Shape = Circle Float |	Rectangle Float Float | Triangle Float Float Float deriving (Eq,Ord,Show,Read)

shape1 = Circle 3.0
shape2 = Rectangle 45.9 87.6
shape3 = Triangle 8.2 13.1 15.0

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False
isRound (Triangle _ _ _) = False

area :: Shape -> Float
area (Circle r)	= pi*r*r
area (Rectangle h w) = h*w
area (Triangle a b c) = sqrt((perimeter(Triangle a b c)/2)*((perimeter(Triangle a b c)/2)-a)*((perimeter(Triangle a b c)/2)-b)*((perimeter(Triangle a b c)/2)-c))
--	Usado Teorema de Heron

perimeter :: Shape -> Float
perimeter (Circle r) = 2*pi*r
perimeter (Rectangle h w) = 2*(h+w)
perimeter (Triangle a b c)= a+b+c

--	Exercício 5.8	Define a function which decides whether a Shape is regular: a circle is regular, a square is a regular rectangle and being equilateral makes a triangle regular

isRegular :: Shape -> Bool
isRegular (Circle r) = True
isRegular (Rectangle h w)
	|h==w = True
	|otherwise = False
isRegular (Triangle a b c)
	|a==b && b==c && c==a = True
	|otherwise = False

--	Exercício 5.20	Define the function
--					divisors :: Integer -> [Integer]
--					which returns the list of divisors of a positive integer (and the empty list for other inputs).
--					For instance, divisors 12 -> [1,2,3,4,6,12]
--					A prime number n is a number whose only divisors are 1 and n. Using divisors or otherwise define a function
--					isPrime :: Integer -> Bool

divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..n], mod n x==0] -- comprensao de listas

isPrime :: Integer -> Bool
isPrime n = divisors n == [1,n]

--	Exercício 5.21	Define the function
--					matches :: Integer -> [Integer] -> [Integer]
--					which picks out all occurrences of an integer n in a list. For instance,
--					matches 1 [1,2,1,4,5,1] -> [1,1,1]
--					matches 1 [2,3,4,6] -> []
--					Using matches or otherwise, define a fuction
--					elem :: Integer -> [Integer] -> Bool
--					which is True if the Integer is an element of the list, and False otherwise.
--					For the examples above, we have
--					elem 1 [1,2,1,4,5,1] -> True
--					elem 1 [2,3,4,6] ->False
--					Since elem is a prelude function, you need to hide it as described on page 53

matches :: Integer -> [Integer] -> [Integer]
matches n list = [x | x <- list, x==n]

elem' :: Integer -> [Integer] -> Bool
elem' n list = matches n list /= [] --se a lista retornada conter itens, retorna True

--	Exercício 5.26	Define a function
--					fibTable :: Integer -> String
--					which produces a table of Fibonacci numbers. For instance, the effect of putStr(fibTable 6) should be
--					n		fib n
--					0			0
--					1			1
--					2			1
--					3			2
--					4			3
--					5			5
--					6			8

onSeparateLines :: [String] -> String	--recebe a lista com string e serara por linha
onSeparateLines list = [y | x <- list, y <- x ++ "\n"]

pushRight :: String -> Integer -> String
pushRight x espacamento
    | n >= espacamento = x	--caso base, quando chegou no limite de espacamento
    | otherwise = [' ' | n <- [1 .. espacamento - n] ] ++ x  -- imprime a string com o espacamento adequado
	where n = toInteger (length x) -- transforma oo tamanho da string em um numero inteiro

fib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise =  fib (n - 1) + fib (n - 2)

fibTable :: Integer -> String
fibTable n = onSeparateLines $ ["n" ++ pushRight "fib n" 10] ++ [(show i) ++ (pushRight (show (fib i)) 10) | i <- [1..n]] -- grande lista com todos dados da tabela em strings
-- *** usar putStr(fibTable 6)

--	Exercício 7.6	Define the function
--					and, or :: [Bool] -> Bool
--					which gives the conjunction and disjuction of a list of Booleans. For instance, 
--					and [False,True]=False
--					or [False,True]=True
--					On an empty list and gives True and or gives False; explain the reason for these choices.

and', or' :: [Bool] -> Bool

and' (x:y) = x && and' y
and' [] = True

or' (x:y) = x || or' y
or' [] = False

--	Exercício 7.9	Define a function
--					unique :: [Integer]->[Integer]
--					so that unique xs returns the list of elements of xs wich occur exactly once. 
--					For example, unique [4,2,1,3,2,3]is[4,1].
--					You might like to think of two solutions to this problem: one using list comprehension and the other not

elemNum' :: Integer -> [Integer] -> Integer
elemNum' x xs = toInteger(length [y | y <- xs, y == x])

unique' :: [Integer] -> [Integer]
unique' [] = []
unique' [x] = [x]
unique' (x:xs)
    | elemNum' x xs == 0  = x : unique' xs
    | otherwise = unique' xs

-- Usando Comprensão de Lista

unique'' :: [Integer] -> [Integer]
unique'' xs = [x | x <- xs, elemNum' x xs == 1]


--	Exercício 7.19	By modifying the definition of the ins and iSort functions, define a function to sort lists of pairs of numbers. 
--					The ordering should be lexicographic - the dictionary ordering. This ordering first looks at the first halves of the pairs;
--					Only if these values are equal are the second halves compared. For instance, (2, 73) is smaller than (3,0), and this is smaller than (3,2).

iSortPairs :: [(Integer, Integer)] -> [(Integer, Integer)]
iSortPairs [] = []
iSortPairs ((x, y) : xs) = insPair (x, y) (iSortPairs xs)


insPair :: (Integer, Integer) -> [(Integer, Integer)] -> [(Integer, Integer)]
insPair (x, y) [] = [(x, y)]
insPair (x, y) ((x1, y1):ys)
    | x < x1 || ( x == x1 && y <= y1)  = (x, y) : ((x1, y1) : ys)
    | otherwise                        = (x1, y1) : insPair (x, y) ys

--	Exercício 7.25	One list is a sublist of another if the elements of the first occur in the second, in the same order. 
--					For instance, "ship" is a sub list of "Fish & Chips", but not of "hippies".

isSubList :: Eq a => [a] -> [a] -> Bool
isSubList [] _ = True
isSubList _ [] = False
isSubList (x:xs) (y:ys)
    | x == y     = isSubList xs ys
    | otherwise  = isSubList (x:xs) ys

isSubSequence :: Eq a => [a] -> [a] -> Bool
isSubSequence [] _ = True
isSubSequence _ [] = False

-- Note: can't really use a guard here because we want both parts of the 'or' condition to
-- be evaluated sometimes.

isSubSequence (x:xs) (y:ys) = (x == y && isSubSequence xs ys) || (isSubSequence (x:xs) ys)

--	Exercício 7.33	Define a function
--					isPalin :: String -> Bool
--					which tests whether a string is a palindrome - that is whether it is the same read both backwards and forwards, 
--					and only afterwards take account of punctuation and capital letters.
--					Madam I'm Adam
--					Note that punctuation and white space are ignored in the test, and that no distinction is made between capital and small letters. 	
--					You might first like to develop a test which simply tests whether the string is exactly the same backwards and forwards, 
--					and only afterwards take account of punctuation and capital letters.

isPalin :: String -> Bool
isPalin x = palavra == reverse palavra --retorna true or false
	where 
	palavra = [x | x <- [toLower y | y <- x], isAlpha x] -- transforma uma palavra em minuscula(toLower) e filtra suas pontuacoes(isAlpha)
