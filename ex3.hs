--Aluno: Guilherme Saulo Alves e Mateus Lopes Teixeira
--Data: 21/03/2014

-- Ex 8.12 Define a function putNtimes :: Integer -> String -> IO() so that the effect of putNtimes n str is to output str, a string, n times, one per line.

putNtimes :: Integer -> String -> IO()
putNtimes n str = 
	   if n > 0 
	   then do print str
 	           putNtimes (n-1) str --chama função recursivamente
 	   else return ()

-- Ex 8.13 Write an I/O program which will first read a positive integer, n say, and then read n integers and write their sum. 
--		   The program should prompt appropriately for its inputs and explain its output.


getInt :: IO Integer
getInt = do	line <- getLine
         	return (read line :: Integer)

addNnumbers :: IO ()
addNnumbers = do
        putStr "Digite o numero de vezes: "
        nVezes <- getInt
        soma <- soma_os_numeros nVezes
        print soma
    	where soma_os_numeros n = do
            			if n <= 0 then
               				return 0
            			else do	
            				putStr "Digite um numero inteiro: "
                			num <- getInt
                			aux <- soma_os_numeros (n-1) 
                			return (num + aux)

-- Ex 8.14 Define a wc function which copies input to output until an empty line is read.
--		   The program should then output the number of lines, words and characters that have been copied.
--		   wc is a standard unix command line program.

wc :: IO ()
wc = wc' 0 0 0 -- Inicia contadores como 0

wc' :: Int -> Int -> Int -> IO ()
wc' l p c = -- contadores l=numero de linhas, p= numero de palavra, c=numero de caracteres
     do line <- getLine
	if line == ""
	   then	putStrLn ("numero de linhas: " ++ show l ++ "\nnumero de palavras: " ++show p ++ "\nnumero de caracteres: " ++ show c )
	   else do putStrLn line 
		   wc' (l + 1) (p + cntwords line) (c + cntchars line) --incrementa os contadores

cntwords str = length $ words str -- retorna a quantidade de palavras que contem na string
cntchars str = length str --retorna a quatidade de caracteres existente na string

-- Ex 8.19 Explain the behaviour of this copy program, where the definition of whileCopy is modelled on a while loop in a traditional programming language.

copy :: IO ()
copy =
    do
        line <- getLine
        let whileCopy = do
        	--line <- getLine
                if (line == "")
                then return ()
                else do
                    putStrLn line
                    line <- getLine
                    whileCopy
        whileCopy

--	A função copy entra em um loop infinito, pois no final da função (caso ""), whileCopy é novamente chamada de forma recursiva (Linha 68), 
--	sendo que "line" é redefinida para o valor que tinha fora da definição (Linha 59). Uma solução seria colocar a nova linha dentro do whileCopy (Linha 61). 