module PedraPapelTesoura where

import Control.Monad (liftM)

data Mão = Pedra | Papel | Tesoura
  deriving Eq

instance Show Mão where
      show Pedra   = "*"
      show Papel   = "."
      show Tesoura = "x"

-- Converte 0,1,2 em valor Mov
int_Mão :: Integer -> Mão
int_Mão 0 = Pedra
int_Mão 1 = Papel
int_Mão 2 = Tesoura

-- Converte caractere em valor Mov  
ch_Mão :: Char -> Mão
ch_Mão '*' = Pedra
ch_Mão '.' = Papel
ch_Mão 'x' = Tesoura

-- Resultado de uma mão
--   +1: vence primeiro jogador
--   -1: vence segundo jogador
--    0: empate

res :: Mão -> Mão -> Integer
res Pedra Tesoura = 1
res Tesoura Papel = 1
res Papel Pedra   = 1
res a b
  | a == b    = 0
  | otherwise = -1

type Jogo = ([Mão],[Mão])

resultJogo :: Jogo -> (Integer, Integer, Int, Int, Integer)
resultJogo jogo = (vitorias, derrotas, empates, length resultados, sum resultados)
  where resultados = map (uncurry res) . uncurry zip $ jogo
        vitorias   = sum . filter (>0) $ resultados
        derrotas   = abs . sum . filter (<0) $ resultados
        empates    = length . filter (==0) $ resultados

-- Mov que ganha / perde de dado valor Mov.
ganha, perde :: Mão -> Mão

ganha Pedra   = Papel
ganha Papel   = Tesoura
ganha Tesoura = Pedra

perde Pedra   = Tesoura
perde Papel   = Pedra
perde Tesoura = Papel

-- Estratégias de jogo

type Estratégia = [Mão] -> Mão

constante :: Mão -> Estratégia
constante x _ = x

pedra, papel, tesoura :: Estratégia
pedra   = constante Pedra
papel   = constante Papel
tesoura = constante Tesoura

ciclo :: Estratégia
ciclo mãos = case length mãos `rem` 3 of 
               0 -> Pedra
               1 -> Papel
               2 -> Tesoura

-- Joga o que teria feito ganhar da última jogada do oponente (se existir)
ganhaAnt :: Estratégia
ganhaAnt (ult: _) = ganha ult

eco :: Estratégia
eco (mão:_) = mão

--- Jogo Pedra - Papel - Tesoura
--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

jogoEstraytégiaContraEstratégia :: Estratégia -> Estratégia -> Jogo -> Jogo
jogoEstraytégiaContraEstratégia estratégiaA estratégiaB (mãosA, mãosB)
     = ( estratégiaA mãosB : mãosA, estratégiaB mãosA : mãosB)

jogo :: Estratégia -> IO ()
jogo estrat = jogoInterativo estrat ([],[])

jogoInterativo :: Estratégia -> Jogo -> IO ()
jogoInterativo estratégia (minhasJogadas,suasJogadas) =
    do 
      putStr ("\nDigite * (pedra), . (papel), ou x (tesoura): ")
      ch <- liftM (\ s -> if null s then ' ' else head s) getLine 
      -- mesmo que: ch <- do {s <- getLine; return (if null s then ' ' else head s)}
      if not (ch `elem` "*.x") 
        then mostraResultado (minhasJogadas,suasJogadas)
        else do let minhasJogadas' = ch_Mão ch : minhasJogadas
                    suaJogada = estratégia minhasJogadas' 
                jogoInterativo estratégia (minhasJogadas', suaJogada : suasJogadas)


mostraResultado :: Jogo -> IO ()
mostraResultado jogo = 
    do
      let minhasJogadas = reverse $ fst jogo
          suasJogadas   = reverse $ snd jogo
      putStrLn ("Minhas jogadas: " ++ show minhasJogadas)
      putStrLn ("Suas   jogadas: " ++ show suasJogadas)
      let (vitorias, derrotas, empates, numJogadas, res) = resultJogo (minhasJogadas,suasJogadas)
      putStrLn ("Houve " ++ show numJogadas ++ " jogada" ++ if numJogadas > 1 then "s" else "")
      putStrLn ("Venci " ++ show vitorias ++ " vez" ++ if vitorias /= 1 then "es" else "")
      putStrLn ("Perdi " ++ show derrotas ++ " vez" ++ if derrotas /= 1 then "es" else "")
      putStrLn ("Houve " ++ show empates  ++ " empate" ++ if empates /= 1 then "s" else "")
      putStrLn (case compare res 0 of
                  GT -> "Venci!"
                  EQ -> "Empate!"
                  LT -> "Você venceu!")
      
             
