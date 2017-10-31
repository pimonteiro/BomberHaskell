{-|
Module      : Main
Description : Dando uma dimensao e uma semente geradora de numeros random, apresenta um tabuleiro de um jogo;
Copyright   : Filipe Monteiro (a80229@alunos.uminho.pt)//João Amaral (a80694@alunos.uminho.pt);

Esta primeira de três tarefas permite a criação de um tabuleiro de dimensão e variada jogabilidade
definida pelo utilizador. Usa funções recursivas definidas por nós, mas também algumas ja existentes
no Haskell.
-}


module Tarefa1 where

import System.Environment
import Text.Read
import Data.Maybe
import System.Random



main :: IO ()
main = do a <- getArgs
          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
             then putStr $ unlines $ mapa (fromJust s) (fromJust l)
             else putStrLn "Parâmetros inválidos"


{-| 
#  -> representa pedra

?  -> representa tijolo

'' -> representa célula vazia

'+' representa bombs

'!' representa flames

x-coluna y-linha

Para fim de testes, usemos putStr (unlines (mapa n s)) no GHCi para visualizar o resultado.
-}
mapa :: Int -> Int -> [String]  -- s=semente   n=dimensao
mapa n s = ret_Esp_Mapa (auxMapa (linhas n 0)) ++ coordenadas (auxMapa (linhas n 0))
                 where auxMapa :: [String] -> [String]
                       auxMapa [] = []
                       auxMapa (x:xs) = auxMapa2 (x:xs) rnd           
                                         where rnd = take (n*n) $ randomRs (0,99) (mkStdGen s)
{-|
A função __auxMapa2__ serve de auxiliar á função __coordenadas__
Esta coloca os valores da lista Random no mapa.

  1. /rnd/ será a lista dos numeros random

  2. fazemos __insEsp x__ porque como (x:xs) = [String], o "x" será um [Char] (uma linha)
-}
auxMapa2:: [String] -> [Int] -> [String]
auxMapa2 [] _ = []
auxMapa2 (x:xs) rnd = (insEsp x rnd):(auxMapa2 xs left_rnd)           
                where left_rnd = drop (contagem x) rnd                
                      insEsp :: [Char] -> [Int] -> [Char]
                      insEsp [] _ = []                               
                      insEsp (x:xs) (y:ys) | x/='r' = x:(insEsp xs (y:ys))
                                           | (x=='r') && (y==0 || y==1) = '+':(insEsp xs ys)
                                           | (x=='r') && (y==2 || y==3) = '!':(insEsp xs ys)
                                           | (x=='r') && (y>=4 && y<=39) = '?':(insEsp xs ys)
                                           | (x=='r') && (y>=40 && y<=99) = ' ':(insEsp xs ys)

{-|

  1. __ret_Esp_Mapa__ --> esconde os powerUps que se encontram no mapa, isto porque estes 
     estão sempre atrás de um bloco de tijolos __'?'__

  2. __map (\x -> if (x=='+' || x=='!') then '?' else x) (x:xs)__ --> uma função que, se verificar x=='+' ou x=='!'
                                                                      substitui esse __x__ por um __'?'__

  A razão pela qual tivemos que fazer um debug desta função, foi porque nós precisavamos visualizar
  o resultado com os powerUps visíveis para calcular as coordenadas manualmente.
-}

ret_Esp_Mapa :: [String] -> [String]
ret_Esp_Mapa [] = [] 
ret_Esp_Mapa (x:xs) = (aux_ret_Esp_Mapa x):ret_Esp_Mapa xs
            where aux_ret_Esp_Mapa :: [Char] -> [Char]
                  aux_ret_Esp_Mapa [] = []
                  aux_ret_Esp_Mapa (x:xs) = map (\x -> if (x=='+' || x=='!') then '?' else x) (x:xs)
{-| Conta o número de posições onde deverão ser colocados os valores do __rnd__, 
    isto para depois retirar deste os valores que já foram colocados.
-}

contagem :: [Char] -> Int
contagem [] = 0
contagem (x:xs) = if x=='r'
                    then 1 + contagem xs
                    else contagem xs



{-| 
Gerador do mapa com apenas as __paredes (#)__. Nos espaços onde será colocado um __nº random__,
 estará um __'r'__ para mais tarde /substituir/

1. __linhas__ --> cria a lista com as linhas do mapa.   
            
2. __(replicate (n-6) 'r')__ --> apenas usada na primeira linha (linha 0 no Haskell) e na penultima linha (linha (n-1) no Haskell
                                 porque estas precisam de ter as linha 1 e (n-2) com dois lugares vazios no inicio e fim de cada
                                 respetiva linha, para o player conseguir se mover no inicio do jogo.

A linha(n-1) é a ultima linha
Terá sempre (n-1) linhas pois a linha /1/ equivale á posição real /0/

A razão pela qual definimos dois casos especiais para quando __n=5__, foi porque este é o caso mais pequeno possivel
e o método de criação das linhas exige que na /linha1/ e /linha(n-2)/ (segunda e penúltima respetivamente)
tenham __sempre "6 espaços"__ já ocupados para podermos comparar com a dimensão, para ver quantas vezes repetimos
o caracter __'r'__ (para futura substituição por um nº do /rnd/) nestas linhas. Se __n=5__, como __/n-6=-1/__, que
por sua vez /-1<0/, a função iria adicionar __"-1" vezes 'r'__ o que causa um erro.

-}

linhas :: Int -> Int -> [String]
linhas 0 c = []    -- c=contador
linhas n c   | c == 0 = (replicate n '#'):linhas n (c+1)                                -- linha0
             | c == 1 && (n-6)<=0 = ["#   #"] ++ linhas n (c+1)      -- linha1 especial (caso n=5)
             | c == 1 = ["#  " ++ (replicate  (n-6) 'r') ++ "  #"] ++ linhas n (c+1)      -- linha1
             | c == 2 = ["# " ++ (init (concat (replicate ((div n 2) - 1) ("#r")))) ++ " #"] ++ linhas n (c+1) 
             | (c < (n-3)) && ((mod c 2) /= 0) = ["#" ++ (replicate (n-2) 'r') ++ "#"] ++ linhas n (c+1)            -- linhas intermedias
             | (c < (n-3)) && ((mod c 2) == 0) = ["#" ++ (init (concat (replicate (div n 2) ("r#")))) ++ "#"] ++ linhas n (c+1)    -- linhas intermedias
             | c == (n-3) = ["# " ++ (init (concat (replicate ((div n 2) - 1) ("#r")))) ++ " #"] ++ linhas n (c+1)
             | c == (n-2) && (n-6)<=0 = ["#   #"] ++ linhas n (c+1)  --linha(n-2) especial (caso n=5)
             | c == (n-2) = ["#  " ++ (replicate  (n-6) 'r') ++ "  #"] ++ linhas n (c+1)  --linha(n-2)
             | c == (n-1) = [replicate n '#'] --linha(n-1)


{-|
Junta, com o auxilo da __aux__ (B/F) as coordenadas de cada um dos caracteres
para isto, esta usa a função __zip__ já predefinida no Haskell em cada lista de caracteres
para criar uma lista de tuplos (coordenadas de todos os caracteres)

@
Funçao que faz uma lista das linhas onde se encontram os powerups --> __linhaPower__
Depois uma funçao que da a lista das colunas onde os powerups se encontram --> __colunaPower__
@

__contAux__ serve para contar o número de vezes que um determinado powerUp se encontra numa linha
pois pode haver mais que um powerUp na mesma linha, assim como na coluna (__auxcoluna__)

-}

coordenadas :: [String] -> [String]
coordenadas [] = []
coordenadas l = auxB (zip (colunaPower '+' l) (linhaPower 0 '+' l)) ++ auxF (zip (colunaPower '!' l) (linhaPower 0 '!' l))

auxB :: [(Int,Int)] -> [String]
auxB [] = []
auxB ((x,y):t) = ["+ " ++ (show x) ++ " " ++ (show y)] ++ auxB t

auxF :: [(Int,Int)] -> [String]
auxF [] = []
auxF ((x,y):t) = ["! " ++ (show x) ++ " " ++ (show y)] ++ auxF t

linhaPower :: Int -> Char -> [String] -> [Int]
linhaPower a _ [] = [] 
linhaPower a b (x:xs) = if elem b x
                            then (replicate (contAux b x) a) ++ linhaPower (a+1) b xs
                            else linhaPower (a+1) b xs

contAux :: Char -> [Char] -> Int
contAux b [] = 0
contAux b (x:xs) = if b==x
                    then 1 + contAux b xs
                    else contAux b xs

colunaPower :: Char -> [String] -> [Int]
colunaPower b [] = [] 
colunaPower b (x:xs) | elem b x = (auxcoluna 0 b x) ++ colunaPower b xs
                     | otherwise = colunaPower b xs 

auxcoluna :: Int -> Char -> String -> [Int]  
auxcoluna a b [] = [] 
auxcoluna a b (x:xs) | b == x = a : auxcoluna (a+1) b xs 
                     | otherwise = auxcoluna (a+1) b xs

{-|
==__Debug Area__

Esta parte do código daqui pra frente serve para resolver bugs existentes no código.

As funçoes fazem quase o mesmo que as suas correspondentes, apenas possuem uma leves alteraçoes:

    1. __mapaDebug__ --> tem o mesmo efeito da função __mapa__, mas em vez de apresentar o resultado com os powerUps
                         escondidos, mostra onde estes estão realmente. Esta funçao serviu para determinar se esta estava
                         a produzir as coordenadas corretas.

-}

mapaDebug :: Int -> Int -> [String]  --s=__semente__   n=__dimensao__
mapaDebug n s = auxMapa (linhas n 0) ++ coordenadas (auxMapa (linhas n s))
    where auxMapa :: [String] -> [String]
          auxMapa [] = []
          auxMapa (x:xs) = auxMapa2 (x:xs) rnd    --"rnd" será a lista dos numeros random
                            where rnd = take (n*n) $ randomRs (0,99) (mkStdGen s)