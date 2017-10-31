{-|
Module      : Main
Description : Dado um estado do jogo (mapa) faz a compressao e dado o estado "comprimido" faz a descompressão;
Copyright   : Filipe Monteiro (a80229@alunos.uminho.pt)//João Amaral (a80694@alunos.uminho.pt);

Esta última tarefa executa uma compressão do jogo (a decorrer ou não), tornando-a mais compacta para armazenamento.
Executa também a descompressão deste, retornando o estado do jogo tal como antes da compresso. Contém funções
definidas por nos, mas também algumas já existentes no Haskell.
-}

module Main where

import System.Environment
import Data.Char
import Text.Read
import Data.Maybe

main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          w <- getContents
          if length a == 1 && length p == 2 && (p=="-e" || p=="-d")
             then if p=="-e" then putStr $ encode $ lines w
                             else putStr $ unlines $ decode w
             else putStrLn "Parâmetros inválidos"


{-|
Trata-se de uma função que recebe um mapa, nomeadamente o que está a ser utilizado e calcula a sua dimensão a partir do primeiro elemento da lista, que se trata da primeira linha.

=Explicação da função:
    
    1. Recebe uma ["String"] que se trata do mapa em utilização.
    
    2. Recorre á função length e aplica-a á cabeça da lista pois para determinar a dimensão apenas necessitamos de um elemento da ["String"] (uma linha)

>>> ["###############","#    ?   ? ?  #","# # #?#?# #?# #","#?  ? ?       #","#?# #?#?#?# # #","# ?   ??      #","# # # #?#?#?#?#","#  ??     ? ? #","# # #?# # #?# #","#???       ?  #","# # #?# #?# #?#","#  ?     ?  ? #","# #?# #?# #?# #","#       ??    #","###############","+ 12 7","! 7 12","! 8 13","! 9 13","* 7 4 1 1 10","3 7 4","0 2 1"] 
 = 15  

 -}

calcdimensao :: [String] -> Int 
calcdimensao (x:xs) = length x 

{-|
Trata-se de uma função que comprime o estado atual do jogo.
Devido a existirem caratéres que são imõveis (como as paredes a.k.a '#') estes são instantanêamente eliminados do mapa.
Os espaços e os carateres especiais são substiuídos pelo caratér 'p'.
É de referencia também que pretendemos manter as coordenadas do mapa tal como estão, logo foi feita uma condição que diz que se o ultimo elemento de um dos elemntos da ["String"]
não for igual a '#' então ele vai devolver esse elemento e aplicar novamente a funçao recursivamente, separando as coordenadas umas das outras com uma barra.

== Explicação da função:

	1. É fornecido à função encode o mapa em causa, de seguida, é utilizada a funçao "calcdimensao" desse mesmo mapa e depois a função "show" para que o número que a função
calcdimensao nos fornece seja transformado em "String".

	2. De seguida adiciona uma '|' para que seja separado o valor da dimensao do mapa comprimido.

	3. Logo após á aplicação do número da dimensão e da barra, a função recorre a uma função auxiliar "auxencode1".

	4. A função "auxencode1" verifica se os elementos da ["String"] constituem o mapa em si, comparando o __último__ elemento desse elemento da ["String"] a um "#", se
este for igual então é aplicada a função "auxencode" que examina todos os elementos da "String" que pertence à ["String"]. Se este for "#" então são eliminados, se for igual a ' '
então é substituído por um char = 'e', se não verificar nenhum destes casos então será substituído por um char = 'c'.

	5. Se na função "auxencode1" não for verificada a condição de o último elemento de um elemento da ["String"] então a função vai devolver o elemento a ser avaliado	
pela função e adicionar uma "|" e após isto, fazer novamente a função "auxencode1".

	6. Foi feita esta separação pois queríamos manter as coordenadas do jogo tal e qual como estavam na ["String"].
-}

encode :: [String] -> String 
encode (h:t) = (show (calcdimensao (h:t))) ++ "|" ++ auxencode2 (auxencode1 (h:t)) 0

auxencode2 :: String -> Int -> String
auxencode2 [] _ = []
auxencode2 (x:xs) k | x=='e' = auxencode2 xs (k+1)
                    | x/= 'e' && k==0 = x:auxencode2 xs 0
                    | x/='e' = show k ++ x:auxencode2 xs 0
                    | otherwise = x:(auxencode2 xs k)

auxencode1 :: [String] -> String
auxencode1 [] = [] 
auxencode1 (x:xs) 
    | last x == '#' = auxencode x ++ auxencode1 xs
    | otherwise = "|" ++ x ++ auxencode1 xs  

auxencode :: String -> String 
auxencode [] = [] 
auxencode (x:xs) 
    | x=='#' = auxencode xs 
    | x==' ' = 'e' : auxencode xs 
    |otherwise = 'c' : auxencode xs 

-------------------------------------------------------------------
---------------__Funçoes Decode__----------------------------------
{-|
Pegando no resultado de __encode__, tranforma essa "String" do mapa num mapa igual ao original (__antes__ do encode)

@
Syntax --> __decode__ mapa = . . .
@
-}
decode :: String -> [String]
decode mapa = (juntaTotal (linhas n 0) exp_mapa) ++ init (coordsDownTotal coordenadas)
                where n=read (fromDigInt mapa)::Int
                      exp_mapa=(show n) ++ "|" ++ expandir (mapa_modIntermedio mapa)
                      coordenadas= mapa_modIntermedio (mapa_modInicial exp_mapa)
                      
{-|
Quando encontra um nº, __replica__ /esse nº de vezes/ /'e'/

Se encontrar um 'c', não faz nada

@
Syntax --> __expandir__ (encoded mapa) = . . .
@
-}
expandir :: String -> String
expandir [] = []
expandir [x] = replicate (read (x:[])::Int) 'e'
expandir (x:y:ys) | ord x>= 48 && ord x<=57 && ord y>= 48 && ord y<=57 = (replicate (read (x:y:[])::Int) 'e') ++ expandir ys
                  | ord x>= 48 && ord x<=57 = (replicate (read (x:[])::Int) 'e') ++ (expandir (y:ys))
                  | x=='|' = x:y:ys
                  | otherwise = x:(expandir (y:ys))

{-|
Na nossa função encode, no inicio da nova "String" tem um número e logo a seguir um "|" (para __separar__ a dimensão dos demais caracteres)

__fromDig_Int__ dá como resultado uma "String" com o número

==Nota

Existe uma função no ghci que apresenta um número presente numa "String" na forma de "Inteiro": __read ()::Int__, mas __apenas__
pode ter números dentro dessa "String", senão dá __erro__ (no nosso caso, a "String" tem números e caracteres) 

@
Syntax --> __fromDigInt__ (encoded mapa)
@
-}
fromDigInt :: String -> String
fromDigInt [] = []
fromDigInt [x] = if ord x>= 48 && ord x<=57
                    then x:""
                    else ""
fromDigInt (x:y:ys) = if y=='|'
                            then x:""
                            else (x:"") ++ fromDigInt (y:ys)
{-|
Função com a base da função __linhas__ da "Tarefa 1", mas com uma mudança nos primeiros caracteres da primeira, segunda,
penultima e última linhas: para as funções, ao usarem o resultado desta função, 
nao usarem os espaços __especiais"__ (onde __não__ pode ter nada), o resuldao ada __linhas__ será uma ["String"], onde, por exemplo:

>>> linha 1 --> "#nnrrrnn#" , n==espaço especial       r==espaço a ser substituido

==Nota

@
Temos um caso especial para quando __n==5__, pelos motivos apresentados na Tarefa 1
@

@
Syntax --> __linhas__ n 0 = . . .
@
-}

linhas :: Int -> Int -> [String]
linhas 5 _ = ["#####","#   #","# # #","#   #","#####"]
linhas 0 c = []    -- c=contador
linhas n c   | c == 0 = (replicate n '#'):linhas n (c+1)                                -- linha0
             | c == 1 = ["#nn" ++ (replicate  (n-6) 'r') ++ "nn#"] ++ linhas n (c+1)      -- linha1
             | c == 2 = ["#n" ++ (init (concat (replicate ((div n 2) - 1) ("#r")))) ++ "n#"] ++ linhas n (c+1)
             | (c < (n-3)) && ((mod c 2) /= 0) = ["#" ++ (replicate (n-2) 'r') ++ "#"] ++ linhas n (c+1)            -- linhas intermedias
             | (c < (n-3)) && ((mod c 2) == 0) = ["#" ++ (init (concat (replicate (div n 2) ("r#")))) ++ "#"] ++ linhas n (c+1)    -- linhas intermedias
             | c == (n-3) = ["#n" ++ (init (concat (replicate ((div n 2) - 1) ("#r")))) ++ "n#"] ++ linhas n (c+1)  
             | c == (n-2) = ["#nn" ++ (replicate  (n-6) 'r') ++ "nn#"] ++ linhas n (c+1)    
             | c == (n-1) = [replicate n '#'] 


{-|
Pegando numa /linha do mapa/ e na /string do encode/, substitui cada caracter com "r" ou "n" da __linha__
por uma letra da __string do encode__ (e=" " && c="?" )

== Exemplo
>>> juntaParte "#  rrrrrrrrr  # "eeeeepeeeepeeeepeeeepeepeeepeppe" = "#  eeeeepeee  #"

@
Syntax --> __juntaParte__ list = . . .
@
-}
juntaParte :: String -> String -> String
juntaParte [] _ = []
juntaParte (x:xs) [] = (x:xs)
juntaParte (x:xs) (y:ys) | x=='r' && y=='e' = " " ++ (juntaParte xs ys)
                    | x=='r' && y=='c' = '?':(juntaParte xs ys)
                    | x=='n' = " " ++ (juntaParte xs ys)
                    | x/='r' = x:(juntaParte xs (y:ys))


{-|
Faz o mesmo que __juntaParte__, mas aplicado a um mapa (["String"]) usando a __juntaParte__

>>> Exemplo: juntaTotal ["#  rrrrrrrrr  #","#r#r#r#r#r#r#r#"] "eeeeepeeeepeeeepeeeepeepeeepep" = ["#  eeeeepeee  #","#e#p#e#e#e#e#e#"]

@
Syntax --> __juntaTotal__ list (encoded mapa) = . . .
@
-}
juntaTotal :: [String] -> String -> [String]
juntaTotal (x:xs) [] = (x:xs)
juntaTotal [] _ = []
juntaTotal (x:xs) mapa = (juntaParte x mapa_letras):(juntaTotal xs left_mapa)
                            where left_mapa=drop (contagem x) mapa_letras
                                  mapa_letras=mapa_modInicial mapa

{-|
Esta e a próxima têm efeitos parecidos, apenas com algumas mudanças devidos a condições diferentes. 
Ambas apresentam uma "String" sem __tudo__ o que está até a um "|".

-}
mapa_modInicial :: String -> String
mapa_modInicial [] = []
mapa_modInicial (x:xs) = if ord x>=48 && ord x<=57 || x=='|'
                    then mapa_modInicial xs
                    else (x:xs)

mapa_modIntermedio :: String -> String
mapa_modIntermedio [] = []
mapa_modIntermedio (x:xs) = if x=='|'
                                then xs
                                else mapa_modIntermedio xs

{-|
Conta o nº de caracteres para serem substituidos existentes naquela linha
para depois retirar esse nº de caracteres da "String" do encode

Serão __"r"__ e __"n"__

-}
contagem :: String -> Int
contagem [] = 0
contagem (x:xs) = if x=='r' || x=='n'
                    then 1 + contagem xs
                    else contagem xs
{-|
Pega na "String" do encode e apresenta apenas a parte desta
com as coordenadas de /powerups/, /bombas/ e /players/

>>> = "|+ 3 3|! 5 5|* 7 7 1 1 10|0 4 3 +|1 7 7"
-}
coordsDownParte :: String -> String
coordsDownParte [] = []
coordsDownParte (x:xs) = if x=='|'
                            then []
                            else x:coordsDownParte xs
{-|
Pegando no resultado de __coordsDownParte__ e dá como resultado uma
lista de "String" com cada coordenada
-}
coordsDownTotal :: String -> [String]
coordsDownTotal [] = [[]]
coordsDownTotal coords = linha:(coordsDownTotal coords_left)
                            where linha=coordsDownParte coords 
                                  coords_left=drop ((length linha)+1) coords        -- (length linha)+1 pois temos de contar com o caracter '|'


{-|
=Exemplo:
    1. Mapa: ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"]  
    2. length (concat mapa) = 115 
    3. encode mapa = "9|eeeeeeeeceeeeeeececeececeeceeecceeecceee|+ 3 3|! 5 5|* 7 7 1 1 10|0 4 3 +|1 7 7"
    4. length (encode mapa) = 81 
    5. Redução = ((81 / 115) * 100) - 100 = 30% (aproximadamente)
-}
-- | Mais exemplos de teste
ex = ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"]
ex2 = ["#####","#   #","# # #","#   #","#####"]
ex3 = ["#####","#   #","# # #","#   #","#####","0 1 1","1 3 3"]