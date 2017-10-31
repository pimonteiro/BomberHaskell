{-|
Module      : Main;
Description : Permite a passagem do tempo no jogo, através da função avanca;
Copyright   : Filipe Monteiro (a80229@alunos.uminho.pt)//João Amaral (a80694@alunos.uminho.pt);

Esta tarefa permite que o temporizador das bombas cheguem a zero, explodindo, assim como o "efeito espiral"
no mapa quando o tempo chega a um determinado valor. 
-}

module Main where

import Data.Char 
import Data.List
import System.Environment
import Text.Read
import Data.Maybe

type Mapa = [String]
type Coord = (Int,Int)

{-|
Funções responsáveis para /organizar/ os tipos de dados __internos__, facilitando assim o seu desenvolvimento.

---------------------------------------------__PARSE__-----------------------------------

Transforma o tipo de dados externo para tipo interno (escolhido por nós).

-}
organiza :: [String] -> ([String],[String],[String],[String])
organiza [] = ([],[],[],[])
organiza mapa = (so_mapa mapa,so_powers mapa,procura_bomba mapa,so_players mapa)

{-|
A função __so_mapa__ é aplicada a um mapa inteiro com PowerUps, jogadores e bombas mas apenas devolve a parte do mapa onde decorre o jogo sem informações adicionais.
Função recursiva que quando vê uma String que não começa por '#' elimina-a.
-}
so_mapa :: [String] -> [String]
so_mapa [] = []
so_mapa (x:xs) = if head x=='#'
                    then x:(so_mapa xs)
                    else so_mapa xs
{-|
Função que recebe um mapa e devolve apenas as informaçôes que dizem respeito a PowerUps.
-}
so_powers :: [String] -> [String]
so_powers [] = []
so_powers (x:xs) = if head x=='+' || head x=='!'
                        then x:(so_powers xs)
                        else so_powers xs
{-|
Função que recebe um mapa e devolve apenas as informaçôes que dizem respeito a Bombas.
-}                       
procura_bomba :: [String] -> [String]
procura_bomba [] = []
procura_bomba (x:xs) = if head x== '*'
                        then x:(procura_bomba xs)
                        else procura_bomba xs
{-|
Função que recebe um mapa e devolve apenas as informaçôes que dizem respeito a Jogadores.
-}
so_players :: [String] -> [String]
so_players [] = []
so_players (x:xs) = if head x=='0' || head x=='1' || head x=='2' || head x=='3'
                      then x:(so_players xs)
                      else so_players xs
{-|
Secção da /main/ da tarefa.

---------------------------------------------------------------------------------
-}
main :: IO ()
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"

-- | Função __avanca__ que permite a passagem do tempo.
avanca :: [String] -> Int -> [String]
avanca [] _ = []
avanca mapa x | x<=((n-2)^2) = mapa_final (substitui mapa coord)
              | otherwise = mapa_final mapa
        where n=length (head mapa)
              coord=take t (coordenadasEspiral (1,1) n)
              t=((n-2)^2)-x

------------------------------------------------------

-- | Função que inicia as mudanças no mapa (chama /outras funçoes/)
mapa_final :: [String] -> [String]
mapa_final mapa = auxiliar3 (auxiliar1 (organiza mapa))

{-|
Aplica uma série de funções, específicas a determinadas partes do mapa, mas apenas __existir bombas prestes a explodir__
(contador 0)

== Casos:

>>> __muda3__ -> recebendo as __coordenadas afetadas pelas bombas__ e as __coordenadas das bombas__
                 verifica se alguma bomba que __explode__ afetará alguma /bomba existente/. Se isto
                 se verificar, esta bomba afetada terá o seu contador a __1__ (para explodir
                 no próximo /tick/.

>>> __muda2__ -> recebendo as __coordenadas afetadas pelas bombas__ e as __cordenadas dos players__
                 verifica se alguma bomba /afetará/ algum player. Se se verificar, este desaparece.
-}
auxiliar3 :: ([String],[String],[String],[String]) -> [String]
auxiliar3 (mapa,powers,bombas,players) | bombas_zero bombas = (muda mapa powers (explode bombas)) ++ muda3 coords (retirar bombas) ++ (muda2 coords players)
                                       | otherwise = mapa ++ powers ++ bombas ++ players
                              where coords = nub $ coords_afetadas (explode bombas) (coords_blocos mapa)

muda3 :: [(Int,Int)] -> [String] -> [String]
muda3 [] bombas = bombas
muda3 _ [] = []
muda3 (x:xs) (h:hs) = muda3 xs (aux_muda3 x (h:hs))
            where aux_muda3 :: (Int,Int) -> [String] -> [String]
                  aux_muda3 (a,b) [] = []
                  aux_muda3 (a,b) (x:xs) = if  show a==z1 && show b==z2
                                              then unwords (chr:z1:z2:pl:r:"1":tail):(aux_muda3 (a,b) xs)
                                              else x:(aux_muda3 (a,b) xs)
                                  where (chr:z1:z2:pl:r:tmp:tail)=words x


muda2 :: [(Int,Int)] -> [String] -> [String]
muda2 [] players = players
muda2 _ [] = []
muda2 (x:xs) (y:ys) = muda2 xs (aux_muda2 x (y:ys))
            where aux_muda2 :: (Int,Int) -> [String] -> [String]
                  aux_muda2 (a,b) [] = []
                  aux_muda2 (a,b) (x:xs) = if show a==z1 && show b==z2
                                            then aux_muda2 (a,b) xs
                                            else x:(aux_muda2 (a,b) xs)
                                 where (pl:z1:z2:tail)=words x

-- | apresenta a string com contador 0
explode :: [String] -> [String]
explode [] = []
explode (x:xs) = if t=="0"
                   then x:(explode xs)
                   else explode xs
          where (chr:a:b:pl:r:t:resto)=words x


-- | retira do mapa paredes e power ups afetados pela bomba que explodiu
muda :: [String] -> [String] -> [String] -> [String]
muda [] _ _  = []
muda mapa powers bombas_explode = compara_retira mapa powers coords_explode
                        where paredes=coords_blocos mapa
                              coords_explode= nub $ coords_afetadas bombas_explode paredes

-- | compara e retira paredes, etc do mapa
compara_retira :: [String] -> [String] -> [(Int,Int)] -> [String]
compara_retira [] _ _ = []
compara_retira list powers [] = list ++ powers
compara_retira (x:xs) powers ((a,b):t) | ((x:xs) !! b) !! a =='#' = (compara_retira (x:xs) powers t)
                                       | ((x:xs) !! b) !! a =='?' = compara_retira nv_mapa powers t
                                       | ((x:xs) !! b) !! a ==' ' && pertence (a,b) powers = compara_retira (x:xs) nv_powers t
                                       | ((x:xs) !! b) !! a ==' ' =  compara_retira (x:xs) powers t
                                where line=splitAt (a+1) ((x:xs) !! b)
                                      mapp=splitAt (b+1) (x:xs)
                                      nv_mapa=(init (fst mapp)) ++ [(init (fst line)) ++ " " ++ (snd line)] ++ (snd mapp)
                                      nv_powers=(deletar (a,b) powers)

-- | verifica se um __par de coordenadas__ correspondem a alguma string do mapa __(powerups,bombas ou players)__
pertence :: (Int,Int) -> [String] -> Bool
pertence (a,b) [] = False
pertence (a,b) (y:ys) = if x1==show a && y1==show b
                            then True
                            else pertence (a,b) ys
                where (pU:x1:y1:tail)=words y

-- | Apresenta uma lista de coordenadas a serem __afetadas pela bomba__
coords_afetadas :: [String] -> [(Int,Int)] -> [(Int,Int)]
coords_afetadas [] _ = []
coords_afetadas (x:xs) list = aux_coords_afetadas x list ++ coords_afetadas xs list
               

-- | auxiliar de coods_afetadas, onde é __tratada uma bomba__
aux_coords_afetadas :: String -> [(Int,Int)] -> [(Int,Int)]
aux_coords_afetadas [] _ = []
aux_coords_afetadas bomba list = aux_auxL crd list raio 1 ++ aux_auxR crd list raio 1 ++ aux_auxU crd list raio 1 ++ aux_auxD crd list raio 1 ++ [crd]
                  where (chr:a:b:pl:r:t:resto)=words bomba
                        crd=(read a::Int,read b::Int)
                        raio=(read r::Int)

--------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------
{-|
Função que determina as coordenadas de uma bomba que __irá__ explodir, com raio=/r/

Para isto, usa um contador /k/ que serve para comparação com /r/. Quando __k==r__, a função termina.
-}
aux_auxL :: (Int,Int) -> [(Int,Int)] -> Int -> Int -> [(Int,Int)]
aux_auxL (a,b) list r k | k<=r && elem (a-k,b) list==False = (a-k,b):(aux_auxL (a,b) list r (k+1))
                        | k<=r = (a-k,b):[]
                        | otherwise = []

aux_auxR :: (Int,Int) -> [(Int,Int)] -> Int -> Int -> [(Int,Int)]
aux_auxR (a,b) list r k | k<=r && elem (a+k,b) list==False = (a+k,b):(aux_auxR (a,b) list r (k+1))
                        | k<=r = (a+k,b):[]
                        | otherwise = []

aux_auxU :: (Int,Int) -> [(Int,Int)] -> Int -> Int -> [(Int,Int)]
aux_auxU (a,b) list r k | k<=r && elem (a,b-k) list==False = (a,b-k):(aux_auxU (a,b) list r (k+1))
                        | k<=r = (a,b-k):[]
                        | otherwise = []                        

aux_auxD :: (Int,Int) -> [(Int,Int)] -> Int -> Int -> [(Int,Int)]
aux_auxD (a,b) list r k | k<=r && elem (a,b+k) list==False = (a,b+k):(aux_auxD (a,b) list r (k+1))
                        | k<=r = (a,b+k):[]
                        | otherwise = []

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

-- | Determina todas as coordenadas referentes aos blocos __'#'__ e __'?'__
coords_blocos :: [String] -> [(Int,Int)]
coords_blocos [] = []
coords_blocos (x:xs) = aux_blocos1 (x:xs) 0


aux_blocos1 :: [String] -> Int -> [(Int,Int)]
aux_blocos1 [] _ = []
aux_blocos1 (x:xs) l = aux_blocos2 x l 0 ++ aux_blocos1 xs (l+1)

aux_blocos2 :: String -> Int -> Int -> [(Int,Int)]
aux_blocos2 [] _ _ = []
aux_blocos2 (x:xs) l c | x=='#' || x=='?' = (c,l):(aux_blocos2 xs l (c+1))
                       | otherwise = aux_blocos2 xs l (c+1)

-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------


-- | retira do mapa uma string
retirar :: [String] -> [String]
retirar [] = []
retirar (x:xs) = if t=="0"
                   then retirar xs
                   else x:(retirar xs)
            where (chr:a:b:pl:r:t:resto)=words x


-- | retira do mapa uma string (outro tipo)
deletar :: (Int,Int) -> [String] -> [String]
deletar _ [] = []
deletar (a,b) (x:xs) = if show a==z1 && show b==z2
                         then xs
                         else x:(deletar (a,b) xs)
                where (chr:z1:z2:tail)=words x

-- | verifica se existem bombas com contador "0"
bombas_zero :: [String] -> Bool
bombas_zero [] = False
bombas_zero (x:xs) = if t=="0"
                        then True
                        else bombas_zero xs
            where (chr:a:b:pl:r:t:resto)=words x



-- | Executa a primeira mudança no mapa, neste caso passará o contador das bombas para tmp-1, através da __muda_bomba__
auxiliar1 :: ([String],[String],[String],[String]) -> ([String],[String],[String],[String])
auxiliar1 (mapa,powers,bombas,players) = (mapa,powers,(muda_bomba bombas),players)

-- | Retira __1 segundo__ ao contador de todas as bombas existentes no mapa
muda_bomba :: [String] -> [String]
muda_bomba [] = []
muda_bomba (x:xs) = (unwords (chr:a:b:pl:r:(show ((read t::Int)-1)):resto):[]) ++ muda_bomba xs
           where (chr:a:b:pl:r:t:resto)=words x

---------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------
{-|
Função que recebe uma String que corresponde a informações de jogadores, bombas e powerUps e as transforma em coordenadas.
Primeiro geramos a partir da String de cada elemento do jogo uma lista de Srtrings utilizando a função __words__ .
A partir da função __words__ retiramos apenas o que nos interessa para gerar as coordenadas, os elementos da posição 1 e 2.
Após a função __words__ utilizamos a função __drop__ e a função __take__. 
-}
stringToCoord :: String -> Coord 
stringToCoord s = (numero(head(take 2 ( drop 1 (words s)))),(numero(last(take 2(drop 1 (words s)))))) 

{-|
A função __número__ gera números inteiros (coordenadas) a partir de Strings, se a string tiver length 1, ou seja, for apenas um digito então é aplicada
diretamente a fução digitToInt, caso contrário, se a String tiver length superior a 1 então utilizamos a função __read__ para ler o número com mais de um algarismo.
-}

numero :: String -> Int 
numero l      
    | length l == 1 = digitToInt (head l) 
    | otherwise = read l::Int



------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

{-|
A função __coordenadasEspiral__ recebe a coordenada inicial de todas as espirais, (1,1), e a dimensão do
  mapa em causa. De seguida utiliza a função __faseEspiral__ que vai gerar camadas da espiral sequencialmente
  formando "quadrados" quando dá uma volta inteira. Após uma volta a funçao recursivamente repete-se adicionando
  ás coordenadas iniciais mais um valor para iniciar um novo ciclo.

-}
coordenadasEspiral :: Coord -> Int -> [Coord] 
coordenadasEspiral  (x,y) 1 = [] 
coordenadasEspiral  (x,y) p = faseEspiral (x,y) p ++ coordenadasEspiral (x+1,y+1) (p-2)

{-|
Função que recebe a coordenada inicial do início da espiral e a sua dimensão. A partir destes argumentos compacta em uma lista de Coordenadas
um ciclo completo da espiral, chamando funções como __direita__ , __baixo__ , __esquerda__ , __cima__.
O mecanismo desta funçao consiste em útilizar o último elemento que foi gerado da função anterior para que seja
criado um ciclo da espiral.
O contador presente é essencial para se poderem realizar os casos de paragem.
-}

faseEspiral :: Coord -> Int -> [Coord] 
faseEspiral (x,y) n = direita (x,y) (n-3) ++ tail b2 ++ tail e2 ++ drop 1 (init c2) 
                        where b2 = baixo (last (direita (x,y) (n-3))) (n-3) 
                              e2 = esquerda (last b2) (n-3)
                              c2 = cima (last e2) (n-3)

{-|
Estas quatro funções são utilizadas para gerar coordenadas que mais tarde possam ser compactadas em uma lista
formando assim um ciclo da espiral.
O processo foi dividido em 4 funçôes: __direita__ pois é o primeiro passo da espiral, de seguida __baixo__, __esquerda__ 
e finalmente __cima__.
As funções possuem um contador para saber quando parar a sequencia. 
-}

direita :: Coord -> Int -> [Coord]
direita (x,y) 0 = [(x,y)]
direita (x,y) c = (x,y) : direita (x+1,y) (c-1) 

baixo :: Coord -> Int -> [Coord] 
baixo (x,y) 0 = [(x,y)] 
baixo (x,y) c = (x,y) : baixo (x,y+1) (c-1)

esquerda :: Coord -> Int -> [Coord] 
esquerda (x,y) 0 = [(x,y)] 
esquerda (x,y) c = (x,y) : esquerda (x-1,y) (c-1)

cima :: Coord -> Int -> [Coord] 
cima (x,y) 0 = [(x,y)] 
cima (x,y) c = (x,y) : cima (x,y-1) (c-1)

--------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------
{-| 
A função __substitui__ recebe o mapa em causa e as coordenadas que vai ter de substituir pelo char '#'. 
A função é dividida em 4 partes:
__nv_mapa__ que utiliza a função __so_mapa__ para que as alterações sejam feitas apenas na parte "jogável" do mapa; 
__nv_Pu__ que utiliza a função __so_powers__ para aplicar a função __verifica__ apenas nas informações dos PowerUps;
__nv_Bombas__ que utiliza a função __procura_bomba__ para aplicar a função __verifica__ apenas nas informações das Bombas; 
__nv_Pl__  que utiliza a função __so_players__ para aplicar a função __verifica__ apenas nas informaçôes dos Jogadores;

É utilizada a função __splitAt__ na __nv_mapa__ para separar a linha que pretendemos do mapa, e depois separar a coordenada que queremos substituir do resto da linha.

-}
substitui :: Mapa -> [Coord] -> Mapa
substitui m [] = m
substitui m ((x,y):t) = substitui (nv_mapa ++ nv_Pu ++ nv_Bombas ++ nv_Pl) t
          where nv_mapa= (init (fst mapp)) ++ [init (fst line) ++ "#" ++ (snd line)] ++ snd mapp  
                mapp=(splitAt (y+1)) (so_mapa m) --escolhe a linha do mapa (mapa ate onde queres substituir,map_resto)
                line=(splitAt (x+1)) ((so_mapa m) !! y) --divide a linha que vamos editar (inicio_linha ++ char a ser editado,resto_linha)
                nv_Pu = verifica (so_powers m) (x,y)
                nv_Bombas = verifica (procura_bomba m) (x,y)
                nv_Pl = verifica (so_players m) (x,y) 

{-|
A função __verifica__ é utilizada nas listas de Strings que contêm as informações que dizem respeito aos PowerUps, Bombas e Jogadores.
A função converte as strings em coordenadas para que possam ser comparadas com a coordenada que a espiral vai afetar.
Se as coordenadas forem iguais, então a String é eliminada do mapa, caso contrário ela continua no mapa.
-}
verifica :: [String] -> Coord -> [String]
verifica [] _ = []
verifica (x:xs) (z1,z2) = if a==show z1 && b==show z2
                                then verifica xs (z1,z2)
                                else x:(verifica xs (z1,z2))
              where (chr:a:b:tail)=words x

------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- | Conjunto de exemplos de teste
ex0 = ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","! 7 5","* 7 7 1 1 10","0 4 3 +","1 7 7","3 6 5 +"]
ex1 = ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","! 7 5","* 7 7 1 1 2","0 4 3 +","1 7 7","3 6 5 +"]
ex2 = ["###############","#    ?   ? ?  #","# # #?#?# #?# #","#?  ? ?       #","#?# #?#?#?# # #","# ?   ??      #","# # # #?#?#?#?#","#  ??     ? ? #","# # #?# # #?# #","#???       ?  #","# # #?# #?# #?#","#  ?     ?  ? #","# #?# #?# #?# #","#        ?    #","###############","+ 12 7","! 7 6","! 8 13","! 9 13","* 8 3 1 1 10","0 6 7","2 7 13 +!","3 8 3"]
ex3 = ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","! 7 6","* 7 7 1 1 1","0 6 7 +","1 7 7","3 6 5 +"]
ex4 = ["#####","#   #","# # #","#   #","#####"]
ex5 = ["#####","#   #","# # #","#   #","#####","* 1 1 0 1 1","0 1 1","1 3 3","2 1 3","3 3 1"]
ex6 = ["#####","#   #","# # #","#   #","#####","* 1 1 0 2 1","0 1 1","1 3 3","2 1 3","3 3 1"]
ex7 = ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","! 7 6","* 7 7 1 1 1","* 4 5 3 1 10","0 6 7 +","1 7 7","3 6 5 +"]
ex8 = ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","! 7 6","* 7 7 1 1 1","* 4 5 3 1 1","0 6 7 +","1 7 7","3 6 5 +"]
ex9 = ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","! 7 6","* 7 7 1 3 1","* 4 5 3 2 1","0 6 7 +","1 7 7","3 6 5 +"]
ex10 = ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","! 7 6","* 4 5 3 2 1","* 7 6 0 1 10","* 7 7 1 3 1","0 6 7 +","1 7 7","3 6 5 +"]
ex12 = ["#############","#     ??    #","# #?# # #?# #","# ?  ? ?  ? #","# #?#?#?#?# #","#  ? ????   #","#?# # #?# # #","#         ? #","#?# #?# # #?#","#   ? ?  ?  #","# #?# # # # #","#  ????  ?  #","#############","+ 7 1","+ 3 2","+ 7 5","+ 8 5","! 10 3","! 1 8","! 4 9","! 3 10","! 6 11","* 3 1 2 1 10","* 3 3 0 1 10","0 1 1 +","1 7 7","2 5 10","3 11 7"]
ex18 =  ["#############","#     ??    #","# #?# # #?# #","# ?  ? ?  ? #","# #?#?#?#?# #","#  ? ????   #","#?# # #?# # #","#         ? #","#?# #?# # #?#","#   ? ?  ?  #","# #?# # # # #","#  ????  ?  #","#############","+ 7 1","+ 3 2","+ 7 5","+ 8 5","! 10 3","! 1 8","! 4 9","! 3 10","! 6 11","* 3 1 2 1 1","* 3 3 0 1 10","* 1 9 3 1 10","0 1 1 +","1 7 7","2 5 10","3 11 7 +"]
ex99= ["#############","#    ? ?    #","# # #?#?# # #","#   ???    ?#","# #?# #?# #?#","#  ?   ?????#","#?#?# #?# # #","#  ? ? ???? #","# #?# #?# # #","#?  ? ? ??? #","# # # #?#?# #","#    ?   ?  #","#############","+ 5 3","+ 11 3","! 10 7","0 1 7"]