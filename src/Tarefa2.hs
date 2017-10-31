{-|
Module      : Main
Description : Dando um estado do mapa, o player e a ação, apresenta o estado seguinte do jogo;
Copyright   : Filipe Monteiro (a80229@alunos.uminho.pt)//João Amaral (a80694@alunos.uminho.pt);

Esta segunda tarefa permite a execução de determinados comandos que movem o jogador ('U','D','L','R') e colocação de bombas ('B'),
 apresentado o estado final do jogo depois da ação. Usa funções recursivas definidas por nós, mas também algumas ja existentes
no Haskell.
-}

module Tarefa2 where

import Data.Char
import System.Environment

main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          let c = a !! 1
          w <- getContents
          if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
             then putStr $ unlines $ move (lines w) (read p) (head c)
             else putStrLn "Parâmetros inválidos"
{-|

__mapa__ -> estado atual do jogo

__pl__ -> nº do jogador a utilizar

__chr__ -> character da ação (comando)

__novo_mapa__ -> mapa com o movimento efectuado

__list__ -> uma lista de elementos

__pU__ -> powerUp

__coordenadasDoJogador__ -> lista com todas as informações da posição do jogador e seus respetivos powerUps




-}
{-|
Apresenta o seguinte estado do jogo, conforme a ação (__chr__) 

@
Syntax --> __move__ mapa pl chr = . . . 
@
-}
move :: [String] -> Int -> Char -> [String]
move mapa pl chr | player_no_mapa mapa pl ==False = mapa
                 | chr == 'B' = porBomba mapa pl chr  
                 | chr == 'U' ||  chr == 'D' || chr == 'L' || chr == 'R' = final (mudanca mapa pl chr ) pl chr

{-|
Verifica se o player /pl/ introduzido existe no /mapa/

@
Syntax --> __player_no_mapa__ mapa pl = . . . 
@
-} 
player_no_mapa :: [String] -> Int -> Bool
player_no_mapa [] pl = False
player_no_mapa (x:xs) pl = if ((head x):[])==show pl
                            then True
                            else player_no_mapa xs pl 

{-|
/Adiciona possiveis powerUps/ apanhados pelo __player__ /pl/, ou seja, averigua se existe
__powerUps__ na nova posição (atraves da função __fake_positionB__ e __fake_positionF__) e caso 
existam adicioná los á /string/ do __player__ (usando a __adicionaP__ para adicionar o powerUp e a __remove__ para retirar
a string do powerUp do mapa)

@
Syntax --> __final__ novo_mapa pl chr = . . . 
@
-}                
final :: [String] -> Int -> Char -> [String]
final (x:xs) pl chr
        | elem pos_fakeB (x:xs) = adicionaP "+" (remove (x:xs) pos_fakeB) pl
        | elem pos_fakeF (x:xs) = adicionaP "!" (remove (x:xs) pos_fakeF) pl
        | otherwise = (x:xs)
  where pos_fakeB=fake_positionB (x:xs) pl
        pos_fakeF=fake_positionF (x:xs) pl

{-|
Cria uma string a __imitar uma string de powerUps__, neste caso de /aumento do nº de bombas/, 
usando a nova posição do __player__ /pl/ , para comparar na função __final__

@
Syntax --> __fake_positionB__ novo_mapa pl = . . .  
@
-}
fake_positionB :: [String] -> Int -> String
fake_positionB (x:xs) pl = "+ " ++ b ++ " " ++ c
                where (a:b:c:t)= pos (x:xs) pl
{-|
Cria uma string a __imitar uma string de powerUps__, neste caso de /aumento do raio da bomba/, 
usando a nova posição do __player__ /pl/, para comparar na função __final__

@
Syntax --> __fake_positionF__ novo_mapa pl = . . .  
@
-}
fake_positionF :: [String] -> Int -> String
fake_positionF (x:xs) pl = "! " ++ b ++ " " ++ c
                where (a:b:c:t)= pos (x:xs) pl

{-|
Remove de um /mapa/ uma determinada /linha/

Neste caso foi apenas usada para retirar do /novo_mapa/ a /string do powerUp/ 
apanhado pelo __player__ /pl/

@
Syntax --> __remove__ mapa list = . . . 
@

==Nota
@
Nest caso, __list__ foi subsituida pelo resultado de /fake_positionF||B/
para retirar do mapa a string com o powerUp
@
-}
remove :: [String] -> String -> [String]
remove [] list = []
remove (x:xs) list = if x==list
                        then xs
                        else x:(remove xs list)
{-|
Adiciona ao __player__ /pl/ de um mapa um powerUp,
substituindo (usando a função __replace__) no mapa uma nova string do __player__ /pl/
 
@
Syntax --> __adicionaP__ "+"||"!" mapa pl = . . . 
@
-}

adicionaP :: String -> [String] -> Int -> [String]
adicionaP "+" (x:xs) pl = replace (x:xs) (adiciona_aux "+" (pos (x:xs) pl))
adicionaP "!" (x:xs) pl = replace (x:xs) (adiciona_aux "!" (pos (x:xs) pl))

{-|
Auxiliar de /adicionaP/

==Nota

@
>>> words "2 39 12 ++"=["2","39","12","++"]
>>> ["2","39","12","++"]==(n:a:b:c:tail) onde /n=="2"/, /a=="39"/, /b=="12"/, /c=="++"/
>>> unwords ["2","39","12","++"]=/"2 39 12 ++"/
Sendo uma "antiga String" de /pl/, o words desta pode ter duas formas:
    
    1. __length==3__, ou seja, nao possui nenhum /powerUp/, logo pode se
       adicionar diretamente a seguir ao unwords desta.

    2. __length==4__, ou seja, possui alguns /powerUp/, logo tem de se ordenar os /powerUp/
       existentes no player mais o novo (usando a função __ordena_powerUps__) 
@

@
Syntax --> __adiciona_aux__ pU player = . . . 
@
-}

adiciona_aux :: String -> [String] -> String
adiciona_aux pU player 
          | length player==3 = (unwords player) ++ " " ++ pU
          | otherwise = (unwords [n,a,b]) ++ " " ++ (ordena_powerUps c pU)
    where (n:a:b:c:tail)=player

{-|
Ordena os /powerUps/ a serem colocados na "String" do player _pl_

@
Syntax --> __ordena_powerUps__ player pU = . . .
@
-}
ordena_powerUps :: String -> String -> String
ordena_powerUps [] [] = []
ordena_powerUps (x:xs) pU | pU=="+" = pU ++ (x:xs)
                          | otherwise = (x:xs) ++ "!"

-- | Função /intermédia/ para fazer o mapa final 

mudanca :: [String] -> Int -> Char -> [String]
mudanca mapa pl chr | chr=='U' && mov_valida mapa pl chr = nova_pos mapa pl chr
                    | chr=='D' && mov_valida mapa pl chr = nova_pos mapa pl chr
                    | chr=='L' && mov_valida mapa pl chr = nova_pos mapa pl chr
                    | chr=='R' && mov_valida mapa pl chr = nova_pos mapa pl chr
                    | otherwise = mapa

{-|
Averigua se o movimento apresentado é válido (apenas pode se mover se for um __' '__)

@
Syntax --> __move_valida__ mapa pl chr = . . . 
@
-}
mov_valida :: [String] -> Int -> Char -> Bool
mov_valida mapa pl chr
        | chr=='U' && (mapa !! ((snd pos_pl)-1) !! (fst pos_pl)     ==' ') = True
        | chr=='D' && (mapa !! ((snd pos_pl)+1) !! (fst pos_pl)     ==' ') = True
        | chr=='L' && (mapa !! (snd pos_pl)     !! ((fst pos_pl)-1) ==' ') = True
        | chr=='R' && (mapa !! (snd pos_pl)     !! ((fst pos_pl)+1) ==' ') = True
        | otherwise = False
                    where pos_pl=stringToTuplo (pos mapa pl)

{-| 
Retira do mapa a /String/ com os dados de um determinado player __pl__

@
Syntax --> __pos__ mapa pl = . . . 
@

==Nota

@
Usamos a words para ser mais facil usar as coordenadas do player __pl__

>>> words "1 45 790 ++" = ["1","45","790","++"] 
@
-}
pos :: [String] -> Int -> [String]
pos (x:xs) pl = if (head x:[])==show pl
                        then words x
                        else pos xs pl
{-|
Substitui num mapa, a String do player __pl__ pela nova String deste (/nova posição e powerUps/)

@
Syntax --> nova_pos__ mapa pl chr = . . .
@
-}
nova_pos :: [String] -> Int -> Char -> [String]
nova_pos mapa pl chr = replace mapa (pos_final mapa pl chr)

{-|
Substitui no mapa, uma "String" por outra

@
Syntax --> __replace__ mapa pos_final = . . .
@

==Nota

__pos_final__ é a nova "String"
-}
replace :: [String] -> String -> [String]
replace [] _ = []
replace (x:xs) pos_final = if head x==head pos_final
                             then pos_final:xs
                             else x:(replace xs pos_final) 

{-|
Cria uma string nova para o player __pl__ com a sua posição nova
depois do movimento

@
Syntx --> __pos_final__ mapa pl chr = . . .
@
-}
pos_final :: [String] -> Int -> Char -> String
pos_final mapa pl chr
        | chr=='U' =  unwords (n:x:(show ((read y::Int)-1)):t)
        | chr=='D' =  unwords (n:x:(show ((read y::Int)+1)):t)
        | chr=='L' =  unwords (n:(show ((read x::Int)-1)):y:t)
        | chr=='R' =  unwords (n:(show ((read x::Int)+1)):y:t)
            where (n:x:y:t)=pos mapa pl

{-|
Pega numa lista (resuldado de __words__ da /"String"/ do player __pl__/)
e cria um tuplo, neste caso de coordenadas

==Nota

>>> read ("6789")::Int=6789
>>> read ("797a3")::Int=__error__
-}
stringToTuplo :: [String] -> (Int,Int)
stringToTuplo (n:x:y:t) = (read x::Int,read y::Int)


-- | Exemplos para testar o /main/
ex = ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","! 7 5","* 7 7 1 1 10","0 4 3 +","1 7 7","3 6 5 +"]
ex2 = ["###############","#    ?   ? ?  #","# # #?#?# #?# #","#?  ? ?       #","#?# #?#?#?# # #","# ?   ??      #","# # # #?#?#?#?#","#  ??     ? ? #","# # #?# # #?# #","#???       ?  #","# # #?# #?# #?#","#  ?     ?  ? #","# #?# #?# #?# #","#        ?    #","###############","+ 12 7","! 7 12","! 8 13","! 9 13","* 8 3 1 1 10","0 2 1","2 7 13 +!","3 8 3"]

-------------------------Funções para colocar bombas-------------------------------

{-| 
Função núcleo que inicia o processo de verificação da possibilidade de realizar a ação por uma bomba

@
Syntax --> __porBomba__ mapa pl chr = . . .
@
-}

porBomba :: [String] -> Int -> Char -> [String] 
porBomba [] _ _ = [] 
porBomba mapa pl chr 
    | chr == 'B' = verificaBomba mapa pl 
    | otherwise = mapa


{-|
Função muito __importante__ no estudo da possibilidade de colocar uma bomba. A partir do estado do mapa e fornecendo o char '*' 
a função __posBomba__ devolve uma lista de Strings com __todas as coordenadas de bombas presentes no jogo__

@
Syntax --> __posBomba__ mapa chr = . . . 
@

==Nota 

@
Dando um mapa a função devolve algo como: ["* 7 7 1 1 10","* 4 5 2 1 10","..."]
@ 
-}

posBomba :: [String] -> Char -> [String] 
posBomba [] _ = [] 
posBomba (x:xs) chr = if (head x)== chr
                        then x : posBomba xs chr 
                        else posBomba xs chr                        

{-| 
Chama todas as funções que verificam os parâmetros necessários á colocação de uma bomba.
Se a função __verificaBomba2__ se verificar, a função aplica uma função definida por nós: __ordenaBomba__,
 que ordena a coordenada da nova bomba a ser colocada no mapa.

@
Syntax --> __verificaBomba__ mapa pl = . . . 
@
-}

verificaBomba :: [String] -> Int -> [String]
verificaBomba (x:xs) pl
  | podeBomba (x:xs) pl = if verificaBomba2 (bombasNoMapa (posBomba (x:xs) '*') pl) (contarPowerupsMais (drop 3 (pos (x:xs) pl))) 
                            then ordenaBomba (x:xs) (criarCoordBomba (pos (x:xs) pl))
                            else (x:xs) 
  | otherwise = (x:xs)  


{-| 
Faz uma comparação entre as bombas que o jogador tem colocadas no mapa e os seus powerUps '+'
Se __b = número de bombas no mapa do player /pl/__, for __maior ou igual__ a __n = numero de powerUps '+' que o jogador tem__, então
esse jogador __não__ pode colocar bombas no mapa.

@
Syntax --> __verificaBomba2__ b n = . . .
@
-}

verificaBomba2 :: Int -> Int -> Bool 
verificaBomba2 b n 
  | b >= n = False 
  | otherwise = True  


{-|
Cria coordenadas da __nova bomba__ a ser colocada no mapa. Recebe as coordenadas do jogador que a deseja colocar
sobre a forma de /[String]/, devido á utilização da função pré-definida /"words"/

@
Syntax --> __criarCoordBomba__ coordenadasDoJogador = . . .
@
-}
criarCoordBomba :: [String] -> String 
criarCoordBomba (a:b:c:d) = "* " ++ b ++ " " ++ c ++ " " ++ a ++ " " ++ (raioBomba (contarPowerupsExclama d)) ++ " " ++ "10"

{-|
Calcula o __raio de explosáo__ da nova bomba

Utiliza a função pré-definida __show__ para devolver uma String para ser utilizada na função /criarCoordBomba/

Função utilizada em conjunto com a __contarPowerupsExclama__ para somar o inteiro que devolve ao _raio de uma bomba normal__ (==1)

@
Syntax --> __raioBomba__ n = . . .
@
-}
raioBomba :: Int -> String
raioBomba n = show (n+1)

{-| 
Ordena as coordenadas da nova bomba no estado atual de um mapa.

@
Syntax --> __ordenaBomba__ mapa bomba = . . . 
@
-}
ordenaBomba :: [String] -> String -> [String]
ordenaBomba  (x:xs) bomba 
        | head x=='*' && (read c::Int)>(read c'::Int) = x:(ordenaBomba xs bomba)
        | head x=='*' && (read c::Int)<(read c'::Int) = bomba:(x:xs)
        | head x=='*' && (read c::Int)==(read c'::Int) && (read b::Int)>(read b'::Int) = x:bomba:xs
        | head x=='*' && (read c::Int)==(read c'::Int) && (read b::Int)<(read b'::Int) = bomba:(x:xs)
        | ord (head x)>=48 && ord (head x)<=57 = bomba:x:xs
        | otherwise = x:(ordenaBomba xs bomba)
    where (a:b:c:d:t)=words bomba
          (a':b':c':d':t')=words x


{-|
Recorre á função __compara__ para poder ser feita a comparação entre as /coordenadas onde o jogador se encontra/
e as /coordenadas das bombas já colocadas/

@
Syntax --> mapa pl = . . . 
@
-}
podeBomba :: [String] -> Int -> Bool
podeBomba (x:xs) pl 
  | compara (take 2 (drop 1 (pos (x:xs) pl))) (posBomba (x:xs) '*') = True
  | otherwise = False

{-|
Compara as coordenadas da posição de um certo player _pl__ com as coordenadas de bombas já colocadas no mapa,
se estas coincidirem, então o jogador __não pode colocar uma bomba__ naquele local pois ja se encontra lá uma.

Se a função retribuir __True__, então pode ser colocada lá uma bomba

@
Syntax --> __compara__ coordenadasDoJogador bomba = . . .
@
==Nota 
@
>>> compara ["2","2"] ["* 2 2 1 1 10","* 7 7 1 1 10"] = False
@

-}
compara :: [String] -> [String] -> Bool 
compara _ [] = True 
compara (h:t) (x:xs) 
  | elem (head h) (head(drop 1 (words (x)))) = if elem (head (head t)) (head (tail (drop 1 (words x)))) then False else True
  | otherwise = compara (h:t) xs  

{-| Função que tem como objetivo devolver um número inteiro, que está associado ao número de powerUps '+' que um certo
jogador possui.

@
Syntax --> __contarPowerupsMais __ coordenadasDoJogador = . . .
@

==Nota 

@
Recorre á função auxiliar __contarMais__ que pega na linha do mapa (String) e verifica se esta possui os char '+', se possuír adiciona 
sempre "+1" ao resultado final, dando como resultado o número de powerUps '+'.
No fim é sempre adicionado 1 pois o jogador pode sempre por mais uma bomba so que os powerUps que tem.

>>> contarMais "1 5 5 +++" = 4 
@
-}

contarPowerupsMais :: [String] -> Int 
contarPowerupsMais [] = 1 
contarPowerupsMais (x:xs) = contarMais x + contarPowerupsMais xs 

contarMais :: String -> Int
contarMais [] = 0 
contarMais (x:xs) 
  | x == '+' = 1 + contarMais xs 
  | otherwise = 0 + contarMais xs 


{-| Função muito idêntica à __contarPowerUpsMais__, pois segue o mesmo mecanismo, mas para powerUps '!'

@
Syntax --> __contarPowerupsExclama__ coordenadasDoJogador = . . .
@
-}
contarPowerupsExclama :: [String] -> Int 
contarPowerupsExclama [] = 0 
contarPowerupsExclama (x:xs) = contarExclama x + contarPowerupsExclama xs 

{-|
Função auxiliar da __contarPowerupsExclama__, que serve como método de contagem, avaliando 
cada lista criada a partir da função /words/ das coordenadas do jogador.

Se reconhecer um caratér '!' adiciona sempre mais um ao resultado.

@
Syntax --> __contarExclama__ lista = . . .
@
-}

contarExclama :: String -> Int 
contarExclama [] = 0
contarExclama (x:xs) 
  | x == '!' = 1 + contarExclama xs 
  | otherwise = 0 + contarExclama xs 


{-|
==Bombas no Mapa de um certo jogador

Recebe uma lista que contem as coordenadas e informação de todas as bombas. Verifica quantas bombas do jogador pl é que existem no mapa.
O sexto elemento de uma coordenada de uma bomba é sempre o número do jogador que a colocou, logo, utilizamos a função pré-definida __(!!)__ para 
recolhermos o sexto elemento dessa lista, podendo assim compará-lo ao número do jogador que estamos a estudar.
No fim, devolve um inteiro que representa o número de bombas de um jogador no mapa.

-}
bombasNoMapa :: [String] -> Int -> Int
bombasNoMapa [] _ = 0
bombasNoMapa (x:xs) pl = auxBombas x pl + bombasNoMapa xs pl 

auxBombas :: String -> Int -> Int 
auxBombas x pl 
  | pl == (digitToInt ((!!) x 6)) = 1 
  | otherwise = 0 