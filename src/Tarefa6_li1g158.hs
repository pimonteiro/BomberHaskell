{-|
Module      : Tarefa6_li1g158;
Description : Inteligência Aritificial do jogo.
Copyright   : Filipe Monteiro (a80229@alunos.uminho.pt)//João Amaral (a80694@alunos.uminho.pt);

Nesta tarefa, criamos um bot para inteligente para jogar o jogo.
-}

module Tarefa6_li1g158 where
import Data.Char
import AcoesTempo(coords_afetadas,aux_coords_afetadas,aux_auxL,aux_auxR,aux_auxU,aux_auxD,coords_blocos,aux_blocos1,aux_blocos2,organiza)
import System.Random

{-|
Tipo definido para apresentar Coordenadas.
-}
type Coord = (Int,Int)

{-|
A função __bot__ recebe o mapa o número do jogador e o tempo de jogo.
Primeiro avalia o tempo de jogo comparando-o á fórmula __(n-2)^2__.
Se o tempo for maior então chama a função __moves__.
Se o tempo for menor ou igual á fórmula então chama a função __moves_final__.

@
Syntax --> __bot__ mapa pl tempo = . . . 
@
-}
bot :: [String] -> Int -> Int -> Maybe Char
bot [] _ _ = Nothing
bot mapa player ticks = if ticks <=(((n-2)^2))
                            then moves_final mapa player
                            else moves mapa player
               where n=length (head mapa)


{-| 
Avalia a posição do jogador e se as suas coordenadas pertencerem as coordenadas que vão ser afetadas pela bomba no mapa então é chamada a função __descobre_proximo_passo__.
Caso contrário são comparadas as sua coordenadas á dimensão do mapa e atribuídos comandos para iniciar movimento.
Esta função serve para a altura do jogo em que o tempo é menor ou igual a (n-2)^2.

@
Syntax --> __pos__ mapa pl = . . . 
@
-}
moves_final :: [String] -> Int -> Maybe Char
moves_final [] player = Nothing
moves_final mapa pl | elem (x,y) coords_bombas = descobre_prox_passo mapa (x,y) coords_bombas (coords_blocos (so_mapa))
                    | x==n+1 && y==n+1 = Just 'B'
                    | x>n+1 && (mapa!!y)!!(x-1)=='#' = Just 'R'
                    | y>n+1 && (mapa!!y)!!(x-1)=='#'= Just 'D'
                    | y<n+1 = Just 'U'
                    | x<n+1 && (mapa!!y)!!(x-1)=='#'= Just 'R'
      where (x,y)=posPlayer mapa pl
            n=length $ head mapa
            (so_mapa,powers,bombas,players)=organiza mapa
            coords_bombas=coords_afetadas bombas (coords_blocos so_mapa)
      

{-|
Função que decide se irá ser utilizada uma função de jogada mais defensiva (__descobre_prox_passo__)
ou uma função mais ofensiva (__ofensivo__) na descoberta do próximo passo do bot.
O parâmetro que está presente é o facto de o jogador estar ou não em uma posição que será ou não afetada por uma bomba.
Esta função é utilizada enquanto o tempo de jogo é maior que __(n-2)^2__.

@
Syntax --> __moves__ mapa pl = . . . 
@
-}
moves :: [String] -> Int -> Maybe Char
moves [] _ = Nothing
moves mapa pl = if elem (x,y) expl_bombas
                  then descobre_prox_passo so_mapa (x,y) expl_bombas blocos
                  else ofensivo so_mapa (x,y)
        where (so_mapa,powers,bombas,players)=organiza mapa
              expl_bombas=coords_afetadas bombas (coords_blocos so_mapa)
              blocos=coords_blocos so_mapa
              (x,y)=posPlayer mapa pl



{-|
Função que trata de descobrir qual vai ser o próximo passo que o jogador vai efetuar.
Verifica se certas coordenadas pertencem a coordenadas de bombas, ou bombas que vão explodir, 
ou se são paredes, e dá o comando ao bot.

A função pode ser descrita como um sistema defensivo onde o jogador apenas /"foge"/ aos problemas.
-}
descobre_prox_passo :: [String] -> Coord -> [Coord] -> [Coord] -> Maybe Char
descobre_prox_passo [] _ _ _ = Nothing
descobre_prox_passo mapa (x,y) bombas blocos 
        | elem (x,y) bombas && ((mapa!!y)!!(x+1)=='#' || (mapa!!y)!!(x+1)=='?') && ((mapa!!y)!!(x-1)=='#' || (mapa!!y)!!(x-1)=='?') && ((mapa!!(y-2))!!x=='#' || (mapa!!(y-2))!!x=='?') = Just 'D'
        | elem (x,y) bombas && ((mapa!!y)!!(x+1)=='#' || (mapa!!y)!!(x+1)=='?') && ((mapa!!y)!!(x-1)=='#' || (mapa!!y)!!(x-1)=='?') && ((mapa!!(y+2))!!x=='#' || (mapa!!(y+2))!!x=='?') = Just 'U'
        | elem (x,y) bombas && ((mapa!!y)!!(x+1)=='#' || (mapa!!y)!!(x+1)=='?') = Just 'U'
        | elem (x,y) bombas && (mapa!!y)!!(x+1)== ' ' && (mapa!!y)!!(x+2)==' '  = Just 'R'
        | elem (x,y) bombas && (mapa!!y)!!(x-1)== ' ' && (mapa!!y)!!(x-2)==' '  = Just 'L'
        | elem (x,y) bombas && (mapa!!(y+1))!!x== ' ' && (mapa!!(y+2))!!x==' '  = Just 'D'
        | elem (x,y) bombas && (mapa!!(y-1))!!x== ' ' && (mapa!!(y-2))!!x==' '  = Just 'U'
        | elem (x+1,y) bombas && elem (x+1,y) blocos && (mapa!!(y-1))!!x=='#' && (mapa!!y)!!(x-1)=='#' = Just 'R'
        | elem (x+1,y) bombas && elem (x+1,y) blocos && (mapa!!(y-1))!!x=='#' && (mapa!!y)!!(x+1)=='#' = Just 'L'
        | elem (x+1,y) bombas && elem (x+1,y) blocos && (mapa!!(y+1))!!x=='#' && (mapa!!y)!!(x-1)=='#' = Just 'R'
        | elem (x+1,y) bombas && elem (x+1,y) blocos && (mapa!!(y+1))!!x=='#' && (mapa!!y)!!(x+1)=='#' = Just 'L'
        | elem (x+1,y) bombas && elem (x+1,y) blocos && (mapa!!y)!!(x+1)=='#' && (mapa!!y)!!(x-1)=='#' && (mapa!!(y-1)!!x)=='#' = Just 'D'
        | elem (x+1,y) bombas && elem (x+1,y) blocos && (mapa!!y)!!(x+1)=='#' && (mapa!!y)!!(x-1)=='#' && (mapa!!(y+1)!!x)=='#' = Just 'U'
        | elem (x+1,y) bombas && elem (x+1,y) blocos && (mapa!!y)!!(x+1)=='#' && (mapa!!y)!!(x-1)=='#' = Just 'D' -- queria por random
        | (mapa!!(y-1))!!x=='#' && (mapa!!y)!!(x-1)=='#' = Just 'R' --Pos Up/Left
        | (mapa!!(y-1))!!x=='#' && (mapa!!y)!!(x+1)=='#' = Just 'L' --Pos Up/Right
        | (mapa!!(y+1))!!x=='#' && (mapa!!y)!!(x-1)=='#' = Just 'R' --Pos Down/Left
        | (mapa!!(y+1))!!x=='#' && (mapa!!y)!!(x+1)=='#' = Just 'L' --Pos Down/Right
        | (mapa!!(y+1))!!x=='?' || (mapa!!(y-1))!!x=='?' || (mapa!!y)!!(x+1)=='?' || (mapa!!y)!!(x-1)=='?' = Just 'B'
        | (mapa!!(y-1))!!x=='#' && (mapa!!(y+1))!!x=='#' && (mapa!!(y-1))!!(x+2)=='#' && (mapa!!(y+1))!!(x+2)=='#' = Just 'R'
        | (mapa!!(y-1))!!x=='#' && (mapa!!(y+1))!!x=='#' && (mapa!!(y-1))!!(x-2)=='#' && (mapa!!(y+1))!!(x-2)=='#' = Just 'L'
        | otherwise = Just 'U'-- queria por random
    
{-|
A função __ofensivo__ avalia as coordenadas que estao nas /"redondezas"/ do bot e verifica se alguma destas se trata de um elemento destrutível.
Caso se confirme então o bot irá colocar bombas, caso contrário chama a função __descobre_prox_passo__. 
Resumidamente, avalia as condições para ver se é __benéfico__ colocar uma bomba ou não.
-}      
ofensivo :: [String] -> Coord -> Maybe Char
ofensivo [] _ = Nothing
ofensivo mapa (x,y) | (mapa!!(y+1))!!x=='?' = Just 'B'
                    | (mapa!!(y-1))!!x=='?' = Just 'B'
                    | (mapa!!y)!!(x+1)=='?' = Just 'B'
                    | (mapa!!y)!!(x-1)=='?' = Just 'B'
                    | otherwise = descobre_prox_passo so_mapa (x,y) coords_bombas blocos
              where (so_mapa,powers,bombas,players)=organiza mapa
                    blocos=coords_blocos so_mapa
                    coords_bombas=coords_afetadas bombas (coords_blocos so_mapa)


{-|
Função que nos dá a posição de todas as bombas em coordenadas no mapa.
Recebe um estado do jogo e devolve as coordenadas em que se encontram as bombas.

@
Syntax --> __posBombas__ mapa = . . . 
@
-}
posBombas :: [String] -> [Coord]
posBombas [] = []
posBombas (x:xs) = if head x=='*'
                    then (a,b):posBombas xs
                    else posBombas xs
        where (chr:z1:z2:tail)=words x
              a=read z1::Int
              b=read z2::Int

{-|
Função que nos dá a posição de um jogador em coordenadas no mapa.
Recebe um estado do jogo e devolve a coordenada em que se encontra o jogador. 

@
Syntax --> __posPlayer__ mapa pl = . . . 
@
-}
posPlayer :: [String] -> Int -> Coord
posPlayer (x:xs) pl = if ((head x):[])==show pl
                         then (read z1::Int,read z2::Int) 
                         else posPlayer xs pl
           where (n:z1:z2:tail)=words x

