{-|
Module      : Main;
Description : Parte funcional do jogo, com interface gráfica.
Copyright   : Filipe Monteiro (a80229@alunos.uminho.pt)//João Amaral (a80694@alunos.uminho.pt);

Nesta tarefa, o jogo encontra-se funcional, com interface gráfica (graças ao Gloss), usando também as
outras tarefas para implementar a passagem do tempo, movimentos e geração de mapas.
-}

module Main where

import Graphics.Gloss         
import Graphics.Gloss.Data.Picture  
import Graphics.Gloss.Interface.Pure.Game
import Data.Char
import GeraMapas
import AcoesBomberman
import AcoesTempo
import Tarefa6_li1g158

-- | Uma representação do estado do jogo.
data Bomba = B { playerB::Int, posB::(Int,Int), r::Int, tmp::Int }
instance Show Bomba where 
  show b = "B" ++ " " ++ show (playerB b) ++ " " ++ show (posB b) ++ " " ++ show (r b) ++ " " ++ show (tmp b)
data Player = Pl { player::Int, pospl::(Int,Int), pM::Int, pF::Int }
instance Show Player where
  show plr = "Pl" ++ " " ++ show (pospl plr) ++ " " ++ show (pM plr) ++ " " ++ show (pF plr)
data PowerMais = PM { posM::(Int,Int) }
instance Show PowerMais where
  show powM = "PM" ++ " " ++ show (posM powM)
data PowerFlame = PF { posF::(Int,Int) }
instance Show PowerFlame where
  show powF = "PF" ++ " " ++ show (posF powF)

type Estado=(Float,Mapa,[PowerMais],[PowerFlame],[Bomba],[Player],[Picture],Int)


{-|
Estado == (tmp_ini,m,pM,pF,b,pl,iM,time) 

    1. tmp_ini = __variável__ usada para contar o /tempo/
    2. m = mapa (/[string]/ com apenas os blocos existentes no /mapa/)
    3. pM = lista de /powerMais/
    4. pF = lista de /powerFlame/
    5. b = lista de /bombas/
    6. pl = lista de /players/
    7. iM = lista dos __bmp__ usados para fazer os /desenhos/
    8. time = __contador__ com o /tempo/

Esta função permite transformar os dados recebidos num tipo de dados interno
para facilitar o desenvolvimento. 

>>> [String] ---> Estado
-}
parse :: [String] -> Estado -> Estado
parse [] estado = estado 
parse (x:xs) (tmp_ini,m,pM,pF,b,pl,iM,time) 
            | head x=='#' = parse xs (tmp_ini,m++[x],pM,pF,b,pl,iM,time)
            | head x=='+' = parse xs (tmp_ini,m,pM++[cM],pF,b,pl,iM,time)
            | head x=='!' = parse xs (tmp_ini,m,pM,pF++[cF],b,pl,iM,time)
            | head x=='*' = parse xs (tmp_ini,m,pM,pF,b++[cB],pl,iM,time)
            | head x=='0' || head x=='1' || head x=='2' || head x=='3' = parse xs (tmp_ini,m,pM,pF,b,pl++[cP],iM,time)
    where cB=converteBomba x
          cP=convertePl x
          cM=convertePM x
          cF=convertePF x
-- | Converte uma /String/ de __bomba__ para o tipo __Bomba__
converteBomba :: String -> Bomba
converteBomba bomba = B (read pl::Int) ((read x::Int),(read y::Int)) (read r::Int) (read tmp::Int)
        where (chr:x:y:pl:r:tmp:tail)=words bomba

-- | Converte uma /String/ de __PowerMais__ para o tipo __PowerMais__
convertePM :: String -> PowerMais
convertePM power = PM ((read x::Int),(read y::Int))
        where (chr:x:y:tail)=words power

-- | Converte uma /String/ de __PowerFlame__ para o tipo __PowerFlame__
convertePF :: String -> PowerFlame
convertePF power = PF ((read x::Int),(read y::Int))
        where (chr:x:y:tail)=words power

-- | Converte uma /String/ de __Player__ para o tipo __Player__        
convertePl :: String -> Player 
convertePl player = Pl (read pl::Int) ((read x::Int),(read y::Int)) pM pF
        where (pl:x:y:tail)=words player
              pM=contagemchar (concat tail) '+'
              pF=contagemchar (concat tail) '!'

-- | Conta quantos caracteres /chr/ existem dentro de uma __String__
contagemchar :: String -> Char -> Int
contagemchar [] _ = 0
contagemchar (x:xs) chr = if x==chr
                        then 1+(contagemchar xs chr)
                        else (contagemchar xs chr)

-- | Converte do tipo de dados interno __Estado__ para um tipo de dados que umas funções usam __[String]__
inv_parse :: Estado -> [String]
inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time) = m ++ str_pM ++ str_pF ++ str_B ++ str_pl
                    where str_pM=inv_convertepM pM
                          str_pF=inv_convertepF pF
                          str_B=inv_converteB b
                          str_pl=inv_convertepl pl
{-| 
Converte dados do tipo __PowerMais__ para __String__

== Exemplo

>>> PM (x,y) = "+ x y" 
-}
inv_convertepM :: [PowerMais] -> [String]
inv_convertepM [] = []
inv_convertepM ((PM (x,y)):t) = bomba:(inv_convertepM t)
                        where bomba="+" ++ " " ++ show x ++ " " ++ show y
{-| 
Converte dados do tipo __PowerFlame__ para __String__

== Exemplo

>>> PF (x,y) = "! x y" 
-}
inv_convertepF :: [PowerFlame] -> [String]
inv_convertepF [] = []
inv_convertepF ((PF (x,y)):t) = flame:(inv_convertepF t)
                        where flame="!" ++ " " ++ show x ++ " " ++ show y
{-| 
Converte dados do tipo __Bomba__ para __String__

== Exemplo

>>> B pl (x,y) r tmp = "* x y pl r tmp"
-}
inv_converteB :: [Bomba] -> [String]
inv_converteB [] = []
inv_converteB ((B pl (x,y) r tmp):t) = bomba:(inv_converteB t)
                        where bomba="*" ++ " " ++ show x ++ " " ++ show y ++ " " ++ show pl ++ " " ++ show r ++ " " ++ show tmp
{-| 
Converte dados do tipo __Player__ para __String__

== Exemplo

>>> Pl pl (x,y) pM pF = "pl x y (pM*'+' ++ pF*'!')" 
-}
inv_convertepl :: [Player] -> [String]
inv_convertepl [] = []
inv_convertepl ((Pl pl (x,y) pM pF):t) = player:(inv_convertepl t)
                        where player=show pl ++ " " ++ show x ++ " " ++ show y ++ " " ++ (replicate pM '+') ++ (replicate pF '!')


-- | O estado inicial do jogo.
estadoInicial :: Int -> Int -> [Picture] -> Estado
estadoInicial seed pls [bloco,player0,tijolo,chama,bomba,powerUpMais,player1,player2,papel_parede,powerUpFlame,victory1,victory2,victory3,lose,robot]
            = parse lista (0,[],[],[],[],players,[bloco,player0,tijolo,chama,bomba,powerUpMais,player1,player2,papel_parede,powerUpFlame,victory1,victory2,victory3,lose,robot],250)
         where lista=mapa 13 seed
               players=gera_players pls

gera_players :: Int -> [Player]
gera_players pls | pls==1 = [(Pl 0 (1,1) 0 0)]
                 | pls==2 = [(Pl 0 (1,1) 0 0),(Pl 1 (11,11) 0 0)]
                 | pls==3 = [(Pl 0 (1,1) 0 0),(Pl 1 (11,11) 0 0),(Pl 2 (11,1) 0 0),(Pl 3 (1,11) 0 0)]
                 | otherwise = []

{-|
Função que desenha o jogo.

A razão pela qual temos uma condição, é porque caso os /players/ morram todos, o jogo acabará, apresentando umas imagens
a festejar a vitória.
-}
desenhaEstado :: Estado -> Picture
desenhaEstado (tmp_ini,m,pM,pF,b,pl,[bloco,player0,tijolo,chama,bomba,powerUpMais,player1,player2,papel_parede,powerUpFlame,victory1,victory2,victory3,lose,robot],time)
          | length pl==1 = pictures ([auxiliar_desenho paredes players tijolos bombas powerMais exp_bomb raio_aum [bloco,player0,tijolo,chama,bomba,powerUpMais,player1,player2,papel_parede,powerUpFlame,victory1,victory2,victory3,lose,robot]]++[tempo]++extras_pl0++extras_pl1++extras_pl2++extras_pM++extras_pF++finalBom)
          | length pl==0 = pictures ([auxiliar_desenho paredes players tijolos bombas powerMais exp_bomb raio_aum [bloco,player0,tijolo,chama,bomba,powerUpMais,player1,player2,papel_parede,powerUpFlame,victory1,victory2,victory3,lose,robot]]++[tempo]++extras_pl0++extras_pl1++extras_pl2++extras_pM++extras_pF++finalMau)
          | otherwise = pictures ([auxiliar_desenho paredes players tijolos bombas powerMais exp_bomb raio_aum [bloco,player0,tijolo,chama,bomba,powerUpMais,player1,player2,papel_parede,powerUpFlame,victory1,victory2,victory3,lose,robot]]++[tempo]++extras_pl0++extras_pl1++extras_pl2++extras_pM++extras_pF)
        where paredes=coords_blocos_importantes m '#'
              players=myScaleYourScale (coords_players pl)
              tijolos=coords_blocos_importantes m '?'
              bombas=coords_bombasToScale $ coords_bombas b
              powerMais=coords_bombasToScale $ coords_powersM pM
              raio_aum=coords_bombasToScale $ coords_powersF pF
              tempo=Color white $ scale 0.75 0.75 $ Translate 500 300 $ Text (show time)
              exp_bomb=coords_bombasToScale $ blocos_inuteis (coords_afetadas (inv_converteB $ expl_bombas b) (coords_blocos m)) m--Retirar blocos fixos nao afetaveis
              extras_pF=[scale 1.3 1.3 $ Translate 470 (-230) $ powerUpFlame] ++ [scale 0.22 0.22 $ Translate 1500 (-1350) $ Color orange $ Text "Aumenta raio"]
              extras_pM=[scale 2 2 $ Translate 300 (-100) $ powerUpMais] ++ [scale 0.22 0.22 $ Translate 1500 (-1000) $ Color orange $ Text "Aumenta bombas"]
              extras_pl0=[scale 1.25 1.25 $ Translate 300 100 $ player0] ++ [scale 0.25 0.25 $ Translate 1700 450 $ Color red $ Text "Player 0"]
              extras_pl1=[scale 1.25 1.25 $ Translate 300 25 $ player1] ++ [scale 0.25 0.25 $ Translate 1700 50 $ Color blue $ Text "Player 1"]
              extras_pl2=[scale 1.25 1.25 $ Translate 300 (-50) $ player2] ++ [scale 0.25 0.25 $ Translate 1700 (-300) $ Color violet $ Text "Player 2"]
              finalBom=[Translate (-400) (-200) $ scale 1 1 $ victory1] ++ [Translate 0 250 $ scale 1 1 $ victory2] ++ [Translate 300 (-100) $ scale 1 1 $ victory3]
              finalMau=[scale 3 3 lose]

{-|
Auxiliar da função __desenhaEstado__, onde se desenha cada parte independente no ecrã.

    1. __p__ = desenha as /paredes/
    2. __pl__ = desenha os /players/
    3. __tij__ = desenha os /tijolos/ (blocos que são removíveis)
    4. __bb__ = desenha as /bombas/
    5. __pM__ = desenha os /powerMais/ (aumenta a capacidade de bombas)
    6. __expl__ = desenha as /explosões/
    7. __back__ = desenha o /background/
    8. __pF__ = desenha os /powerFlames/ (aumenta o raio das explosões)
-}
auxiliar_desenho :: [(Int,Int)] -> [(Int,Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> [Picture] -> Picture
auxiliar_desenho paredes players tijolos bombas powerMais exp_bomb raio_aum [bloco,player0,tijolo,chama,bomba,powerUpMais,player1,player2,papel_parede,powerUpFlame,victory1,victory2,victory3,lose,robot] = scale 0.75 0.75 $ pictures (back++pF++pM++p++tij++bb++pl++expl)
    where p=desenha paredes bloco
          pl=desenha_players players [player0,player1,player2,robot]
          tij=desenha tijolos tijolo
          bb=desenha bombas bomba
          pM=desenha powerMais powerUpMais
          expl=desenha exp_bomb chama
          back=[papel_parede]
          pF=desenha raio_aum powerUpFlame

{-|
Função que desenha os players. Tem de se ter uma diferente da outra desenha, porque os players estão no mapa
com um tipo de dados /incompativeis/ com os outros.
-}
desenha_players :: [(Int,Int,Int)] -> [Picture] -> [Picture]
desenha_players [] _ = []
desenha_players ((pl,x,y):plS) [player0,player1,player2,robot]
                | pl==0 = (Translate c d player0):desenha_players plS [player0,player1,player2,robot]
                | pl==1 = (Translate c d player1):desenha_players plS [player0,player1,player2,robot]
                | pl==2 = (Translate c d player2):desenha_players plS [player0,player1,player2,robot]
                | pl==3 = (Translate c d robot):desenha_players plS [player0,player1,player2,robot]
        where c = fromIntegral x :: Float
              d = fromIntegral y :: Float

-- | Desenha as imagens no __ecra__, recebendo um /par de ordenadas/ (correspondes a um __eixo imaginário__ no mapa)
desenha :: [(Int,Int)] -> Picture -> [Picture]
desenha [] _ = []
desenha ((a,b):t) bloco = (Translate c d bloco):(desenha t bloco)
                  where c = fromIntegral a :: Float
                        d = fromIntegral b :: Float

-- | Função que altera o estado do jogo quando acontece um evento.
--Importar a tarefa2.hs
reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (tmp_ini,m,pM,pF,b,pl,iM,time)    = parse (move (inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time)) 0 'U') (tmp_ini,[],[],[],[],[],iM,time)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (tmp_ini,m,pM,pF,b,pl,iM,time)  = parse (move (inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time)) 0 'D') (tmp_ini,[],[],[],[],[],iM,time) 
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (tmp_ini,m,pM,pF,b,pl,iM,time)  = parse (move (inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time)) 0 'L') (tmp_ini,[],[],[],[],[],iM,time) 
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (tmp_ini,m,pM,pF,b,pl,iM,time) = parse (move (inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time)) 0 'R') (tmp_ini,[],[],[],[],[],iM,time) 
reageEvento (EventKey (Char 'b') Down _ _) (tmp_ini,m,pM,pF,b,pl,iM,time)            = parse (move (inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time)) 0 'B') (tmp_ini,[],[],[],[],[],iM,time)
reageEvento (EventKey (Char 'w') Down _ _) (tmp_ini,m,pM,pF,b,pl,iM,time)            = parse (move (inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time)) 1 'U') (tmp_ini,[],[],[],[],[],iM,time)
reageEvento (EventKey (Char 's') Down _ _) (tmp_ini,m,pM,pF,b,pl,iM,time)            = parse (move (inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time)) 1 'D') (tmp_ini,[],[],[],[],[],iM,time) 
reageEvento (EventKey (Char 'a') Down _ _)(tmp_ini,m,pM,pF,b,pl,iM,time)             = parse (move (inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time)) 1 'L') (tmp_ini,[],[],[],[],[],iM,time) 
reageEvento (EventKey (Char 'd') Down _ _) (tmp_ini,m,pM,pF,b,pl,iM,time)            = parse (move (inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time)) 1 'R') (tmp_ini,[],[],[],[],[],iM,time) 
reageEvento (EventKey (Char 'g') Down _ _) (tmp_ini,m,pM,pF,b,pl,iM,time)            = parse (move (inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time)) 1 'B') (tmp_ini,[],[],[],[],[],iM,time)
reageEvento (EventKey (Char 'i') Down _ _) (tmp_ini,m,pM,pF,b,pl,iM,time)            = parse (move (inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time)) 2 'U') (tmp_ini,[],[],[],[],[],iM,time)
reageEvento (EventKey (Char 'k') Down _ _) (tmp_ini,m,pM,pF,b,pl,iM,time)            = parse (move (inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time)) 2 'D') (tmp_ini,[],[],[],[],[],iM,time) 
reageEvento (EventKey (Char 'j') Down _ _)(tmp_ini,m,pM,pF,b,pl,iM,time)             = parse (move (inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time)) 2 'L') (tmp_ini,[],[],[],[],[],iM,time) 
reageEvento (EventKey (Char 'l') Down _ _) (tmp_ini,m,pM,pF,b,pl,iM,time)            = parse (move (inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time)) 2 'R') (tmp_ini,[],[],[],[],[],iM,time) 
reageEvento (EventKey (SpecialKey KeyEnd) Down _ _) (tmp_ini,m,pM,pF,b,pl,iM,time)   = parse (move (inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time)) 2 'B') (tmp_ini,[],[],[],[],[],iM,time)
reageEvento _ s = s

{-|
Função que altera o estado do jogo quando o tempo avança @n@ segundos.
Importamos as funções da /Tarefa4.hs/ para fazer avançar o tempo.

==Nota

O bot apenas aparece com 3 players em jogo. Como tem um bug, implementamos no jogo apenas em algumas circunstancias
(neste caso precisa de ter 3 players em jogo). Precisamos corrigir o bug.
-}
--Importar a Tarefa4.hs
reageTempo :: Float -> Estado -> Estado
reageTempo f (tmp_ini,m,pM,pF,b,pl,iM,time) | time==0 = (tmp_ini,m,pM,pF,b,pl,iM,time)
                                            | length pl==0 || length pl==1 = (tmp_ini,m,pM,pF,b,pl,iM,time)
                                            | tmp_ini>=1 && bot_vivo = parse (avanca (move (inv_parse (tmp_ini,m,pM,pF,b,pl,iM,time)) 3 x) time) (0,[],[],[],[],[],iM,time-1)
                                            | tmp_ini>=1 = parse (avanca (inv_parse $ (0,m,pM,pF,b,pl,iM,time)) time) (0,[],[],[],[],[],iM,time-1)
                                            | tmp_ini<1 = (tmp_ini+t,m,pM,pF,b,pl,iM,time)
                              where t=2/(fromIntegral fr) 
                                    x=getMaybe $ bot (avanca (inv_parse $ (0,m,pM,pF,b,pl,iM,time)) time) 3 time
                                    bot_vivo=verificaBot pl
                                    -- Cópia == | tmp_ini>=1 = parse (avanca (inv_parse $ (0,m,pM,pF,b,pl,iM,time)) time) (0,[],[],[],[],[],iM,time-1)

-- | Frame rate
fr :: Int
fr = 50

-- | Display mode
dm :: Display
dm = InWindow "BomberHaskell" (800, 600) (0, 0)
--dm_pixeis = (61,46) --(800/13,600/13)
      
-- | Função principal que invoca o jogo.
main :: IO ()
main = do putStrLn "Introduza o nº de jogadores:"
          a <- getLine
          putStrLn "Introduza o nº do nível:"
          b <- getLine
          let pls=read a::Int   
          let seed=read b::Int
          bloco <- loadBMP "bloco.bmp"
          player0 <- loadBMP "player0.bmp"
          tijolo <- loadBMP "tijolo.bmp"
          chama <- loadBMP "chama.bmp"
          bomba <- loadBMP "bomba.bmp"
          powerUpMais <- loadBMP "powerMais.bmp"
          player1 <- loadBMP "player1.bmp"
          player2 <- loadBMP "player2.bmp"
          papel_parede <- loadBMP "background.bmp"
          powerUpFlame <- loadBMP "powerFlame.bmp"
          victory1 <- loadBMP "victory1.bmp"
          victory2 <- loadBMP "victory2.bmp"
          victory3 <- loadBMP "victory3.bmp"
          lose <- loadBMP "lose.bmp"
          robot <- loadBMP "robot.bmp"
          play dm              -- display mode
            (greyN 0.5)     -- côr do fundo da janela
            fr              -- frame rate
            (estadoInicial seed pls [bloco,player0,tijolo,chama,bomba,powerUpMais,player1,player2,papel_parede,powerUpFlame,victory1,victory2,victory3,lose,robot])  -- estado inicial
            desenhaEstado   -- desenha o estado do jogo
            reageEvento     -- reage a um evento
            reageTempo      -- reage ao passar do tempo

{-|
Verifica se o __bot__ está ainda em jogo.

-}
verificaBot :: [Player] -> Bool
verificaBot [] = False
verificaBot ((Pl pl (x,y) pM pF):t) = if pl==3
                                    then True
                                    else verificaBot t

{-|
Retira do tipo __PowerMais__, as coordenadas correspondentes a um /powerUpMais/.
 
==Exemplo

>>> (PM (x,y)) = (x,y)
-}
coords_powersM :: [PowerMais] -> [(Int,Int)]
coords_powersM [] = []
coords_powersM ((PM (x,y)):t) = (x,y):(coords_powersM t)

{-|
Retira do tipo __PowerFlame__, as coordenadas correspondentes a um /powerUpFlame/.
 
==Exemplo

>>> (PF (x,y)) = (x,y)
-}
coords_powersF :: [PowerFlame] -> [(Int,Int)]
coords_powersF [] = []
coords_powersF ((PF (x,y)):t) = (x,y):(coords_powersF t)

{-|
Retira do tipo __Bomba__, as coordenadas correspondentes a um /bomba/.
 
==Exemplo

>>> (B pl (x,y) r tmp) = (x,y)
-}
coords_bombas :: [Bomba] -> [(Int,Int)]
coords_bombas [] = []
coords_bombas ((B pl (x,y) r tmp):xs) = (x,y):coords_bombas xs

{-|
Retira do tipo __PLayer__, as coordenadas correspondentes a um /player/.
 
==Exemplo

>>> (Pl pl (x,y) pM pF) = (x,y)
-}
coords_players :: [Player] -> [(Int,Int,Int)]
coords_players [] = []
coords_players ((Pl pl (x,y) _ _):t) = (pl,x,y):(coords_players t)


{-|
Transforma uma coordenada do tipo __1,2,3,4...__ em __pixeis__
 
==Exemplo

>>> (1,1) = (334,-334)
>>> (11,1) = (326,334)
>>> (5,8) = (-70,-128)
-}
coords_bombasToScale :: [(Int,Int)] -> [(Int,Int)]
coords_bombasToScale [] = []
coords_bombasToScale (x:xs) = aux_coords_bombasToScale1 (x:xs) 334 (1,1)

aux_coords_bombasToScale1 :: [(Int,Int)] -> Int -> (Int,Int) -> [(Int,Int)]
aux_coords_bombasToScale1 [] _ _ = []
aux_coords_bombasToScale1 (x:xs) l (z1,z2) = aux_coords_bombasToScale2 x l (-334) (z1,z2):(aux_coords_bombasToScale1 xs l (1,1))

aux_coords_bombasToScale2 :: (Int,Int) -> Int -> Int -> (Int,Int) -> (Int,Int)
aux_coords_bombasToScale2 (x,y) l c (z1,z2) | x==z1 && y==z2 = (c,l)
                                            | x/=z1 && y/=z2 = aux_coords_bombasToScale2 (x,y) (l-66) (c+66) (z1+1,z2+1)
                                            | x==z1 && y/=z2 = aux_coords_bombasToScale2 (x,y) (l-66) c (z1,z2+1)
                                            | x/=z1 && y==z2 = aux_coords_bombasToScale2 (x,y) l (c+66) (z1+1,z2)
                                            | True = error $ show (x,y) ++ " " ++ show (z1,z2)

-- | Faz o mesmo que /coords_bombasToScale/, mas para um determinado /char/, e usando __strings__ em vez de coodenadas.
coords_blocos_importantes :: [String] -> Char -> [(Int,Int)]
coords_blocos_importantes [] _ = []
coords_blocos_importantes (x:xs) chr = aux_coords_blocos_importantes1 (x:xs) chr 400


aux_coords_blocos_importantes1 :: [String] -> Char -> Int -> [(Int,Int)]
aux_coords_blocos_importantes1 [] _ _ = []
aux_coords_blocos_importantes1 (x:xs) chr l = aux_coords_blocos_importantes2 x chr l (-400) ++ aux_coords_blocos_importantes1 xs chr (l-66)

aux_coords_blocos_importantes2 :: String -> Char -> Int -> Int -> [(Int,Int)]
aux_coords_blocos_importantes2 [] _ _ _ = []
aux_coords_blocos_importantes2 (x:xs) chr l c | x==chr = (c,l):(aux_coords_blocos_importantes2 xs chr l (c+66))
                                              | otherwise = aux_coords_blocos_importantes2 xs chr l (c+66)


-- | Faz o mesmo que /coords_bombasToScale/, mas com um tipo ligeiramente diferente.
myScaleYourScale :: [(Int,Int,Int)] -> [(Int,Int,Int)]
myScaleYourScale [] = []
myScaleYourScale players = myScaleYourScale_aux1 players 334 (1,1)

myScaleYourScale_aux1 :: [(Int,Int,Int)]-> Int -> (Int,Int) -> [(Int,Int,Int)]
myScaleYourScale_aux1 [] _ _ = []
myScaleYourScale_aux1 (x:xs) l (z1,z2) = myScaleYourScale_aux2 x l (-334) (z1,z2):(myScaleYourScale_aux1 xs l (z1,z2))

myScaleYourScale_aux2 :: (Int,Int,Int) -> Int -> Int -> (Int,Int) -> (Int,Int,Int)
myScaleYourScale_aux2 (pl,x,y) l c (z1,z2) | x==z1 && y==z2 = (pl,c,l)
                                           | x/=z1 && y/=z2 = myScaleYourScale_aux2 (pl,x,y) (l-66) (c+66) (z1+1,z2+1)
                                           | x==z1 && y/=z2 = myScaleYourScale_aux2 (pl,x,y) (l-66) c (z1,z2+1)
                                           | x/=z1 && y==z2 = myScaleYourScale_aux2 (pl,x,y) l (c+66) (z1+1,z2)
                                           | True = error $ show (pl,x,y) ++ " " ++ show (z1,z2)

{-|
Filtra coordenadas, de modo a que apenas apresente aquelas que não
correspondem ao bloco '#' no mapa.
-}
blocos_inuteis :: [(Int,Int)] -> [String] -> [(Int,Int)]
blocos_inuteis [] _ = []
blocos_inuteis ((a,b):xs) mapa | (mapa!!b)!!a=='#' = blocos_inuteis xs mapa
                               | otherwise = (a,b):(blocos_inuteis xs mapa)


-- | Filtra as bombas (tipo __Bomba__), apresentando apenas aquelas que têm temporizador __1__
expl_bombas :: [Bomba] -> [Bomba]
expl_bombas [] = []
expl_bombas ((B pl (x,y) r tmp):t) = if tmp==1
                                        then (B pl (x,y) r tmp):expl_bombas t
                                        else expl_bombas t

{-|
Retira o valor de um __Data /Maybe/__.

==Exemplo

>>> Just x = x
>>> Nothing = ' '
-}
getMaybe :: Maybe Char -> Char
getMaybe (Just x) = x
getMaybe Nothing = ' '