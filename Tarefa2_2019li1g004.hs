-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g004 where

import Tarefa1_2019li1g004

import Tarefa0_2019li1g004

import LI11920

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0 ,(Movimenta C) ,(Estado (gera 5 6 5) [(Jogador 0 3 4 5 (Chao True))])),
            (0,(Movimenta C),(Estado (gera 4 6 4) [(Jogador 1 3.5 2 4 (Chao True))])),
            (0,(Movimenta C),(Estado (gera 6 6 6) [(Jogador 3 1.9 2 4 (Chao True))])),
            (0,(Movimenta B),(Estado (gera 4 6 4) [(Jogador 0 2.9 2 4 (Chao True))])),
            (0,(Movimenta B),(Estado (gera 5 5 5) [(Jogador 4 3 3 4 (Ar 5 50 0))])),
            (0,(Movimenta C),(Estado (gera 5 5 5) [(Jogador 2 3 3 4 (Morto 1.0))])),
            (0,(Movimenta C),(Estado (gera 5 5 5) [(Jogador 2 3 3 4 (Ar 5 50 0))])),
            (0,(Movimenta E),(Estado (gera 4 4 4) [(Jogador 0 1 3 4 (Ar 5 50 0))])),
            (0,(Movimenta D),(Estado (gera 5 5 5) [(Jogador 0 3 3 4 (Ar 5 (-90) 0))])),
            (0,(Movimenta E),(Estado (gera 5 5 5) [(Jogador 0 3 3 4 (Ar 5 90 0))])),
            (0,(Acelera),(Estado (gera 5 5 5) [(Jogador 0 3 3 4 (Ar 5 50 0))])),
            (0,(Acelera),(Estado (gera 5 5 5) [(Jogador 0 3 3 4 (Morto 1.0))])),
            (0,(Acelera),(Estado (gera 5 5 5) [(Jogador 0 3 3 4 (Chao False))])),
            (0,(Acelera),(Estado (gera 5 5 5) [(Jogador 0 3 3 4 (Chao True))])),
            (0,(Dispara),(Estado (gera 5 5 5) [(Jogador 0 0.9 3 4 (Chao True))])),
            (0,(Dispara),(Estado (gera 5 5 5) [(Jogador 0 3 3 4 (Chao True))])),
            (0,(Dispara),(Estado (gera 5 5 5) [(Jogador 0 3 3 4 (Morto 1.0))])),
            (0,(Dispara),(Estado (gera 5 5 5) [(Jogador 0 3 3 4 (Ar 5 50 0))]))]


-- * Funções principais da Tarefa 2.
--
-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada n (Movimenta x) (Estado m ljog) = (Estado m lnjogM)

                                       where jog = encontraIndiceLista n ljog 
                                             jogAlteradoM = alteraJogador jog  (Movimenta x) m
                                             lnjogM = atualizaIndiceLista n jogAlteradoM ljog 
jogada n Acelera (Estado m ljog) = (Estado m lnjogA ) 

                                  where  jog = encontraIndiceLista n ljog 
                                         jogAlteradoA = alteraJogador jog Acelera m
                                         lnjogA = atualizaIndiceLista n jogAlteradoA ljog

jogada n Desacelera (Estado m ljog) = (Estado m lnjogD)
                                    
                                    where jog = encontraIndiceLista n ljog 
                                          jogAlteradoD = alteraJogador jog Desacelera m
                                          lnjogD = atualizaIndiceLista n jogAlteradoD ljog 

jogada n Dispara (Estado m ljog) = (Estado newmap lnjogS)
  
                                 where jog = encontraIndiceLista n ljog
                                       newmap = colocaCola jog m
                                       jogAlteradoS = alteraJogador jog  Dispara m
                                       lnjogS = atualizaIndiceLista n jogAlteradoS ljog


-- | Função que altera um jogador consoante a jogada selecionada

alteraJogador :: Jogador -> Jogada -> Mapa -> Jogador
alteraJogador jogador (Movimenta x) m = case x of C -> alteraPistaJogador jogador (Movimenta C) m
                                                  B -> alteraPistaJogador jogador (Movimenta B) m
                                                  E -> alteraInclinacao jogador (Movimenta E)
                                                  D -> alteraInclinacao jogador (Movimenta D)
alteraJogador (Jogador pista d v c (Morto t)) Dispara m = (Jogador pista d v c (Morto t)) 
alteraJogador (Jogador pista d v c (Ar h i g)) Dispara m = (Jogador pista d v c (Ar h i g)) 
alteraJogador (Jogador pista d v c e) Dispara m | c > 0 = if (floor d)/= 0 then (Jogador pista d v (c-1) e) else (Jogador pista d v c e) 
                                                | otherwise = (Jogador pista d v c e) 
alteraJogador (Jogador pista d v c (Ar h i g)) x m = case x of Acelera -> (Jogador pista d v c (Ar h i g))
                                                               Desacelera -> (Jogador pista d v c (Ar h i g))
alteraJogador (Jogador pista d v c (Morto t)) x m = case x of Acelera -> (Jogador pista d v c (Morto t))
                                                              Desacelera -> (Jogador pista d v c (Morto t))
alteraJogador (Jogador pista d v c (Chao b)) x m = case x of Acelera -> (Jogador pista d v c (Chao True))
                                                             Desacelera -> (Jogador pista d v c (Chao False))






-- | Função auxiliar da função alteraJogador
--
-- Altera o jogador a nível da pista em que se encontras

alteraPistaJogador :: Jogador -> Jogada -> Mapa -> Jogador

alteraPistaJogador (Jogador pista d v c (Chao b)) (Movimenta C) m | pista == 0 = (Jogador pista d v c (Chao b))
                                                                  | a == 0 = (Jogador pista d 0 c (Morto 1.0))
                                                                  | a == 1 = (Jogador (pista-1) d v c (Chao b))
                                                                  | a == 2 = (Jogador (pista-1) d v c (Ar h inclinacao 0))
                                                                  | a == 3 = (Jogador pista d v c (Ar h inclinacao 0))

                                                                where a = testaMudanca (Jogador pista d v c (Chao b)) (Movimenta C) m 
                                                                      p = encontraPosicaoMatriz (pista,fromIntegral(floor d)) m
                                                                      inclinacao = case p of Rampa _ _ _ -> atan(hPeca pista d m)*(180/pi)
                                                                                             Recta _ _-> 0
                                                                      h = hJogador (Jogador pista d v c (Chao b)) m 


alteraPistaJogador (Jogador pista d v c (Chao b)) (Movimenta B) m | pista == (length m-1) = (Jogador pista d v c (Chao b))
                                                                  | a == 0 = (Jogador pista d 0 c (Morto 1.0))
                                                                  | a == 1 = (Jogador (pista+1) d v c (Chao b))
                                                                  | a == 2 = (Jogador (pista+1) d v c (Ar h inclinacao 0))
                                                                  | a == 3 = (Jogador pista d v c (Ar h inclinacao 0))

                                                                where a = testaMudanca (Jogador pista d v c (Chao b)) (Movimenta B) m 
                                                                      p = encontraPosicaoMatriz (pista,fromIntegral(floor d)) m
                                                                      inclinacao = case p of Rampa _ _ _ -> atan(hPeca pista d m)*(180/pi)
                                                                                             Recta _ _-> 0
                                                                      h = hJogador (Jogador pista d v c (Chao b)) m 

alteraPistaJogador (Jogador pista d v c e) jogada m = (Jogador pista d v c e)

-- | Função auxiliar de alteraPistaJogador
--
-- Testa o que acontece quando uma mudança de pista é efetuada

testaMudanca :: Jogador -> Jogada -> Mapa -> Int
testaMudanca (Jogador p d v c (Ar h i g)) jogada m = 3
testaMudanca j jogada m | abs (hAtual - hProxima) > 0.2 = if hAtual > hProxima then 2 else 0
                        | otherwise = 1 
                        where hAtual =  hJogador j m
                              hProxima = hAdjacente j jogada m



-- | Função que calcula a altura atual do jogador

hJogador :: Jogador -> Mapa -> Double
hJogador (Jogador pista d v c (Chao b)) m = (d-fromIntegral(floor d))*(hPeca pista d m)

-- | Função que calcula a altura do jogador na peça de mudança pretendida

hAdjacente :: Jogador -> Jogada -> Mapa -> Double
hAdjacente (Jogador pista d v c (Chao b)) (Movimenta x) m = case x of C -> hJogador (Jogador (pista-1) d v c (Chao b)) m
                                                                      B -> hJogador (Jogador (pista+1) d v c (Chao b)) m





-- | Função que calcula a altura total da peça onde se encontra o jogador atravês da distancia percorrida e pista onde se encontra

hPeca :: Int -> Double -> Mapa -> Double
hPeca  p d m = case encontraPosicaoMatriz (p,fromIntegral(floor d)) m of Recta _ h  -> fromIntegral h
                                                                         Rampa _ t h' -> fromIntegral (max h' t)


-- | Função que altera a Inclinação do  Jogador

alteraInclinacao :: Jogador -> Jogada -> Jogador
alteraInclinacao (Jogador p d v c (Ar h i g)) (Movimenta x) = case x of E -> if (i + 15) > 90 then (Jogador p d v c (Ar h i g)) else (Jogador p d v c (Ar h (i+15) g))
                                                                        D -> if (i - 15) < (-90) then (Jogador p d v c (Ar h i g)) else (Jogador p d v c (Ar h (i-15) g))  
alteraInclinacao (Jogador p d v c e) (Movimenta x) = (Jogador p d v c e) 


-- | Função que altera o mapa quando a jogada selecionada é Dispara

colocaCola :: Jogador -> Mapa -> Mapa
colocaCola (Jogador p d v c (Morto t)) m = m
colocaCola (Jogador p d v c (Ar h i g)) m = m
colocaCola (Jogador pista d v c e) m | c>0 = if (floor d) == 0 then m else atualizaPosicaoMatriz pos novoElem m 
                                     | otherwise = m

                                     where pos = (pista,(floor d)-1)
                                           novoElem = alteraTipoPecaCola(encontraPosicaoMatriz pos  m)


-- | Função auxiliar da coloca cola que altera a peça pretendida ,no caso em que, se encontra imediatamnete antes do jogador que efetua o disparo
 
alteraTipoPecaCola :: Peca -> Peca 
alteraTipoPecaCola (Recta a b) = (Recta Cola b)
alteraTipoPecaCola (Rampa a b c) = (Rampa Cola b c)