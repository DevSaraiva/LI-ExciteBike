-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g004 where

import Tarefa0_2019li1g004
import Tarefa1_2019li1g004
import Tarefa2_2019li1g004
import Tarefa3_2019li1g004
import LI11920

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [(0.2,(gera 2 5 1),(Jogador 1 0 3 5 (Chao True))),
            (0.2,(gera 5 5 1),(Jogador 4 3.2 3 5 (Chao False))),
            (0.2,(gera 2 5 1),(Jogador 1 3.6 1 5 (Ar 3 15 1))), 
            (0.2,(gera 6 5 6),(Jogador 4 2.7 6 5 (Ar 6 34 1))), 
            (0.2,(gera 6 5 6),(Jogador 4 2.7 0 5 (Chao False))),
            (0.2,(gera 6 5 6),(Jogador 4 2.7 0 5 (Morto 4))),
            (0.2,(gera 2 10 10),(Jogador 1 5.1 6 5 (Chao True))),
            (0.2,(gera 6 6 6),(Jogador 1 2.1 0.2 5 (Chao True))),
            (0.2,(gera 6 6 6 ),(Jogador 2 3.1 0.2 5 (Chao True))),
            (0.2,(gera 6 5 6),(Jogador 4 3.5 6 5 (Ar 6 45 1))),
            (0.2,(gera 5 5 1),(Jogador 2 2.2 30 5 (Chao False))),
            (0.2,(gera 6 5 6),(Jogador 4 3.5 0 5 (Morto 1))),
            (0.2,(gera 6 6 6),(Jogador 0 1 1 5 (Ar 1 15 2))),
            (0.2,(gera 2 5 1),(Jogador 1 3.9 3 5 (Chao True))),
            (0.2,(gera 5 17 666),(Jogador 4 4.1 3 5 (Ar 2.1 60 1))),
            (0.2,(gera 5 17 666),(Jogador 4 4.1 3 5 (Chao True)))]

-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera t m (Jogador pista x v c (Chao bool)) | (v < 2 && bool && novaV >= 0) = (Jogador pista x novaV c (Chao bool))
                                              | novaV' >= 0 = (Jogador pista x novaV' c (Chao bool))
                                              | otherwise = (Jogador pista x 0 c (Chao bool))
                                              where novaV = v + (1 - (atritoPeca pista x m)  * v) * t
                                                    novaV' = v + (- (atritoPeca pista x m)  * v) * t 
acelera t m (Jogador pista x v c (Ar h i g)) = (Jogador pista x newV c (Ar h i (g+t)))  
                                             where newV = if (v-(0.125*v*t)) >= 0 then(v-(0.125*v*t)) else 0
acelera t m j = j


-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t m j = case j of (Jogador pista x v c (Morto timeout)) -> if timeout-t > 0 
                                                                then (Jogador pista x 0 c (Morto (timeout-t))) 
                                                                else (Jogador pista x 0 c (Chao False))
                       _ -> testaTransicao j m t

-- | Função que avalia a peça para onde o jogador transitou e a peça anterior

testaTransicao :: Jogador -> Mapa -> Double -> Jogador
testaTransicao (Jogador pista x v c (Chao bool)) m t | x  >= fromIntegral (length (head m)-1)= (Jogador pista (fromIntegral (ceiling x)) 0 c (Chao False))
                                                     | x  == fromIntegral (ceiling x) && x+(vx)< (x+1) = (Jogador pista (x+(vx)) v c (Chao bool))
                                                     | x  == fromIntegral (ceiling x) && x+(vx)>= (x+1) = if  (incPecaAtual <=  incProxPeca)
                                                                                                         then (Jogador pista (x+1) v c (Chao bool))
                                                                                                         else (Jogador pista (x+1) v c (Ar ((alturaPeca pista (x+1) m)+0.01) incPecaAtual 0))
                                                     | x + vx > fromIntegral (ceiling x) = if  (incPecaAtual <= incProxPeca)
                                                                                             then (Jogador pista (fromIntegral (ceiling x)) v c (Chao bool))
                                                                                             else (Jogador pista (fromIntegral (ceiling x)) v c (Ar ((hPeca pista x m)+0.01) incPecaAtual 0))
                                                     | otherwise = (Jogador pista (x+vx) v c (Chao bool))  
                                                                 
                                                     where incPecaAtual = (inclinacaoPeca pista x m)
                                                           incProxPeca = (inclinacaoPeca pista (x+1) m)
                                                           vx = cos(incPecaAtual*(pi/180))*v*t
                                                       

testaTransicao (Jogador pista x v c (Ar h i g)) m t  | x  >=  fromIntegral (length (head m)-1)= (Jogador pista (fromIntegral (ceiling x)) 0 c (Chao False))
                                                     | x  == fromIntegral (ceiling x) && altura > alturaPeca pista x m = (Jogador pista  (x+vx) v c (Ar altura i g))
                                                     | x  == fromIntegral (ceiling x) && altura <= alturaPeca pista x m = (Jogador pista (x+vx) v c (Chao False))
                                                     | x + vx > fromIntegral (ceiling x) && altura > alturaPeca pista x m  = (Jogador pista (fromIntegral (ceiling x)) v c (Ar yTransicao i g))
                                                     | x + vx > fromIntegral (ceiling x) && altura <= alturaPeca pista x m = if abs((inclinacaoPeca pista x m) - i) >= 45 
                                                                                                                              then (Jogador pista xEmbate v c (Morto 1)) 
                                                                                                                              else (Jogador pista xEmbate v c (Chao False))
                                                     |otherwise = if altura > alturaPeca pista x m
                                                                  then (Jogador pista (x+vx) v c (Ar altura i g))
                                                                  else if abs((inclinacaoPeca pista x m) - i) >= 45 
                                                                       then (Jogador pista xEmbate v c (Morto 1))
                                                                       else (Jogador pista xEmbate v c (Chao False))
                                                     where incPecaAtual = (inclinacaoPeca pista x m)
                                                           incProxPeca = (inclinacaoPeca pista (x+1) m)
                                                           a = x + vx
                                                           vy = sin(i*(pi/180))*v*t
                                                           vx = cos(i*(pi/180))*v*t
                                                           altura = (h+vy-(g*t))
                                                           xEmbateInt = returnX(intersecao ((Cartesiano (fromIntegral (ceiling x)+1) (alturaPeca pista ((fromIntegral(ceiling x ))+1) m )),(Cartesiano (fromIntegral(floor(x))) (alturaPeca pista (fromIntegral(floor x )) m ))) ((Cartesiano x h),(Cartesiano (x+vx) altura)))
                                                           yTransicaoInt = returnY (intersecao ((Cartesiano x (alturaPeca pista x m )),(Cartesiano x h )) ((Cartesiano x h),(Cartesiano xEmbateInt altura))) 
                                                           xEmbate = returnX(intersecao ((Cartesiano (fromIntegral (ceiling x)) (alturaPeca pista (fromIntegral(ceiling x )) m )),(Cartesiano (fromIntegral(floor(x))) (alturaPeca pista (fromIntegral(floor x )) m ))) ((Cartesiano x h),(Cartesiano (x+vx) altura)))
                                                           yTransicao = returnY (intersecao ((Cartesiano (fromIntegral (ceiling x)) (alturaPeca pista (fromIntegral(ceiling x )) m )),(Cartesiano (fromIntegral(ceiling(x))) h )) ((Cartesiano x h),(Cartesiano (x+vx) altura))) 


-- Função que dado um ponto no mapa calcula a altura a que este se encontra

alturaPeca :: Int -> Double -> Mapa -> Double
alturaPeca pista x m = case encontraPosicaoMatriz (pista,fromIntegral(floor x)) m of Recta _ h  -> fromIntegral h
                                                                                     Rampa _ t h' -> if (t<h')
                                                                                                       then if (x == fromIntegral(floor x))
                                                                                                          then fromIntegral (t)
                                                                                                          else ((fromIntegral h') - (fromIntegral t))*(x - fromIntegral(floor x))+ (fromIntegral t)
                                                                                                       else if (x == fromIntegral(floor x))
                                                                                                          then fromIntegral (t)
                                                                                                          else (fromIntegral t - fromIntegral h')*(fromIntegral (ceiling x) -x)+ (fromIntegral h')
                                                                                                          
                                                                                                                                                                                                          
                                                                                                          

-- | Função que calcula a inclinação da Peca para onde o jogador transita
inclinacaoPeca :: Int -> Double -> Mapa -> Double
inclinacaoPeca pista x m = case encontraPosicaoMatriz (pista,fromIntegral(floor x)) m of Recta _ h  -> 0
                                                                                         Rampa _ t h' -> if (t<h') then atan (fromIntegral (h'-t)) * (180/pi)
                                                                                                                   else -atan (fromIntegral (t-h')) * (180/pi)
                                                                                      
                                                                              
-- |Devolve o valor do atrito da peça
atritoPeca :: Int    -- ^ A pista do jogador
           -> Double -- ^ A posição no eixo Ox do jogador
           -> Mapa   -- ^ O mapa atual
           -> Double -- ^ O atrito
atritoPeca pista x m = case pisoPeca (encontraPosicaoMatriz (pista,floor x) m) of Terra -> 0.25
                                                                                  Relva -> 0.75
                                                                                  Lama  -> 1.50
                                                                                  Boost -> -0.50
                                                                                  Cola  -> 3

-- | Dada uma peça devolve o piso correspondente                                                                                 
pisoPeca :: Peca -> Piso
pisoPeca (Recta piso _) = piso
pisoPeca (Rampa piso _ _) = piso

-- | Função que dado um ponto cartesiano devolve a abcissa desse ponto

returnX :: Ponto -> Double
returnX (Cartesiano x y) = x

-- | Função que dado um ponto cartesiano devolve a ordenada desse ponto

returnY :: Ponto -> Double
returnY (Cartesiano x y) = y