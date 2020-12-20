-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g004 where

import LI11920
import System.Random
import Tarefa0_2019li1g004
-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(3,5,354),
            (10,20,51),
            (1,1,1),
            (2,5,8),
            (2,4,34),
            (4,4,467),
            (3,9,5),
            (10,10,54),
            (23,56,43),
            (1,45,66)]


-- * Funções pré-definidas da Tarefa 1.

-- | Gera valores aleaorios
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.
--
-- | Gera uma mapa dado um número de pistas, um comprimento e uma seed
gera :: Int -> Int -> Int -> Mapa
gera npistas 1 l | npistas >0 = [[(Recta Terra 0)]]++(gera (npistas-1) 1 l)
                 |otherwise = []
gera npistas comprimento semente = geranpistas 1 ((comprimento-1)*2) a
                                 where a = geraAleatorios (2*(comprimento-1)*npistas) semente

-- | Gera Piso dado uma lista         
gGround :: Int -> [Int] -> Piso
gGround i l | elem a [0..1] = Terra
              | elem a [2..3] = Relva
              | a==4 = Lama
              | a==5 = Boost
              | otherwise = if (i==0) then Terra else gGround (i-2) l 
              where a = encontraIndiceLista i l 

-- | Gera uma Peca dado um índice de uma lista
gPeca :: Int -> [Int] -> Peca
gPeca i l | a == 0 = Rampa x h (h+1)
          | a == 1 = Rampa x h (h+2)
          | elem a [2..5] && h==0 = Recta x h
          | a == 2 && (h-1)>=0 = Rampa x h (h-1)
          | a == 3 && (h-2)>=0 = Rampa x h (h-2)
          | a == 4 && (h-3)>=0 = Rampa x h (h-3)
          | a == 5 && (h-4)>=0 = Rampa x h (h-4) 
          | elem a [2..5] = Rampa x h 0
          | elem a [6..9] = Recta x h
          where x = gGround (i-1) l
                h = if (i<=1) then 0 else rHigh(gPeca (i-2) l)
                a = encontraIndiceLista i l


-- | Dada uma Peca dá a respetiva altura
rHigh :: Peca -> Int
rHigh (Rampa _ _ h) = h
rHigh (Recta _ h) = h

-- | Gera uma pista dado uma lista e um comprimento
gAllTrack :: Int -> Int -> [Int] -> [Peca]
gAllTrack _ _ [] = []
gAllTrack  i c l |i < c = (gPeca i l):(gAllTrack(i+2) c l)
                 | otherwise = [] 

-- | Gera n pistas
geranpistas :: Int -> Int -> [Int] -> Mapa
geranpistas i c l |l/=[] = ([Recta Terra 0]++(gAllTrack i c l)):(geranpistas i c (drop' c l))
                  |otherwise = []

-- | Retira o ultimo elemento da lista
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 ys = ys
drop' x ys = drop' (x-1) (tail ys)
