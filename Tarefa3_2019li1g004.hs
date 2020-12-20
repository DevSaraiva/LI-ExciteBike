
{- |

= Introdução
    O objetiva desta Tarefa é comprimir ao máximo um Mapa de forma a que o resultado dessa compressão
seja um conjunto de instruções que sendo executadas por bulldozers contruam o mapa previamente comprimido. 

= Objetivos
    Inicialmente o objetivo foi traduzir o mapa de forma a que cada peça do mesmo correspondesse a uma instrução,
posteriormente esse conjunto de instruções teria de ser comprimido de forma a reduzir a lista de instruções a ser
efetuada.
    Depois da tradução comprimimos verticalmente o mapa comparando os tipos das instruções referentes ao fim de uma pista 
e ao ínicio de outra, quando estas são iguais ocorre compressão em instruções aplicadas às duas pistas.
    Por fim, realiza-se a compressão horizontal que usa o tipo repete para diminuir o número de instruções. Assim quando
n intruções aplicadas à mesma pista  são semelhantes estas são comprimidas em apenas uma instrução do tipo repete n, que repete
n vezes a mesma instrução. Para isso comparamos pista a pista as instruções semelhantes que ocorriam de seguida, agrupando-as 
posteriormente.

= Discussão e Conclusão
    Como resultado, obtivemos uma taxa de compressão não muito boa  a nível vertical, podendo ser melhorada caso comparassemos
não só o fim e o início de duas pistas adjacentes mas sim todas as pistas nas várias posições relativas, mas tivemos problemas ao
faze-lo a nível da ordem de leitura das instruções, o que provocava erros na leitura do mapa. A nível horizontal a compressão demonstrou 
ser bastante satisfatória e fiavél já que comprimiu o possível segundo o oráculo e devolveu o mapa correto em todos os testes efetuados.


-}


module Tarefa3_2019li1g004 where

import LI11920
import Tarefa0_2019li1g004
import Tarefa1_2019li1g004
import Tarefa2_2019li1g004

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [(gera 2 5 1),
            (gera 3 3 3),
            (gera 6 6 6),
            (gera 7 8 6),
            (gera 3 8 45),
            (gera 9 5 7),
            (gera 10 10 10),
            (gera 4 7 8),
            (gera 5 6 3),
            (gera 25 25 450),
            (gera 1 50 45),
            (gera 20 2 45)]

-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.
desconstroi :: Mapa -> Instrucoes
desconstroi m = agrupah (agrupaV (traducaoMapa m 0))

-- | Dado um mapa converte-o em instruções

traducaoMapa :: Mapa -> Int -> Instrucoes
traducaoMapa [] n = []
traducaoMapa (h:t) n = (traducaoPista h n) ++ (traducaoMapa t (n+1))

-- | Dado uma pista converte-a em instruções

traducaoPista :: [Peca] -> Int -> Instrucoes
traducaoPista [] n = []
traducaoPista (h:t) n | t /= [] = (traducaoPeca (head t) n):(traducaoPista t n)
                      | otherwise = []

-- | Dada uma peça converte-a numa instrução
traducaoPeca :: Peca -> Int -> Instrucao
traducaoPeca (Recta t h) n = Anda [n] t
traducaoPeca (Rampa t  hi hf) n | hi<hf = Sobe [n] t (hf-hi)
                                | otherwise = Desce [n] t (hi-hf)
    
-- | Agrupa verticalmente padrões semelhantes
agrupaV :: Instrucoes -> Instrucoes
agrupaV [] = []
agrupaV l  = somaListaI (take (nPistas l) l) ++ agrupaV (drop (nPistas l) l)

-- | Agrupa horizontamente instrucoes semelhantes
agrupah :: Instrucoes -> Instrucoes
agrupah [] = []
agrupah (h:t) |t /= [] = if (a > 1) then [(Repete a [h])]++(agrupah (dropWhile (==h) (h:t))) else h:(agrupah t)
              |otherwise = h:(agrupah [])
                       where a = length (takeWhile (==h) (h:t))

-- 

-- | Soma as listas de duas Instruções do mesmo tipo
somaI :: Instrucao -> Instrucao -> Instrucao
somaI (Anda l t) i2 = case i2 of (Anda l2 t) -> (Anda (l ++l2) t)
somaI (Sobe l t h) i2 = case i2 of (Sobe l2 t h) ->  (Sobe (l ++l2) t h)
somaI (Desce l t h) i2 = case i2 of (Desce l2 t h) -> (Desce (l ++l2) t h)
somaI (Repete n i) i2 = case i2 of (Repete n2 ii) -> if (i == ii && n /= n2) then (Repete (n+n2) i) else (Repete n (i++ii))

-- | Soma uma lista de instruções 
somaListaI :: Instrucoes -> Instrucoes
somaListaI [] = []
somaListaI [h] = [h]
somaListaI (x:y:ys) = if (insTest x y) then (somaListaI (a:ys)) else x:((somaListaI (y:ys)))
                    where a = (somaI x y)

-- | Testa se duas instruções são do mesmo tipo

insTest :: Instrucao -> Instrucao -> Bool
insTest (Anda l t) i2 = case i2 of {(Anda l2 t2) -> (if (t == t2 && subList l2 l == False) then True else False); _ -> False}
insTest (Sobe l t h) i2 = case i2 of {(Sobe l2 t2 h2) -> (if (t == t2 && subList l2 l == False && h ==h2) then True else False); _ -> False}
insTest (Desce l t h) i2 = case i2 of {(Desce l2 t2 h2) -> (if (t == t2 && subList l2 l == False && h ==h2) then True else False); _ -> False}
insTest (Repete n i) i2 = case i2 of {(Repete n2 ii) -> (if ((i == ii && n/= n2) || (i /= ii && n == n2)) then True else False); _ -> False}

-- | Dada uma lista de Instruções dá o número de pistas
nPistas :: Instrucoes -> Int
nPistas [h] = case (h) of Anda [n] a -> n+1
                          Sobe [n] a b -> n+1
                          Desce [n] a b -> n+1
                          Teleporta [n] b -> n+1
                          Repete n c -> nPistas c
nPistas (h:t) = case (last t) of Anda [n] a -> n+1
                                 Sobe [n] a b -> n+1
                                 Desce [n] a b -> n+1
                                 Teleporta [n] b -> n+1
                                 Repete n c -> nPistas c

-- | Testa se uma lista está contida noutra

subList :: Eq a => [a] -> [a] -> Bool
subList [] [] = True
subList _ []  = False
subList [] _  = True
subList (x:xs) (y:ys) 
    | x == y    = subList xs ys   
    | otherwise = subList (x:xs) ys

-- | Função que dada uma Instrução dá a primeira pista em que a mesma se aplica

getPista :: Instrucao -> Int
getPista a = case a of Anda (h:t) _ -> h
                       Sobe (h:t) _ _ -> h
                       Desce (h:t) _ _ -> h
                       Teleporta (h:t) _ -> h
                       Repete  _ (h:t)  -> getPista h

-- | Função que dada uma lista de Instruções e uma determinada pista separa todas as instruções que se aplicam a essa pista

separaPorPista :: Int -> Instrucoes -> Instrucoes
separaPorPista n [] = []
separaPorPista n (h:t)| getPista h == n = h:separaPorPista n t
                      | otherwise = separaPorPista n t
