{- |
= Introdução
    Esta tarefa consistiu na criação de um bot, isto é, na criação de uma inteligência artificial primitiva capaz de jogar o jogo automaticamente
e de forma possivelmente mais eficaz que um humano.


= Objetivos
    Como principal objetivo desta Tarefa assumimos o tempo, ou seja, tentamos minimizar o tempo necessário para concluir um mapa ou percorrer
determinado número de peças, criando assim um bot semi passivo que prioriza o seu avanço em relação à posição dos oponentes. Com isto queremos
dizer que o robot apenas dispara quando se encontra na posição ideal, posição esta que é verificada atravês da função passo da tarefa 4. A cada
jogada o robot calcula a distância que percorrerá até à próxima jogada em diferentes pistas. Caso a melhor posição para o jogador seja numa pista 
diferente da que se encontra no momento da jogada, o bot seleciona a jogada necessária para que o jogador se mova para a melhor posição (considerando 
que a mudança pode ser mortal), caso contrário o robot opta por um comportamento mais agressivo e Dispara. Quando o jogador se encontra no ar 
o robot calcula qual será a peça de embate com o solo e consecutivamente a sua inclinação , determinado assim as jogadas necessárias para não 
morrer no embate devido à sua inclinação.

= Discussão e conclusão
    A nível de resultados, obtivemos um bot que em não morreu em nenhum dos testes localmente efectuados e que torna quase impossivel a vitória
de um concorrente humano. Demonstra uma grande capacidade de evitar obstáculos que abrandem a sua progressão e usufrui sempre dos  blocos boost
espalhados pelas diferentes pistas.  


-}

module Tarefa6_2019li1g004 where
import Tarefa0_2019li1g004
import Tarefa1_2019li1g004
import Tarefa2_2019li1g004
import Tarefa3_2019li1g004
import Tarefa4_2019li1g004
import LI11920

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot  n (Estado m jog) = case e of (Chao False) -> (Just Acelera)
                                  (Morto _) -> (Just Acelera)
                                  (Ar _ _ _) -> verfInc j m
                                  _ -> verfPista j m 
                      where (Jogador pista x v c e) = retJog n (Estado m jog)
                            j = retJog n (Estado m jog)

-- | Função que dado um estado e o identificador de Jogador devolve o jogador desejado

retJog :: Int -> Estado -> Jogador
retJog 0 (Estado m (x:xs)) = x
retJog n (Estado m (x:xs)) = retJog (n-1) (Estado m xs)

-- | Função que dado um jogador devolve a distancia percorrida pelo mesmo

distJog :: Jogador -> Double
distJog (Jogador pista x v c e) = x


-- | Função que sendo fornecidos 3 posições de um Jogador devolve o que se encontra em melhor posição

bestOf3 :: (Jogador,Int) -> (Jogador,Int) -> (Jogador,Int) -> Int
bestOf3 (a,a1) (b,b1) (c,c1) = if (a' >= b' && a'>= c') then a1 else if (b' >= a' && b' >= c') then b1 else c1
                            where a' = distJog a
                                  b' = distJog b
                                  c' = distJog c
              

-- | Função que retarda a posição do jogador para que o estado de tempo em que se encontra morto seja penalizado pela função verfPista

retardMorto :: Jogador -> Jogador
retardMorto (Jogador pista x v c e) = case e of (Morto _) -> (Jogador pista 0 v c e)
                                                _ -> (Jogador pista x v c e)




-- | Função que verifica qual a melhor posição do Jogador e devolve a jogada a ser efetuada

verfPista :: Jogador -> Mapa -> Maybe Jogada
verfPista j m | x == 1 = Just (Movimenta C)
              | x == 2 = Just Dispara
              | x == 3 = Just (Movimenta B) 
              where c = alteraJogador j (Movimenta C) m
                    b = alteraJogador j (Movimenta B) m
                    w = retardMorto(passo 0.2 m c)
                    f = retardMorto(passo 0.2 m j)
                    s = retardMorto(passo 0.2 m b)
                    x = bestOf3 (w,1) (f,2) (s,3)


-- | Função que verifica qual a melhor jogada quando o jogador se encontra no ar

verfInc :: Jogador -> Mapa -> Maybe Jogada
verfInc (Jogador pista x v c (Ar h i g)) m | abs (inc - i) >= 45 = if i > inc then Just (Movimenta D) else Just (Movimenta E)
                                           | otherwise = Nothing
                                          where x' = aterragemX (Jogador pista x v c (Ar h i g)) m
                                                inc = inclinacaoPeca pista x' m

-- | Função que calcula peça de aterragem do jogdador que se encontra no ar

aterragemX :: Jogador -> Mapa -> Double
aterragemX j m = case e of (Chao _) -> x
                           _ -> aterragemX (Jogador pista x v c e) m
              where (Jogador pista x v c e) = (passo 0.2 m j)

