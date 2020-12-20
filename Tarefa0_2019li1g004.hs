-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2019li1g004 where
-- * Funções não-recursivas.

-- | Um ponto a duas dimensões dado num referencial cartesiano (distâncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesiano>>
-- , ou num referencial polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
data Ponto = Cartesiano Double Double | Polar Double Angulo 
      deriving Show

-- | Um ângulo em graus.
type Angulo = Double 

-- ** Funções sobre vetores

-- | Um 'Vetor' na representação escalar é um 'Ponto' em relação à origem.
type Vetor = Ponto
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>

--Funções gerais sobre 'Vetor'es------------------------------------------------

-- | Polar para cartesiano

polar2cartesian :: Ponto -> Ponto
polar2cartesian (Polar a b) = Cartesiano (a*cos(b*(pi/180))) (a*sin (b*(pi/180)))
polar2cartesian a = a
--https://www.mathsisfun.com/polar-cartesian-coordinates.html
--https://www.rapidtables.com/convert/number/degrees-to-radians.html

-- | 1. Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = Cartesiano (x1+x2) (y1+y2)
somaVetores a b = somaVetores (polar2cartesian a) (polar2cartesian b)

-- | 2. Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = Cartesiano (x1-x2) (y1-y2)
subtraiVetores a b = subtraiVetores (polar2cartesian a) (polar2cartesian b)

-- | 3. Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor x (Cartesiano x1 y1) = Cartesiano (x*x1) (x*y1)
multiplicaVetor x a = multiplicaVetor x (polar2cartesian a)




--Funções sobre rectas----------------------------------------------------------

-- | Um segmento de reta é definido por dois pontos.
type Reta = (Ponto,Ponto)

-- | 4. Testar se dois segmentos de reta se intersetam.

-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.

intersetam :: Reta -> Reta -> Bool
intersetam ((Cartesiano x1 y1),(Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) = 
    let a=(y3-y4)*(x1-x3)+(x4-x3)*(y1-y3)
        b=(x4-x3)*(y1-y2)-(x1-x2)*(y4-y3)
        c=(y1-y2)*(x1-x3)+(x2-x1)*(y1-y3)
    in if ((a/b)<1) && (0<(a/b)) && ((c/b)<1) && (0<(c/b)) then True else False
intersetam (a,b) (c,d) = intersetam ((polar2cartesian a),(polar2cartesian b)) ((polar2cartesian c),(polar2cartesian d))
                                                                                                 


-- | 5. Calcular o ponto de intersecao entre dois segmentos de reta.

-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.

intersecao :: Reta -> Reta -> Ponto
intersecao ((Cartesiano x1 y1),(Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) =
    let t=((y3-y4)*(x1-x3)+(x4-x3)*(y1-y3))/((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))
    in somaVetores (Cartesiano x1 y1) (multiplicaVetor t (subtraiVetores (Cartesiano x2 y2) (Cartesiano x1 y1)))
intersecao (a,b) (c,d) = intersecao (polar2cartesian a,polar2cartesian b) (polar2cartesian c,polar2cartesian d) 




-- * Funções sobre listas

--Funções gerais sobre listas.

-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | 6. Verifica se o indice pertence à lista.

-- __Sugestão:__ use a função 'length' que calcula tamanhos de listas

eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido i a = if (i<=(length a)-1 && i>=0) then True else False

-- * Funções sobre matrizes

--Funções gerais sobre matrizes.

-- | A dimensão de um mapa dada como um par (/número de linhas/,/número de colunhas/).
type DimensaoMatriz = (Int,Int)

-- | Uma posição numa matriz dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
--
-- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
type PosicaoMatriz = (Int,Int)

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | 7. Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
--
-- __Sugestão:__ relembre a função 'length', referida anteriormente.
dimensaoMatriz ::Eq a=> Matriz a -> DimensaoMatriz
dimensaoMatriz []= (0,0)
dimensaoMatriz (h:t) = if (h==[]) then dimensaoMatriz t else (length (h:t),length h)

-- | 8. Verifica se a posição pertence à matriz.

ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool 
ePosicaoMatrizValida (0,0) [] = False
ePosicaoMatrizValida (a,b) [] = False
ePosicaoMatrizValida (x,y) (h:t) = if ((length (h:t)-1>=x) && (length h-1>=y) && (x>=0) && (y>=0)) then True else False

-- | Funções recursivas
-- * Funções sobre ângulos

--  9. Normaliza um ângulo na gama [0..360).
--  Um ângulo pode ser usado para representar a rotação
--  que um objecto efectua. Normalizar um ângulo na gama [0..360)
--  consiste, intuitivamente, em extrair a orientação do
--  objecto que resulta da aplicação de uma rotação. Por exemplo, é verdade que:
--
-- prop> normalizaAngulo 360 = 0
-- prop> normalizaAngulo 390 = 30
-- prop> normalizaAngulo 720 = 0
-- prop> normalizaAngulo (-30) = 330

-- | 9. Normaliza um ângulo na gama [0..360).
normalizaAngulo :: Angulo -> Angulo
normalizaAngulo 0 = 0
normalizaAngulo a = if a>=360 then  normalizaAngulo(a-360) else if a<0 then normalizaAngulo(a+360) else a


-- | 10. Devolve o elemento num dado índice de uma lista.

-- __Sugestão:__ Não use a função (!!) :: [a] -> Int -> a :-)

encontraIndiceLista :: Int -> [a] ->a
encontraIndiceLista 0 (h:t)= h
encontraIndiceLista x (h:t)|eIndiceListaValido x (h:t)==True =  if (x-1 == 0) then head t else encontraIndiceLista (x-1) t


-- | 11. Modifica um elemento num dado índice.

-- __NB:__ Devolve a própria lista se o elemento não existir.

atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista x y (h:t)|eIndiceListaValido x (h:t)==True= if (x==0) then (y:t) else h:(atualizaIndiceLista (x-1) y t)
                             |otherwise = (h:t)
                             
-- * Funções sobre matrizes

-- | 12. Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.

encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz (x,y) z = encontraIndiceLista y (encontraIndiceLista x z)

-- | 13. Modifica um elemento numa dada 'Posicao'

-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.

atualizaPosicaoMatriz :: PosicaoMatriz -> b -> Matriz b -> Matriz b
atualizaPosicaoMatriz _ _ []= []
atualizaPosicaoMatriz (x,y) b (h:t)= atualizaIndiceLista x (atualizaIndiceLista y b (encontraIndiceLista x (h:t))) (h:t)


