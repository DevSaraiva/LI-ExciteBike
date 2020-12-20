{- |
= Introdução

A tarefa 5 constitui a tarefa central do projeto, pois é esta que liga todas as outras e nos permite visualizar o jogo. Esta tarefa utiliza a biblioteca “gloss”
 que permite representar graficamente o mapa, os jogadores e o decorrer do jogo. A principal dificuldade desta parte do projeto foi ter de aprender a usar o “gloss”
 , que é diferente daquilo que estávamos habituados em haskell.

= Objetivos

Todos os elementos importantes para o desenho do jogo num determinado instante estão incluídos no EstadoGloss. O primeiro objetivo que nós definimos foi desenhar um mapa
 usando a função “gera” da tarefa 1. A nossa abordagem passou por definir um “x” e um “y” iniciais que corresponderiam ao canto superior esquerdo do mapa (ponto (x,y)). Desse 
 ponto (x,y), a primeira linha corresponderia a traduzir a primeira peça para a imagem correspondente e somar ao “x” o comprimento da imagem.
Em seguida desenha-se a próxima peça aplicando uma translação para a direita correspondente ao comprimento da imagem da peça anterior. Este processo repete-se até acabar de desenhar a linha. 
A próxima linha seria desenhada aplicando uma translação para baixo correspondente à altura da imagem de uma peça. As rampas são desenhadas como polígonos pois este método é mais eficaz
 e produz melhores resultados que adicionar imagens individuais para cada tipo de rampa. 
Deste modo já era possível desenhar qualquer mapa.  A “seed” do mapa é gerada aleatoriamente ao abrir o jogo.

Em seguida desenhamos os jogadores, tarefa que foi facilitada pela existência dos tais “x”, “y” iniciais. Ao “x” inicial adicionamos a distancia percorrida pelo jogador e o jogador já é desenhado na posição correta. 
Para a altura, fazemos uma translação para cima correspondente à altura do jogador. Aqui foi preciso criar uma função que devolve a altura das peças para podermos fazer a referida translação quando o jogador está no estado “Chao”.
A função altura peça também serve para adicionar altura às peças do mapa, fazendo também uma translação vertical de cada peça pela sua altura.
Depois de termos o mapa e os jogadores a funcionar tivemos de adicionar à função “reageTempo” a tarefa 4, que corresponde à passagem do tempo. A tarefa 2 foi adicionada na função reageEvento, o que permitiu controlar os jogadores e efetivamente jogar.

Um problema surgiu neste ponto, ao abrir o jogo num computador com maior resolução o mapa ficava cortado. Assim foi preciso implementar um mecanismo de “scalling”. Foi definida uma função “comp :: Int -> Float ” que dado o comprimento
 e a altura de uma janela dava o comprimento que as peças individuas deveriam ter. Implementado este mecanismo a janela do jogo já podia ser redimensionada e o jogo responderia a este redimensionamento.
O passo seguinte foi desenvolver um menu de jogo que inclui três opções : um jogador, dois jogadores e jogar contra o bot da tarefa 6. Foi adicionado ao “EstadoGloss” um inteiro que correspondia ao botão clicado.
O “bot” foi adicionado na função “reageTempo”, onde a cada instante de tempo o “bot” faz uma decisão.
Foi adicionado um cronómetro no canto superior direito que mostra o tempo decorrido. 

Por último, foram adicionadas as mensagens de vitória que indicam o jogador vencedor e o tempo realizado por este e foi modificada a função “desenhaMapa” para passar a desenhar os jogadores intercalados com o mapa de modo que o 
 jogador não fique sempre acima do mapa, mesmo quando existe algo à sua frente.

= Conclusão

Esta tarefa colocou-nos diante de vários desafios que nos obrigaram a sermos criativos para os solucionar.  A nossa tarefa apresenta-se como um jogo funcional onde é possível jogar sozinho, jogar com outra pessoa e jogar contra um “bot”.
O jogo reage ao tamanho da janela, ajustando-se convenientemente e os mapas são aleatoriamente gerados no inicio do jogo.
Podemos considerar esta tarefa como a mais importante do jogo, pois é nesta tarefa onde acontece a magia de transformar código em algo com que podemos interagir.

-}
module Main where

 import Tarefa0_2019li1g004
 import Tarefa1_2019li1g004
 import Tarefa2_2019li1g004
 import Tarefa3_2019li1g004
 import Tarefa4_2019li1g004
 import Tarefa6_2019li1g004
 import LI11920
 import Graphics.Gloss
 import Graphics.Gloss.Juicy
 import Graphics.Gloss.Interface.Pure.Game
 import Graphics.Gloss.Interface.Pure.Simulate
 import Graphics.Gloss.Interface.Environment
 import System.Random

 -- | Estado que contem as caracteristicas necessárias á renderização do jogo
 type EstadoGloss = (Float,Float,Estado,[Picture],Menu,Int,(Int,Int),Int)
 -- __NB:__ Os dois Floats iniciais correspondem ao xInicial e yInicial respectivamente
 -- __NB:__ O antepenúltimo Int corresponde ao botão no menu inicial cujo rato está por cima
 -- __NB:__ O par (Int,Int) corresponde ás dimensões da janela no momento
 -- __NB:__ O último Int corresponde ao tempo

 -- | Informação sobre os menus 
 data Menu = Inicial | Pausa | NoMenu
           deriving Eq
 
 -- | Valor predefinido da largura da janela inicial
 
 xWindow :: Int
 xWindow = 900
 
 -- | Valor predefinido da altura da janela inicial

 yWindow :: Int 
 yWindow = 600

 -- | Comprimento individual de cada peça dada a largura da janela

 comp :: Int -> Float
 comp xW = (55/990)*(fromIntegral xW)
    
 -- | Função que cria o estado inicial do jogo atravês dos parametros estabelecidos pelo utilizador

 criaEstado :: Int -> Int -> Int -> Int -> Estado 
 criaEstado pista comprimento seed nJog = (Estado m (criaJog m nJog 0))
                                        where m = gera pista comprimento seed
   
 -- | Função auxiliar da função "criaEstado" que cria os a lista de jogadores a ser implementada no estado inicial do jogo

 criaJog :: Mapa -> Int -> Int -> [Jogador]
 criaJog m n i  | n > t = criaJog m t i              
                | i < n = (Jogador i 0 0 5 (Chao False)):(criaJog m n (i+1))
                | otherwise =  []
                where t = length m
                        
 -- | Função que cria o EstadoGloss inicial do jogo 

 estadoInicial :: [Picture] -> Int -> EstadoGloss
 estadoInicial li rnd = ((-400),(-50),(criaEstado 5 30 rnd 2),li,Inicial,0,(xWindow,yWindow),0)
   
 -- | Função que apresenta no ecrã o estado atual do jogo

 desenhaEstado :: EstadoGloss -> Picture
 desenhaEstado (x,y,(Estado m lj),li,menu,n,(xW,yW),t) | menu == Inicial && n == 0 = Pictures (fundo ++ logo ++ singlePlayerButton ++ multiplayerButton ++ info ++ botButton)
                                                       | menu == Inicial && n == 1 = Pictures (fundo ++ logo ++ [Translate 0 (50) (encontraIndiceLista 12 li)] ++ multiplayerButton ++ info ++ botButton)
                                                       | menu == Inicial && n == 2 = Pictures (fundo ++ logo ++ singlePlayerButton ++ [Translate 0 (-55) (encontraIndiceLista 14 li)] ++ info ++ botButton)
                                                       | menu == Inicial && n == 3 = Pictures (fundo ++ logo ++ singlePlayerButton ++ multiplayerButton ++ info ++ [Translate 0 (-160) (encontraIndiceLista 18 li)])
                                                       | menu == NoMenu && n == 4 = Pictures ([(Scale ((1.11/999)*fromIntegral (xW)) ((1.665/999)*fromIntegral (yW)) (encontraIndiceLista 20 li))] ++ [Translate (-50) 0 (Scale 0.4 0.4 (Color white (Text ((show (div t 100))++"s"))))])
                                                       | menu == NoMenu && n == 5 = Pictures ([(Scale ((1.11/999)*fromIntegral (xW)) ((1.665/999)*fromIntegral (yW)) (encontraIndiceLista 19 li))] ++ [Translate (-50) 0 (Scale 0.4 0.4 (Color white (Text ((show (div t 100))++"s"))))])
                                                       | menu == Pausa = encontraIndiceLista 6 li
                                                       | otherwise = Pictures ([(Scale ((1.11/999)*fromIntegral (xW)) ((1.665/999)*fromIntegral (yW)) (encontraIndiceLista 15 li))] ++ jogo ++ time)
                                                                   where l = ((comp xW)/2)
                                                                         logo = [Translate 0 200 (encontraIndiceLista 10 li)]
                                                                         info = [Translate 0 (-275) (encontraIndiceLista 16 li)]
                                                                         botButton = [Translate 0 (-160) (encontraIndiceLista 17 li)]
                                                                         multiplayerButton = [Translate 0 (-55) (encontraIndiceLista 13 li)]
                                                                         singlePlayerButton = [Translate 0 (50) (encontraIndiceLista 11 li)]
                                                                         fundo = [(Scale ((1.11/999)*fromIntegral (xW)) ((1.665/999)*fromIntegral (yW)) (encontraIndiceLista 6 li))]
                                                                         jogo = [Scale 0.5 0.5 (Translate (fromIntegral(div (-xW) 2)) (fromIntegral(div (-yW) 4)) (Pictures ((desenhaMapa (xW,yW) m m (-8*(comp xW)) (fromIntegral (div yW 16)) lj li))))]
                                                                         time =  [Translate ((fromIntegral(div xW 2)-l*2)) ((fromIntegral(div yW 2)-l*2)) (Scale 0.3 0.3 (Color white (Text (show (div t 100)))))]



 -- | Função auxiliar da função "desenhaEstado" que possiblita a vizualização de determinada lista de jogadores
    
 desenhaListaJogador :: (Int,Int) -> Int -> Mapa -> [Jogador] -> [Picture] -> [Picture]
 desenhaListaJogador _ _ _ [] _ = [] 
 desenhaListaJogador (xW,yW) n m (h:t) li = (desenhaJogador (xW,yW) n m h li : desenhaListaJogador (xW,yW) (n+1) m t li)
 
 -- | Função auxiliar da função "desenhaListaJogadores" que desenha individualmente um jogador

 desenhaJogador :: (Int,Int) -> Int -> Mapa -> Jogador -> [Picture] -> Picture
 desenhaJogador (xW,yW) n m (Jogador pista x v c e) li = case e of (Morto t)  -> if n == 0 then Translate ((fromIntegral(-(div xW 2))-l+(realToFrac x)*(comp xW))+(comp xW)) ((fromIntegral (div yW 16))+l-(fromIntegral pista)*(comp xW) +x') (Scale ((comp xW)/50) ((comp xW)/50) (encontraIndiceLista 8 li))
                                                                                           else Translate ((fromIntegral(-(div xW 2))-l+(realToFrac x)*(comp xW))+(comp xW)) ((fromIntegral (div yW 16))+l-(fromIntegral pista)*(comp xW) +x') (Scale ((comp xW)/50) ((comp xW)/50) (encontraIndiceLista 7 li))
                                                                                 where x' = realToFrac (alturaPeca pista x m)*(comp xW) 
                                                                                       l = (comp xW)/2
                                                                   (Ar h i g) -> if n == 0 then Translate ((fromIntegral(-(div xW 2))-l+(realToFrac x)*(comp xW))+(comp xW)) ((fromIntegral (div yW 16))+l-(fromIntegral pista)*(comp xW) +h') (Rotate (-i') (Scale ((comp xW)/50) ((comp xW)/50) (encontraIndiceLista 9 li)))
                                                                                           else Translate ((fromIntegral(-(div xW 2))-l+(realToFrac x)*(comp xW))+(comp xW)) ((fromIntegral (div yW 16))+l-(fromIntegral pista)*(comp xW) +h') (Rotate (-i') (Scale ((comp xW)/50) ((comp xW)/50) (encontraIndiceLista 5 li)))
                                                                                 where i' = realToFrac i
                                                                                       h' = (realToFrac h)*(comp xW)
                                                                                       l = (comp xW)/2
                                                                   (Chao _ )  -> if n == 0 then Translate ((fromIntegral(-(div xW 2))-l+(realToFrac x)*(comp xW))+(comp xW)) ((fromIntegral (div yW 16))+l-(fromIntegral pista)*(comp xW) + x') (Rotate (incChao) (Scale ((comp xW)/50) ((comp xW)/50) (encontraIndiceLista 9 li)))
                                                                                           else Translate ((fromIntegral(-(div xW 2))-l+(realToFrac x)*(comp xW))+(comp xW)) ((fromIntegral (div yW 16))+l-(fromIntegral pista)*(comp xW) + x') (Rotate (incChao) (Scale ((comp xW)/50) ((comp xW)/50) (encontraIndiceLista 5 li)))
                                                                                 where x' = realToFrac (alturaPeca pista x m)*(comp xW) 
                                                                                       incChao = (realToFrac (inclinacaoPeca pista x m) *(-1)) 
                                                                                       l = (comp xW)/2

 -- | Função auxiliar da função "desenhaEstado" que possiblita a vizualização de determinado mapa
 
 desenhaMapa ::  (Int,Int) -> Mapa -> Mapa -> Float -> Float -> [Jogador] -> [Picture]-> [Picture]
 desenhaMapa _ [] _ _ _ _ _ = []
 desenhaMapa (xW,yW) (h:t) m x y [(Jogador pista1 x1 v1 c1 e1)] li | pista1 == (5 - length (h:t)) = linha ++ [desenhaJogador (xW,yW) 0 m (Jogador pista1 x1 v1 c1 e1) li] ++ desenhaMapa ((fromIntegral xW),(fromIntegral yW)) t m (fromIntegral (-(div xW 2))) (y-(comp xW)) [(Jogador pista1 x1 v1 c1 e1)] li
                                                                   | otherwise = linha ++ desenhaMapa ((fromIntegral xW),(fromIntegral yW)) t m (fromIntegral (-(div xW 2))) (y-(comp xW)) [(Jogador pista1 x1 v1 c1 e1)] li
                                                                   where linha = desenhaLinha xW h (fromIntegral (-(div xW 2))) y li
 desenhaMapa (xW,yW) (h:t) m x y [(Jogador pista1 x1 v1 c1 e1),(Jogador pista2 x2 v2 c2 e2)] li | pista1 == (5 - length (h:t)) && pista2 == (5 - length (h:t)) =  linha ++ desenhaListaJogador (xW,yW) 0 m lj li ++  mapa
                                                                                                | pista1 == (5 - length (h:t)) = linha ++ [desenhaJogador (xW,yW) 0 m (Jogador pista1 x1 v1 c1 e1) li] ++ mapa
                                                                                                | pista2 == (5 - length (h:t)) = linha ++ [desenhaJogador (xW,yW) 1 m (Jogador pista2 x2 v2 c2 e2) li] ++ mapa
                                                                                                | otherwise = linha ++ mapa
                                                                                                where lj = [(Jogador pista1 x1 v1 c1 e1),(Jogador pista2 x2 v2 c2 e2)]
                                                                                                      linha = desenhaLinha xW h (fromIntegral (-(div xW 2))) y li
                                                                                                      mapa =  desenhaMapa ((fromIntegral xW),(fromIntegral yW)) t m (fromIntegral (-(div xW 2))) (y-(comp xW)) lj li
 -- | Função auxiliar da função desenhaMapa que desenha as pistas do mapa

 desenhaLinha :: Int -> [Peca] -> Float -> Float -> [Picture] -> [Picture]
 desenhaLinha _ [] _ _ _ = []
 desenhaLinha xW (h:t) x y li = desenhaPeca xW h x y li : desenhaLinha xW t (x+(comp xW)) y li

 -- | Função auxiliar da função desenhaMapa que desenha as peças do mapa e os poligonos abaixo destas

 desenhaPeca :: Int -> Peca -> Float -> Float -> [Picture] -> Picture
 desenhaPeca xW (Recta Terra h) x y li = Translate (x+(comp xW)) (y+((fromIntegral h)*(comp xW))) (Pictures [(Scale ((comp xW)/50) ((comp xW)/50) (encontraIndiceLista 0 li)),Color (makeColor 0.45 0.24 0.05 0.8) (polygon [(-l,-l),(-l,-l-((fromIntegral h)*(comp xW))),(l,-l-((fromIntegral h)*(comp xW))),(l,-l)])])
                                       where l = ((comp xW))/2                                  
 desenhaPeca xW (Recta Lama h) x y li  = Translate (x+(comp xW)) (y+((fromIntegral h)*(comp xW))) (Pictures [(Scale ((comp xW)/50) ((comp xW)/50) (encontraIndiceLista 1 li)),Color (makeColor 0.24 0.15 0.07 0.8) (polygon [(-l,-l),(-l,-l-((fromIntegral h)*(comp xW))),(l,-l-((fromIntegral h)*(comp xW))),(l,-l)])])
                                       where l = ((comp xW))/2
 desenhaPeca xW (Recta Relva h) x y li = Translate (x+(comp xW)) (y+((fromIntegral h)*(comp xW))) (Pictures [(Scale ((comp xW)/50) ((comp xW)/50) (encontraIndiceLista 2 li)),Color (makeColor 0.04 0.29 0.12 0.8) (polygon [(-l,-l),(-l,-l-((fromIntegral h)*(comp xW))),(l,-l-((fromIntegral h)*(comp xW))),(l,-l)])])
                                       where l = ((comp xW))/2
 desenhaPeca xW (Recta Boost h) x y li = Translate (x+(comp xW)) (y+((fromIntegral h)*(comp xW))) (Pictures [(Scale ((comp xW)/50) ((comp xW)/50) (encontraIndiceLista 3 li)),Color (makeColor 0.27 0.27 0.27 0.8) (polygon [(-l,-l),(-l,-l-((fromIntegral h)*(comp xW))),(l,-l-((fromIntegral h)*(comp xW))),(l,-l)])])
                                       where l = ((comp xW))/2
 desenhaPeca xW (Recta Cola h) x y li  = Translate (x+(comp xW)) (y+((fromIntegral h)*(comp xW))) (Pictures [(Scale ((comp xW)/50) ((comp xW)/50) (encontraIndiceLista 4 li)),Color (makeColor 0.76 0.66 0 0.8) (polygon [(-l,-l),(-l,-l-((fromIntegral h)*(comp xW))),(l,-l-((fromIntegral h)*(comp xW))),(l,-l)])])
                                       where l = ((comp xW))/2
 desenhaPeca xW (Rampa Terra hi hf) x y _ = auxDesenhaPeca xW (Rampa Terra hi hf) (makeColor 0.6 0.38 0.26 0.8) (makeColor 0.45 0.24 0.05 0.85) x y
 desenhaPeca xW (Rampa Lama hi hf) x y _  = auxDesenhaPeca xW (Rampa Lama hi hf) (makeColor 0.29 0.15 0.07 0.8) (makeColor 0.24 0.15 0.07 0.85) x y
 desenhaPeca xW (Rampa Relva hi hf) x y _ = auxDesenhaPeca xW (Rampa Relva hi hf) (makeColor 0.07 0.79 0.12 0.8) (makeColor 0.04 0.29 0.12 0.85) x y
 desenhaPeca xW (Rampa Boost hi hf) x y _ = auxDesenhaPeca xW (Rampa Relva hi hf) (makeColor 0.17 0.17 0.17 0.8) (makeColor 0.27 0.27 0.27 0.85) x y
 desenhaPeca xW (Rampa Cola hi hf) x y _  = auxDesenhaPeca xW (Rampa Relva hi hf) (makeColor 1 0.87 0 0.8) (makeColor 0.76 0.66 0 0.85) x y
                                          where l = ((comp xW))/2
 
 -- | Função que desenha rampas e os poligonos abaixo destas

 auxDesenhaPeca :: Int -> Peca -> Color -> Color -> Float -> Float -> Picture
 auxDesenhaPeca xW (Rampa _ hi hf) color color2 x y = if (hi<hf) 
                                                      then Translate (x+(comp xW)) (y+hRampaI) (Pictures [Color color (polygon [(-l,-l),(-l,l),(l,hRampaU+(comp xW)),(l,hRampaU)]),Color color2 (polygon [(-l,-l),(l,hRampaU),(l,-l)]),Color color2 (polygon [(-l,-l),(l,-l),(l,-l-hRampaI),(-l,-l-hRampaI)])])
                                                      else Translate (x+(comp xW)) (y+hRampaF) (Pictures [Color color (polygon [(-l,hRampaD+(comp xW)),(-l,hRampaD),(l,-l),(l,l)]),Color color2 (polygon [(-l,-l),(l,-l),(-l,hRampaD)]),Color color2 (polygon [(-l,-l),(l,-l),(l,-l-hRampaF),(-l,-l-hRampaF)])])
                                                      where l = (comp xW)/2
                                                            hRampaI = (fromIntegral hi)*(comp xW)
                                                            hRampaF = (fromIntegral hf)*(comp xW)
                                                            hRampaU = 2*((fromIntegral (abs (hf-hi)))*l)-l
                                                            hRampaD = 2*((fromIntegral (abs (hf-hi)))*l)-l

 -- | Função que calcula a jogada efetuada atravês dos diferentes eventos do teclado
    
 reageEvento :: Event -> EstadoGloss -> EstadoGloss
 reageEvento (EventResize (x1,y1)) (x,y,(Estado m lj),li,menu,n,(xW,yW),t) = (x,y,(Estado m lj),li,menu,n,(x1,y1),t)
 reageEvento (EventMotion (x1,y1)) (x,y,(Estado m lj),li,menu,n,(xW,yW),t) = if (x1<=100 && x1>=(-100) && y1<=100 && y1>=(0) && menu == Inicial) 
                                                                             then (x,y,(Estado m lj),li,Inicial,1,(xW,yW),t)
                                                                             else if (x1<=100 && x1 >= (-100) && y1<= (-5) && y1 >= (-105) && menu == Inicial)
                                                                                  then (x,y,(Estado m lj),li,Inicial,2,(xW,yW),t)
                                                                                  else if (x1<=100 && x1 >= (-100) && y1<= (-110) && y1 >= (-210) && menu == Inicial)
                                                                                       then (x,y,(Estado m lj),li,Inicial,3,(xW,yW),t)
                                                                                       else if menu == Inicial then (x,y,(Estado m lj),li,menu,0,(xW,yW),t)
                                                                                                               else (x,y,(Estado m lj),li,menu,n,(xW,yW),t)
 reageEvento (EventKey (MouseButton rightClick) Down _ (x1,y1)) (x,y,(Estado m lj),li,menu,n,(xW,yW),t) = if (x1<=100 && x1>=(-100) && y1<=100 && y1>=(0) && menu /= NoMenu) 
                                                                                                        then (x,y,(Estado (gera 5 35 t) (criaJog (gera 5 30 t) 1 0)),li,NoMenu,1,(xW,yW),0)
                                                                                                        else if (x1<=100 && x1 >= (-100) && y1<= (-5) && y1 >= (-105) && menu /= NoMenu)
                                                                                                             then (x,y,(Estado (gera 5 35 t) (criaJog (gera 5 30 t) 2 0)),li,NoMenu,2,(xW,yW),0)
                                                                                                             else if (x1<=100 && x1 >= (-100) && y1<= (-110) && y1 >= (-210) && menu /= NoMenu)
                                                                                                                  then (x,y,(Estado (gera 5 35 t) (criaJog (gera 5 30 t) 2 0)),li,NoMenu,3,(xW,yW),0)
                                                                                                                  else (x,y,(Estado m lj),li,menu,n,(xW,yW),t)
 reageEvento (EventKey (SpecialKey KeySpace) Down _ _) (x,y,(Estado m lj),li,menu,n,(xW,yW),t) = (x,y,(Estado m (criaJog (gera 5 35 t) 2 0)),li,Inicial,0,(xW,yW),t)                                                                                   
 reageEvento evt (x,y,e,li,menu,n,(xW,yW),t) = (x,y,e',li,menu,n,(xW,yW),t) 
                                   where e' = selecionaJogada evt e n   
    
 -- | Funçãu auxiliar da funcção reageEvento que seleciona a jogada a efetuar    

 selecionaJogada :: Event -> Estado -> Int -> Estado 
 selecionaJogada (EventKey (SpecialKey KeyUp) Down _ _) e n = (jogada 0 (Movimenta C) e)
 selecionaJogada (EventKey (SpecialKey KeyDown) Down _ _) e n = (jogada 0 (Movimenta B) e)
 selecionaJogada (EventKey (SpecialKey KeyLeft) Down _ _) e n = (jogada 0 (Movimenta E) e)
 selecionaJogada (EventKey (SpecialKey KeyRight) Down _ _) e n = (jogada 0 (Movimenta D) e)
 selecionaJogada (EventKey (Char 'c' ) Down _ _) e n = (jogada 0 (Dispara) e)
 selecionaJogada (EventKey (Char 'w' ) Down _ _) e n = if (n==2) then (jogada 1 (Movimenta C) e) else e
 selecionaJogada (EventKey (Char 's' ) Down _ _) e n = if (n==2) then (jogada 1 (Movimenta B) e) else e
 selecionaJogada (EventKey (Char 'a' ) Down _ _) e n = if (n==2) then (jogada 1 (Movimenta E) e) else e
 selecionaJogada (EventKey (Char 'd' ) Down _ _) e n = if (n==2) then (jogada 1 (Movimenta D) e) else e
 selecionaJogada (EventKey (Char 'f' ) Down _ _) e n = if (n==2) then (jogada 1 (Dispara) e) else e
 selecionaJogada (EventKey (SpecialKey KeyCtrlR) Down _ _) e n =  (jogada 0 Acelera e)
 selecionaJogada (EventKey (SpecialKey KeyCtrlL) Down _ _) e n = if (n==2) then (jogada 1 Acelera e) else e
 selecionaJogada (EventKey (SpecialKey KeyCtrlR) Up _ _) e n =  (jogada 0 Desacelera e)
 selecionaJogada (EventKey (SpecialKey KeyCtrlL) Up _ _) e n = if (n==2) then (jogada 1 Desacelera e) else e
 selecionaJogada _ e _ = e  --ignora qualquer outro Evento                                                             
                                                                 
    
 -- | Função que altera o estadoGloss do jogo mediante a passagem do tempo 

 reageTempo :: Float -> EstadoGloss -> EstadoGloss
 reageTempo t' (x,y,(Estado m lj),li,menu,n,(xW,yW),t) | n == 4 || n == 5 = (x,y,(Estado m []),li,menu,n,(xW,yW),t)
                                                       | vencedor lj m n == 0 = (x,y,e',li,menu,n,(xW,yW),(t+1))
                                                       | vencedor lj m n == 1 = (x,y,e',li,menu,4,(xW,yW),t)
                                                       | vencedor lj m n == 2 = (x,y,e',li,menu,5,(xW,yW),t)
                                                       | vencedor lj m n == 3 = (x,y,(Estado m lj),li,menu,n,(xW,yW),t)
                                                       | otherwise = (x,y,e',li,menu,n,(xW,yW),t)
                                                       where e' = if n==3 then auxTempo (bot 1 (Estado m lj)) (alteraEstado t' (Estado m lj))
                                                                          else alteraEstado t' (Estado m lj)

 -- | Função que devolve o jogador vencedor. 0 se nenhum venceu ainda , 1 se foi o primeiro e 2 se foi o segundo e 3 se a lista de jogadores for vazia

 vencedor :: [Jogador] -> Mapa -> Int -> Int
 vencedor [] _ _ = 3
 vencedor [(Jogador pista x v c e)] m n = if x >= fromIntegral (length (head m)-1) then 1 else 0
 vencedor [(Jogador pista x v c e),(Jogador pista' x' v' c' e')] m n | x >= fromIntegral (length (head m)-1) = 1
                                                                     | x'>= fromIntegral (length (head m)-1) = 2
                                                                     | otherwise = 0



 -- | Função auxiliar que converte as jogadas do bot num estado

 auxTempo :: Maybe Jogada -> Estado -> Estado
 auxTempo Nothing e = e
 auxTempo (Just jog) e = jogada 1 jog e


 -- | Função auxiliar da função reageTempo que altera o estado de acordo com a passagem do tempo
    
 alteraEstado :: Float  -> Estado -> Estado
 alteraEstado t' (Estado m lj) = (Estado m lj')
                               where lj' = alteralj t' (Estado m lj)
                                
    
 -- | Função auxiliar da função altera estado que é aplicada apenas às listas de jogadores

 alteralj :: Float -> Estado -> [Jogador]
 alteralj t (Estado m []) = []
 alteralj t (Estado m (x:xs)) = (passo t' m x):(alteralj t (Estado m xs))
                              where t' = realToFrac t   


 -- | Frame rate
 fr :: Int
 fr = 100

 -- | Display 
 dm :: Display
 dm = InWindow "Excite Bike" displaySize (0,0)

 -- | Tamanho do display
 displaySize :: (Int,Int)
 displaySize = (xWindow,yWindow)
    
 -- | Função principal da Tarefa 5.
 --
 -- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
 
 main :: IO ()
 main = do
        rnd <- randomRIO (1,100)
        boost <- loadBMP "Textures/boost.bmp"
        dirt <- loadBMP "Textures/dirt.bmp"
        glue <-loadBMP "Textures/glue.bmp"
        grass <- loadBMP "Textures/grass.bmp"
        mud <- loadBMP "Textures/mud.bmp"
        player <- loadBMP "Textures/bike.bmp"
        player2 <- loadBMP "Textures/bike2.bmp"
        playerDead <- loadBMP "Textures/playerDead.bmp"
        playerDead2 <- loadBMP "Textures/playerDead2.bmp"
        logo <- loadBMP "Textures/logo.bmp"
        button0 <- loadBMP "Textures/button0.bmp"
        button1 <- loadBMP "Textures/button1.bmp"
        button2 <- loadBMP "Textures/button2.bmp"
        button3 <- loadBMP "Textures/button3.bmp"
        button4 <- loadBMP "Textures/button4.bmp"
        button5 <- loadBMP "Textures/button5.bmp"
        vitoria1 <- loadBMP "Textures/vitoria1.bmp"
        vitoria2 <- loadBMP "Textures/vitoria2.bmp"
        Just fundo <- loadJuicy "Textures/fundo.jpg"
        Just fundo1 <- loadJuicy "Textures/fundo1.jpg"
        info <- loadBMP "Textures/info.bmp"
        play dm         -- janela onde irá correr o jogo
             (greyN 0.5)     -- côr do fundo da janela
             fr              -- frame rate
             (estadoInicial [dirt,mud,grass,boost,glue,player,fundo,playerDead,playerDead2,player2,logo,button0,button1,button2,button3,fundo1,info,button4,button5,vitoria1,vitoria2] rnd)   -- estado inicial
             desenhaEstado  -- desenha o estado do jogo
             reageEvento    -- reage a um evento
             reageTempo     -- reage á passagem de tempo