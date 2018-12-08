import System.Random
import Control.Concurrent
import Text.Printf
import Control.Monad
import System.IO

--CHECKLIST / O QUE PRECISA
--CADA JOGADOR JOGA O(S) DADO(S) e coloca o resultado da pontuacao na MVar
--O placar pega as pontuacoes da MVar e vai mostrando o placar a cada jogada

--Funcoes

--Funcao do dado


main :: IO ()
main = do
    --MVar que a gente vai usar
    tabela <- newEmptyMVar
    jogo <- newEmptyMVar
    --Pontos iniciais
    putMVar tabela [50,50]

    --Inicio, a pessoa aposta num valor que vai cair(1-6)
    print "Jogador 1 aposte: "
    palpite <- getLine
    let input = (read palpite :: Int)
    
    --Lanca o(s) dado(s)
    number <- randomRIO (1,6) :: IO Int

    --Inicia duas thread, uma salva a aposta na MVar e bloqueia
    --A outra vai ficar esperando a MVar ficar livra pra colocar o resultado do(s) dado(s)
    forkIO $ do putMVar jogo input; putMVar jogo number
    
    --Pega o valor de MVar(aposta inicial) e coloca e salva
    aposta <- takeMVar jogo
    --Assim a MVar fica livre e recebe o valor dos dados (acima)
    print aposta

    --MVar fica livre de novo
    dado   <- takeMVar jogo
    print dado
    --if(aposta == dado) then 
    --     let pontos1 = (pontos + aposta)
    --else let pontos1 = (pontos - aposta)

    --O mesmo de cima, so que pro Jogador 2
    print "Jogador 2 aposte: "
    palpite <- getLine
    let input = (read palpite :: Int)
    
    --Lanca o(s) dado(s)
    number <- randomRIO (1,6) :: IO Int

    --Inicia duas thread, uma salva a aposta na MVar e bloqueia
    --A outra vai ficar esperando a MVar ficar livra pra colocar o resultado do(s) dado(s)
    forkIO $ do putMVar jogo input; putMVar jogo number
    
    --Pega o valor de MVar(aposta inicial) e coloca e salva
    aposta <- takeMVar jogo
    --Assim a MVar fica livre e recebe o valor dos dados (acima)
    print aposta

    --MVar fica livre de novo
    dado   <- takeMVar jogo
    print dado
    --if(aposta == dado) then 
    --     let pontos2 = (pontos + aposta)
    --else let pontos2 = (pontos - aposta)
