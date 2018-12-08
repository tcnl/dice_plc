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

    --While(ngm ganhou nem zerou) comecaria aqui

    --Inicio, a pessoa aposta num valor que vai cair(1-6)
    auxTabela <- takeMVar tabela
    --if(auxTabela[0] < 70)
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

    --Se a pessoa acertar o valor do dado, ganha os pontos se nao, perde.
    --Mas o if/else ta sempre dando erro de parse
    --if(aposta == dado) then 
    --     let parcial1 =  aposta
    --else let parcial1 = -aposta

    --FIM if(auxTabela[0] < 70)
    --else if(auxTabela[0] >= 70 & auxTabela[0] < 85)
        print "Jogador 1 aposte um numero: "
        palpite <- getLine
        let input = (read palpite :: Int)

        print "Jogador 1 aposte outro numero: "
        palpite <- getLine
        let input = (read palpite :: Int)
        --PAREI POR AQUI sono da mizera
        
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
    --     let parcial2 =  aposta
    --else let parcial2 = -aposta

    forkIO $ do putMVar tabela parcial1; putMVar tabela number
    --Salva a tabela original [50,50]
    auxTabela <- takeMVar tabela
    --tabela livre, MVar recebe parcial1

    let auxTabela[0] = auxTabela[0] + (takeMVar tabela)
    --valor dos pontos do J1 ajustados, libera tabela

    let auxTabela[1] = auxTabela[1] + (takeMVar tabela)
    --valor dos pontos do J2 ajustados, libera tabela
    putMVar tabela auxTabela
    
    --Fim do while
    
