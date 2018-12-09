import System.Random
import Control.Concurrent
import Text.Printf
import Control.Monad
import System.IO


--CHECKLIST / O QUE PRECISA (ORDEM DE PRIORIDADE)
-- 1- Fazer algo similar a um while pro jogo poder ficar rodando, dei uma lida e talvez algo semelhante a recursao
-- 2- Arrumar os if/else, ja que eles que vao ditar a pontuacao, se e positiva ou negativa
-- 3- Terminar dificuldade 2
-- 4- Colocar dificuldade 2 no jogador 2
-- 5- Fazer dificuldade 3
-- 6- Colocar dificuldade 3 no jogador 2
-- 7- Testar

--Eletivos
-- 1- Generalizar 4 e 6

parcial :: Int -> Int -> Int -> Int
parcial aposta dado parcial
    | aposta == dado = (parcial + dado)
    | otherwise = (parcial - dado)

level :: Int -> Int
level n
    | n >= 100 = error "Você ganhou!"
    | n > 85 = 3
    | n > 70 = 2
    | n > 50 = 1
    | n == 0 = error "Você perdeu!"

pontos_nivel_dois :: Int -> Int -> Int
nivel_dois dado1 dado2
    | dado1 == dado2 = dado1
    | otherwise = 0

pontos_nivel_tres :: Int -> Int -> Int -> Int
pontos_nivel_dois dado1 dado2 dado3
    | (dado1 == dado2 && dado2 == dado3) = dado1
    | otherwise = 0



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
    --Aqui entra um if/else das dificuldades
        --Se tiver >70 pontos, lanca dois dados
        --Se tiver >85 pontos, lanca tres dados
        --Ai so precisa ajustar depois a ordem na MVar
    number <- randomRIO (1,6) :: IO Int

    --Inicia duas thread, uma salva a aposta na MVar e bloqueia
    --A outra vai ficar esperando a MVar ficar livra pra colocar o resultado do(s) dado(s)
    forkIO $ do putMVar jogo input; putMVar jogo number

    --Pega o valor de MVar(aposta inicial) e coloca e salva
    aposta <- takeMVar jogo
    --Assim a MVar fica livre e recebe o valor dos dados (acima)
    print $ "Aposta: " ++ show aposta

    --MVar fica livre de novo
    dado   <- takeMVar jogo
    print $ "Dado: " ++ show dado

    let parcial1 = 50

    -- let parcial1 = (parcial aposta dado parcial1)

    print $ "Parcial 1: " ++ show (parcial aposta dado parcial1)

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
    print $ "Aposta: " ++ show aposta

    --MVar fica livre de novo
    dado   <- takeMVar jogo
    print $ "Dado: " ++ show dado

    let parcial2 = 50

    print $ "Parcial 2: " ++ show (parcial aposta dado parcial2)


    -- forkIO $ do putMVar tabela parcial1; putMVar tabela number
    -- --Salva a tabela original [50,50]
    -- auxTabela <- takeMVar tabela
    -- --tabela livre, MVar recebe parcial1

    -- let auxTabela[0] = auxTabela[0] + (takeMVar tabela)
    -- --valor dos pontos do J1 ajustados, libera tabela

    -- let auxTabela[1] = auxTabela[1] + (takeMVar tabela)
    -- --valor dos pontos do J2 ajustados, libera tabela
    -- putMVar tabela auxTabela

    -- --Fim do while

