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

    --Se a pessoa acertar o valor do dado, ganha os pontos se nao, perde.
    --Mas o if/else ta sempre dando erro de parse
    --if(aposta == dado) then
    --     let parcial1 =  aposta
    --else let parcial1 = -aposta

    -- parcial1 :: Int -> Int -> Int
    -- parcial1 aposta dado =
    --     let acerto = (parcial1 = parcial1 + aposta)
    --         erro = (parcial1 = parcial1 - aposta)
    --     if (aposta = dado)
    --         then acerto
    --         else erro

    -- parcial1 aposta dado
    --     | aposta == dado = acerto
    --     | otherwise = erro
    --     where   acerto = (parcial1 = parcial1 + dado)
    --             erro = (parcial1 = parcial1 - dado)

    -- parcial_um :: Int -> Int -> Int -> Int
    -- parcial_um aposta dado parcial1
    --     | aposta == dado = acerto
    --     | otherwise = erro
    --     where   acerto = (let parcial1 = (parcial1 + aposta))
    --             erro = (let parcial1 = (parcial1 - aposta))

    -- parcial_um :: Int -> Int -> Int -> Int
    -- parcial_um aposta dado parcial1 =
    --     if aposta == dado then (parcial1 + aposta)
    --     else (parcial1 - aposta)

        parcial_um :: Int -> Int -> Int -> Int
        parcial_um aposta dado parcial1
            | aposta == dado = (parcial1 + aposta)
            | otherwise = (parcial1 - aposta)

 
    print $ "Parcial 1: " ++ show (parcial_um aposta dado parcial1)



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
    --if(aposta == dado) then
    --     let parcial2 =  aposta
    --else let parcial2 = -aposta

    let parcial2 = 50

        parcial_dois :: Int -> Int -> Int -> Int
        -- parcial_dois _ _ 100 = error "Ganhasse!"
        -- parcial_dois _ _ 0 = error "Perdesse!"
        parcial_dois aposta dado parcial2
            | aposta == dado = (parcial2 + aposta)
            | otherwise = (parcial2 - aposta)

    let parcial2 = parcial_dois aposta dado parcial2
    -- Fico tentando atualizar o valor de parcial2, mas imprime:
    -- "Jogador 1 aposte: "
    -- 3
    -- "Aposta: 3"
    -- "Dado: 5"
    -- "Parcial 1: 47"
    -- "Jogador 2 aposte: "
    -- 4
    -- "Aposta: 4"
    -- "Dado: 6"
    -- dado.exe: <<loop>>          <<<<<------------------- loop infinito??

    print $ "Parcial 2: " ++ show parcial2


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

