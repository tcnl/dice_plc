import System.Random
import Control.Concurrent
import Text.Printf
import Control.Monad
import System.IO

--Cada um dos niveis basicamente solicita que o jogador digite um valor/aposta para o dado
    -- e em seguida ele 'lanca' o dado (randomRIO(1,6))
    -- depois duas threads sao iniciadas, disputando a memoria da MVar jogo
    -- a primeira que a utiliza eh a 'palpite', deixando a MVar bloqueada
    -- em seguida a variavel aposta le o valor da MVar e a libera para que o valor do dado seja colocado
    -- depois a variavel dado le a MVar e a libera
    -- por fim chama uma funcao que verifica as regras do jogo e atualiza o placar
    -- chama a funcao de loop para continuar jogando
    -- OBS: nivel dois e tres sao essencialmente iguais ao um, mudando so as expressoes na regra do jogo

nivel_um :: Int -> IO()
nivel_um n = do tabela <- newEmptyMVar
                jogo <- newEmptyMVar
                print "Jogador 1 aposte: "
                palpite <- getLine
                let input = (read palpite :: Int)
                number <- randomRIO (1,6) :: IO Int
                forkIO $ do putMVar jogo input; putMVar jogo number
                aposta <- takeMVar jogo
                print $ "Aposta: " ++ show aposta
                dado   <- takeMVar jogo
                print $ "Dado: " ++ show dado
                --let parcial1 = 50
                let valor1 = (parcial_nivel_um aposta dado n)
                let parcial1 = valor1
                print $ "Parcial 1: " ++ show (parcial1)
                niveis parcial1

nivel_dois :: Int -> IO()
nivel_dois n = do tabela <- newEmptyMVar
                  jogo <- newEmptyMVar
                  print "Jogador 1 aposte: "
                  palpite <- getLine
                  let input = (read palpite :: Int)
                  number <- randomRIO (1,6) :: IO Int
                  number2 <- randomRIO (1,6) :: IO Int
                  forkIO $ do putMVar jogo input; putMVar jogo number; putMVar jogo number2
                  aposta <- takeMVar jogo
                  print $ "Aposta: " ++ show aposta
                  dado   <- takeMVar jogo
                  print $ "Dado 1: " ++ show dado
                  dado2   <- takeMVar jogo
                  print $ "Dado 2: " ++ show dado2
                  --let parcial1 = 50
                  let valor1 = (parcial_nivel_dois aposta dado dado2 n)
                  let parcial1 = valor1
                  print $ "Parcial 1: " ++ show (parcial1)
                  niveis parcial1

nivel_tres :: Int -> IO()
nivel_tres n = do tabela <- newEmptyMVar
                  jogo <- newEmptyMVar
                  print "Jogador 1 aposte: "
                  palpite <- getLine
                  let input = (read palpite :: Int)
                  number <- randomRIO (1,6) :: IO Int
                  number2 <- randomRIO (1,6) :: IO Int
                  number3 <- randomRIO (1,6) :: IO Int
                  forkIO $ do putMVar jogo input; putMVar jogo number; putMVar jogo number2; putMVar jogo number3
                  aposta <- takeMVar jogo
                  print $ "Aposta: " ++ show aposta
                  dado   <- takeMVar jogo
                  print $ "Dado: " ++ show dado
                  dado2   <- takeMVar jogo
                  print $ "Dado 2: " ++ show dado2
                  dado3   <- takeMVar jogo
                  print $ "Dado 3: " ++ show dado3
                --let parcial1 = 50
                  let valor1 = (parcial_nivel_tres aposta dado dado2 dado3 n)
                  let parcial1 = valor1
                  print $ "Parcial 1: " ++ show (parcial1)
                  niveis parcial1


--Eh aqui onde sao feitas as expressoes das regras do jogo e atualizacao do placar
parcial_nivel_um :: Int -> Int -> Int -> Int
parcial_nivel_um aposta dado parcial
    | aposta == dado = (parcial + aposta)
    | otherwise = (parcial - aposta)

parcial_nivel_dois :: Int -> Int -> Int -> Int -> Int
parcial_nivel_dois aposta dado1 dado2 parcial
    | (aposta == dado1 && dado1 == dado2) = (parcial + aposta)
    | otherwise = (parcial - aposta)

parcial_nivel_tres :: Int -> Int -> Int -> Int -> Int -> Int
parcial_nivel_tres aposta dado1 dado2 dado3 parcial
    | (aposta == dado1 && dado1 == dado2 && dado2 == dado3) = (parcial + aposta)
    | otherwise = (parcial - aposta)


--loop que coordena vitoria, derrota e passagem de nivel
niveis :: Int -> IO()
niveis n
    |(n >= 100)            = print $ "Você ganhou!!!" 
    |(n >=  85 && n < 100) = nivel_tres n
    |(n >=  70 && n <  85) = nivel_dois n
    |(n <    0)            = print $ "Perdeu!"
    |otherwise             = nivel_um n

main :: IO()
main = do niveis 50