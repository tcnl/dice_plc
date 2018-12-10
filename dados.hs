import System.Random
import Control.Concurrent
import Text.Printf
import Control.Monad
import System.IO

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
                loopDoSatanas parcial1

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
                  print $ "Dado: " ++ show dado
                  dado2   <- takeMVar jogo
                  print $ "Dado: " ++ show dado2
                  --let parcial1 = 50
                  let valor1 = (parcial_nivel_dois aposta dado dado2 n)
                  let parcial1 = valor1
                  print $ "Parcial 1: " ++ show (parcial1)
                  loopDoSatanas parcial1

nivel_tres :: Int -> IO()
nivel_tres n = do tabela <- newEmptyMVar
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
                  dado2   <- takeMVar jogo
                  print $ "Dado 2: " ++ show dado2
                  dado3   <- takeMVar jogo
                  print $ "Dado 23: " ++ show dado3
                --let parcial1 = 50
                  let valor1 = (parcial_nivel_tres aposta dado dado2 dado3 n)
                  let parcial1 = valor1
                  print $ "Parcial 1: " ++ show (parcial1)
                  loopDoSatanas parcial1

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

loopDoSatanas :: Int -> IO()
loopDoSatanas n
    |(n >= 100)            = print $ "Ganhou, otário!" 
    |(n >=  85 && n < 100) = nivel_tres n
    |(n >=  70 && n <  85) = nivel_dois n
    |(n <    0)            = print $ "CHORA!"
    |otherwise             = nivel_um n

main :: IO()
main = do loopDoSatanas 50