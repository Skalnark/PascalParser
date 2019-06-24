module Main where

import Lexer
import System.IO

main :: IO ()
main = do
       putStrLn "Iniciando..."
       source1 <- readFile "files/meu_codigo.txt"
       source2 <- readFile "files/codigo_professor1.txt"
       source3 <- readFile "files/codigo_professor2.txt"
       writeFile "files/tabela_meu_codigo.txt" (writeTokens source1)
       writeFile "files/tabela_codigo_professor1.txt" (writeTokens source2)
       writeFile "files/tabela_codigo_professor2.txt" (writeTokens source3)
       putStrLn "Escrita realizada com sucesso"
