module Main where

import Lexer
import System.IO

main :: IO ()
main = do
       putStrLn "Iniciando..."
       source <- readFile "files/source.txt"
       writeFile "files/table.txt" (writeTokens source)
       putStrLn "Escrita realizada com sucesso"
