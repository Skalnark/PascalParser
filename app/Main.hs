module Main where

import Lexer
import Parser
import System.IO
import Data.Maybe
--DEBUG
import Debug.Trace 
import System.IO.Unsafe as IOU
--DEBUG

main :: IO ()
main = do
       putStrLn "Iniciando..."
       source1 <- readFile "files/meu_codigo.txt"
       source2 <- readFile "files/codigo_professor1.txt"
       source3 <- readFile "files/codigo_professor2.txt"
       source4 <- readFile "files/meu_codigo2.txt"
       putStrLn "Arquivos Carregados"

       writeFile "files/tabela_meu_codigo.txt" (writeTokens source1)
       writeFile "files/tabela_meu_codigo2.txt" (writeTokens source4)
       writeFile "files/tabela_codigo_professor1.txt" (writeTokens source2)
       writeFile "files/tabela_codigo_professor2.txt" (writeTokens source3)
       putStrLn "Tabelas de arquivos gravados"

       table1 <- readFile "files/tabela_meu_codigo.txt"
       table2 <- readFile "files/tabela_meu_codigo2.txt"
       table3 <- readFile "files/tabela_codigo_professor1.txt"
       table4 <- readFile "files/tabela_codigo_professor2.txt"
       putStrLn "Tabelas carregadas"

       if uRead (parseProg(fromJust (readTable table1))) then
         putStrLn("meu_codigo.txt ok")
       else 
         putStrLn("")

       if uRead (parseProg(fromJust (readTable table2))) then
         putStrLn("meu_codigo2.txt ok")
       else
         putStrLn("")

       if uRead (parseProg(fromJust (readTable table3))) then
         putStrLn("codigo_professor1.txt ok")
       else
         putStrLn("")

       if uRead (parseProg(fromJust (readTable table4))) then
         putStrLn("codigo_professor2.txt ok")
       else
         putStrLn("")

       putStrLn("finalizado")


uRead :: IO Bool -> Bool
uRead x = unsafePerformIO(x)