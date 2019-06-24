module Lexer(Line, Content, Delimiter, Keyword, Identifier,
             Token(..), Type(..), lexicalAnalysis, showToken, writeTokens, mapType)
  where
  
  import Data.Maybe
  import Data.Char

  type Line       = Integer
  type Content    = String
  type Delimiter  = String
  type Keyword    = String
  type Identifier = String
  data Token = Token (Content, Type, Line)

  data Type = Null
            | Delimiter 
            | Keyword 
            | Identifier 
            | RelOperator
            | AddOperator
            | MulOperator
            | Assignment
            | IntegerNumber
            | RealNumber
      deriving(Eq, Show)

  reservedNames = ["program", "var", "integer", "real", "bolean",
                   "procedure", "begin", "end", "if", "then", "else",
                   "while", "do", "not"]

  delimiters = [",", "(", ")", ".", ":", ";"]

  relationalOp = [">", "<", "=", ">=", "<=", "<>"]

  addOp = ["+", "-"]
  mulOp = ["*", "/", "and"]
  
  find :: String -> [String] -> Bool
  find "" _     = False
  find _ []     = False
  find x (v:vs) = if x == v then
                    True
                  else
                    find x vs

  isNumberStr :: String -> Bool
  isNumberStr []     = False
  isNumberStr (x:[]) = isDigit x
  isNumberStr (x:xs) = if isDigit x then isNumberStr xs
                       else False

  isRealStr :: String -> Bool
  isRealStr []  = False
  isRealStr str = loop "" str
    where
      loop :: String -> String -> Bool
      loop _ [] = False
      loop r (n:ns) | n == '.'  = if (isNumberStr r) && (isNumberStr ns) 
                                    then True 
                                    else False
                    | otherwise = loop (r++[n]) ns

  mapType :: String -> Type
  mapType [] = Null
  mapType (x:[])  = if find [x] delimiters
                      then Delimiter
                    else Null
  mapType x 
          | find x delimiters     = Delimiter
          | find x addOp          = AddOperator
          | find x mulOp          = MulOperator
          | find x relationalOp   = RelOperator
          | find x reservedNames  = Keyword
          | x == ":="             = Assignment
          | isNumberStr x         = IntegerNumber
          | isRealStr x           = RealNumber
          | otherwise             = Identifier

  lexicalAnalysis :: String -> Maybe [Token]
  lexicalAnalysis []  = Nothing -- Undefined for empty input
  lexicalAnalysis str = loop [] 1 Null "" str
    where
      loop :: [Token] -> Integer -> Type -> String -> String -> Maybe [Token]
      loop tks n t "" ""             = Just tks
      loop tks n t x ""              =  if mapType x == Null then
                                          Nothing -- Undefined if the end of string is unknown
                                        else
                                          Just (tks ++ [(Token (x, t, n))])

      loop tks n t x (' ':code)
            | x == ""                = loop tks n t x code
                | otherwise          = let t' = mapType x
                                       in loop (tks ++ [(Token (x, t', n))]) n Null "" code

      -- If a linebreak is found, just increment line counter
      loop tks n t x ('\n':code)     = loop (tks ++ [(Token (x, t, n))]) (n + 1) Null "" code
      loop tks n t x ('{':code)      = loop' tks n t x code -- removing comments
        where
          loop' :: [Token] -> Integer -> Type -> String -> String -> Maybe [Token]
          loop' tks n t x ('}':code') = loop tks n t x code'
          loop' tks n t x (_:code')   = loop' tks n t x code'
          -- Undefined if curly brackets are not closed
          loop' tks n t x []          = Nothing

      loop tks n t x ('.':code)       = if (isNumberStr x) &&
                                          isDigit (head code)
                                         then loop tks n RealNumber x code
                                       else
                                         loop (tks ++ [(Token (x, t, n))]) n Null "" code
      loop tks n t x (c:code)
        | isAlphaNum c || c == '_'            
                                     =   let t' = mapType (x ++ [c])
                                           in if t' == Null then
                                                if x == "" then
                                                  loop tks n t' [c] code
                                                else
                                                  loop (tks ++ [(Token (x, t, n))]) n Null "" (c:code)
                                              else
                                                loop tks n t' (x ++ [c]) code   
{-
        | x /= "" && 
          mapType [c] == Delimiter &&
          mapType (x ++ [c]) == Identifier
                                     = loop (tks ++ [(Token (x, t, n))]) n Delimiter [c] code
        | x == "" &&
          mapType [c] == Delimiter   = loop tks n Delimiter [c] code
        | otherwise                  = Just tks-}

  showToken :: Maybe Token -> String
  showToken Nothing                  = ""
  showToken (Just (Token (c, t, l))) = c ++ " " ++ (show t) ++ " " ++ (show l) ++ "\n"

  writeTokens :: String -> String
  writeTokens "" = "empty"
  writeTokens x  = process "" (lexicalAnalysis x)
    where
      process :: String -> Maybe [Token] -> String
      process _ Nothing           = "parsing error"
      process "" (Just [])        = ""
      process r (Just [])         = r
      process r (Just (t:tokens)) = process (r ++ (showToken (Just t))) (Just tokens)