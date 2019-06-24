module Lexer(Line, Content, Delimiter, Keyword, Identifier, Token(..), Type(..), 
             lexicalAnalysis, showToken, writeTokens, mapType)
  where
  
  import Data.Maybe
  import Data.Char

  type Line       = Integer
  type Content    = String
  type Delimiter  = String
  type Keyword    = String
  type Identifier = String
  data Token = Token (Content, Maybe Type, Line)

  data Type = Null | Delimiter | Keyword | Identifier | RelOperator | AddOperator
            | MulOperator | Assignment | IntegerNumber | RealNumber
      deriving(Eq, Show)

  reservedNames = ["program", "var", "integer", "real", "bolean", "procedure", "begin",
                   "end", "if", "then", "else", "while", "do", "not"]
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
      loop r (n:ns) | n == '.'  = if (isNumberStr $ r ++ ns) then True else False
                    | otherwise = loop (r++[n]) ns

  isIdentifier :: String -> Bool
  isIdentifier ""     = False
  isIdentifier (x:[]) = isAlphaNum x
  isIdentifier (x:xs) = if isAlphaNum x || x == '_' then isIdentifier xs else False

  mapType :: String -> Maybe Type
  mapType [] = Nothing
  mapType x 
          | x == ":="             = Just Assignment
          | find x delimiters     = Just Delimiter
          | find x addOp          = Just AddOperator
          | find x mulOp          = Just MulOperator
          | find x relationalOp   = Just RelOperator
          | find x reservedNames  = Just Keyword
          | isNumberStr x         = Just IntegerNumber
          | isRealStr x           = Just RealNumber
          | isIdentifier x        = Just Identifier
          | otherwise             = Nothing

  lexicalAnalysis :: String -> Maybe [Token]
  lexicalAnalysis []  = Nothing -- Undefined for empty input
  lexicalAnalysis str = loop [] 1 (Just Null) "" str
    where
      loop :: [Token] -> Integer -> Maybe Type -> String -> String -> Maybe [Token]
      loop tks n t "" ""             = Just tks
      loop tks n t x ""              =  if isNothing(mapType x) then
                                          Nothing -- Undefined if the end of string is unknown
                                        else
                                          Just (tks ++ [(Token (x, t, n))])

      loop tks n t "" (' ':code)     = loop tks n t "" code
      loop tks n t x  (' ':code)     = let t' = mapType x
                                       in if (isJust t') then 
                                            loop (tks ++ [(Token (x, t', n))]) n (Just Null) "" code
                                          else Nothing
      -- If a linebreak is found, just increment line counter
      loop tks n t "" ('\n':code)    = loop tks (n + 1) t "" code
      loop tks n t x ('\n':code)     = loop (tks ++ [(Token (x, t, n))]) (n + 1) (Just Null) "" code
      loop tks n t x ('{':code)      = loop' tks n t x code -- removing comments
        where
          loop' :: [Token] -> Integer -> Maybe Type -> String -> String -> Maybe [Token]
          loop' tks n t x ('}':code') = loop tks n t x code'
          loop' tks n t x (_:code')   = loop' tks n t x code'
          -- Undefined if curly brackets are not closed
          loop' tks n t x []          = Nothing
      loop tks n t "" (c:code)
           | isJust(mapType [c])      = loop tks n (mapType [c]) [c] code
           | otherwise                = do Nothing
      loop tks n t x (c:code)
        | isJust (mapType (x ++ [c])) = loop tks n (mapType (x++[c])) (x++[c]) code
        | isJust (mapType x)          = loop (tks ++ [Token (x, mapType x, n)])
                                           n (mapType[c]) [c] code
        | otherwise                   = Nothing
           
  showToken :: Maybe Token -> String
  showToken Nothing                  = ""
  showToken (Just (Token (c, t, l))) = c ++ " " ++ (show (fromJust t)) ++ " " ++ (show l) ++ "\n"
                                       

  writeTokens :: String -> String
  writeTokens "" = "empty"
  writeTokens x  = process "" (lexicalAnalysis x)
    where
      process :: String -> Maybe [Token] -> String
      process _ Nothing           = "parsing error"
      process "" (Just [])        = ""
      process r (Just [])         = r
      process r (Just (t:tokens)) = process (r ++ (showToken (Just t))) (Just tokens)
