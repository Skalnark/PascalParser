module Lexer(reservedNames, delimiters, relationalOp, addOp, mulOp,
             Line, Content, Delimiter, Keyword, Identifier, Token(..), Type(..), 
             lexicalAnalysis, showToken, writeTokens, mapType, makeType, rewrite,
             isIdentifier, isNumberStr, getLineNumber, getContent, getType, find, showErrors)
  where
  
  import Data.Maybe
  import Data.Char

  type Line       = Integer
  type Content    = String
  type Delimiter  = String
  type Keyword    = String
  type Identifier = String
  data Token = Token (Content, Maybe Type, Line) 
                               deriving(Eq, Show)

  data Type = Null | Delimiter | Keyword | Identifier | RelOperator | AddOperator
            | MulOperator | Assignment | IntegerNumber | RealNumber
      deriving(Eq, Show)

  reservedNames = ["program", "boolean", "var", "integer", "real", "procedure", "begin",
                   "end", "if", "then", "else", "while", "do", "not", "case", "of"]
  delimiters = [",", "(", ")", ".", ":", ";"]
  relationalOp = [">", "<", "=", ">=", "<=", "<>"]
  addOp = ["+", "-"]
  mulOp = ["*", "/", "and"]
  
  getType :: Token -> Maybe Type
  getType (Token(_, t, _)) = t

  getContent :: Token -> String
  getContent (Token(s, _, _)) = s

  getLineNumber :: Token -> Integer
  getLineNumber (Token(_, _, l)) = l

  makeType :: String -> Maybe Type
  makeType "" = Nothing
  makeType s  = mapType s

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
  isIdentifier (x:xs) = isAlphaNum x && isIdentifier' xs
    where 
      isIdentifier' :: String -> Bool
      isIdentifier' "" = True
      isIdentifier' (x:xs) = if isAlphaNum x || x == '_' then
                                 isIdentifier' xs
                             else False

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
  lexicalAnalysis []  = Just [(Token ("empty text", Nothing, 0))]
  lexicalAnalysis str = loop [] 1 (Just Null) "" str
    where
      loop :: [Token] -> Integer -> Maybe Type -> String -> String -> Maybe [Token]
      loop tks n t "" ""             = Just tks
      loop tks n t x ""              =  if isNothing(mapType x) then
                                          Just [(Token (x, Nothing, n))]
                                        else
                                          Just (tks ++ [(Token (x, t, n))])

      loop tks n t "" (' ':code)     = loop tks n t "" code
      loop tks n t x  (' ':code)     = let t' = mapType x
                                       in if (isJust t') then 
                                            loop (tks ++ [(Token (x, t', n))]) n (Just Null) "" code
                                          else Just [(Token(x, Nothing, n))]
      -- If a linebreak is found, just increment line counter
      loop tks n t "" ('\n':code)    = loop tks (n + 1) t "" code
      loop tks n t x ('\n':code)     = loop (tks ++ [(Token (x, t, n))]) (n + 1) (Just Null) "" code
      loop tks n t x ('}':code)      = Just [(Token("unmatching curly bracket", Nothing, n))]
      loop tks n t x ('{':code)      = loop' tks n t x code -- removing comments
        where
          loop' :: [Token] -> Integer -> Maybe Type -> String -> String -> Maybe [Token]
          loop' tks n t x ('}':code') = loop tks n t x code'
          loop' tks n t x (_:code')   = loop' tks n t x code'
          -- Undefined if curly brackets are not closed
          loop' tks n t x []          = Just [(Token("open curly brackets", Nothing, n))]
      loop tks n t "" (c:code)
           | isJust(mapType [c])      = loop tks n (mapType [c]) [c] code
           | otherwise                = Just [(Token("undefined symbol " ++ [c], Nothing, n))]
      loop tks n t x (c:code)
        | isJust (mapType (x ++ [c])) = loop tks n (mapType (x++[c])) (x++[c]) code
        | isJust (mapType x)          = loop (tks ++ [Token (x, mapType x, n)])
                                           n (mapType[c]) [c] code
        | otherwise                   = Just tks
           
  showToken :: Maybe Token -> String
  showToken Nothing                  = ""
  showToken (Just (Token (c, Nothing, l))) = "Error: " ++ c ++ " on line " ++ (show l) ++ "\n"
  showToken (Just (Token (c, Just t, l))) = c ++ " " ++ (show t) ++ " " ++ (show l) ++ "\n"
                                       
  showErrors :: Token -> String
  showErrors (Token (c, Nothing, l)) = "Error: " ++ c ++ "on line " ++ (show l) ++ "\n"
  showErrors t = "\n"

  writeTokens :: String -> String
  writeTokens "" = "empty"
  writeTokens x  = process "" (lexicalAnalysis x)
    where
      process :: String -> Maybe [Token] -> String
      process _ Nothing           = "parsing error"
      process "" (Just [])        = ""
      process r (Just [])         = r
      process r (Just (t:tokens)) = process (r ++ (showToken (Just t))) (Just tokens)

  rewrite :: Maybe [Token] -> String
  rewrite Nothing    = "error"
  rewrite (Just [])  = "empty"
  rewrite (Just tks) = loop "" (Just tks)
    where 
      loop :: String -> Maybe [Token] -> String
      loop k (Just [])      = k
      loop k (Just (t:tks)) = loop (k ++ (showToken (Just t))) (Just tks)
      loop k Nothing        = k