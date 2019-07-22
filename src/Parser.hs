module Parser (parseToken, readTable, parseProg, varTypeList)
  where

    import Data.Maybe
    import Data.List.Split as Sp
    import Lexer

    typeName = [ ("Null", Null), ("Delimiter", Delimiter), ("Keyword", Keyword),
                ("Identifier", Identifier), ("RelOperator", RelOperator),
                ("AddOperator", AddOperator), ("MulOperator", MulOperator),
                ("Assignment", Assignment), ("IntegerNumber", IntegerNumber),
                ("RealNumber", RealNumber)]

    varTypeList = ["real", "integer", "boolean"]
    strToType :: String -> Maybe Type
    strToType "" = Nothing
    strToType s  = let x = (findThis s typeName)
                   in if isJust x then x
                      else Nothing 

    findThis :: (Eq a) => a -> [(a, b)] -> Maybe b
    findThis x [] = Nothing
    findThis x (n:ns) | x == (fst n)    = Just (snd n)
                      | otherwise = findThis x ns

    toJust :: t -> Maybe t
    toJust t = Just t

    sublist :: [a] -> (a -> Bool) -> [a]
    sublist [] _  = []
    sublist lst f = sublist' [] f (lst)
      where
        sublist' :: [a] -> (a -> Bool) -> [a] -> [a]
        sublist' rs _ [] = rs
        sublist' rs f (x:xs) = if f x then reverse (x:rs)
                       else sublist' (x:rs) f xs
    
    pop :: (Eq a) => [a] -> [a] -> [a]
    pop [] _   = []
    pop lst [] = lst
    pop (l:lst) (r:rs) = if r == l then pop lst rs
                    else (l:lst)

    parseToken :: String -> Maybe Token
    parseToken ""        = Just (Token ("empty string", Just Lexer.Identifier, 0))
    parseToken (s:[])    = Just (Token ("empty string", Just Lexer.Identifier, 0))
    parseToken s = let c = (Sp.splitOn " " s)
                   in if (length c) == 3 then
                        make c
                      else Just (Token ("error while parsing tokens", Just Lexer.Identifier, 0))
                      where
                        make :: [String] -> Maybe Token
                        make []  = Nothing
                        make str = let s = head str
                                       t = (strToType (head (tail str)))
                                       l = (str !! 2)
                                    in
                                      if Lexer.isNumberStr l then
                                        Just (Token(s, t, read l :: Integer))
                                      else Nothing

    readTable :: String -> Maybe [Lexer.Token]
    readTable ""  = Nothing
    readTable src = Just(map (fromJust . parseToken) (filter (\x -> x /= []) (splitOn "\n" src)))

    parseProg :: [Token] -> IO Bool -- to remove the program ...end.
    parseProg ((Token ("program", Just Keyword, l1)):
               (Token (name, Just Identifier, l2)):
               (Token (";", Just Delimiter, l3)):
                tks) = let l = (last tks)
                       in if getContent l == "." &&
                             getType l == Just Delimiter
                            then let sv = vdl (init tks)
                                     sp = parseSP sv
                                     cc = parseCC sp
                                  in if cc == [] then do {return True} 
                                  else do 
                                    { putStrLn $ "VDL:" ++ (show sv)
                                    ; putStrLn $ "Parse SP:" ++ (show sp)
                                    ; putStrLn $ "Parse CC:" ++ (show cc)
                                    ; return False}
                         else do{ putStrLn ("'.' missing" ++ (show (getLineNumber l)))
                                ; return False}
    parseProg tks = do{ putStrLn (show (head tks))
                    ; return False}

    vdl :: [Token] -> [Token]
    vdl [] = []
    vdl ((Token("var", Just Keyword, n1)):tks) = vdl' tks
      where
        vdl' :: [Token] -> [Token]
        vdl' ((Token(id, Just Identifier, n)):tks) = vdl'' tks
          where 
            vdl'' :: [Token] -> [Token]
            vdl'' [] = [(Token("Sem variáveis", Nothing, 0))]
            vdl'' ((Token(",", Just Delimiter, l)):tks) = vdl' tks
            vdl'' ((Token(":", Just Delimiter, l)):tks) = vdlType tks
              where
                vdlType :: [Token] -> [Token]
                vdlType [] =  [(Token("Sem variável irmão", Nothing, 0))]
                vdlType ((Token(c, Just Keyword, l1))
                        :(Token(";", Just Delimiter, l2))
                        :tks) =  if (Lexer.find c varTypeList) then
                                    vdl' tks
                                  else 
                                    [Token("error: ", Nothing, getLineNumber (head tks))]
                vdlType tks = tks
            vdl'' tks = tks 
        vdl' tks = tks     
    vdl tks = tks

    args :: [Token] -> [Token]
    args [] = []
    args ((Token(id, Just Identifier, n)):tks) = args' tks
      where
        args' :: [Token] -> [Token]
        args' [] = []
        args' ((Token(",", Just Delimiter, n))
                    :tks) = args' (dropWhile (\x -> isID x || getContent x == ",") tks)
        args' ((Token(":", Just Delimiter, l)):(Token(c, Just Keyword, l1))
                  :tks) =  if (find c varTypeList) then
                              args tks
                           else tks      
    args tks = tks

    isID :: Token -> Bool
    isID (Token(c, Just Identifier, l)) = True
    isID _ = False

    parseSP :: [Token] -> [Token]
    parseSP [] = []
    parseSP ((Token("procedure", Just Keyword, n)):
             (Token(_, Just Identifier, n')):
             tks) = let va = vdl tks
                        sp = parseSP va
                        cc = parseCC sp
                        in cc
    parseSP tks = tks

    parseCC :: [Token] -> [Token]
    parseCC [] = []
    parseCC ((Token("begin", Just Keyword, n)):
             tks) = let l = last tks 
                    in if getContent l == "end" && getType l == Just Keyword then
                         parseLC (init tks)                      
                       else tks
    parseCC tks = tks

    parseOPC [] = []
    parseOPC ((Token(";", Just Identifier, _)):tks) = parseLC tks
    parseOPC tks = parseLC tks

    parseLC :: [Token] -> [Token]
    parseLC [] = []
    parseLC ((Token(";", Just Delimiter, l)):tks) = parseLC tks
    parseLC ((Token(_, Just Identifier, l1)):
             (Token(":=", Just Assignment, l2)):
             tks) = parseExp tks
    parseLC ((Token(id, Just Identifier, l)):tks) = parseExp tks
    parseLC ((Token("if", Just Keyword, l)): tks) = parseIf tks
      where
        parseIf :: [Token] -> [Token]
        parseIf [] = []
        parseIf tks = parseThen (parseExp tks)
          where
            parseThen :: [Token] -> [Token]
            parseThen [] = [Token("error", Nothing, -1)]
            parseThen tks = parseElse (parseOPC tks)
              where
                parseElse :: [Token] -> [Token]
                parseElse [] = []
                parseElse tks = parseCC tks
    parseLC ((Token("while", Just Keyword, l)):tks) = parseWhile tks
      where
        parseWhile :: [Token] -> [Token]
        parseWhile [] = [Token("error", Nothing, -1)]
        parseWhile tks = parseDo (parseExp tks)
          where
            parseDo :: [Token] -> [Token]
            parseDo [] = []
            parseDo tks = parseLC tks
    parseLC ((Token("case", Just Keyword, _)):
             (Token(_, Just Identifier, _)):
             (Token("of", Just Keyword, _)):
              tks) = parseCase tks
      where
        parseCase :: [Token] -> [Token]
        parseCase [] = []
        parseCase tks = let pc = parseLC tks
                         in if (getContent (head tks) == "else" && getType (head tks) == Just Keyword)
                            then parseLC (tail tks)
                            else tks
    parseLC tks = tks
                            
    parseExp :: [Token] -> [Token]
    parseExp [] = []
    parseExp ((Token(_, Just Identifier, l)):tks) = let n = parseTerm tks
                                                     in if (getType (head n)) == Just AddOperator then
                                                           parseTerm (init tks)
                                                        else tks
    parseExp ((Token(_, Just AddOperator, l)):tks) = parseTerm tks
    parseExp tks = parseExp' tks
      where
        parseExp' :: [Token] -> [Token]
        parseExp' [] = []
        --parseExp' ((Token(_, Just Identifier, l)):tks)  = parseExp tks
        parseExp' ((Token(_, Just AddOperator, l)):tks) = parseExp tks
        parseExp' tks = parseExp'' tks
          where
            parseExp'' [] = []
            parseExp'' tks = let k = parseTerm tks
                              in if getType (head k) == Just AddOperator then
                                    parseTerm (init tks)
                                 else
                                    tks

    parseLExp :: [Token] -> [Token]
    parseLExp [] = []
    parseLExp (t:tks) = let k = parseExp [t]
                        in if k == [] then
                             if getContent (head tks) == "," then
                               parseExp (init tks)
                             else
                               tks
                           else (t:tks)
    
    parseFactor :: [Token] -> [Token]
    parseFactor [] = []
    parseFactor ((Token(_, Just Identifier, l)):tks) = parseExp tks
    parseFactor (Token ((_, Just IntegerNumber, l)):tks) = tks
    parseFactor (Token ((_, Just RealNumber, l)):tks) = tks
    parseFactor (Token (("true", Just Keyword, l)):tks) = tks
    parseFactor (Token (("false", Just Keyword, l)):tks) = tks
    parseFactor (Token (("not", Just RelOperator, l)):tks) = parseFactor tks
    parseFactor tks = tks

    parseTerm :: [Token] -> [Token]
    parseTerm [] = []
    parseTerm (t:tks) = if (parseFactor [t] == []) then
                            parseTerm' tks
                        else (t:tks) 
      where
        parseTerm' :: [Token] -> [Token]
        parseTerm' [] = tks
        parseTerm' ((Token(_, Just MulOperator, _)):
                      tks) = parseFactor tks
        parseTerm' tks = tks
 
    turnProc :: [Token] -> [Token]
    turnProc [] = []
    turnProc (t:tks) = if isID t then 
                         if getContent (head tks) == "(" then
                            let lde = sublist (init tks) (\x -> (getContent x) == ")")
                             in if [] == (parseExp lde) then
                                    (pop tks lde)
                                else
                                  tks
                        else tks
                      else tks