module Parser (parseToken, readTable, parseProg, varTypeList, parseExp, isTermExp, sublist, pop)
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
    findThis x []                       = Nothing
    findThis x (n:ns) | x == (fst n)    = Just (snd n)
                      | otherwise       = findThis x ns

    sublist :: [Token] -> String -> [Token]
    sublist [] _             = []
    sublist lst c            = sublist' [] c lst
      where
        sublist' rs _ []     = rs
        sublist' rs c (x:xs) = if c == (getContent x) then reverse (x:rs)
                       else sublist' (x:rs) c xs
    
    pop :: (Eq a) => [a] -> [a] -> [a]
    pop [] _           = []
    pop lst []         = lst
    pop (l:lst) (r:rs) = if r == l then pop lst rs
                    else (l:lst)

    isTermExp :: Token -> Bool
    isTermExp (Token(_, Just Identifier, _))    = True
    isTermExp (Token(_, Just IntegerNumber, _)) = True
    isTermExp (Token(_, Just RealNumber, _))    = True
    isTermExp (Token("true", _, _))             = True
    isTermExp (Token("false", _, _))            = True
    isTermExp (Token("not", _, _))              = True
    isTermExp (Token("(", _, _))                = True
    isTermExp (_)                               = False

    parseToken :: String -> Maybe Token
    parseToken ""        = Just (Token ("empty string", Just Lexer.Identifier, 0))
    parseToken (s:[])    = Just (Token ("empty string", Just Lexer.Identifier, 0))
    parseToken s         = let c = (Sp.splitOn " " s)
                           in 
      if (length c) == 3 then
        make c
      else Just (Token ("error while parsing token " ++
                (head c), Nothing, 0))
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
                                  { putStrLn "Parse error:"
                                  ; putStrLn ((getContent (head cc)) ++ " " ++
                                               show (getType (head cc)) ++
                                               " on line " ++ 
                                              (show (getLineNumber (head cc))))
                                  ; print cc
                                  ; return False}
                          else do{ putStrLn ("'.' missing" ++ (show (getLineNumber l)))
                                ; return False}
    parseProg tks    = do{ putStrLn (show (head tks))
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
                vdlType []    =  [(Token("Sem variável irmão", Nothing, 0))]
                vdlType ((Token(c, Just Keyword, l1))
                        :(Token(";", Just Delimiter, l2))
                        :tks) =  if (Lexer.find c varTypeList) then
                                    vdl' tks
                                  else 
                                    [Token("error: ", Nothing, getLineNumber (head tks))]
                vdlType tks   = tks
            vdl'' tks = tks 
        vdl' tks = tks     
    vdl tks = tks

    args :: [Token] -> [Token]
    args []                                    = []
    args ((Token("var", Just Keyword, l)):tks) = args tks
    args tks                                   = args' tks
      where 
        args' [] = []
        args' ((Token(id, Just Identifier, n)):tks) = args'' tks
          where
            args'' []       = []
            args'' ((Token(",", Just Delimiter, n))
                      :tks) = args'' (dropWhile (\x -> isID x || getContent x == ",") tks)
            args'' ((Token(":", Just Delimiter, l)):(Token(c, Just Keyword, l1))
                      :tks) =  if (find c varTypeList) then
                                  args' tks
                               else tks
            args'' tks = tks
        args' tks = tks

    isID :: Token -> Bool
    isID (Token(c, Just Identifier, l)) = True
    isID _                              = False

    parseSP :: [Token] -> [Token]
    parseSP []    = []
    parseSP ((Token("procedure", Just Keyword, n)):
             (Token(_, Just Identifier, n')):
             tks) = let p = (sublist tks "end")
                     in parseSp' ((args (init p)) ++ (pop tks p))
      where 
        parseSp' :: [Token] -> [Token]
        parseSp' []     = []
        parseSp' ((Token("(", Just Delimiter, l)):
                   tks) = let k = sublist tks ")"
                              in if args (init k) == []
                                 then
                                    let i = pop tks k
                                    in if i /= [] && getContent (head i) == ";"
                                       then let va = vdl (tail i)
                                                sp = parseSP va
                                                cc = parseCC sp
                                             in cc
                                       else
                                            [Token("expected ';' after procedure"++ 
                                                   "declaration, found: " ++
                                                    getContent (last k) ++ " instead ",
                                                    getType (last k), 
                                                    getLineNumber (last k))]
                                 else [Token("error parsing arguments on "++ 
                                 "procedure declaration: " ++
                                  getContent (head(args (init k))),
                                  getType (head(args (init k))), 
                                  getLineNumber (head(args (init k))))]
        parseSp' (t:tks)  = [Token("unexpected symbol after procedure declaration: " ++
                                   getContent t, getType t, getLineNumber t)]
    parseSP tks = tks

    parseCC :: [Token] -> [Token]
    parseCC []    = []
    parseCC ((Token("begin", Just Keyword, n)):
             tks) = let l = last tks 
                    in if getContent l == "end" && getType l == Just Keyword then
                         parseOPC (init tks)                      
                       else tks
    parseCC tks   = tks

    parseOPC []  = []
    parseOPC tks = parseLC tks

    parseLC :: [Token] -> [Token]
    parseLC []    = []
    parseLC ((Token(";", Just Delimiter, l)):
             tks) = parseLC (parseC tks)
    parseLC tks   = parseC tks

    parseC :: [Token] -> [Token]
    parseC ((Token(_, Just Identifier, l1)):
            (Token(":=", Just Assignment, l2)):tks) = parseLC(parseExp tks)
    parseC ((Token("(", Just Delimiter, l1)):tks)   = parseActi tks

    parseC ((Token("begin", Just Keyword, l)):tks)  = parseCC ((Token("begin", Just Keyword, l)):tks)
    parseC ((Token("if", Just Keyword, l)):tks)     = parseIf tks
    parseC ((Token("while", Just Keyword, l)):tks)  = parseWhile tks
    parseC ((Token("case", Just Keyword, _)):
             (Token(_, Just Identifier, _)):
             (Token("of", Just Keyword, _)):tks)    = parseCase tks
    parseC tks                                      = tks

    parseIf :: [Token] -> [Token]
    parseIf []                                    = []
    parseIf ((Token("(", Just Delimiter, l)):tks) = parseThen((parseExp (init k))++(pop tks k))
      where k = sublist tks  ")"
    parseIf tks                                   = let k = parseExp tks
                                                    in
        if getContent (head k) == "then"
        then 
          parseThen k
        else
          [Token("Error after if expression: " ++ getContent(head k),
                Nothing,
                getLineNumber (head k))] 

    parseCase :: [Token] -> [Token]
    parseCase []      = []
    parseCase ((Token("else", Just Keyword, l)):
                tks)  = parseLC tks
    parseCase tks = let c =  (parseFactor tks)
                     in if getContent (head c) == ":"
                        then parseCase(parseLC (tail c))
                        else tks  
    
    parseWhile :: [Token] -> [Token]
    parseWhile []  = [Token("error", Nothing, -1)]
    parseWhile tks = parseDo (parseExp tks)
      where
        parseDo :: [Token] -> [Token]
        parseDo []                                    = []
        parseDo ((Token("do", Just Keyword, l1)):tks) = parseC tks
        parseDo tks                                   = tks

    parseThen :: [Token] -> [Token]
    parseThen []   = [Token("error: end of file after if expression", Nothing, -1)]
    parseThen ((Token("then", Just Keyword, l)):
              tks) = parseElse (parseC tks)
              where
                parseElse :: [Token] -> [Token]
                parseElse []  = []
                parseElse ((Token("else", Just Keyword, l)):
                         tks) = parseLC tks    
                parseElse tks = tks    
         
    parseActi :: [Token] -> [Token]
    parseActi []     = [Token("error after '('", Nothing, 0)]
    parseActi ((Token(id, Just Identifier, l)):
                tks) = let k = last tks
                        in if getContent k == ")" &&
                              getType k == Just Delimiter
                           then 
                              parseLExp (init tks)
                           else
                              tks
    parseActi tks    = tks       

    parseLExp :: [Token] -> [Token]
    parseLExp []  = [Token("error: empty expressions", Nothing, 0)]
    parseLExp tks = let k = parseSimpExp tks
                        in if k == [] then
                             []
                           else
                             if getContent (head tks) == "," then
                               parseLExp (tail tks)
                             else
                               tks

    parseExp :: [Token] -> [Token]
    parseExp []      = [Token("Error, empty expression", Nothing, 0)]
    parseExp ((Token(_, Just RelOperator, l)):
               tks)  = parseExp tks
    parseExp (t:tks) = if isTermExp t then
                          parseSimpExp (t:tks)
                        else
                          (t:tks)

    parseSimpExp :: [Token] -> [Token]
    parseSimpExp []                                      = []
    parseSimpExp ((Token(_, Just RelOperator, l)):tks)   = parseSimpExp tks
    parseSimpExp ((Token("+", Just AddOperator, l)):tks) = parseSimpExp tks
    parseSimpExp ((Token("-", Just AddOperator, l)):tks) = parseSimpExp tks
    parseSimpExp tks = parseSimpExp' tks
      where
        parseSimpExp' []      = []
        parseSimpExp' (t:tks) = if isTermExp t
                                then
                                  parseSimpExp (parseTerm (t:tks))
                                else 
                                  (t:tks)

    parseTerm :: [Token] -> [Token]
    parseTerm []      = []
    parseTerm (t:tks) = if (parseFactor [t] == [])
                        then
                            parseTerm' tks
                        else
                            (t:tks) 
      where
        parseTerm' :: [Token] -> [Token]
        parseTerm' []     = []
        parseTerm' ((Token(_, Just MulOperator, _)):
                     tks) = parseFactor tks
        parseTerm' tks    = parseFactor tks
 
    
    parseFactor :: [Token] -> [Token]
    parseFactor [] = []
    parseFactor ((Token(c, Just Identifier, l)):
                 (Token("(", Just Delimiter, l1)):
                  tks) = if getContent (last k) == ")"
                         then parseLC (pop tks (init k))
                         else ((Token(c, Just Identifier, l)):
                               (Token("(", Just Delimiter, l1)):
                                tks) 
                      where k = sublist tks ")"
    parseFactor ((Token(_, Just Identifier, l)):tks)       = tks
    parseFactor ((Token (_, Just IntegerNumber, l)):tks)   = tks
    parseFactor ((Token (_, Just RealNumber, l)):tks)      = tks
    parseFactor ((Token ("true", Just Keyword, l)):tks)    = tks
    parseFactor ((Token ("false", Just Keyword, l)):tks)   = tks
    parseFactor ((Token ("not", Just RelOperator, l)):tks) = parseFactor tks
    parseFactor ((Token("(", Just Delimiter, l)):tks)      = if getContent (head k) == ")" &&
                                                                getType (head k) == Just Delimiter
                                                             then
                                                               parseExp (pop tks (tail k))
                                                             else tks
                                                             where k = parseExp tks
    parseFactor tks                                        = tks

    turnProc :: [Token] -> [Token]
    turnProc []      = []
    turnProc (t:tks) = if isID t then 
                         if getContent (head tks) == "(" then
                            let lde = sublist (tail tks) ")"
                                sbe = init lde
                             in if [] == (parseExp sbe) then
                                    (pop tks sbe)
                                else
                                  tks
                        else tks
                      else tks