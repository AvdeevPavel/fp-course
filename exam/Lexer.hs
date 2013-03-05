module Lexer where 

import Monstupar

-- You can see this comonnly lexer parser in this libs:
-- Text.Parsec.Language 
-- Text.Parsec.Expression

data LEXEM = LProgram  -- +
	   | LFunction  -- +
	   | LProcedure -- + 
           | OBRACKET -- + 
	   | CBRACKET -- + 
	   | NEWLINE -- +  
	   | LFOR -- + 
	   | LWHILE  -- + 
	   | LTO -- + 
	   | LDO -- +
	   | LIF -- +
	   | LTHEN -- + 
	   | LELSE --  +
	   | LVAR -- + 
   	   | LBEGIN -- + 
	   | LEND -- + 	
	   | LARRAY -- + 
	   | LOF -- + 
	   | OSQBRACKET -- + 
	   | CSQBRACKET -- +
	   | LASSIGMENT -- + 
	   | LPLUS -- +
	   | LMINUS -- +
	   | LDIV -- + 
	   | LMULT -- + 
	   | SEMICOLON -- + 
	   | COLON -- + 
	   | COMMA -- + 	   
	   | Ident String -- +
	   | IConst Int -- + 
	   | BConst Bool -- +
		deriving (Show)

stringList :: [(String, LEXEM)]
stringList = [("program", (LProgram)),
     	      ("function", (LFunction)),
	      ("procedure", (LProcedure)), 
	      ("for", (LFOR)), 
	      ("while", (LWHILE)), 
	      ("do", (LDO)), 
	      ("if", (LIF)), 
	      ("then", (LTHEN)), 
	      ("else", (LELSE)), 
	      ("var", (LVAR)), 
	      ("begin", (LBEGIN)), 
	      ("end", (LEND)), 
	      ("of", (LOF)), 
	      ("array", (LARRAY)), 
	      (":=", (LASSIGMENT))	    
	   ]

charList :: [(Char, LEXEM)]
charList = [ ('(', (OBRACKET)), 
	     (')', (CBRACKET)),
	     ('[', (OSQBRACKET)), 
	     (']', (CSQBRACKET)),
	     ('+', (LPLUS)),
	     ('-', (LMINUS)),
	     ('*', (LMULT)), 
	     ('/', (LDIV)),
	     (';', (SEMICOLON)),
	     (':', (COLON)),
	     (',', (COMMA)),
	     ('\n', (NEWLINE))
	   ]

-- LLVM 
 
parseLexerTERM :: Monstupar Char LEXEM
parseLexerTERM = (
		 foldr1 (\x acc -> x <|> acc) $ map (\x -> stringWithSpace (fst x) (snd x)) stringList
	  	) <|>  ( 
		 foldr1 (\x acc -> x <|> acc) $ map (\x -> charWithSpace (fst x) (snd x)) charList
		) <|> (do 
		 spaces
		 f <- parseString 
		 spaces 
		 return (Ident f)
		) <|> (do
		 spaces 
		 f <- natNumber 
		 spaces 
		 return $ IConst (read f)
		) <|> (do
		 spaces 
		 f <- (string "True" <|> string "False") 
		 spaces
		 return $ BConst (read f) 
		)   

letter = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']
digit = oneOf ['0' .. '9']
operation = oneOf ['=', '+', '-', '*', '/', '>', '<', '<']
symbol = oneOf ['[', ']', '"']
space = oneOf [' ', '\t']

natNumber = many1 digit
spaces = many space

parseString :: Monstupar Char String  
parseString = do 
	c <- letter 
	cs <- many $ letter <|> digit <|> operation <|> symbol 
	return (c:cs)

charWithSpace :: Char -> a -> Monstupar Char a  
charWithSpace c f = (do 
			spaces
			char c
			spaces
			return f
		  )

stringWithSpace :: String -> a -> Monstupar Char a
stringWithSpace s f = (do 
			spaces
			string s
			spaces
			return f
		    )
