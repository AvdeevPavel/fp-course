import System.Environment
import Control.Exception

import Prelude hiding (catch)

import Monstupar 
--import Lexer
import Datatype

-- grammar
parseCode :: Monstupar Char Term 
parseCode = do 
	string "program"	
	spaces
	name <- parseString 
	charWithSpace ';'
	other <- (sepBy parseFunc spaces)
	return $ PROGRAM name other

parseFunc :: Monstupar Char CREATEFUNC 
parseFunc = (do 
		string "procedure"
		spaces
		name <- parseString 		
		charWithSpace '('
		args <- (sepBy parseInit (charWithSpace ';'))
		charWithSpace ')'
		charWithSpace ';'
		vars <- parseInitBlock
		block <- parseBlock
		return $ PROCEDURE name args vars block
	    ) <|> (do  
		string "function"
		spaces
		name <- parseString
		charWithSpace '('
		args <- (sepBy parseInit (charWithSpace ';'))
		charWithSpace ')'
		charWithSpace ':'
		retType <- parseType				
		charWithSpace ';'
		vars <- parseInitBlock
		block <- parseBlock
		return $ FUNCTION name retType args vars block
	    ) 

parseBlock :: Monstupar Char BLOCK 
parseBlock = (do
		stringWithSpace "begin"
		other <- (sepEndBy parseAtom $ charWithSpace ';') 
		string "end"
		charWithSpace ';'
		return $ BODY other
	     )	  
			  	
parseInitBlock :: Monstupar Char INITBLOCK
parseInitBlock = (do
		  string "var" 
		  spaces
		  other <- (sepEndBy parseInit $ charWithSpace ';')
		  return $ INITBLOCK other
		) <|> (return $ INITBLOCK [])

parseAtom :: Monstupar Char ATOM 
parseAtom = (do 
		name <- parseString
		charWithSpace '('
		args <- (sepBy parseEXPR $ charWithSpace ',')
		charWithSpace ')'
		return $ APP name args		
	    ) <|> (do 
		name <- parseNameVar
		stringWithSpace ":="
		expr <- parseEXPR
		return $ PUT name expr
	    ) <|> (do 
		string "for"
		name <- parseNameVar
		stringWithSpace ":="
		left <- natNumber 
		stringWithSpace "to"
		right <- natNumber 
		stringWithSpace "do"
		block <- parseBlock
		return $ FOR name ((read left :: Int), (read right :: Int)) block
	    ) <|> (do 
		string "while"
		charWithSpace '('
		expr <- parseEXPR
		charWithSpace ')'
		stringWithSpace "do"
		block <- parseBlock
		return $ WHILE expr block
	    ) <|> (do 
		string "if"
		spaces 
		expr <- parseEXPR
		spaces 
		string "then"
		tblock <- parseBlock 
		string "else"
		eblock <- parseBlock 
		return $ IF expr tblock eblock
	    )    

parseInit :: Monstupar Char INIT 
parseInit = (do 
		name <- parseString
		charWithSpace ':'
		t <- parseType
		return $ INIT t name
	    ) <|> (do 
		name <- parseString
		charWithSpace ':'
		string "array"		
		charWithSpace '['
		left <- natNumber 
		stringWithSpace ".."
		right <- natNumber
		charWithSpace ']'
		stringWithSpace "of"
		t <- parseType
		return $ ARRAY name ((read left :: Int), (read right :: Int)) t
	    ) 

parseEXPR :: Monstupar Char EXPR  
parseEXPR = (do 
		f <- natNumber
		return $ IVAL (read f)
	    ) <|> (do 
		f <- (string "True") <|> (string "False")
		return $ BVAL (read f)
	    ) <|> (do 
		f <- parseString
		return $ SVAL f
	    ) <|> (do 
		charWithSpace '('
		name <- parseString
		charWithSpace '('
		args <- (sepBy parseEXPR $ charWithSpace ',')
		charWithSpace ')'
		charWithSpace ')'
		return (APPP name args)	
	    ) <|> (do 
		exprWithBracket '+' (PLUS)
	    ) <|> (do 
		exprWithBracket '-' (MINUS)
	    ) <|> (do 
		exprWithBracket '*' (MULT)
	    ) <|> (do 
		exprWithBracket '/' (DIV)
	    )
	where exprWithBracket c func = (do 
		charWithSpace '('
		f <- parseEXPR
		char c
		s <- parseEXPR
		charWithSpace ')'
		return $ func f s 
		) 

parseType :: Monstupar Char Type 
parseType = (do 
		string "String" 
		return STRING
  	    ) <|> (do 
		string "Integer"
		return Integer
	    ) <|> (do
		string "Boolean"
		return Boolean
	    )  

parseNameVar :: Monstupar Char String  
parseNameVar = do 
	c <- letter 
	cs <- many $ letter <|> digit <|> (oneOf ['[', ']'])
	return (c:cs)

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

stringWithSpace :: String -> Monstupar Char String
stringWithSpace s = (do 
			spaces
			string s
			spaces
		    )

charWithSpace :: Char -> Monstupar Char String
charWithSpace c = (do 
			spaces
			char c
			spaces
		  )


-- main
main :: IO()  
main = do 
	(filename:_) <- getArgs
 	contents <- readFile filename	
	case runParser parseCode contents of 
		Left _ -> error "Bad syntax file" 
		Right (_, term) -> putStrLn . prettyPrint $ term
	return ()
