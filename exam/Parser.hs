import System.Environment
import Control.Exception
import Prelude hiding (catch)

-- ghc -o Parser Parser.hs ../Monstupar.hs ../Monstupar/Derived.hs ../Monstupar/Core.hs
import Monstupar 
import Datatype

-- grammar
parseCode :: Monstupar Char Term 
parseCode = do 
	string "program"	
	spaces
	name <- parseString 
	spaces
	other <- parseFuncs
	return $ PROGRAM name other

parseFuncs :: Monstupar Char [CREATEFUNC] 
parseFuncs = undefined 

parseFunc :: Monstupar Char CREATEFUNC 
parseFunc = (do 
		string "procedure"
		spaces
		name <- parseString 		
		charWithSpace '('
		args <- parseTypeVars
		charWithSpace ')'
		vars <- parseInitBlock
		block <- parseBlock
		return $ PROCEDURE name args vars block
	) <|> (do  
		string "function"
		spaces
		name <- parseString
		charWithSpace '('
		args <- parseTypeVars
		charWithSpace ')'
		char ':'
		space
		retType <- parseType				
		vars <- parseInitBlock
		block <- parseBlock
		return $ FUNCTION name retType args vars block
	) 

parseBlock :: Monstupar Char BLOCK 
parseBlock = undefined 

----problem in this, because last haven't ,
parseTypeVars :: Monstupar Char [INIT]
parseTypeVars = do
		  name <- parseString
		  charWithSpace ':'
		  t <- parseType
		  charWithSpace ','
		  	

parseInitBlock :: Monstupar Char INITBLOCK
parseInitBlock = undefined 

parseAtom :: Monstupar Char ATOM 
parseAtom = (do 
		name <- parseString
		charWithSpace '('
		args <- parseVars
		charWithSpace ')'
		char ';'
		return $ APP name args		
	    ) <|> (do 
		name <- parseString
		stringWithSpace ":="
		expr <- parseExpr
		char ';'
		return $ PUT name expr
	    ) <|> (do 
		string "for"
		name <- parseString
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
		expr <- parseExpr
		charWithSpace ')'
		string "do"
		spaces
		block <- parseBlock
		return $ WHILE expr block
	    ) <|> (do 
		string "if"
		spaces 
		expr <- parseExpr
		spaces 
		string "then"
		tblock <- parseBlock 
		spaces
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
		string "arrray"		
		charWithSpace '['
		left <- natNumber 
		stringWithSpace ".."
		right <- natNumber
		charWithSpace ']'
		string "of"
		spaces
		t <- parseType
		return $ ARRAY name ((read left :: Int), (read right :: Int)) t
	    ) 

--problem in this, because last haven't ,
parseVars :: Monstupar Char [Variable]
parseVars = do
		var <- parseString
		charWithSpace ','
		other <- parseVars
		return (var:other)

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

parseString :: Monstupar Char String  
parseString = do 
	c <- letter 
	cs <- many $ letter <|> digit
	return (c:cs)

parseExpr :: Monstupar Char String  
parseExpr = many1 $ letter <|> digit <|> operation

charWithSpace :: Char -> Monstupar Char String  
charWithSpace c = do 
		  	spaces
			char c 
			spaces

stringWithSpace :: String -> Monstupar Char String  
stringWithSpace s = do 
		  	spaces
			string s 
			spaces

letter = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']
digit = oneOf ['0' .. '9']
operation = oneOf ['=', '+', '-', '*', '/', '>', '<', '<']
space = oneOf [' ', '\t', '\n']
spaces = many space
natNumber = many1 digit

-- main
main :: IO()  
main = do 
	(filename:_) <- getArgs  -- Как навесить исключение на это, что бы в случае не совпадения с паттерном выдовать разумную надпись? 
	contents <- readFile filename	
	case runParser parseCode contents of 
		Left _ -> error "Bad syntax file" 
		Right (_, term) -> putStrLn . prettyPrint $ term
	return ()
