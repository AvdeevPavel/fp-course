import System.Environment
import Control.Exception

import Prelude hiding (catch)

import Monstupar 
import Lexer
import Datatype

-- grammar
parseCode :: Monstupar LEXEM Term 
parseCode = (do 
	      char LProgram
	      name <- like isIndentifer
	      char SEMICOLON
	      other <- many parseFunc
	      case name of 
		(Ident n) -> return $ PROGRAM n other
		otherwise -> return $ PROGRAM "" []
	    ) 

parseFunc :: Monstupar LEXEM CREATEFUNC 
parseFunc = (do 
		char LProcedure
		--name <- parseString 		
		char OBRACKET
		--args <- (sepBy parseInit (charWithSpace ';'))
		char CBRACKET
		char SEMICOLON
		--vars <- parseInitBlock
		--block <- parseBlock
		return $ PROCEDURE "" [] (INITBLOCK []) (BODY []) --name args vars block
	    ) <|> (do  
		char LFunction
		--name <- parseString
		char OBRACKET
		---args <- (sepBy parseInit (charWithSpace ';'))
		char CBRACKET
		char COLON
		retType <- like isType	-- think about			
		char SEMICOLON
		--vars <- parseInitBlock
		--block <- parseBlock
		return $ FUNCTION "" STRING [] (INITBLOCK []) (BODY []) --name retType args vars block
	    ) 


parseBlock :: Monstupar LEXEM BLOCK 
parseBlock = (do
		char LBEGIN
		-- other <- (sepEndBy parseAtom $ char SEMICOLON) 
		char LEND
		char SEMICOLON
		return $ BODY [] --other
	     )	  
{-			  	
parseInitBlock :: Monstupar LEXEM INITBLOCK
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
-}

isIndentifer :: LEXEM -> Bool 
isIndentifer (Ident _) = True 
isIndentifer _ = False

isType :: LEXEM -> Bool 
isType (LTSTRING) = True 
isType (LTINTEGER) = True
isType (LTBOOL) = True
isType _ = False

-- main
main :: IO()  
main = do 
	(filename:_) <- getArgs
 	contents <- readFile filename
	case runParser (many parseLexem) contents of 	
		Left _ -> error "Bad syntax file" 
		Right (_, lexems) -> case runParser parseCode lexems of 
					Left _ -> error "Bad syntax file" 
					Right (_, term) -> putStrLn . prettyPrint $ term
	return ()

