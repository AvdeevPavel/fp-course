-- ghc -o Parser Parser.hs ../Monstupar.hs ../Monstupar/Derived.hs ../Monstupar/Core.hs
import System.Environment
import Control.Exception
import Prelude hiding (catch)

import Monstupar 
import Datatype

-- grammar
parseCode :: Monstupar Char Term 
parseCode = undefined

parserFunc :: Monstupar Char Term 
parserFunc = undefined 

parserBlock :: Monstupar Char Term 
parserBlock = undefined 

parserInitBlock :: Monstupar Char Term 
parserInitBlock = undefined 

parserAtom :: Monstupar Char Term 
parserAtom = undefined 

parserInit :: Monstupar Char Term 
parserInit = undefined 

parserType :: Monstupar Char Term 
parserType = undefined 

letter = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']
digit = oneOf ['0' .. '9']
space = oneOf [' ', '\t', '\n']
spaces = many space

-- main
main :: IO()  
main = do 
	(filename:_) <- getArgs  -- Как навесить исключение на это, что бы в случае не совпадения с паттерном выдовать разумную надпись? 
	contents <- readFile filename	
	case runParser parseCode contents of 
		Left _ -> error "Bad syntax file" 
		Right (_, term) -> putStrLn . prettyPrint $ term
	return ()
