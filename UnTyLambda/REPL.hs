-- REPL for untyped lambda calculus
module UnTyLambda.REPL where

import Monstupar
import UnTyLambda.Interpreter

-- Парсим строку в терм
parseLambda :: Monstupar Char Term
parseLambda = do 
	term <- parseTerm
	spaces 
	other <- parseLambda' 
	return $ foldl App term other

parseLambda' :: Monstupar Char [Term]
parseLambda' = (do 
	term <- parseTerm 
	spaces 
	other <- parseLambda'  
	return $ term:other) <|> (return [])

parseTerm :: Monstupar Char Term
parseTerm = parseLam <|> parseBraces <|> parseVar

parseLam :: Monstupar Char Term
parseLam =  do 
	char '\\'
	spaces
	name <- parseVarName
	spaces
	char '.'
	spaces
	expr <- parseLambda
	spaces 
	return $ Lam name expr 
	
  
parseBraces :: Monstupar Char Term
parseBraces = do 
	char '('
	spaces
	expr <- parseLambda
	spaces
	char ')'
	return expr

parseVar :: Monstupar Char Term
parseVar = do 
	name <- parseVarName
	return $ Var name 

parseVarName :: Monstupar Char String
parseVarName = do 
	c <- letter 
	cs <- many $ letter <|> digit
	return (c:cs)

letter = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']
digit = oneOf ['0' .. '9']
space = oneOf [' ', '\t', '\n']
spaces = many space 
--------------------------------------------------------------------------------
-- Заметье, что грамматика лямбда-выражений леворекурсивна.
-- Перед тем как бросаться кодить, сначала уберите леворекурсивность
-- (неопределённость тоже стоит убрать) на бумаге, а потом напишите
-- получившуюся грамматику в EBNF вот сюда:
--
-- Term = "\" Var "." Lambda | "(" Lambda ")" | Var
-- Lambda  = Term Lambda'
-- Lambda' = Term Lambda' 
-- Var = letter (letter | digit)* 
--
--------------------------------------------------------------------------------

-- Красиво печатаем терм (можно с лишними скобками, можно без)
prettyPrint :: Term -> String
prettyPrint (Var x) = x
prettyPrint (Lam v e) = "\\" ++ v ++ "." ++ prettyPrint e
prettyPrint (App f t) = "(" ++ prettyPrint f ++ " " ++ prettyPrint t ++ ")"

-- Собственно сам REPL. Первый аргумент — максимальное число итераций при
-- попытке нормализации стратегией из второго аргумента.
replLoop :: Integer -> (Integer -> Term -> Term) -> IO ()
replLoop patience strategy = do 
	putStr "> "
	line <- getLine 
	case runParser parseLambda line of 
		Left _ -> putStrLn "Parse error"
		Right (_, term) -> putStrLn . prettyPrint $ strategy patience term

-- Диалог с (replLoop 100 no) должен выглядеть так:
-- > \x . (\y . y) x x
-- \x . x x
