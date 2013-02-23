module Datatype where

data Term = PROGRAM String [CREATEFUNC] -- Название программы и содержимое  

data CREATEFUNC = FUNCTION Func Type [INIT] INITBLOCK BLOCK 
		| PROCEDURE Func [INIT] INITBLOCK BLOCK -- Задание функции и процедур 

data BLOCK = BODY [ATOM] -- Тело функции, процедуры 

data ATOM = APP Func [Expr] 
	  | PUT Variable Expr 
	  | FOR Variable (Int, Int) BLOCK 
          | WHILE Expr BLOCK 
	  | IF Expr BLOCK BLOCK  -- Возможные операции  

data INITBLOCK = INITBLOCK [INIT] -- инициализация в начале функции 

data INIT = INIT Type Variable 
	  | ARRAY Variable (Int, Int) Type -- Виды инициализаций  

data Type = Integer 
	  | STRING 
	  | Boolean -- Типы переменных 

type Variable = String -- имена переменных

type Func = String --имена функции

type Expr = String --выражение

-- --------------------- 
prettyPrint :: Term -> String
prettyPrint (PROGRAM [] []) = error "Bad Input"
prettyPrint (PROGRAM n xs) = "program " ++ n ++ "\n" ++ foldr (\x acc -> (printFUNC x) ++ "\n" ++ acc) [] xs  

printFUNC :: CREATEFUNC -> String
printFUNC (FUNCTION name t xs init block) = "function " ++ name ++ "(" ++ (printSepBy "; " xs (printINIT)) ++ "): " ++ (printTYPE t) ++ ";\n" 
			++ (printINITBLOCK init) ++ printBODY block
printFUNC (PROCEDURE name xs init block) = "procedure " ++ name ++ "(" ++ (printSepBy "; " xs (printINIT)) ++ "); \n"
			++ (printINITBLOCK init) ++ printBODY block

printBODY :: BLOCK -> String
printBODY (BODY xs) = "begin\n" ++ printSepEndBy "" xs (printATOM) ++ "end;\n"

printINITBLOCK :: INITBLOCK -> String
printINITBLOCK (INITBLOCK []) = []
printINITBLOCK (INITBLOCK xs) = "var\n" ++ printSepEndBy ";\n" xs (printINIT)

printATOM :: ATOM -> String 
printATOM (APP name vs) = name ++ " (" ++ (printSepBy ", " vs (\x -> x)) ++ "); \n"
printATOM (PUT nv expr) = nv ++ " := " ++ expr ++ "; \n"
printATOM (FOR nv (l, r) block) = "for " ++ nv ++ " := " ++ (show l) ++ " to " ++ (show r) ++ " do \n" ++ printBODY block
printATOM (WHILE expr block) = "while (" ++ expr ++ ") do\n" ++ printBODY block
printATOM (IF expr tblock eblock) = "if " ++ expr ++ " then " ++ printBODY tblock ++ " else " ++ printBODY eblock 
 
printINIT :: INIT -> String
printINIT (INIT t v) = v ++ (":" ++ printTYPE t)
printINIT (ARRAY v (l, r) t) = v ++ ": array [" ++ (show l) ++ ".." ++ (show r) ++ "] of " ++ printTYPE t

printTYPE :: Type -> String
printTYPE Integer = "Integer"
printTYPE STRING = "String"
printTYPE Boolean = "Boolean"

printSepBy :: String -> [a] -> (a -> String) -> String
printSepBy _ [] _ = []
printSepBy sep (x:[]) f = f x
printSepBy sep (x:xs) f = f x ++ sep ++ printSepBy sep xs f

printSepEndBy :: String -> [a] -> (a -> String) -> String
printSepEndBy _ [] _ = []
printSepEndBy sep (x:xs) f = f x ++ sep ++ printSepEndBy sep xs f

test = PROGRAM "HelloWorld" [ 
		PROCEDURE "myprint" ([]) (INITBLOCK [
			(INIT STRING "str")
			]) (BODY [
			(PUT "str" "hello world!")
			, (APP "println" ["str"])
			])
		, FUNCTION "main" Integer ([]) (INITBLOCK []) (BODY [
			(APP "myprint" [])			
		]) 
	]

test2 = (PUT "str" "hello world!") 


