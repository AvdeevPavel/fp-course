module Datatype where

data Term = PROGRAM String [CREATEFUNC] deriving (Show) -- Название программы и содержимое  

data CREATEFUNC = FUNCTION Func Type [INIT] INITBLOCK BLOCK 
		| PROCEDURE Func [INIT] INITBLOCK BLOCK deriving (Show)-- Задание функции и процедур 

data BLOCK = BODY [ATOM] deriving (Show)-- Тело функции, процедуры 

data ATOM = APP Func [EXPR] 
	  | PUT Variable EXPR 
	  | FOR Variable (Int, Int) BLOCK 
          | WHILE EXPR BLOCK 
	  | IF EXPR BLOCK BLOCK  deriving (Show)-- Возможные операции  

data EXPR = IVAL Int
	  | BVAL Bool
	  | SVAL String
	  | PLUS EXPR EXPR 
	  | MINUS EXPR EXPR 
	  | MULT EXPR EXPR 
	  | DIV EXPR EXPR
	  | APPP Func [EXPR] deriving (Show)

data INITBLOCK = INITBLOCK [INIT] deriving (Show)-- инициализация в начале функции 

data INIT = INIT Type Variable 
	  | ARRAY Variable (Int, Int) Type deriving (Show)-- Виды инициализаций  

data Type = Integer 
	  | STRING 
	  | Boolean deriving (Show)-- Типы переменных 
 
type Variable = String -- имена переменных

type Func = String --имена функции

type Expr = String --выражение

-- --------------------- 
prettyPrint :: Term -> String
prettyPrint (PROGRAM [] []) = error "Bad Input"
prettyPrint (PROGRAM n xs) = "program " ++ n ++ ";\n" ++ foldr (\x acc -> (printFUNC x) ++ "\n" ++ acc) [] xs 

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
printATOM (APP name vs) = name ++ " (" ++ (printSepBy ", " vs (printEXPR)) ++ "); \n"
printATOM (PUT nv expr) = nv ++ " := " ++ (printEXPR expr) ++ "; \n"
printATOM (FOR nv (l, r) block) = "for " ++ nv ++ " := " ++ (show l) ++ " to " ++ (show r) ++ " do \n" ++ printBODY block
printATOM (WHILE expr block) = "while (" ++ printEXPR expr ++ ") do\n" ++ printBODY block
printATOM (IF expr tblock eblock) = "if " ++ printEXPR expr ++ " then " ++ printBODY tblock ++ " else " ++ printBODY eblock 
 
printINIT :: INIT -> String
printINIT (INIT t v) = v ++ (":" ++ printTYPE t)
printINIT (ARRAY v (l, r) t) = v ++ ": array [" ++ (show l) ++ ".." ++ (show r) ++ "] of " ++ printTYPE t

printTYPE :: Type -> String
printTYPE Integer = "Integer"
printTYPE STRING = "String"
printTYPE Boolean = "Boolean"

printEXPR :: EXPR -> String 
printEXPR (IVAL v) = (show v)
printEXPR (BVAL v) = (show v)
printEXPR (SVAL v) = (show v)
printEXPR (PLUS l r) = "(" ++ (printEXPR l) ++ " + " ++ (printEXPR r) ++ ")"
printEXPR (MINUS l r) = "(" ++ (printEXPR l) ++ " - " ++ (printEXPR r) ++ ")"
printEXPR (MULT l r) = "(" ++ (printEXPR l) ++ " * " ++ (printEXPR r) ++ ")"
printEXPR (DIV l r) = "(" ++ (printEXPR l) ++ " / " ++ (printEXPR r) ++ ")"
printEXPR (APPP name xs) = "(" ++ name ++ " (" ++ (printSepBy ", " xs (printEXPR)) ++ ") " ++ ")"

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
			(PUT "str" (PLUS (IVAL 3) (MINUS (IVAL 4) (IVAL 5))))
			, (APP "println" [(SVAL "str")])
			])
		, FUNCTION "main" Integer ([]) (INITBLOCK []) (BODY [
			(APP "myprint" [])			
		]) 
	]

test2 = (PUT "str" (SVAL "1435")) 


