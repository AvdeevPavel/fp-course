module Datatype {- (
	Term, CREATEFUNC, BLOCK, ATOM, INITBLOCK, INIT, Type, Variable, Func, Expr
	, prettyPrint 
	)-} where

data Term = PROGRAM String [CREATEFUNC] -- Название программы и содержимое  

data CREATEFUNC = FUNCTION Func Type [INIT] INITBLOCK BLOCK | PROCEDURE Func [INIT] INITBLOCK BLOCK -- Задание функции и процедур 

data BLOCK = BODY [ATOM] -- Тело функции, процедуры 

data ATOM = APP Func [Variable] | PUT Variable Expr | FOR Variable (Int, Int) BLOCK | WHILE Expr BLOCK | IF Expr BLOCK BLOCK  -- Возможные операции  

data INITBLOCK = INITBLOCK [INIT] -- инициализация в начале функции 

data INIT = INIT Type Variable | ARRAY Variable (Int, Int) Type -- Виды инициализаций  

data Type = Integer | STRING | Boolean -- Типы переменных 

type Variable = String -- имена переменных

type Func = String --имена функции

type Expr = String --выражение

-- ---------------------
foldlPrint f s = foldr (\x acc -> (f x) ++ s ++ acc) 

foldlPrint' = foldr (\x acc -> x ++ acc) 
 
prettyPrint :: Term -> String
prettyPrint (PROGRAM [] []) = error "Bad Input"
prettyPrint (PROGRAM n xs) = "program " ++ n ++ "\n" ++ foldlPrint (printFUNC) "\n" [] xs  

printFUNC :: CREATEFUNC -> String
printFUNC (FUNCTION name t xs (INITBLOCK init) (BODY bl)) = "function " ++ name ++ "(" ++ (foldlPrint (printINIT) ", " [] xs) ++ "): " ++ 			(printTYPE t) ++ "\n" ++ (foldlPrint (printINIT) "\n" [] init) ++ "BEGIN \n" ++ printBLOCK bl
printFUNC (PROCEDURE name xs (INITBLOCK init) (BODY bl)) = "procedure " ++ name ++ "(" ++ (foldlPrint (printINIT) ", " [] xs) ++ ") \n"
		++ (foldlPrint (printINIT) "\n" [] init) ++ "BEGIN\n" ++ printBLOCK bl

printBLOCK :: [ATOM] -> String 
printBLOCK [] = "END;\n"
printBLOCK (x:xs) = printATOM x ++ printBLOCK xs

printATOM :: ATOM -> String 
printATOM (APP name vs) = name ++ "(" ++ (foldlPrint' [] vs) ++ "); \n"
printATOM (PUT nv expr) = nv ++ ":=" ++ (show expr) ++ ";\n"
printATOM (FOR nv (l, r) (BODY bs)) = "for " ++ nv ++ ":= " ++ (show l) ++ " to " ++ (show r) ++ " do" ++ "\n" ++ "BEGIN\n" ++ printBLOCK bs
printATOM (WHILE expr (BODY bs)) = "while (" ++ expr ++ ") do\n" ++ printBLOCK bs
printATOM (IF expr (BODY ts) (BODY es)) = "if " ++ expr ++ " then BEGIN " ++ printBLOCK ts ++ "else BEGIN \n" ++ printBLOCK es 
 
printINIT :: INIT -> String
printINIT (INIT t v) = v ++ (":" ++ printTYPE t)
printINIT (ARRAY v (l, r) t) = v ++ ": array [" ++ (show l) ++ ".." ++ (show r) ++ "] of " ++ printTYPE t

printTYPE :: Type -> String
printTYPE Integer = "Integer"
printTYPE STRING = "String"
printTYPE Boolean = "Boolean"

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


