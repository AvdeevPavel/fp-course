{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- В данном задании требуется реализовать интерпретатор для
-- нетипизированной лямбды
--------------------------------------------------------------------------------

module UnTyLambda.Interpreter where

-- Какие-то импорты. Заметьте, что в этом задании надо
-- использовать обычную Prelude
import Prelude hiding (catch)
import Control.Exception

------------------------------------------------------------
-- Определение дататайпа для нетипизированной лямбды
type Variable = String
data Term = Var Variable | Lam Variable Term | App Term Term deriving (Show,Read)

------------------------------------------------------------
-- Дальше всё на ваше усмотрение

-- Если внутри будете использовать именованное представление, то
-- я тут решил немного вам помочь
-- (иначе говоря, код из этого раздела можно совсем выкинуть,
-- если хочется)

-- свободные переменные в редексе 
freeVars :: Term -> [String]
freeVars (Var v)    = [ v ]
freeVars (Lam v expr)  = filter (/= v) . freeVars $ expr
freeVars (App e e') = (freeVars e) ++ (freeVars e')

newname :: [String] -> String -> String
newname fv = head . filter (not . flip elem fv) . iterate('_':)

subst :: Variable -> Term -> Term -> Term
subst var what term@(Var v) = if v == var then what else term
subst var what term@(Lam v t) = if v == var then term else Lam v (subst var what t)
subst var what (App e e') = App (subst var what e) (subst var what e')

-- Является ли терм редексом?
hasRedex :: Term -> Bool 
hasRedex (Var _) = False
hasRedex (Lam _ expr) = hasRedex expr
hasRedex (App (Lam _ _) _) = True 
hasRedex (App e e') = hasRedex e || hasRedex e' 

-- определение слабой заголовочной нормальной формы
weakHeadForm :: Term -> Bool 
weakHeadForm (Var _) = True
weakHeadForm (Lam _ _) = True
weakHeadForm (App (Lam _ _) _) = False
weakHeadForm (App e e') = weakHeadForm e

-- Альфа редукция, а именно переименование переменных
alphaReduct :: [String] -> Term -> Term
alphaReduct vars term@(Var _) = term 
alphaReduct vars (App e e') = App (alphaReduct vars e) (alphaReduct vars e')
alphaReduct vars term@(Lam v expr) = Lam newv newe
	where newv = if (elem v vars) then newname (vars ++ freeVars expr) v else v  
	      newe = if (elem v vars) then (subst v (Var newv) expr) else expr

-- Бета редукция
betaReduct :: Variable -> Term -> Term -> Term
betaReduct var what term = subst var what $ alphaReduct (freeVars what) term

-- нормальный порядок редукции
normalReduce :: Term -> Term  
normalReduce term@(Var _) = term
normalReduce (Lam v e) = Lam v (normalReduce e)
normalReduce (App (Lam v e) e') = betaReduct v e' e 
normalReduce (App e e') = if hasRedex e then App (normalReduce e) e' else App e (normalReduce e')

-- аппликативный порядок редукции
applicativeReduce :: Term -> Term
applicativeReduce term@(Var _) = term
applicativeReduce (Lam v e) = Lam v (applicativeReduce e)
applicativeReduce (App e e') = if hasRedex e' then App e (applicativeReduce e') 
				else case e of
					Lam v subt -> betaReduct v e' subt
		                	_ -> App (applicativeReduce e) e'

-- редукция в слабую головную нормальную форму 
weakHeadReduce :: Term -> Term 
weakHeadReduce (App (Lam v e) e') = subst v e' e  
weakHeadReduce term@(App e e') = if weakHeadForm e' then term else App (weakHeadReduce e) e'
weakHeadReduce term = term

-- редукция "слабым" аппликативным порядком.
weakAppForm :: Term -> Bool 
weakAppForm (Var _) = True
weakAppForm (Lam _ _) = True
weakAppForm (App (Lam _ _) _) = False
weakAppForm (App e e') = weakAppForm e'

weakApplicativeReduce :: Term -> Term 
weakApplicativeReduce term@(App e e') = if weakAppForm e' then App e (weakApplicativeReduce e') 
					else case e of
						Lam v subt -> subst v e' subt  
						_ -> term  
weakApplicativeReduce term = term

------------------------------------------------------------
-- За исключением того, что требуется реализовать следующие
-- стратегии нормализации (они все принимают максимальное
-- число шагов интерпретатора в качестве первого 
-- параметра (n); если за n шагов нормализовать не удаётся,
-- то следует бросать error, тестер его поймает):

wh, no, wa, sa :: Integer -> Term -> Term

-- Редукция аппликативным порядком
sa 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
sa n t = if hasRedex t then sa (n - 1) (applicativeReduce t) else t

-- Нормализация нормальным порядком
no 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
no n t = if hasRedex t then no (n - 1) (normalReduce t) else t

-- Редукция в слабую головную нормальную форму
wh 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
wh n t = if (weakHeadForm t) then t else wh (n - 1) (weakHeadReduce t)

-- (*) (не обязательно) Редукция "слабым" аппликативным порядком.
-- Отличается от обычного аппликативного тем, что не лезет внутрь
-- лямбд и правые части аппликаций, когда это возможно.
wa 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
wa n t = if (weakAppForm t) then t else wh (n - 1) (weakApplicativeReduce t)

-- Замечание: cкорость работы вашего интерпретатора специально не оценивается,
-- потому можно использовать свой изоморфный (с точностью до альфа-конверсии)
-- тип для представления термов и преобразовывать Term в него и обратно.

-- Перечисление всех этих порядков (в порядке отличном от
-- определения, да)
orders =
    [ ("wh", wh)
    , ("no", no)
    , ("wa", wa) -- Можно раскоментировать, да
    , ("sa", sa) ]

------------------------------------------------------------
-- Игнорируйте это, если выглядит непонятно
pall term = mapM_ $ \(d, x) -> putStr (d ++ ": ") >> catch (let t = x 1000 term in seq t (print t)) (\(e :: SomeException) -> print e)
testfuncs funcs = mapM_ $ \t -> putStr "===== " >> print t >> pall t funcs

------------------------------------------------------------
-- Сюда можно добавлять тесты
lamxx = Lam "x" $ App (Var "x") (Var "x")
omega = App lamxx lamxx

mytest = Lam "a" $ Lam "b" $ ((Lam "x" (Var "x")) `App` (Var "a")) `App` (Var "b")

test = testfuncs orders
    [ Var "a"
    , Lam "x" $ (Lam "y" $ Var "y") `App` (Var "x")
    , (Lam "x" $ Lam "y" $ Var "x") `App` (Var "y")
    , omega
    , lamxx
    , mytest
    ]

------------------------------------------------------------
-- Немного теоретических замечаний, если они вас волнуют
--
-- Следует специально отметить, что поскольку в конце теста
-- результат вычисления печатают, то ленивость Haskell не
-- влияет на семантику интерпретируемого исчисления.
--
-- Чтобы это особенно подчеркнуть в тестере выше я написал
-- seq в интересном месте (хотя конкретно это там ничего не
-- гарантирует, на самом-то деле).
