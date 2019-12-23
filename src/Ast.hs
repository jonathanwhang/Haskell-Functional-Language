module Ast where

-- | the abstract syntax tree for the language
data Ast = ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast

         | ValInt Integer | ValFloat Double
         | Plus Ast Ast | Minus Ast Ast | Mult Ast Ast | Div Ast Ast 
         | FloatDiv Ast Ast | Mod Ast Ast | FloatExp Ast Ast | Exp Ast Ast

         | Nil
         | Cons Ast Ast
         | Concat Ast Ast
         | Index Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Eq Ast Ast | Neq Ast Ast | Lt Ast Ast | Lte Ast Ast | Gte Ast Ast | Gt Ast Ast
         
         | Character Char | Var String | String String
         | Lam String Ast
         | App Ast Ast 

         | Separator Ast Ast | Print Ast

         | Neg Ast 
         
         | Comp Ast Ast deriving (Show, Eq) -- helpful to use this during testing
         --deriving Eq

-- instance Show Ast where
--    -- display the ast in a readable way
--    show ast = showFullyParen ast

parenthesize :: Integer -- ^ the precedence level of outer expression
              -> Integer -- ^ the precedence level of the current expression
              -> String -- ^ string representation current expression
              -> String -- ^ the properly (not necessarily fully) parenthesized current expression

parenthesize outerLevel curLevel showExp 
  | outerLevel < curLevel = "(" ++ showExp ++ ")"
  | otherwise             =        showExp
  

-- | output the fully parenthesized statement
showFullyParen :: Ast  -- ^ The Ast to show
                -> String  -- ^ the fully parenthesized string representing the input Ast
showFullyParen (ValInt i) = "(" ++ show i ++ ")"
showFullyParen (ValBool True) = "(" ++ "true" ++ ")"
showFullyParen (ValBool False) = "(" ++ "false" ++ ")"
showFullyParen (And l r) = "(" ++ (showFullyParen l) ++ " && " ++ (showFullyParen r) ++ ")"
showFullyParen (Or l r) = "(" ++ (showFullyParen l) ++ " || " ++ (showFullyParen r) ++ ")"
showFullyParen (Not a) = "(" ++ " ! " ++ (showFullyParen a) ++ ")"
showFullyParen (Plus l r) = "(" ++ (showFullyParen l) ++ " + " ++ (showFullyParen r) ++ ")"
showFullyParen (Minus l r) = "(" ++ (showFullyParen l) ++ " - " ++ (showFullyParen r) ++ ")"
showFullyParen (Mult l r) = "(" ++ (showFullyParen l) ++ " * " ++ (showFullyParen r) ++ ")"
showFullyParen (Div l r) = "(" ++ (showFullyParen l) ++ " // " ++ (showFullyParen r) ++ ")"
showFullyParen (If b t e) = "(if " ++ (showFullyParen b) ++ " then " ++ (showFullyParen t) ++ " else " ++ (showFullyParen e) ++ ")"
showFullyParen (Let v a bod) = "(let " ++ v ++ " = " ++ (showFullyParen a) ++ " in " ++ (showFullyParen bod) ++ ")"
showFullyParen (Lam v bod) = "(\\ " ++ v ++ " -> " ++ (showFullyParen bod) ++ ")"
showFullyParen (App f a) = "( " ++ (showFullyParen f)  ++ " " ++ (showFullyParen a) ++ ")"
showFullyParen (Var s) = "( " ++ s ++ ")"
showFullyParen (Cons h t) = "(" ++ (showFullyParen h)  ++ " : " ++ (showFullyParen t) ++ ")"
showFullyParen (Character c) = "(" ++ (show c) ++ ")"
showFullyParen (Exp x y) = "(" ++ (showFullyParen x) ++ " ** " ++ (showFullyParen y) ++ ")"
showFullyParen (FloatExp x y) = "(" ++ (showFullyParen x) ++ " ^ " ++ (showFullyParen y) ++ ")"
showFullyParen (FloatDiv x y) = "(" ++ (showFullyParen x) ++ " / " ++ (showFullyParen y) ++ ")"
showFullyParen (Mod x y) = "(" ++ (showFullyParen x) ++ " % " ++ (showFullyParen y) ++ ")"
showFullyParen (Neg x) = "(-" ++ (showFullyParen x) ++ ")"
showFullyParen (Nil) = "( [] )"
showFullyParen (ValFloat x) = "(" ++ show x ++ ")"

showFullyParen (Eq x y) = "(" ++ (showFullyParen x) ++ " == " ++ (showFullyParen y) ++ ")"
showFullyParen (Neq x y) = "(" ++ (showFullyParen x) ++ " /= " ++ (showFullyParen y) ++ ")"
showFullyParen (Lt x y) = "(" ++ (showFullyParen x) ++ " < " ++ (showFullyParen y) ++ ")"
showFullyParen (Gt x y) = "(" ++ (showFullyParen x) ++ " > " ++ (showFullyParen y) ++ ")"
showFullyParen (Lte x y) = "(" ++ (showFullyParen x) ++ " <= " ++ (showFullyParen y) ++ ")"
showFullyParen (Gte x y) = "(" ++ (showFullyParen x) ++ " >= " ++ (showFullyParen y) ++ ")"
showFullyParen (Index x y) = "(" ++ (showFullyParen x) ++ " !! " ++ (showFullyParen y) ++ ")"
showFullyParen (Concat x y) = "(" ++ (showFullyParen x) ++ " ++ " ++ (showFullyParen y) ++ ")"
showFullyParen (Separator x y) = "(" ++ (showFullyParen x) ++ "; " ++ (showFullyParen y) ++ ")"
showFullyParen (Print x) = "(print(" ++ (showFullyParen x) ++ ")"
showFullyParen (String x) = "(\"" ++ show x ++ "\")"


-- | provide a nice show with minimal parentheses
showPretty :: Ast  -- ^ The Ast to show
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
showPretty (ValInt i) _ =  if i < 0
                           then  "(" ++ show i ++ ")"
                           else show i
showPretty (ValBool True) _ =  "true"
showPretty (ValBool False)  _  = "false"
showPretty Nil _ = "[]"
showPretty (Var s) _ = s

showPretty (Lam v bod) i = parenthesize 1 i $ "\\ " ++ v ++ " -> " ++ (showPretty bod 1)
showPretty (Let v a bod)  i = parenthesize 1 i $  "let " ++ v ++ " = " ++ (showPretty a 1) ++ " in " ++ (showPretty bod 1)
showPretty (If b t e) i = parenthesize 1 i $  "if " ++ (showPretty b 1) ++ " then " ++ (showPretty t 1) ++ " else " ++ (showPretty e 1)

showPretty (App l r) i = parenthesize 2 i $ (showPretty l 2) ++ " " ++ (showPretty r 3)
showPretty (Cons l r) i = parenthesize 4 i $ (showPretty l 5) ++ " : " ++ (showPretty r 4)
showPretty (Or l r) i = parenthesize 6 i $ (showPretty l 6) ++ " || " ++ (showPretty r 7)
showPretty (And l r) i = parenthesize 8 i $ (showPretty l 8) ++ " && " ++ (showPretty r 9)
showPretty (Minus l r) i = parenthesize 10 i $ (showPretty l 10) ++ " - " ++ (showPretty r 11)
showPretty (Plus l r) i = parenthesize 10 i $ (showPretty l 10) ++ " + " ++ (showPretty r 11)
showPretty (Mult l r) i = parenthesize 12 i $ (showPretty l 12) ++ " * " ++ (showPretty r 13)
showPretty (Div l r) i = parenthesize 12 i $ (showPretty l 12) ++ " / " ++ (showPretty r 13)

showPretty (Not l ) i = parenthesize 14 i $  " ! " ++ (showPretty l 14)
