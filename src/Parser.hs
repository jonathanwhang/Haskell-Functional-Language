module Parser where

import Ast
import ParserMonad

keywords = ["if","then","else", "let", "in", "true","false", "print"]

vars :: Parser Ast
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

ints :: Parser Ast
ints = token intParser `mapParser` (\x -> ValInt x)
                
bools :: Parser Ast
bools =
    do b <- token ((literal "true") <||> (literal "false"))
       let res = case b of
            Left _  -> ValBool True
            Right _ -> ValBool False
       return res

nil :: Parser Ast
nil =
    do x <- token $ literal "[]"
       return Nil

ifParser :: Parser Ast
ifParser = do t <- token $ literal "if"
              expr <- parser
              t' <- token $ literal "then"
              then_expr <- parser
              t'' <- token $ literal "else"
              else_expr <- parser
              return (If expr then_expr else_expr)

letParser :: Parser Ast
letParser = do t <- token $ literal "let"
               var <- varParser
               t' <- token $ literal "="
               value <- parser
               t'' <- token $ literal "in"
               body <- parser
               return (Let var value body)

lambdaParser :: Parser Ast
lambdaParser = do t <- token $ literal "\\"
                  var <- varParser
                  t' <- token $ literal "->"
                  expr <- parser
                  return (Lam var expr)

chars :: Parser Ast
chars = do token $ literal "\'"
           c <- varParser
           token $ literal "\'"
           return (Character (head c))

strings :: Parser Ast
strings = do token $ literal "\""
             s <- varParser
             token $ literal "\""
             return (String s)
listParseSingleton :: Parser Ast
listParseSingleton = do token $ literal "["
                        left <- parser
                        token $ literal ","
                        right <- parser
                        token $ literal "]"
                        return (Cons left right)
-- list parse multiple does not work
listParseMultiple :: Parser Ast
listParseMultiple = do token $ literal "["
                       contents <- listParseMultiple'
                       token $ literal "]"
                       return contents

listParseMultiple' :: Parser Ast
listParseMultiple' = do head <- parser
                        token $ literal ","
                        rest <- listParseMultiple'
                        return (Cons head rest)

atoms:: Parser Ast -- level 16 precedence
atoms = ints <|> bools <|> nil <|> parens <|> ifParser <|> letParser <|> lambdaParser <|> vars <|> strings

notExpr :: Parser Ast -- right associative function
notExpr = do expr <- token $ literal "!"
             p    <- notParse
             return (Not p)

notParse :: Parser Ast -- makes notExpr left associative
notParse = notExpr <|> atoms

index :: Parser Ast -- indexing is left associative
index = withInfix consParse [("!!", Index)]

expo :: Parser Ast
expo = withInfix notParse [("^", FloatExp), ("**", Exp)]

multOrDivOrMod :: Parser Ast
multOrDivOrMod = withInfix expo [("*", Mult), ("//", Div), ("/", FloatDiv), ("%", Mod)]

addOrSub :: Parser Ast
addOrSub = withInfix multOrDivOrMod [("+", Plus), ("-", Minus)]

cons :: Parser Ast -- cons is right associative
cons = do left <- addOrSub
          middle <- token $ literal ":"
          right <- consParse
          return (Cons left right)

consParse :: Parser Ast -- make left associative
consParse = cons <|> addOrSub

concatExpr :: Parser Ast -- concat is right associative
concatExpr = do left <- index
                token $ literal "++"
                right <- concatParse
                return (Concat left right)

concatParse :: Parser Ast -- makes concat left associative
concatParse = concatExpr <|> index

eqParse :: Parser Ast
eqParse = do left <- concatParse
             token $ literal "=="
             right <- concatParse
             return (Eq left right)

neqParse :: Parser Ast
neqParse = do left <- concatParse
              token $ literal "/="
              right <- concatParse
              return (Neq left right)

ltParse :: Parser Ast
ltParse = do left <- concatParse
             token $ literal "<"
             right <- concatParse
             return (Lt left right)

lteParse :: Parser Ast
lteParse = do left <- concatParse
              token $ literal "<="
              right <- concatParse
              return (Lte left right)

gtParse :: Parser Ast
gtParse = do left <- concatParse
             token $ literal ">"
             right <- concatParse
             return (Gt left right)

gteParse :: Parser Ast
gteParse = do left <- concatParse
              token $ literal "<="
              right <- concatParse
              return (Gte left right)

-- eqOrOrd :: Parser Ast -- Combines all of the parsers for Eq and Ord
-- eqOrOrd = eqParse <|> neqParse <|> ltParse <|> lteParse <|> gtParse <|> gteParse

equality :: Parser Ast
equality = withInfix concatParse [("==", Eq), ("/=", Neq), ("<=", Lte), ("<", Lt), (">=", Gte), (">", Gt)]

andExpr :: Parser Ast
andExpr = withInfix equality [("&&", And)]

orExpr :: Parser Ast
orExpr = withInfix andExpr [("||", Or)]

apps :: Parser Ast
apps = withInfix orExpr [("",App)] -- the tokens eat up all the spaces so we split on the empty string

sepExpr :: Parser Ast
sepExpr = do left <- apps
             token $ literal ";"
             right <- sepParse
             return (Separator left right)

sepParse :: Parser Ast
sepParse = sepExpr <|> apps


parens :: Parser Ast
parens = do token $ literal "("
            ast <- parser
            token $ literal ")"
            return ast

parser :: Parser Ast
parser = sepParse

