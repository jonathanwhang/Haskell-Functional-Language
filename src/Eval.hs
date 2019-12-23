module Eval where

import Data.Map (Map)
import qualified Data.Map as Map

import Ast
import LangMonad


-- the goal of the program is to return a value, what values are possible?
data Val = I Integer | B Bool | F Double | C Char | S String
         | Ls [Val]
         | Fun (Val -> (Unsafe Val, [String]))

instance Show Val where
  show (I i) = show i
  show (B b) = show b
  show (F d) = show d
  show (C c) = show c
  show (S s) = show s
  show (Ls ls) = show ls
  show (Fun _) = "\\ x -> ?" -- no good way to show a function

-- on functions, we want equality to return an error. equality should work on everything except functions.
-- ordereds should work on floats and ints.

-- examples for testing
plusTwo = Fun $ \i -> case i of I int -> (Ok (I (int+2)), [])
                                _     -> (Error "did not pass in int", [])

instance Eq Val where
    x == y = eqVal x y

type Env = Map String Val

stdLib = Map.fromList
  [("head", Fun $ \ value -> case value of Ls (h:_)  -> (Ok h, [])
                                           _         -> (Error "can only call head on a non empty list",[]) ),
   ("tail", Fun $ \ value -> case value of Ls (_:rest) -> (Ok (Ls rest), [])
                                           _         -> (Error "can only call tail on a non empty list",[]) ),
   ("elem", Fun $ \lst -> ((Ok (Fun (\el -> (case lst of
                                                (Ls lst') -> (elem' lst' el, []) 
                                                _         -> (Error "first arg of elem must be a list", [])
                                                ))), []))),
   ("map", Fun $ \lst -> ((Ok (Fun (\f -> (case lst of
                                                (Ls lst') -> case f of 
                                                                  (Fun f') -> (map' lst' f, [])
                                                                  _        -> (Error "second arg of map must be a function", [])
                                                _         -> (Error "first arg of map must be a list", [])
                                                ))), []))),
   ("filter", Fun $ \lst -> ((Ok (Fun (\f -> (case lst of 
                                                  (Ls lst') -> case f of
                                                                    (Fun f') -> (filter' lst' f, [])
                                                                    _        -> (Error "second arg of map must be a function", [])
                                                  _         -> (Error "first arg of filter must be a list", [])
                                                  ))), []))),
   ("ord", Fun $ \ value -> case value of C c -> (Ok (I (fromIntegral (fromEnum c))), [])
                                          _   -> (Error "can only call ord on a char",[]) ),
   ("chr", Fun $ \ value -> case value of I i -> (Ok (C (toEnum (fromIntegral i) :: Char)), []) -- I don't know why but you have to cast to Char for toEnum to behave
                                          _   -> (Error "can only call chr on an int",[]) ),
   ("float", Fun $ \ value -> case value of I i -> (Ok (F (fromInteger i :: Double)), [])
                                            _   -> (Error "can only call float on an int", []) ),
   ("int", Fun $ \value -> case value of F f -> (Ok (I (round f)), [])
                                         _   -> (Error "can only call int on a float", []) )
   ]

elem' :: [Val] -> Val -> Unsafe Val
elem' [] _  = Ok (B False)
elem' (x:xs) el = if (eqVal el x) then (Ok (B True)) else (elem' xs el)

--tests for map
--run $ App (App (Var "map") (listInts)) (test)
nonsense = Lam "x"
listInts = Cons (ValInt 1) (Cons (ValInt 2) Nil)


map' :: [Val] -> Val -> Unsafe Val
map' [] _ = Ok (Ls [])
map' (x:xs) f = case (map' xs f) of 
                      Ok (Ls mapped) -> case f of
                                          Fun f' -> case (f' x) of 
                                                      (Ok y, _) -> Ok (Ls ([y] ++ mapped))
                                                      _         -> Error "fun didn't return an unsafe?"
                                          _      -> Error "second arg of map' must be a function"
                      _              -> (Error "function does not apply to this type of val")

filter' :: [Val] -> Val -> Unsafe Val
filter' [] _ = Ok (Ls [])
filter' (x:xs) f = case (filter' xs f) of
                          Ok (Ls filtered) -> case f of
                                                Fun f' -> case (f' x) of
                                                            ((Ok (B True)), _)  -> Ok (Ls ([x] ++ filtered))
                                                            ((Ok (B False)), _) -> Ok (Ls filtered)
                                                            _                   -> Error "f doesn't return a bool"
                                                _      -> Error "second arg of filter' must be a function"
                          _                -> Error "function does not apply to this type of val"

eval :: Ast -> EnvUnsafeLog Env Val
eval (ValBool b) = return $ B b
eval (And l r) =
    do l' <- evalBool l
       r' <- evalBool r
       if (l' && r')
       then return (B (True))
       else return (B (False))
eval (Or l r) =
    do l' <- evalBool l
       r' <- evalBool r
       if (l' || r')
       then return (B (True))
       else return (B (False))
eval (Not b) =
    do b' <- evalBool b
       if (b')
       then return (B (False))
       else return (B (True))


eval (String s) = return $ S s
eval (ValInt i) = return $ I i
eval (ValFloat i) = return $ F i
eval (Plus l r) = -- adds ints to ints, floats to floats, not ints and floats.  TODO Overload these
    do l' <- eval l
       r' <- eval r
       case l' of
            (I i) -> case r' of
                    (I j) -> return (I (i + j))
                    _     -> err "second element does not typematch"
            (F i) -> case r' of
                    (F j) -> return (F (i + j))
                    _     -> err "second element does not typematch"
            _     -> err "plus works on ints and ints, or floats and floats"
eval (Minus l r) =
    do l' <- eval l
       r' <- eval r
       case l' of
            (I i) -> case r' of
                    (I j) -> return (I (i - j))
                    _     -> err "second element does not typematch"
            (F i) -> case r' of
                    (F j) -> return (F (i - j))
                    _     -> err "second element does not typematch"
            _     -> err "minus works on ints and ints, or floats and floats"
eval (Mult l r) =
    do l' <- eval l
       r' <- eval r
       case l' of
            (I i) -> case r' of
                    (I j) -> return (I (i * j))
                    _     -> err "second element does not typematch"
            (F i) -> case r' of
                    (F j) -> return (F (i * j))
                    _     -> err "second element does not typematch"
            _     -> err "mult works on ints and ints, or floats and floats"
eval (Div l r) =
    do l' <- evalInt l
       r' <- evalInt r
       if (r' == 0)
       then err "cannot divide by 0"
       else return (I (l' `div` r'))
eval (Exp base exp) = do base' <- evalInt base
                         exp' <- evalInt exp
                         return (I (base' ^ exp'))
eval (FloatExp base exp) = do base' <- evalFloat base
                              exp' <- evalFloat exp
                              return (F (base' ** exp'))
eval (FloatDiv l r) = do l' <- evalFloat l
                         r' <- evalFloat r
                         if (r' == 0)
                         then err "cannot divide by 0"   
                         else return (F (l' / r'))
eval (Mod l r) = do l' <- evalInt l
                    r' <- evalInt r
                    if (r' == 0)
                    then err "cannot mod by 0"
                    else return (I (l' `mod` r'))
eval (Neg x) = do x' <- eval x
                  case x' of
                    (I i) -> return (I (negate i))
                    (F f) -> return (F (negate f))
                    _     -> err "unary minus only works on ints or floats"

    {- do if (isFloat x)
                  then x' <- evalFloat x
                  else x' <- evalInt x
                  case x' of
                        (I i) -> return (I (-(i)))
                        (F f) -> return (F (-(f))) -}

eval (Eq l r) = do l' <- eval l
                   r' <- eval r
                   case l' of 
                        (Fun _) -> err "cannot find equality of a function"
                        (I i) -> case r' of
                                    (I j) -> return $ B (eqVal l' r')
                                    _     -> err "first ast is an int, types don't match"
                        (B i) -> case r' of
                                    (B j) -> return $ B (eqVal l' r')
                                    _     -> err "first ast is a bool, types don't match"
                        (F i) -> case r' of
                                    (F j) -> return $ B (eqVal l' r')
                                    _     -> err "first ast is a float, types don't match"
                        (C i) -> case r' of
                                    (C j) -> return $ B (eqVal l' r')
                                    _     -> err "first ast is a char, types don't match"
                        (S i) -> case r' of
                                    (S j) -> return $ B (eqVal l' r')
                                    _     -> err "first ast is a string, types don't match"
                        (Ls i) -> case r' of
                                    (Ls j) -> return $ B (eqVal l' r')
                                    _     -> err "first ast is a list, types don't match"                                    
eval (Neq l r) = do l' <- eval l
                    r' <- eval r
                    case l' of 
                        (Fun _) -> err "cannot find equality of a function"
                        (I i) -> case r' of
                                    (I j) -> return $ B (not (eqVal l' r'))
                                    _     -> err "first ast is an int, types don't match"
                        (B i) -> case r' of
                                    (B j) -> return $ B (not (eqVal l' r'))
                                    _     -> err "first ast is a bool, types don't match"
                        (F i) -> case r' of
                                    (F j) -> return $ B (not (eqVal l' r'))
                                    _     -> err "first ast is a float, types don't match"
                        (C i) -> case r' of
                                    (C j) -> return $ B (not (eqVal l' r'))
                                    _     -> err "first ast is a char, types don't match"
                        (S i) -> case r' of
                                    (S j) -> return $ B (not (eqVal l' r'))
                                    _     -> err "first ast is a string, types don't match"
                        (Ls i) -> case r' of
                                    (Ls j) -> return $ B (not (eqVal l' r'))
                                    _     -> err "first ast is a list, types don't match"
eval (Lt l r) = do l' <- eval l
                   r' <- eval r
                   case l' of 
                        (Fun _) -> err "cannot find equality of a function"
                        (I i) -> case r' of
                                    (I j) -> return $ B (ltVal l' r')
                                    _     -> err "first ast is an int, types don't match"
                        (B i) -> case r' of
                                    (B j) -> return $ B (ltVal l' r')
                                    _     -> err "first ast is a bool, types don't match"
                        (F i) -> case r' of
                                    (F j) -> return $ B (ltVal l' r')
                                    _     -> err "first ast is a float, types don't match"
                        (C i) -> case r' of
                                    (C j) -> return $ B (ltVal l' r')
                                    _     -> err "first ast is a char, types don't match"
                        (S i) -> case r' of
                                    (S j) -> return $ B (ltVal l' r')
                                    _     -> err "first ast is a string, types don't match"
                        (Ls i) -> case r' of
                                    (Ls j) -> return $ B (ltVal l' r')
                                    _     -> err "first ast is a list, types don't match"
eval (Gt l r) = do l' <- eval l
                   r' <- eval r
                   case l' of 
                        (Fun _) -> err "cannot find equality of a function"
                        (I i) -> case r' of
                                    (I j) -> return $ B (gtVal l' r')
                                    _     -> err "first ast is an int, types don't match"
                        (B i) -> case r' of
                                    (B j) -> return $ B (gtVal l' r')
                                    _     -> err "first ast is a bool, types don't match"
                        (F i) -> case r' of
                                    (F j) -> return $ B (gtVal l' r')
                                    _     -> err "first ast is a float, types don't match"
                        (C i) -> case r' of
                                    (C j) -> return $ B (gtVal l' r')
                                    _     -> err "first ast is a char, types don't match"
                        (S i) -> case r' of
                                    (S j) -> return $ B (gtVal l' r')
                                    _     -> err "first ast is a string, types don't match"
                        (Ls i) -> case r' of
                                    (Ls j) -> return $ B (gtVal l' r')
                                    _     -> err "first ast is a list, types don't match"
eval (Lte l r) = do l' <- eval l
                    r' <- eval r
                    case l' of 
                        (Fun _) -> err "cannot find equality of a function"
                        (I i) -> case r' of
                                    (I j) -> return $ B ((ltVal l' r') || (eqVal l' r'))
                                    _     -> err "first ast is an int, types don't match"
                        (B i) -> case r' of
                                    (B j) -> return $ B ((ltVal l' r') || (eqVal l' r'))
                                    _     -> err "first ast is a bool, types don't match"
                        (F i) -> case r' of
                                    (F j) -> return $ B ((ltVal l' r') || (eqVal l' r'))
                                    _     -> err "first ast is a float, types don't match"
                        (C i) -> case r' of
                                    (C j) -> return $ B ((ltVal l' r') || (eqVal l' r'))
                                    _     -> err "first ast is a char, types don't match"
                        (S i) -> case r' of
                                    (S j) -> return $ B ((ltVal l' r') || (eqVal l' r'))
                                    _     -> err "first ast is a string, types don't match"
                        (Ls i) -> case r' of
                                    (Ls j) -> return $ B ((ltVal l' r') || (eqVal l' r'))
                                    _     -> err "first ast is a list, types don't match"
eval (Gte l r) = do l' <- eval l
                    r' <- eval r
                    case l' of 
                        (Fun _) -> err "cannot find equality of a function"
                        (I i) -> case r' of
                                    (I j) -> return $ B ((gtVal l' r') || (eqVal l' r'))
                                    _     -> err "first ast is an int, types don't match"
                        (B i) -> case r' of
                                    (B j) -> return $ B ((gtVal l' r') || (eqVal l' r'))
                                    _     -> err "first ast is a bool, types don't match"
                        (F i) -> case r' of
                                    (F j) -> return $ B ((gtVal l' r') || (eqVal l' r'))
                                    _     -> err "first ast is a float, types don't match"
                        (C i) -> case r' of
                                    (C j) -> return $ B ((gtVal l' r') || (eqVal l' r'))
                                    _     -> err "first ast is a char, types don't match"
                        (S i) -> case r' of
                                    (S j) -> return $ B ((gtVal l' r') || (eqVal l' r'))
                                    _     -> err "first ast is a string, types don't match"
                        (Ls i) -> case r' of
                                    (Ls j) -> return $ B ((gtVal l' r') || (eqVal l' r'))
                                    _     -> err "first ast is a list, types don't match"

eval (Character c) = return $ C c
eval (Nil) = return (Ls []) -- list evaluators
eval (Cons l r) =
    do l' <- eval l
       r' <- eval r
       case r' of
         Ls r'' -> return (Ls ([l'] ++ r'')) 
         _    -> err "second ast must be a list"
eval (Index lst idx) = do lst' <- evalList lst
                          idx' <- evalInt idx
                          case idx' of
                                0 -> case lst' of
                                        (x:rest) -> return x
                                        [] -> err "cannot index an empty list"
                                n -> case lst' of
                                        (x:rest) -> return (lst' !! (fromIntegral n))
                                        [] -> err "cannot index an empty list"
eval (Concat l r) = do l' <- evalList l
                       r' <- evalList r
                       return (Ls (l' ++ r'))

eval (If b then_expr else_expr) = -- other evaluators
    do b' <- evalBool b
       then_expr' <- eval then_expr
       else_expr' <- eval else_expr
       if b'
       then return then_expr'
       else return else_expr'
eval (Let str value body) =
    do env <- getEnv
       v' <- eval value
       EnvUnsafeLog (\env -> runEnvUnsafeLog (eval body) (Map.insert str v' env))
eval (Var s) = valOf s
eval (Lam x body) = do env <- getEnv
                       return $ Fun $ \v -> runEnvUnsafeLog (eval body) (Map.insert x v env)
eval (App f input) = do f' <- evalFun f
                        i' <- eval input
                        let res = (f' i')
                        case res of
                            (Ok a, log) -> return a
                            (Error s, log) -> err s
eval (Separator x y) = do x' <- eval x
                          y' <- eval y
                          return y'

-- eval (Comp f g) = let x = new variable
--                   in eval (Lam x (App f (App g x)))

eval (Print x) = do x' <- eval x
                    EnvUnsafeLog (\env -> (Ok x', [show x']))

-- snyder:
-- eval (Print x) -> EnvUnsafeLog (\env -> (Unsafe (), )
-- " \" "



valOf :: String -> EnvUnsafeLog Env Val
valOf var = do env <- getEnv
               case (Map.lookup var env) of
                  Just var' -> return var'
                  Nothing   -> err "did not pass in a zvar"

-- add a val into the environment
withVal :: String -> Val -> EnvUnsafeLog Env a -> EnvUnsafeLog Env a
withVal var v comp = undefined


evalFun :: Ast -> EnvUnsafeLog Env (Val -> (Unsafe Val, [String]))
evalFun a = do f <- eval a
               case f of
                  Fun f' -> return f'
                  _      -> err "not a valid function"

evalBool :: Ast -> EnvUnsafeLog Env Bool
evalBool b =
    do b' <- eval b
       case b' of
            B b'' -> return b''
            _     -> err "not a valid boolean"

evalInt :: Ast -> EnvUnsafeLog Env Integer
evalInt a =
    do x <- eval a
       case x of
            I i -> return i
            _   -> err "not a valid integer"

evalFloat :: Ast -> EnvUnsafeLog Env Double
evalFloat f = do f' <- eval f
                 case f' of
                    F x -> return x
                    _   -> err "not a valid float"

evalList :: Ast -> EnvUnsafeLog Env [Val]
evalList a = do a' <- eval a
                case a' of (Ls x) -> return x
                           _    -> err "not a valid list"

eqVal :: Val -> Val -> Bool
eqVal l r = case l of 
              (I i) -> case r of
                         (I j)   -> i == j
                         _       -> False
              (B i) -> case r of
                         (B j)   -> i == j
                         _       -> False
              (F i) -> case r of
                         (F j)   -> i == j
                         _       -> False
              (C i) -> case r of
                         (C j)   -> i == j
                         _       -> False
              (S i) -> case r of
                         (S j)   -> i == j
                         _       -> False
              (Ls []) -> case r of 
                            (Ls []) -> True
                            _       -> False
              (Ls (x:xs)) -> case r of
                                (Ls (y:ys))   -> (eqVal x y) && (eqVal (Ls xs) (Ls ys))
                                _             -> False

ltVal :: Val -> Val -> Bool
ltVal l r = case l of 
              (Fun _) -> undefined
              (I i) -> case r of
                         (Fun _) -> undefined
                         (I j)   -> i < j
                         _       -> False
              (B i) -> case r of
                         (Fun _) -> undefined
                         (B j)   -> i < j
                         _       -> False
              (F i) -> case r of
                         (Fun _) -> undefined
                         (F j)   -> i < j
                         _       -> False
              (C i) -> case r of
                         (Fun _) -> undefined
                         (C j)   -> i < j
                         _       -> False
              (S i) -> case r of
                         (S j)   -> i < j
                         _       -> False
              (Ls []) -> case r of 
                            (Fun _)     -> undefined
                            (Ls (x:xs)) -> True
                            _           -> False
              (Ls (x:xs)) -> case r of -- [4, 9] and [5, 7]
                                (Fun _)       -> undefined
                                (Ls [])       -> False
                                (Ls (y:ys))   -> if (ltVal x y)
                                                 then True
                                                 else if (eqVal x y)
                                                      then (ltVal (Ls xs) (Ls ys))
                                                      else False
                                _             -> False

gtVal :: Val -> Val -> Bool
gtVal l r = case l of 
              (Fun _) -> undefined
              (I i) -> case r of
                         (Fun _) -> undefined
                         (I j)   -> i > j
                         _       -> False
              (B i) -> case r of
                         (Fun _) -> undefined
                         (B j)   -> i > j
                         _       -> False
              (F i) -> case r of
                         (Fun _) -> undefined
                         (F j)   -> i > j
                         _       -> False
              (C i) -> case r of
                         (Fun _) -> undefined
                         (C j)   -> i > j
                         _       -> False
              (S i) -> case r of
                         (S j)   -> i > j
                         _       -> False
              (Ls []) -> case r of 
                            (Fun _) -> undefined
                            _       -> False
              (Ls (x:xs)) -> case r of
                                (Fun _)       -> undefined
                                (Ls [])       -> True
                                (Ls (y:ys))   -> if (gtVal x y)
                                                 then True
                                                 else if (eqVal x y)
                                                      then (gtVal (Ls xs) (Ls ys))
                                                      else False
                                _             -> False

run :: Ast -> (Unsafe Val, [String])
run a = runEnvUnsafeLog (eval a) (stdLib)

