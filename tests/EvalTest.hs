module EvalTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 

import Ast
import Eval
import LangMonad

-- provide tests that show your run/eval works

boolTests = testGroup "Boolean Tests" 
    [

        testCase "Testing ValBool" $ do assertEqual "(ValBool True) = True" ((Ok (B True)), []) (run (ValBool True))
                                        assertEqual "(ValBool False) = False" ((Ok (B False)), []) (run (ValBool False)),
        testCase "Testing And" $ do assertEqual "(And False False) = False" ((Ok (B False)), []) (run (And (ValBool False) (ValBool False)))
                                    assertEqual "(And False True) = False" ((Ok (B False)), []) (run (And (ValBool False) (ValBool True)))
                                    assertEqual "And True False = False" ((Ok (B False)), []) (run (And (ValBool True) (ValBool False)))
                                    assertEqual "And True True = False" ((Ok (B True)), []) (run (And (ValBool True) (ValBool True))),
        testCase "Testing Or" $ do assertEqual "Or False False = False" ((Ok (B False)), []) (run (Or (ValBool False) (ValBool False)))
                                   assertEqual "Or False True = False" ((Ok (B True)), []) (run (Or (ValBool False) (ValBool True)))
                                   assertEqual "Or True False = False" ((Ok (B True)), []) (run (Or (ValBool True) (ValBool False)))
                                   assertEqual "Or True True = False" ((Ok (B True)), []) (run (Or (ValBool True) (ValBool True))),
        testCase "Testing Not" $ do assertEqual "Not False = True" ((Ok (B True)), []) (run (Not (ValBool False)))
                                    assertEqual "Not True = False" ((Ok (B False)), []) (run (Not (ValBool True)))
        
    ]

arithmeticTests = testGroup "Arithmetic Tests"
    [
        testCase "Testing ValInt" $ do assertEqual "(ValInt 3) = 3" ((Ok (I 3)), []) (run (ValInt 3)),
        testCase "Testing Neg" $ do assertEqual "(Neg 3) = -3" ((Ok (I (-3))), []) (run (Neg (ValInt 3)))
                                    assertEqual "(Neg 3.7) = -3.7" ((Ok (F (-3.7))), []) (run (Neg (ValFloat 3.7)))
                                    assertEqual "(Neg -3) = 3" ((Ok (I 3)), []) (run (Neg (ValInt (-3))))
                                    assertEqual "(Neg -3.7) = 3.7" ((Ok (F 3.7)), []) (run (Neg (ValFloat (-3.7))))
                                    assertEqual "(Neg False = Error" ((Error "unary minus only works on ints or floats"), []) (run (Neg (ValBool False))),
        testCase "Testing Plus" $ do assertEqual "(Plus 1 5) = 6" ((Ok (I 6)), []) (run (Plus (ValInt 1) (ValInt 5)))
                                     assertEqual "(Plus 0 7) = 7" ((Ok (I 7)), []) (run (Plus (ValInt 0) (ValInt 7)))
                                     assertEqual "(Plus 1.7 3.5) = 5.2" ((Ok (F 5.2)), []) (run (Plus (ValFloat 1.7) (ValFloat 3.5)))
                                     assertEqual "(Plus 1.7 3 = Error" ((Error "second element does not typematch"), []) (run (Plus (ValFloat 1.7) (ValInt 3)))
                                     assertEqual "(Plus True 3) = Error" ((Error "plus works on ints and ints, or floats and floats"), []) (run (Plus (ValBool True) (ValInt 3))),
        testCase "Testing Minus" $ do assertEqual "(Minus 7 3) = 4" ((Ok (I 4)), []) (run (Minus (ValInt 7) (ValInt 3)))
                                      assertEqual "(Minus 3.3 1.7) = 1.5999999999999999" ((Ok (F 1.5999999999999999)), []) (run (Minus (ValFloat 3.3) (ValFloat 1.7)))
                                      assertEqual "(Minus 1.7 3) = Error" ((Error "second element does not typematch"), []) (run (Minus (ValFloat 1.7) (ValInt 3)))
                                      assertEqual "(Minus True 3) = Error"  ((Error "minus works on ints and ints, or floats and floats"), []) (run (Minus (ValBool True) (ValInt 3))),
        testCase "Testing Mult" $ do assertEqual "(Mult 3 4) = 12" ((Ok (I 12)), []) (run (Mult (ValInt 3) (ValInt 4)))
                                     assertEqual "(Mult 7.2 4.3) = 30.96" ((Ok (F 30.96)), []) (run (Mult (ValFloat 7.2) (ValFloat 4.3)))
                                     assertEqual "(Mult 1.7 3) = Error" ((Error "second element does not typematch"), []) (run (Mult (ValFloat 1.7) (ValInt 3)))
                                     assertEqual "(Mult True 3) = Error" ((Error "mult works on ints and ints, or floats and floats"), []) (run (Mult (ValBool True) (ValInt 3))),
        testCase "Testing Integer Div" $ do assertEqual "(Div 9 2) = 4" ((Ok (I 4)), []) (run (Div (ValInt 9) (ValInt 2)))
                                            assertEqual "(Div 9 0) = Error" ((Error "cannot divide by 0"), []) (run (Div (ValInt 9) (ValInt 0)))
                                            assertEqual "(Div 0.0 3) = Error" ((Error "not a valid integer"), []) (run (Div (ValFloat 0.0) (ValInt 3))),
        testCase "Floating-point Number Div" $ do assertEqual "(FloatDiv 9.3 2.7) = 3.4444444444444446" ((Ok (F 3.4444444444444446)), []) (run (FloatDiv (ValFloat 9.3) (ValFloat 2.7)))
                                                  assertEqual "(FloatDiv 9.3 0.0) = Error" ((Error "cannot divide by 0"), []) (run (FloatDiv (ValFloat 9.3) (ValFloat 0.0)))
                                                  assertEqual "(FloatDiv 0 9.3) = Error" ((Error "not a valid float"), []) (run (FloatDiv (ValInt 0) (ValFloat 9.3))),
        testCase "Testing Mod" $ do assertEqual "(Mod 9 2) = 1" ((Ok (I 1)), []) (run (Mod (ValInt 9) (ValInt 2)))
                                    assertEqual "(Mod 9.2 2) = Error" ((Error "not a valid integer"), []) (run (Mod (ValFloat 9.2) (ValInt 2)))
                                    assertEqual "(Mod 9 0) = Error" ((Error "cannot mod by 0"), []) (run (Mod (ValInt 9) (ValInt 0))),
        testCase "Testing Floating-Point Exponentiation" $ do assertEqual "(FloatExp 2.2 3.2) = 12.466730713250179" ((Ok (F 12.466730713250179)), []) (run (FloatExp (ValFloat 2.2) (ValFloat 3.2)))
                                                              assertEqual "(FloatExp 2 3.2) = Error" ((Error "not a valid float"), []) (run (FloatExp (ValInt 2) (ValFloat 3.2))),
        testCase "Testing Integer Exponentiation" $ do assertEqual "(Exp 2 4) = 16" ((Ok (I 16)), []) (run (Exp (ValInt 2) (ValInt 4)))
                                                       assertEqual "(Exp 2.4 3) = Error" ((Error "not a valid integer"), []) (run (Exp (ValFloat 2.4) (ValInt 3)))
    ]

consTests = testGroup "List Tests"
    [
        testCase "Testing Nil" $ do assertEqual "Nil = []" ((Ok (Ls []),[])) (run (Nil)),
        testCase "Testing Cons" $ do assertEqual "Cons 5 4 = [5,4]" (Ok (Ls [(I 5),(I 4)]),[]) (run (Cons (ValInt 5) (Cons (ValInt 4) Nil)))
                                     assertEqual "Cons 5 True = [5,True]" (Ok (Ls [(I 5),(B True)]),[]) (run (Cons (ValInt 5) (Cons (ValBool True) Nil))),
        testCase "Testing Concat" $ do assertEqual "Concat [5] [4] = [5,4]" (Ok (Ls [(I 5), (I 4)]),[]) (run (Concat (Cons (ValInt 5) Nil) (Cons (ValInt 4) Nil))),
        testCase "Testing Index" $ do assertEqual "[4,5] !! 1 = 4" (Ok (I 5),[]) (run (Index (Cons (ValInt 4) (Cons (ValInt 5) Nil)) (ValInt 1)))
                                     -- assertEqual "[4,5] !! 2 = Error" (Ok *** Exception: Prelude.!!: index too large) (run (Index (Cons (ValInt 4) (Cons (ValInt 5) Nil)) (ValInt 2))) -- not sure if this works
    ]

ifLetTests = testGroup "If and Let Tests"
    [
        testCase "Testing If" $ do assertEqual "If (And True False) then True else False" (Ok (B False),[]) (run (If (And (ValBool True) (ValBool False)) (ValBool True) (ValBool False))),
        testCase "Testing Let" $ do assertEqual "Let x = 3 in x+3 = 6" (Ok (I 6),[]) (run (Let "x" (ValInt 3) (Plus (Var "x") (ValInt 3))))
    ]

eqOrdTests = testGroup "Eq and Ord Tests"
    [
        testCase "Testing Eq" $ do assertEqual "3 = 2+1" (Ok (B True),[]) (run (Eq (ValInt 3) (Plus (ValInt 2) (ValInt 1))))
                                   assertEqual "3 != 2+2" (Ok (B False),[]) (run (Eq (ValInt 3) (Plus (ValInt 2) (ValInt 2)))),
        testCase "Testing Neq" $ do assertEqual "3 != 2+2" (Ok (B True),[]) (run (Neq (ValInt 3) (Plus (ValInt 2) (ValInt 2)))),
        testCase "Testing Lt/Lte" $ do assertEqual "3 !< 2" (Ok (B False),[]) (run (Lt (ValInt 3) (ValInt 2))) 
                                       assertEqual "2 < 3" (Ok (B True),[]) (run (Lt (ValInt 2) (ValInt 3)))
                                       assertEqual "3 ≤ 3" (Ok (B True),[]) (run (Lte (ValInt 3) (ValInt 3)))
                                       assertEqual "2 ≤ 3" (Ok (B True),[]) (run (Lte (ValInt 2) (ValInt 3))),
        testCase "Testing Gt/Gte" $ do assertEqual "3 > 2" (Ok (B True),[]) (run (Gt (ValInt 3) (ValInt 2)))
                                       assertEqual "3 ≥ 3" (Ok (B True),[]) (run (Gte (ValInt 3) (ValInt 3)))
    ]

sepPrintTests = testGroup "Separator and Print Tests"
    [
        testCase "Testing Separator" $ do assertEqual "4;True = True" (Ok (B True),[]) (run (Separator (ValInt 4) (ValBool True))),
        testCase "Testing Print" $ do assertEqual "print(4) = 4" (Ok (I 4),["4"]) (run (Print (ValInt 4)))
                                      assertEqual "print([4]) = [4]" (Ok (Ls [(I 4)]),["[4]"]) (run (Print (Cons (ValInt 4) Nil)))
    ]

stdlibTests = testGroup "std.lib Tests"
    [
        testCase "Testing head" $ do assertEqual "head([2,3,4]) = 2" (Ok (I 2), []) (run $ App (Var "head") (Cons (ValInt 2) (Cons (ValInt 3) (Cons (ValInt 4) Nil))))
                                     assertEqual "head([]) = Error" ((Error "can only call head on a non empty list"), []) (run $ App (Var "head") (Nil)),
        testCase "Testing tail" $ do assertEqual "tail ([2,3,4]) = [3,4]" ((Ok (Ls [(I 3), (I 4)])), []) (run $ App (Var "tail") (Cons (ValInt 2) (Cons (ValInt 3) (Cons (ValInt 4) Nil))))
                                     assertEqual "tail([]) = Error" ((Error "can only call tail on a non empty list"), []) (run $ App (Var "tail") (Nil)),
        testCase "Testing elem" $ do assertEqual "elem 5 [5,True] = True" (Ok (B True), []) (run $ App (App (Var "elem") (Cons (ValInt 5) (Cons (ValBool True) Nil))) (ValInt 5))
                                     assertEqual "elem 4 [5,True] = False" (Ok (B False), []) (run $ App (App (Var "elem") (Cons (ValInt 5) (Cons (ValBool True) Nil))) (ValInt 4))
                                     assertEqual "elem [5,True] 4 = Error" (Error "first arg of elem must be a list", []) (run $ App (App (Var "elem") (ValInt 4)) (Cons (ValInt 5) (Cons (ValBool True) Nil))),
        testCase "Testing map" $ do assertEqual "map (1+) [-1,3,4] = [0,4,5]" ((Ok (Ls [(I 0), (I 4), (I 5)])), []) (run $ App (App (Var "map") (Cons (Neg (ValInt 1)) (Cons (ValInt 3) (Cons (ValInt 4) Nil)))) (Lam "x" (Plus (ValInt 1) (Var "x"))))
                                    assertEqual "map (/2) [3,4,5] = [1,2,2]" ((Ok (Ls [(I 1), (I 2), (I 2)])), []) (run $ App (App (Var "map") (Cons (ValInt 3) (Cons (ValInt 4) (Cons (ValInt 5) Nil)))) (Lam "x" (Div (Var "x") (ValInt 2)))),
        testCase "Testing filter" $ do assertEqual "filter (==2) [2,3,4] = [2]" ((Ok (Ls[I 2]),[])) (run $ App (App (Var "filter") (Cons (ValInt 2) (Cons (ValInt 3) (Cons (ValInt 4) Nil)))) (Lam "x" (Eq (ValInt 2) (Var "x"))))
                                       assertEqual "filter (1>) [-1,3,4] = [-1]" (Ok (Ls[(I (-1))]),[]) (run $ App (App (Var "filter") (Cons (Neg (ValInt 1)) (Cons (ValInt 3) (Cons (ValInt 4) Nil)))) (Lam "x" (Gt (ValInt 1) (Var "x"))))
                                       assertEqual "filter (1+) [3,4] = Error" (Error "function does not apply to this type of val",[]) (run $ App (App (Var "filter") (Cons (Neg (ValInt 1)) (Cons (ValInt 3) (Cons (ValInt 4) Nil)))) (Lam "x" (Plus (ValInt 1) (Var "x")))),
        testCase "Testing ord" $ do assertEqual "ord 'a' = 97" (Ok (I 97), []) (run $ App (Var "ord") (Character 'a'))
                                    assertEqual "ord 9 = Error" (Error "can only call ord on a char", []) (run $ App (Var "ord") (ValInt 9)),
        testCase "Testing chr" $ do assertEqual "chr 97 = 'a'" (Ok (C 'a'),[]) (run $ App (Var "chr") (ValInt 97))
                                    assertEqual "chr 'a' = 97" (Error "can only call chr on an int", []) (run $ App (Var "chr") (Character 'a')),
        testCase "Testing float" $ do assertEqual "float 9 = 9.0" (Ok (F 9.0), []) (run $ App (Var "float") (ValInt 9))
                                      assertEqual "float 9.0 = Error" (Error "can only call float on an int", []) (run $ App (Var "float") (ValFloat 9.0)),
        testCase "Testing int" $ do assertEqual "int 9.0 = 9" (Ok (I 9), []) (run $ App (Var "int") (ValFloat 9.0))
                                    assertEqual "int 9 = Error" (Error "can only call int on a float", []) (run $ App (Var "int") (ValInt 9))
    ]

    -- run $ App (App (Var "elem") (ValInt 5)) (Cons (ValInt 5) Nil)
    -- -- run $ App (App (Var "elem") (Cons (ValInt 5) Nil)) (ValInt 5)
    -- run $ App (App (Var "map") (Cons (ValInt 1) (Cons (ValInt 2) Nil))) plusTwo
    -- run $ App (App (Var "map") (Cons (ValInt 1) (Cons (ValInt 2) Nil))) plusTwo

    -- plusTwo = Fun $ \i -> case i of I int -> (I (int+2), ["added 2"])
    --                                 _     -> err "not an int"

--For testing lambdas, you have to put in something into the lambda ex: (\x -> x + 3) 2) = 5