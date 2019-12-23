module ParserTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 

import Ast
import Parser
import ParserMonad

-- provide tests that show your parser works

tests = testGroup "Parser Tests" 
  [
  testCase "Testing Atoms" $ do assertEqual "parse parser 5" (Just (ValInt 5,"")) (parse parser "5") 
                                assertEqual "parse parser true" (Just (ValBool True,"")) (parse parser "true") 
                                assertEqual "parse parser []" (Just (Nil, "")) (parse parser "[]") 
                                assertEqual "parse parser if true then 5 else 7" (Just (If (ValBool True) (ValInt 5) (ValInt 7),"")) (parse parser "if true then 5 else 7") 
                                assertEqual "parse parser let x = 5 in 3" (Just (Let "x" (ValInt 5) (ValInt 3),"")) (parse parser "let x = 5 in 3") 
                                assertEqual "parse parser let x = true in 15" (Just (Let "x" (ValBool True) (ValInt 15),"")) (parse parser "let x = true in 15")
                                assertEqual "parse parser \\x -> x" (Just (Lam "x" (Var "x"),"")) (parse parser "\\x -> x") 
                                assertEqual "parse parser \' c \'" (Just (Character 'c',"")) (parse chars "\' c \'")
                                assertEqual "parse parser \' ca \'" (Just (Character 'c',"")) (parse chars "\' ca \'")
                                assertEqual "parse parser \" ca \"" (Just (String "ca","")) (parse parser "\" ca \"")
                                assertEqual "parse parser hello" (Just (Var "hello","")) (parse parser "hello") ,
  
  testCase "Testing Basics" $ do assertEqual "parse parser !true" (Just (Not (ValBool True),"")) (parse parser "!true") 
                                 assertEqual "parse parser !false" (Just (Not (ValBool False),"")) (parse parser "!false") 
                                 assertEqual "parse parser 2:3 !! 1" (Just (Index (Cons (ValInt 2) (ValInt 3)) (ValInt 1),"")) (parse parser "2:3 !! 1") 
                                 assertEqual "parse parser 5^2" (Just (FloatExp (ValInt 5) (ValInt 2),"")) (parse parser "5^2") 
                                 assertEqual "parse parser 5^true" (Just (FloatExp (ValInt 5) (ValBool True),"")) (parse parser "5^true") 
                                 assertEqual "parse parser 5 ** 2" (Just (Exp (ValInt 5) (ValInt 2),"")) (parse parser "5 ** 2") 
                                 assertEqual "parse parser 5 ** true" (Just (Exp (ValInt 5) (ValBool True),"")) (parse parser "5 ** true") 
                                 assertEqual "parse parser 5 * 2" (Just (Mult (ValInt 5) (ValInt 2),"")) (parse parser "5 * 2") 
                                 assertEqual "parse parser 5 // 2" (Just (Div (ValInt 5) (ValInt 2), "")) (parse parser "5 // 2") 
                                 assertEqual "parse parser 2 + 2" (Just (Plus (ValInt 2) (ValInt 2),"")) (parse parser "2 + 2") 
                                 assertEqual "parse parser 5 - 2" (Just (Minus (ValInt 5) (ValInt 2),"")) (parse parser "5 - 2") 
                                 assertEqual "parse parser 2:3" (Just (Cons (ValInt 2) (ValInt 3),"")) (parse parser "2:3") 
                                 assertEqual "parse parser 2:3:5:10" (Just (Cons (ValInt 2) (Cons (ValInt 3) (Cons (ValInt 5) (ValInt 10))),"")) (parse parser "2:3:5:10") 
                                 assertEqual "parse parser 2:3 ++ 5:7" (Just (Concat (Cons (ValInt 2) (ValInt 3)) (Cons (ValInt 5) (ValInt 7)),"")) (parse parser "2:3 ++ 5:7")
                                 assertEqual "parse parser 2:3 ++ 5:7:10" (Just (Concat (Cons (ValInt 2) (ValInt 3)) (Cons (ValInt 5) (Cons (ValInt 7) (ValInt 10))),"")) (parse parser "2:3 ++ 5:7:10")
                                 assertEqual "parse parser 2 == 5" (Just (Eq (ValInt 2) (ValInt 5),"")) (parse parser "2 == 5")
                                 assertEqual "parse parser 2 /= 5" (Just (Neq (ValInt 2) (ValInt 5),"")) (parse parser "2 /= 5") 
                                 assertEqual "parse parser 2 < 5" (Just (Lt (ValInt 2) (ValInt 5),"")) (parse parser "2 < 5") 
                                 assertEqual "parse parser 2 <= 5" (Just (Lte (ValInt 2) (ValInt 5), "")) (parse parser "2 <= 5") 
                                 assertEqual "parse parser 2 > 5" (Just (Gt (ValInt 2) (ValInt 5),"")) (parse parser "2 > 5") 
                                 assertEqual "parse parser 2 >= 5" (Just (Gte (ValInt 2) (ValInt 5), "")) (parse parser "2 >= 5")
                                 assertEqual "parse parser 2 3" (Just (App (ValInt 2) (ValInt 3),"")) (parse parser "2 3")
                                 assertEqual "parse parser 2; 5" (Just (Separator (ValInt 2) (ValInt 5),"")) (parse parser "2; 5"),

  testCase "Testing Weird Ones" $ do assertEqual "parse parser 2 + 5 * 3" (Just (Plus (ValInt 2) (Mult (ValInt 5) (ValInt 3)),"")) (parse parser "2 + 5 * 3")
                                     assertEqual "parse parser 2 * 15; true" (Just (Separator (Mult (ValInt 2) (ValInt 15)) (ValBool True),"")) (parse parser " 2 * 15; true")
                                     assertEqual "parse parser 5; 3; true; qwerty" (Just (Separator (ValInt 5) (Separator (ValInt 3) (Separator (ValBool True) (Var "qwerty"))),"")) (parse parser "5; 3; true; qwerty")
                                     assertEqual "parse parser 5 ** 2; let x = 5 in 3; true" (Just (Separator (Exp (ValInt 5) (ValInt 2)) (Let "x" (ValInt 5) (Separator (ValInt 3) (ValBool True))),"")) (parse parser "5 ** 2; let x = 5 in 3; true")
                                     assertEqual "parse parser true && 54 && if true then 5 else 7" (Just (And (And (ValBool True) (ValInt 54)) (If (ValBool True) (ValInt 5) (ValInt 7)),"")) (parse parser "true && 54 && if true then 5 else 7")
  ]


