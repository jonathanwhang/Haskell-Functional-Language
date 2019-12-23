module CheckTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 

import Data.Set (Set)
import qualified Data.Set as Set

import Ast
import Check

-- provide tests that show your check works

tests = testGroup "CheckTest"
    [
        testCase "Testing Undefined Variable" $ do assertEqual "Should have found undefined y" (Set.fromList [UndefinedVarUse "y"]) (check (Lam "x" (Plus (Var "y") (ValInt 3)))) -- \x -> y + 3
                                                   assertEqual "Should have found undefined y, z" (Set.fromList [UndefinedVarUse "y", UndefinedVarUse "z"]) (check (Let "x" (Var "y") (Lam "y" (Var "z")))) -- let x = y in \y -> z
                                                   assertEqual "Should have found no undefined vars" (Set.empty) (check ( (Let "x" (ValInt 3) (Plus (Var "x") (ValInt 3))) `App` (Lam "x" (Var "x")) )) -- (let x = 3 in x+3) (\x -> x)
                                                   assertEqual "Should have found undefined x, z" (Set.fromList [UndefinedVarUse "x", UndefinedVarUse "z"]) (check (If (Var "x") ( (Lam "y" (Var "y")) `App` (Plus (ValInt 3) (ValInt 5)) ) (Mult (Var "z") (Var "x"))) )
                                                   assertEqual "Should have found undefined x" (Set.fromList [UndefinedVarUse "x"]) (check (Neg (Var "x")))
                                                   assertEqual "Should have found undefined y, z" (Set.fromList [UndefinedVarUse "y", UndefinedVarUse "z"]) (check ((Lam "x" (Div (Var "x") (Var "y"))) `App` (Var "z")))
                                                   assertEqual "Should have found undefined x, y" (Set.fromList [UndefinedVarUse "x", UndefinedVarUse "y"]) (check (Print((Lam "z" ((Var "x") `App` (Var "y"))))))
    ]

-- tests = testGroup "CheckTest"
--     [
--         testCase "Testing Undefined Variable" $ do assertEqual "Should have found undefined var" ((Set.fromList [UndefinedVarUse "y"])) (check (Lam "x" (Plus (Str "y") (ValInt 3) ))
--     ]
        -- testCase "Testing Reused Variable" $ do assertEqual "Should have found reused var" (Set.fromList [UndefinedVarUse "y"]) (run (check (Lam "y" (Lam "y" (Str "y")))), -- (check (Lam "y" (Lam "y" (Str "y")),
        -- testCase "Testing Wrong Scope" $ do assertEqual "Should have found var in wrong scope" (Set.fromList [UndefinedVarUse "y"]) (run (check (Let "x" (Str "y") (Lam "y" (Str "x"))))) -- let x = y in (\y -> x)
        --                                     assertEqual "Should have found var in wrong scope" (Set.fromList [UndefinedVarUse "y"]) (run (Let "y" (ValInt 0) (Plus (Str "x") (Lam "x" (Str "x"))))) -- let y = 0 in x + (\x->x)
    -- [
    --     error "no tests yet!"
    --     -- ...
    -- ]