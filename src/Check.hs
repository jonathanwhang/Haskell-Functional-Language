module Check where

import Data.Set (Set)
import qualified Data.Set as Set

import Ast

-- here you can preform static checks


-- | The data type for all the static check warning
-- Some example include:
--   * use of undefined variable
--   * defined but unused variable
--   * type errors
data WarningMsg = 
    -- UndefinedVarUse "undefined variable"  -- ^ This is the Warning for use of Undefined variable name
    UndefinedVarUse String

  -- ...
  deriving (Show,Eq,Ord) -- !!! I added Ord to this. Idk if this is allowed but the Set operations wouldn't work otherwise

{-
The implementation of check should be similar to freeVars from (I think it was) week8; I left
my freeVars as a comment below.

A WarningMsg will be the constructor UndefinedVarUse, followed by the string variable name, 
e.g. "x", "y", so on. check should return a Set of these warnings.
-}

-- | perform static checking on the Ast
-- the output a set of warning on that input Ast
check :: Ast -> Set WarningMsg
check = checkUndefined

checkUndefined :: Ast -> Set WarningMsg
checkUndefined (Var x) = Set.fromList [UndefinedVarUse x]    -- starting with the more complicated ones
checkUndefined (If expr thenn elsee) = (checkUndefined thenn) `Set.union` (checkUndefined elsee)
checkUndefined (Let var val expr) = Set.delete (UndefinedVarUse var) (checkUndefined val `Set.union` checkUndefined expr)
checkUndefined (App expr1 expr2) = checkUndefined expr1 `Set.union` checkUndefined expr2
checkUndefined (Lam var bod) = Set.delete (UndefinedVarUse var) (checkUndefined bod)
checkUndefined (Separator expr1 expr2) = checkUndefined expr1 `Set.union` checkUndefined expr2
checkUndefined (Print x) = checkUndefined x

checkUndefined (And x y) = checkUndefined x `Set.union` checkUndefined y     -- busywork
checkUndefined (Or x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (Not x) = checkUndefined x
checkUndefined (Plus x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (Minus x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (Mult x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (Div x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (FloatDiv x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (Mod x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (FloatExp x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (Exp x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (Cons x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (Concat x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (Index x y) = checkUndefined y                       -- because x should just be an int
checkUndefined (Eq x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (Neq x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (Lt x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (Lte x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (Gt x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (Gte x y) = checkUndefined x `Set.union` checkUndefined y
checkUndefined (Neg x) = checkUndefined x
checkUndefined (Comp x y) = checkUndefined x `Set.union` checkUndefined y

checkUndefined _ = Set.empty     -- ints, floats, bools, strings, and chars do not contain vars

-- checkReused :: Ast -> Set WarningMsg
-- checkReused (Str x) = Set.empty
-- checkReused (If expr thenn elsee) = (checkReused thenn) `Set.union` (checkReused elsee)

-- checkReused (Lam var bod) = checkReused (Set.delete )

-- collect all the vars that appear in the expression that are not bound
-- freeVars :: Ast -> Set String
-- freeVars (VarStr x) = Set.fromList [x]
-- freeVars (App t1 t2) = (freeVars t1) `Set.union` (freeVars t2)
-- freeVars (Lam input bod) = Set.delete input (freeVars bod)