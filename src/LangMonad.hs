-- This file is just for hints.
-- Don't edit this file unless you are comfortable resolving merge conflicts, since we may add hints in the future
-- Instead copy and paste what you want into your homework

module LangMonad where

import Control.Monad(ap)

--This monad should form the plumbing for the evaluation function
-- This is a very rough outline

data Unsafe a = Error String | Ok a deriving (Show, Eq)

data EnvUnsafeLog env a = EnvUnsafeLog (env -> (Unsafe a, [String]))

-- function that just runs the function contained in EnvUnsafe
runEnvUnsafeLog :: (EnvUnsafeLog env a) -> env -> (Unsafe a, [String])
runEnvUnsafeLog (EnvUnsafeLog f) env = f env

-- a way to easily return an error (for instance in do notation)
err :: String -> EnvUnsafeLog env res
err s = EnvUnsafeLog $ \ _ -> (Error s, [])


-- a way to easily get the entire environment (for instance in do notation)
getEnv :: EnvUnsafeLog env env
getEnv = EnvUnsafeLog $ \ env -> (Ok env, [])


instance Functor (EnvUnsafeLog env) where
  -- fmap :: (a -> b) -> EnvUnsafeLog env res a -> EnvUnsafeLog env res b
  fmap f (EnvUnsafeLog eu) = EnvUnsafeLog (\e ->
                              case eu e of 
                                (Ok res, log) -> (Ok (f res), log)
                                (Error str, log)   -> (Error str, log))
  -- make sure your implementation follows the functor laws

--ignore this for now
instance Applicative (EnvUnsafeLog e) where
  pure = return
  (<*>) = ap

instance Monad (EnvUnsafeLog e) where
  --return :: a -> EnvUnsafe a
  return a = EnvUnsafeLog (\x -> (Ok a, []))

  --(>>=) :: EnvUnsafe a -> (a -> EnvUnsafe b) -> EnvUnsafe b
  (EnvUnsafeLog eu) >>= f = EnvUnsafeLog (\e ->
                                case eu e of
                                    (Ok a, log) -> case (runEnvUnsafeLog (f a) e) of
                                                      (result, log') -> (result, log ++ log')

                                    (Error str, log) -> (Error str, log))

