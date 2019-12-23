eval (Print x) = do x' <- eval x
--                     case (return x') of
--                         Ok x'' -> EnvUnsafe 
--                     EnvUnsafeLog (\env -> (x', [x']))