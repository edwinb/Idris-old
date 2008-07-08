> module Idris.Prover(doProof, runScript, doIvor) where

> import System.Console.Readline

> import Idris.AbsSyntax
> import Idris.Parser

> import Ivor.Shell
> import Ivor.TT

> import Debug.Trace

> doProof :: Ctxt IvorFun -> Context -> Id -> IO Context
> doProof raw ctxt nm = 
>     do ctxt' <- resume ctxt (toIvorName nm)
>        ctxt' <- attack defaultGoal ctxt'
>        putStrLn $ showCtxtState raw ctxt'
>        (ctxt', script, ok) <- proofShell (show nm ++ "> ") raw [] ctxt'
>        if ok then do
>            showScript nm script
>            return ctxt'
>          else do putStrLn "Proof abandoned"
>                  return ctxt

> runScript :: Monad m => Ctxt IvorFun -> Context -> Id -> [ITactic] -> 
>              m Context
> runScript raw ctxt nm tacs =
>     do ctxt <- resume ctxt (toIvorName nm)
>        ctxt <- attack defaultGoal ctxt
>        execScript raw ctxt tacs

This function assumes that it can plough on knowing the proof is fine.
If it isn't, it'll break, but with an error. We probably ought to check
properly, if only for useful diagnostics.

> execScript :: Monad m => Ctxt IvorFun -> Context -> [ITactic] -> m Context
> execScript raw ctxt [] = return ctxt
> execScript raw ctxt (t:ts) = do (ctxt,_,_) <- applyTac raw ctxt [] t
>                                 ctxt <- keepSolving defaultGoal ctxt
>                                 ctxt <- if ((numUnsolved ctxt) > 0)
>                                            then beta defaultGoal ctxt
>                                            else return ctxt
>                                 execScript raw ctxt ts

> showScript :: Id -> [String] -> IO ()
> showScript nm sc 
>    = do putStrLn $ (show nm) ++ " proof {"
>         putStr $ concat (map (\line -> "\t" ++ line ++ ";\n") sc)
>         putStrLn "};"

Remember the proof script (that's the [String]) and output it, without the
undone bits, after a Qed

> proofShell :: String -> Ctxt IvorFun -> [String] -> Context -> 
>               IO (Context, [String], Bool)
> proofShell prompt raw script ctxt = do
>     inp <- readline prompt
>     res <- case inp of
>              Nothing -> return ""
>              Just ":q" -> return "abandon"
>              Just tac -> do addHistory tac
>                             return tac
>     case parseTactic ("%"++res) of
>            Failure err f l -> do putStrLn err
>                                  proofShell prompt raw script ctxt
>            Success tac -> 
>                do let script' = script ++ ["%"++res]
>                   case applyTac raw ctxt script' tac of
>                     Failure err f l -> do putStrLn err
>                                           proofShell prompt raw script ctxt
>                     Success (ctxt, script, True) -> do
>                       ctxt <- keepSolving defaultGoal ctxt
>                       ctxt <- if ((numUnsolved ctxt) > 0)
>                                 then beta defaultGoal ctxt
>                                 else return ctxt
>                       if (proving ctxt)
>                          then do putStrLn $ showCtxtState raw ctxt
>                                  proofShell prompt raw script ctxt
>                          else return (ctxt, script, True)
>                     _ -> return (ctxt, script, False)

> applyTac :: Monad m => Ctxt IvorFun -> Context -> [String] -> ITactic -> 
>             m (Context, [String], Bool)
> applyTac raw ctxt script Abandon = return (ctxt, script, False)
> applyTac raw ctxt script Undo = do ctxt <- restore ctxt
>                                    -- remove the undo and the command
>                                    let script' = init (init script)
>                                    return (ctxt, script', True)
> applyTac raw ctxt script tac = do ctxt <- at (save ctxt) tac 
>                                   return (ctxt, script, True)
>   where
>     at ctxt (Intro []) = intros defaultGoal ctxt
>     at ctxt (Intro ns) = introsNames (map toIvorName ns) defaultGoal ctxt
>     at ctxt (Refine n) = refine (Name Unknown (toIvorName n)) defaultGoal ctxt
>     at ctxt ReflP = refine reflN defaultGoal ctxt
>     at ctxt (Fill t) = fill (ivor t) defaultGoal ctxt
>     at ctxt (Induction t) = induction (ivor t) defaultGoal ctxt
>     at ctxt (Rewrite f t) = replace eqN replN symN (ivor t) f
>                                defaultGoal ctxt
>     at ctxt Compute = compute defaultGoal ctxt
>     at ctxt (Unfold n) = unfold (toIvorName n) defaultGoal ctxt
>     at ctxt Qed = qed ctxt

>     ivor t = makeIvorTerm raw t
>     eqN = Name Unknown $ name "Eq"
>     replN = Name Unknown $ toIvorName (UN "__eq_repl")
>     symN = Name Unknown $ toIvorName (UN "__eq_sym")
>     reflN = Name Unknown $ name "refl"

> showCtxtState :: Ctxt IvorFun -> Context -> String
> showCtxtState raw ctxt
>     | not (proving ctxt) = ""
>     | null (getGoals ctxt) = "\nNo more goals\n"
>     | otherwise = let (g:gs) = getGoals ctxt in
>                      "\n" ++ showGoalState g ++
>                      "\nOther goals: " ++ show gs ++ "\n\n"
>  where
>    showTm t = showImp False (unIvor raw (view t))
>    showGoalState :: Goal -> String
>    showGoalState g = let (Just gd) = goalData ctxt True g
>                          env = bindings gd
>                          ty = goalType gd
>                          nm = goalName gd in
>                        showEnv (reverse env) ++ "\n" ++
>                        "--------------------------------\n" ++
>                        show nm ++ " ? " ++ showTm ty ++ "\n"
>    showEnv [] = ""
>    showEnv ((n,ty):xs) = show n ++ " : " ++ showTm ty ++ "\n" ++
>                          showEnv xs


> doIvor :: Context -> IO Context
> doIvor ctxt = do s <- runShell "Ivor> " (newShell ctxt)
>                  return (getContext s)
>                  