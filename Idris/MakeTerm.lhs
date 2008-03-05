> module Idris.MakeTerm where

> import Idris.AbsSyntax
> import Ivor.TT
> import Debug.Trace

> import Control.Monad

Work out how many implicit arguments we need, then translate our definition
into an ivor definition, with all the necessary placeholders added.

> makeIvorFun ::  Ctxt IvorFun -> Function -> IvorFun
> makeIvorFun ctxt (Function n ty clauses) 
>     = let (rty, imp) = addImpl ctxt ty
>           ity = makeIvorTerm ctxt rty
>           extCtxt = addEntry ctxt n (IvorFun undefined (Just ity) 
>                                              imp undefined)
>           pclauses = map (mkPat extCtxt imp) clauses in
>       IvorFun (toIvorName n) (Just ity) imp 
>                   (PattDef (Patterns pclauses))
>   where mkPat ectx imp (id,(RawClause lhs rhs)) 
>               = let lhs' = addPlaceholders ectx lhs in
>                     case (getFn lhs', getRawArgs lhs') of
>                          (fid, pats) ->
>                            let vpats = map toIvor pats
>                                vrhs = makeIvorTerm ectx rhs in
>                                PClause vpats vrhs

Convert a raw term to an ivor term, adding placeholders

> makeIvorTerm :: Ctxt IvorFun -> RawTerm -> ViewTerm
> makeIvorTerm ctxt tm = let expraw = addPlaceholders ctxt tm in
>                            toIvor expraw

> addPlaceholders :: Ctxt IvorFun -> RawTerm -> RawTerm
> addPlaceholders ctxt tm = ap [] tm
>     -- Count the number of args we've made explicit in an application
>     -- and don't add placeholders for them. Reset the counter if we get
>     -- out of an application
>     where ap ex (RVar n)
>               = case ctxtLookup ctxt n of
>                   Just (IvorFun _ (Just ty) imp _) -> 
>                     mkApp (RVar n) 
>                               (mkImplicitArgs 
>                                (map fst (fst (getBinders ty []))) imp ex)
>                   _ -> RVar n
>           ap ex (RAppImp n f a) = (ap ((toIvorName n,(ap [] a)):ex) f)
>           ap ex (RApp f a) = (RApp (ap ex f) (ap [] a))
>           ap ex (RBind n (Pi p ty) sc)
>               = RBind n (Pi p (ap [] ty)) (ap [] sc)
>           ap ex (RBind n (Lam ty) sc)
>               = RBind n (Lam (ap [] ty)) (ap [] sc)
>           ap ex (RBind n (RLet val ty) sc)
>               = RBind n (RLet (ap [] val) (ap [] ty)) (ap [] sc)
>           ap ex r = r

Go through the arguments; if an implicit argument has the same name as one
in our list of explicit names to add, add it.

> mkImplicitArgs :: [Name] -> Int -> [(Name, RawTerm)] -> [RawTerm]
> mkImplicitArgs _ 0 _ = [] -- No more implicit
> mkImplicitArgs [] i ns = [] -- No more args
> mkImplicitArgs (n:ns) i imps
>      = case lookup n imps of
>          Nothing -> RPlaceholder:(mkImplicitArgs ns (i-1) imps)
>          Just v -> v:(mkImplicitArgs ns (i-1) imps)

> makeIvorFuns :: [Decl] -> Ctxt IvorFun
> makeIvorFuns defs = mif newCtxt defs

> mif :: Ctxt IvorFun -> [Decl] -> Ctxt IvorFun
> mif acc [] = acc
> mif acc ((Fun f):ds) = let fn = makeIvorFun acc f in
>                            mif (addEntry acc (funId f) fn) ds
> mif acc ((Fwd n ty):ds) 
>         = let (rty, imp) = addImpl acc ty
>               ity = makeIvorTerm acc rty in
>               mif (addEntry acc n (IvorFun (toIvorName n) (Just ity) 
>                                             imp Later)) ds
> mif acc ((DataDecl d):ds) = addDataEntries acc d ds -- will call mif on ds
> mif acc ((TermDef n tm):ds) 
>         = let (itmraw, imp) = addImpl acc tm
>               itm = makeIvorTerm acc itmraw in
>               mif (addEntry acc n 
>                      (IvorFun (toIvorName n) Nothing imp (SimpleDef itm))) ds

Add an entry for the type id and for each of the constructors.

> addDataEntries :: Ctxt IvorFun -> Datatype -> [Decl] -> Ctxt IvorFun
> addDataEntries acc (Datatype tid tty cons) ds = 
>     let (tyraw, imp) = addImpl acc tty
>         tytm = makeIvorTerm acc tyraw
>         acctmp = addEntry acc tid (IvorFun (toIvorName tid) (Just tytm) imp 
>                                   undefined)
>         ddef = makeInductive acctmp tid (getBinders tytm []) cons []
>         acc' = addEntry acc tid (IvorFun (toIvorName tid) (Just tytm) imp 
>                                  (DataDef ddef)) in
>         addConEntries acc' cons ds

> getBinders (Forall n ty sc) acc = (getBinders sc ((n,ty):acc))
> getBinders sc acc = (reverse acc, sc)

     Inductive (toIvorName tid) [] 

> makeInductive :: Ctxt IvorFun -> Id -> ([(Name, ViewTerm)], ViewTerm) ->
>                  [(Id,RawTerm)] -> [(Name, ViewTerm)] -> Inductive
> makeInductive ctxt tid (indices, tty) [] acc
>        = Inductive (toIvorName tid) [] indices tty (reverse acc)
> makeInductive ctxt cdec indices ((cid, cty):cs) acc
>        = let (tyraw, imp) = addImpl ctxt cty
>              tytm = makeIvorTerm ctxt tyraw in
>              makeInductive ctxt cdec
>                            indices cs (((toIvorName cid),tytm):acc)

> addConEntries :: Ctxt IvorFun -> [(Id,RawTerm)] -> [Decl] -> Ctxt IvorFun
> addConEntries acc [] ds = mif acc ds
> addConEntries acc ((cid, ty):cs) ds 
>     = let (tyraw, imp) = addImpl acc ty
>           tytm = makeIvorTerm acc tyraw
>           acc' = addEntry acc cid (IvorFun (toIvorName cid) (Just tytm) imp IDataCon) in
>           addConEntries acc' cs ds

> addIvor :: Monad m => 
>             Ctxt IvorFun -> Context -> m Context
> addIvor defs ctxt = foldM addIvorDef ctxt (reverse (ctxtAlist defs))

> addIvorDef :: Monad m =>
>                Context -> (Id, IvorFun) -> m Context
> addIvorDef ctxt (n,IvorFun name tyin _ def) 
>     = trace (show (tyin,def)) $ case def of
>         PattDef ps -> addPatternDef ctxt name (unjust tyin) ps [Partial,GenRec] -- just allow general recursion for now
>         SimpleDef tm -> case tyin of
>                           Nothing -> addDef ctxt name tm
>                           Just ty -> addTypedDef ctxt name tm ty
>         DataDef ind -> addData ctxt ind
>         Later -> case tyin of
>                    Just ty -> declare ctxt name ty
>                    Nothing -> fail $ "No type given for forward declared " ++ show n
>         _ -> return ctxt
>    where unjust (Just x) = x