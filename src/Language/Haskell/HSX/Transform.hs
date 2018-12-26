-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.HSX.Tranform
-- Copyright   :  (c) Niklas Broberg 2004-2012
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, niklas.broberg@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for transforming abstract Haskell code extended with regular
-- patterns into semantically equivalent normal abstract Haskell code. In
-- other words, we transform away regular patterns.
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Language.Haskell.HSX.Transform (
      transform       -- :: HsModule -> HsModule
    , transformExp
    ) where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Build
import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad       (ap)
import Data.List (union)

import Debug.Trace (trace)

-----------------------------------------------------------------------------
-- A monad for threading a boolean value through the boilerplate code,
-- to signal whether a transformation has taken place or not.

newtype HsxM a = MkHsxM (HsxState -> (a, HsxState))

instance Applicative HsxM where
  pure  = return
  (<*>) = ap

instance Monad HsxM where
 return x = MkHsxM (\s -> (x,s))
 (MkHsxM f) >>= k = MkHsxM (\s -> let (a, s') = f s
                                      (MkHsxM f') = k a
                                   in f' s')

getHsxState :: HsxM HsxState
getHsxState = MkHsxM (\s -> (s, s))

setHsxState :: HsxState -> HsxM ()
setHsxState s = MkHsxM (\_ -> ((),s))

instance Functor HsxM where
 fmap f hma = do a <- hma
                 return $ f a

-----

type HsxState = (Bool, Bool)

initHsxState :: HsxState
initHsxState = (False, False)

setHarpTransformed :: HsxM ()
setHarpTransformed =
    do (_,x) <- getHsxState
       setHsxState (True,x)

setXmlTransformed :: HsxM ()
setXmlTransformed =
    do (h,_) <- getHsxState
       setHsxState (h,True)

runHsxM :: HsxM a -> (a, (Bool, Bool))
runHsxM (MkHsxM f) = f initHsxState

-----------------------------------------------------------------------------
-- Traversing and transforming the syntax tree


-- | Transform away occurences of regular patterns from an abstract
-- Haskell module, preserving semantics.
transform :: Module () -> Module ()
transform (Module l m pragmas is decls) =
    let (decls', (harp, hsx)) = runHsxM $ mapM transformDecl decls
        -- We may need to add an import for Match.hs that defines the matcher monad
        imps1 = if harp
             then (:) $ ImportDecl () match_mod True False False Nothing
                            (Just match_qual_mod)
                            Nothing
             else id
        imps2 = {- if hsx
                 then (:) $ ImportDecl s hsx_data_mod False
                         Nothing
                         Nothing
                 else -} id     -- we no longer want to import HSP.Data
     in Module l m pragmas (imps1 $ imps2 is) decls'

-----------------------------------------------------------------------------
-- Declarations

-- | Transform a declaration by transforming subterms that could
-- contain regular patterns.
transformDecl :: Decl () -> HsxM (Decl ())
transformDecl d = case d of
    -- Pattern binds can contain regular patterns in the pattern being bound
    -- as well as on the right-hand side and in declarations in a where clause
    PatBind l pat rhs decls -> do
        -- Preserve semantics of irrefutable regular patterns by postponing
        -- their evaluation to a let-expression on the right-hand side
        let ([pat'], rnpss) = unzip $ renameIrrPats [pat]
        -- Transform the pattern itself
        (patts, attrGuards, guards, decls'') <- transformPatterns [pat']
        let pat'' = case patts of
              [p] -> p
              _   -> error $ "transformDecl: expecting exactly one pattern but got: " ++ show patts
        -- Transform the right-hand side, and add any generated guards
        -- and let expressions to it
        rhs' <- mkRhs (attrGuards ++ guards) (concat rnpss) rhs
        -- Transform declarations in the where clause, adding any generated
        -- declarations to it
        decls' <- case decls of
               Nothing -> return Nothing
               Just (BDecls l ds)
                         -> do ds' <- transformLetDecls ds
                               return $ Just $ BDecls l $ decls'' ++ ds'
               _           -> error "Cannot bind implicit parameters in the \
                        \ \'where\' clause of a function using regular patterns."
        return $ PatBind l pat'' rhs' decls'

    -- Function binds can contain regular patterns in their matches
    FunBind l ms -> fmap (FunBind l) $ mapM transformMatch ms
    -- Instance declarations can contain regular patterns in the
    -- declarations of functions inside it
    InstDecl l mo irule Nothing -> pure d
    InstDecl l mo irule (Just idecls) ->
      fmap (InstDecl l mo irule . Just) $ mapM transformInstDecl idecls
    -- Class declarations can contain regular patterns in the
    -- declarations of automatically instantiated functions
    ClassDecl l c dh fd Nothing -> pure d
    ClassDecl l c dh fd (Just cdecls) ->
        fmap (ClassDecl l c dh fd . Just) $ mapM transformClassDecl cdecls
    -- TH splices are expressions and can contain regular patterns
    SpliceDecl l e ->
        fmap (SpliceDecl l) $ transformExpM e
    -- Type signatures, type, newtype or data declarations, infix declarations,
    -- type and data families and instances, foreign imports and exports,
    -- and default declarations; none can contain regular patterns.
    -- Note that we don't transform inside rules pragmas!
    _ -> return d

transformInstDecl :: InstDecl () -> HsxM (InstDecl ())
transformInstDecl d = case d of
    InsDecl l decl -> fmap (InsDecl l) $ transformDecl decl
    _ -> return d


transformClassDecl :: ClassDecl () -> HsxM (ClassDecl ())
transformClassDecl d = case d of
    ClsDecl l decl -> fmap (ClsDecl l) $ transformDecl decl
    _ -> return d

-- | Transform a function "match" by generating pattern guards and
-- declarations representing regular patterns in the argument list.
-- Subterms, such as guards and the right-hand side, are also traversed
-- transformed.
transformMatch :: Match () -> HsxM (Match ())
transformMatch (Match l name pats rhs decls) = do
    -- Preserve semantics of irrefutable regular patterns by postponing
    -- their evaluation to a let-expression on the right-hand side
    let (pats', rnpss) = unzip $ renameIrrPats pats
    -- Transform the patterns that stand as arguments to the function
    (pats'', attrGuards, guards, decls'') <- transformPatterns pats'
    -- Transform the right-hand side, and add any generated guards
    -- and let expressions to it
    rhs' <- mkRhs (attrGuards ++ guards) (concat rnpss) rhs
    -- Transform declarations in the where clause, adding any generated
    -- declarations to it
    decls' <- case decls of
           Nothing -> return Nothing
           Just (BDecls l ds)
                     -> do ds' <- transformLetDecls ds
                           return $ Just $ BDecls l $ decls'' ++ ds'
           _           -> error "Cannot bind implicit parameters in the \
                     \ \'where\' clause of a function using regular patterns."

    return $ Match l name pats'' rhs' decls'


-- | Transform and update guards and right-hand side of a function or
-- pattern binding. The supplied list of guards is prepended to the
-- original guards, and subterms are traversed and transformed.
mkRhs :: [Guard ()] -> [(Name (), Pat ())] -> Rhs () -> HsxM (Rhs ())
mkRhs guards rnps (UnGuardedRhs l rhs) = do
    -- Add the postponed patterns to the right-hand side by placing
    -- them in a let-expression to make them lazily evaluated.
    -- Then transform the whole right-hand side as an expression.
    rhs' <- transformExpM $ addLetDecls rnps rhs
    case guards of
     -- There were no guards before, and none should be added,
     -- so we still have an unguarded right-hand side
     [] -> return $ UnGuardedRhs l rhs'
     -- There are guards to add. These should be added as pattern
     -- guards, i.e. as statements.
     _  -> return $ GuardedRhss l [GuardedRhs l (map mkStmtGuard guards) rhs']
mkRhs guards rnps (GuardedRhss l gdrhss) = fmap (GuardedRhss l) $ mapM (mkGRhs guards rnps) gdrhss
  where mkGRhs :: [Guard ()] -> [(Name (), Pat ())] -> GuardedRhs () -> HsxM (GuardedRhs ())
        mkGRhs gs rnps (GuardedRhs l oldgs rhs) = do
            -- Add the postponed patterns to the right-hand side by placing
            -- them in a let-expression to make them lazily evaluated.
            -- Then transform the whole right-hand side as an expression.
            rhs' <- transformExpM $ addLetDecls rnps rhs
            -- Now there are guards, so first we need to transform those
            oldgs' <- fmap concat $ mapM (transformStmt GuardStmt) oldgs
            -- ... and then prepend the newly generated ones, as statements
            return $ GuardedRhs l ((map mkStmtGuard gs) ++ oldgs') rhs'

-- | Place declarations of postponed regular patterns in a let-expression to
-- make them lazy, in order to make them behave as irrefutable patterns.
addLetDecls :: [(Name (), Pat ())] -> Exp () -> Exp ()
addLetDecls []   e = e    -- no declarations to add
addLetDecls rnps e =
    -- Place all postponed patterns in the same let-expression
    letE (map mkDecl rnps) e

-- | Make pattern binds from postponed regular patterns
mkDecl :: (Name (), Pat ()) -> Decl ()
mkDecl (n,p) = patBind p (var n)

------------------------------------------------------------------------------------
-- Expressions

-- | Transform expressions by traversing subterms.
-- Of special interest are expressions that contain patterns as subterms,
-- i.e. @let@, @case@ and lambda expressions, and also list comprehensions
-- and @do@-expressions. All other expressions simply transform their
-- sub-expressions, if any.
-- Of special interest are of course also any xml expressions.
transformExp :: Exp () -> Exp ()
transformExp e =
    let (e', _) = runHsxM $ transformExpM e
    in e'

-- | Transform expressions by traversing subterms.
-- Of special interest are expressions that contain patterns as subterms,
-- i.e. @let@, @case@ and lambda expressions, and also list comprehensions
-- and @do@-expressions. All other expressions simply transform their
-- sub-expressions, if any.
-- Of special interest are of course also any xml expressions.
transformExpM :: Exp () -> HsxM (Exp ())
transformExpM e = case e of
    -- A standard xml tag should be transformed into an element of the
    -- XML datatype. Attributes should be made into a set of mappings,
    -- and children should be transformed.
    XTag _ name attrs mattr cs -> do
        -- Hey Pluto, look, we have XML in our syntax tree!
        setXmlTransformed
        let -- ... make tuples of the attributes
            as = map mkAttr attrs
        -- ... transform the children
        cs' <- mapM transformChild cs
        -- ... and lift the values into the XML datatype.
        return $ paren $ metaGenElement name as mattr cs'

    -- An empty xml tag should be transformed just as a standard tag,
    -- only that there are no children,
    XETag _ name attrs mattr -> do
        -- ... 'tis the season to be jolly, falalalalaaaa....
        setXmlTransformed
        let -- ... make tuples of the attributes
            as = map mkAttr attrs
            -- ... and lift the values into the XML datatype.
        return $ paren $ metaGenEElement name as mattr

    -- A child tag should be transformed into an application
    -- of asChild to a list of children.
    XChildTag _ cs  -> do
        -- After all, it IS christmas!
        setXmlTransformed
        -- ... transform the children
        cs' <- mapM transformChild cs
        -- ... and make them into a list
        return $ paren $ metaAsChild $ listE cs'

    -- PCDATA should be lifted as a string into the XML datatype.
    XPcdata _ pcdata    -> do setXmlTransformed
                              return $ metaFromStringLit $ strE pcdata
--                            return $ ExpTypeSig noLoc (strE pcdata) (TyCon (UnQual (Ident "Text")))
    -- Escaped expressions should be treated as just expressions.
    XExpTag _ e     -> do setXmlTransformed
                          e' <- transformExpM e
                          return $ paren $ metaAsChild e'

    -- Patterns as arguments to a lambda expression could be regular,
    -- but we cannot put the evaluation here since a lambda expression
    -- can have neither guards nor a where clause. Thus we must postpone
    -- them to a case expressions on the right-hand side.
    Lambda l pats rhs -> do
        let -- First rename regular patterns
            (ps, rnpss)  = unzip $ renameRPats pats
            -- ... group them up to one big tuple
            (rns, rps) = unzip (concat rnpss)
            alt1 = alt (pTuple rps) rhs
            texp = varTuple rns
            -- ... and put it all in a case expression, which
            -- can then be transformed in the normal way.
            e = if null rns then rhs else caseE texp [alt1]
        rhs' <- transformExpM e
        return $ Lambda l ps rhs'
    -- A let expression can contain regular patterns in the declarations,
    -- or in the expression that makes up the body of the let.
    Let _ (BDecls _ ds) e -> do
        -- Declarations appearing in a let expression must be transformed
        -- in a special way due to scoping, see later documentation.
        -- The body is transformed as a normal expression.
        ds' <- transformLetDecls ds
        e'  <- transformExpM e
        return $ letE ds' e'
    -- Bindings of implicit parameters can appear either in ordinary let
    -- expressions (GHC), in dlet expressions (Hugs) or in a with clause
    -- (both). Such bindings are transformed in a special way. The body
    -- is transformed as a normal expression in all cases.
    Let l (IPBinds l' is) e -> do
        is' <- mapM transformIPBind is
        e'  <- transformExpM e
        return $ Let l (IPBinds l' is') e'
    -- A case expression can contain regular patterns in the expression
    -- that is the subject of the casing, or in either of the alternatives.
    Case l e alts -> do
        e'    <- transformExpM e
        alts' <- mapM transformAlt alts
        return $ Case l e' alts'
    -- A do expression can contain regular patterns in its statements.
    Do l stmts -> do
        stmts' <- fmap concat $ mapM (transformStmt DoStmt) stmts
        return $ Do l stmts'
    MDo l stmts -> do
        stmts' <- fmap concat $ mapM (transformStmt DoStmt) stmts
        return $ MDo l stmts'
    -- A list comprehension can contain regular patterns in the result
    -- expression, or in any of its statements.
    ListComp l e stmts  -> do
        e'     <- transformExpM e
        stmts' <- fmap concat $ mapM transformQualStmt stmts
        return $ ListComp l e' stmts'
    ParComp l e stmtss  -> do
        e'      <- transformExpM e
        stmtss' <- fmap (map concat) $ mapM (mapM transformQualStmt) stmtss
        return $ ParComp l e' stmtss'
    Proc l pat rhs          -> do
        let -- First rename regular patterns
            ([p], [rnps])  = unzip $ renameRPats [pat]
            -- ... group them up to one big tuple
            (rns, rps) = unzip rnps
            alt1 = alt (pTuple rps) rhs
            texp = varTuple rns
            -- ... and put it all in a case expression, which
            -- can then be transformed in the normal way.
            e = if null rns then rhs else caseE texp [alt1]
        rhs' <- transformExpM e
        return $ Proc l p rhs'

    -- All other expressions simply transform their immediate subterms.
    InfixApp l e1 op e2 -> transform2exp e1 e2
                                (\e1 e2 -> InfixApp l e1 op e2)
    App l e1 e2         -> transform2exp e1 e2 (App l)
    NegApp l e          -> fmap (NegApp l) $ transformExpM e
    If l e1 e2 e3       -> transform3exp e1 e2 e3 (If l)
    Tuple l bx es       -> fmap (Tuple l bx) $ mapM transformExpM es
    List l es           -> fmap (List l) $ mapM transformExpM es
    Paren l e           -> fmap (Paren l) $ transformExpM e
    LeftSection l e op  -> do e' <- transformExpM e
                              return $ LeftSection l e' op
    RightSection l op e -> fmap (RightSection l op) $ transformExpM e
    RecConstr l n fus   -> fmap (RecConstr l n) $ mapM transformFieldUpdate fus
    RecUpdate l e fus   -> do e'   <- transformExpM e
                              fus' <- mapM transformFieldUpdate fus
                              return $ RecUpdate l e' fus'
    EnumFrom l e        -> fmap (EnumFrom l) $ transformExpM e
    EnumFromTo l e1 e2  -> transform2exp e1 e2 (EnumFromTo l)
    EnumFromThen l e1 e2      -> transform2exp e1 e2 (EnumFromThen l)
    EnumFromThenTo l e1 e2 e3 -> transform3exp e1 e2 e3 (EnumFromThenTo l)
    ExpTypeSig l e t  -> do e' <- transformExpM e
                            return $ ExpTypeSig l e' t
    SpliceExp l s       -> fmap (SpliceExp l) $ transformSplice s
    LeftArrApp l e1 e2        -> transform2exp e1 e2 (LeftArrApp l)
    RightArrApp l e1 e2       -> transform2exp e1 e2 (RightArrApp l)
    LeftArrHighApp l e1 e2    -> transform2exp e1 e2 (LeftArrHighApp l)
    RightArrHighApp l e1 e2   -> transform2exp e1 e2 (RightArrHighApp l)
    CorePragma l s e      -> fmap (CorePragma l s) $ transformExpM e
    SCCPragma  l s e      -> fmap (SCCPragma  l s) $ transformExpM e
    GenPragma  l s a b e  -> fmap (GenPragma  l s a b) $ transformExpM e
    _           -> return e     -- Warning - will not work inside TH brackets!
  where
    -- | Transform expressions appearing in child position of an xml tag.
    -- Expressions are first transformed, then wrapped in a call to
    -- @toXml@.
    transformChild :: Exp () -> HsxM (Exp ())
    transformChild e = do
        -- Transform the expression
        te <- transformExpM e
        -- ... and apply the overloaded toXMLs to it
        return $ metaAsChild te

transformFieldUpdate :: FieldUpdate () -> HsxM (FieldUpdate ())
transformFieldUpdate (FieldUpdate l n e) =
        fmap (FieldUpdate l n) $ transformExpM e
transformFieldUpdate fup = return fup

transformSplice :: Splice () -> HsxM (Splice ())
transformSplice s = case s of
    ParenSplice l e     -> fmap (ParenSplice l) $ transformExpM e
    _                   -> return s

transform2exp :: Exp () -> Exp () -> (Exp () -> Exp () -> a) -> HsxM a
transform2exp e1 e2 f = do e1' <- transformExpM e1
                           e2' <- transformExpM e2
                           return $ f e1' e2'

transform3exp :: Exp () -> Exp () -> Exp () -> (Exp () -> Exp () -> Exp () -> a) -> HsxM a
transform3exp e1 e2 e3 f = do e1' <- transformExpM e1
                              e2' <- transformExpM e2
                              e3' <- transformExpM e3
                              return $ f e1' e2' e3'

mkAttr :: XAttr () -> Exp ()
mkAttr (XAttr _ name e) =
    paren (metaMkName name `metaAssign` (textTypeSig e))
    where
      textTypeSig e@(Lit _ (String _ _ _)) = metaFromStringLit e
--      textTypeSig e@(Lit (String _)) = ExpTypeSig noLoc e (TyCon (UnQual (Ident "Text")))
      textTypeSig e                  = e

-- | Transform pattern bind declarations inside a @let@-expression by transforming
-- subterms that could appear as regular patterns, as well as transforming the bound
-- pattern itself. The reason we need to do this in a special way is scoping, i.e.
-- in the expression @let a | Just b <- match a = list in b@ the variable b will not
-- be in scope after the @in@. And besides, we would be on thin ice even if it was in
-- scope since we are referring to the pattern being bound in the guard that will
-- decide if the pattern will be bound... yikes, why does Haskell allow guards on
-- pattern binds to refer to the patterns being bound, could that ever lead to anything
-- but an infinite loop??
transformLetDecls :: [Decl ()] -> HsxM [Decl ()]
transformLetDecls ds = do
    -- We need to rename regular patterns in pattern bindings, since we need to
    -- separate the generated declaration sets. This since we need to add them not
    -- to the actual binding but rather to the declaration that will be the guard
    -- of the binding.
    let ds' = renameLetDecls ds
    transformLDs 0 0 ds'
  where transformLDs :: Int -> Int -> [Decl ()] -> HsxM [Decl ()]
        transformLDs k l ds = case ds of
            []     -> return []
            (d:ds) -> case d of
                PatBind l'' pat rhs decls -> do
                    -- We need to transform all pattern bindings in a set of
                    -- declarations in the same context w.r.t. generating fresh
                    -- variable names, since they will all be in scope at the same time.
                    ([pat'], ags, gs, ws, k', l') <- runTrFromTo k l (trPatterns [pat])
                    decls' <- case decls of
                        -- Any declarations already in place should be left where they
                        -- are since they probably refer to the generating right-hand
                        -- side of the pattern bind. If they don't, we're in trouble...
                        Nothing -> return Nothing
                        Just (BDecls l'' decls) -> fmap (Just . BDecls l'') $ transformLetDecls decls
                        -- If they are implicit parameter bindings we simply transform
                        -- them as such.
                        Just (IPBinds l'' decls) -> fmap (Just . IPBinds l'') $ mapM transformIPBind decls
                    -- The generated guard, if any, should be a declaration, and the
                    -- generated declarations should be associated with it.
                    let gs' = case gs of
                           []  -> []
                           [g] -> [mkDeclGuard g ws]
                           _   -> error "This should not happen since we have called renameLetDecls already!"
                        -- Generated attribute guards should also be added as declarations,
                        -- but with no where clauses.
                        ags' = map (flip mkDeclGuard $ []) ags
                    -- We must transform the right-hand side as well, but there are
                    -- no new guards, nor any postponed patterns, to supply at this time.
                    rhs' <- mkRhs [] [] rhs
                    -- ... and then we should recurse with the new gensym argument.
                    ds' <- transformLDs k' l' ds
                    -- The generated guards, which should be at most one, should be
                    -- added as declarations rather than as guards due to the
                    -- scoping issue described above.
                    return $ (PatBind l'' pat' rhs' decls') : ags' ++ gs' ++ ds'

                    -- We only need to treat pattern binds separately, other declarations
                    -- can be transformed normally.
                d -> do d'  <- transformDecl d
                        ds' <- transformLDs k l ds
                        return $ d':ds'



-- | Transform binding of implicit parameters by transforming the expression on the
-- right-hand side. The left-hand side can only be an implicit parameter, so no
-- regular patterns there...
transformIPBind :: IPBind () -> HsxM (IPBind ())
transformIPBind (IPBind l n e) =
    fmap (IPBind l n) $ transformExpM e

------------------------------------------------------------------------------------
-- Statements of various kinds

-- | A simple annotation datatype for statement contexts.
data StmtType = DoStmt | GuardStmt | ListCompStmt

-- | Transform statements by traversing and transforming subterms.
-- Since generator statements have slightly different semantics
-- depending on their context, statements are annotated with their
-- context to ensure that the semantics of the resulting statement
-- sequence is correct. The return type is a list since generated
-- guards will be added as statements on the same level as the
-- statement to be transformed.
transformStmt :: StmtType -> Stmt () -> HsxM [Stmt ()]
transformStmt t s = case s of
    -- Generators can have regular patterns in the result pattern on the
    -- left-hand side and in the generating expression.
    Generator s p e -> do
        let -- We need to treat generated guards differently depending
            -- on the context of the statement.
            guardFun = case t of
                DoStmt       -> monadify
                ListCompStmt -> monadify
                GuardStmt    -> mkStmtGuard
            -- Preserve semantics of irrefutable regular patterns by postponing
            -- their evaluation to a let-expression on the right-hand side
            ([p'], rnpss) = unzip $ renameIrrPats [p]
        -- Transform the pattern itself
        ([p''], ags, gs, ds) <- transformPatterns [p']
        -- Put the generated declarations in a let-statement
        let lt  = case ds of
               [] -> []
               _  -> [letStmt ds]
            -- Perform the designated trick on the generated guards.
            gs' = map guardFun (ags ++ gs)
        -- Add the postponed patterns to the right-hand side by placing
        -- them in a let-expression to make them lazily evaluated.
        -- Then transform the whole right-hand side as an expression.
        e' <- transformExpM $ addLetDecls (concat rnpss) e
        return $ Generator s p'' e':lt ++ gs'
      where monadify :: Guard () -> Stmt ()
            -- To monadify is to create a statement guard, only that the
            -- generation must take place in a monad, so we need to "return"
            -- the value gotten from the guard.
            monadify (p,e) = genStmt p (metaReturn $ paren e)
    -- Qualifiers are simply wrapped expressions and are treated as such.
    Qualifier l e -> fmap (\e -> [Qualifier l $ e]) $ transformExpM e
    -- Let statements suffer from the same problem as let expressions, so
    -- the declarations should be treated in the same special way.
    LetStmt _ (BDecls _ ds)  ->
        fmap (\ds -> [letStmt ds]) $ transformLetDecls ds
    -- If the bindings are of implicit parameters we simply transform them as such.
    LetStmt l (IPBinds l' is) ->
        fmap (\is -> [LetStmt l (IPBinds l' is)]) $ mapM transformIPBind is
    RecStmt l stmts   ->
        fmap (return . RecStmt l . concat) $ mapM (transformStmt t) stmts

transformQualStmt :: QualStmt () -> HsxM [QualStmt ()]
transformQualStmt qs = case qs of
    -- For qual statments in list comprehensions we just pass on the baton
    QualStmt     l s      -> fmap (map (QualStmt l)) $ transformStmt ListCompStmt s
    ThenTrans    l e      -> fmap (return . ThenTrans l) $ transformExpM e
    ThenBy       l e f    -> fmap return $ transform2exp e f (ThenBy l)
    GroupBy      l e      -> fmap (return . GroupBy l) $ transformExpM e
    GroupUsing   l f      -> fmap (return . GroupUsing l) $ transformExpM f
    GroupByUsing l e f    -> fmap return $ transform2exp e f (GroupByUsing l)

------------------------------------------------------------------------------------------
-- Case alternatives

-- | Transform alternatives in a @case@-expression. Patterns are
-- transformed, while other subterms are traversed further.
transformAlt :: Alt () -> HsxM (Alt ())
transformAlt (Alt l pat rhs decls) = do
    -- Preserve semantics of irrefutable regular patterns by postponing
    -- their evaluation to a let-expression on the right-hand side
    let ([pat'], rnpss) = unzip $ renameIrrPats [pat]
    -- Transform the pattern itself
    ([pat''], attrGuards, guards, decls'') <- transformPatterns [pat']
    -- Transform the right-hand side, and add any generated guards
    -- and let expressions to it.
    rhs' <- mkRhs (attrGuards ++ guards) (concat rnpss) rhs
    -- Transform declarations in the where clause, adding any generated
    -- declarations to it.
    decls' <- case decls of
           Nothing -> return Nothing
           Just (BDecls l' ds) -> do ds' <- mapM transformDecl ds
                                     return $ Just $ BDecls l' $ decls'' ++ ds
           _           -> error "Cannot bind implicit parameters in the \
                     \ \'where\' clause of a function using regular patterns."

    return $ Alt l pat'' rhs' decls'

----------------------------------------------------------------------------------
-- Guards

-- In some places, a guard will be a declaration instead of the
-- normal statement, so we represent it in a generic fashion.
type Guard l = (Pat l, Exp l)

mkStmtGuard :: Guard () -> Stmt ()
mkStmtGuard (p, e) = genStmt p e

mkDeclGuard :: Guard () -> [Decl ()] -> Decl ()
mkDeclGuard (p, e) ds = patBindWhere p e ds

----------------------------------------------------------------------------------
-- Rewriting expressions before transformation.
-- Done in a monad for gensym capability.

newtype RN a = RN (RNState -> (a, RNState))

type RNState = Int

initRNState = 0

instance Applicative RN where
  pure  = return
  (<*>) = ap

instance Monad RN where
 return a = RN $ \s -> (a,s)
 (RN f) >>= k = RN $ \s -> let (a,s') = f s
                               (RN g) = k a
                            in g s'

instance Functor RN where
 fmap f rna = do a <- rna
                 return $ f a


runRename :: RN a -> a
runRename (RN f) = let (a,_) = f initRNState
                    in a

getRNState :: RN RNState
getRNState = RN $ \s -> (s,s)

setRNState :: RNState -> RN ()
setRNState s = RN $ \_ -> ((), s)

genVarName :: RN (Name ())
genVarName = do
    k <- getRNState
    setRNState $ k+1
    return $ name $ "harp_rnvar" ++ show k


type NameBind l = (Name l, Pat l)

-- Some generic functions on monads for traversing subterms

rename1pat :: a -> (b -> c) -> (a -> RN (b, [d])) -> RN (c, [d])
rename1pat p f rn = do (q, ms) <- rn p
                       return (f q, ms)

rename2pat :: a -> a -> (b -> b -> c) -> (a -> RN (b, [d])) -> RN (c, [d])
rename2pat p1 p2 f rn = do (q1, ms1) <- rn p1
                           (q2, ms2) <- rn p2
                           return $ (f q1 q2, ms1 ++ ms2)

renameNpat :: [a] -> ([b] -> c) -> (a -> RN (b, [d])) -> RN (c, [d])
renameNpat ps f rn = do (qs, mss) <- fmap unzip $ mapM rn ps
                        return (f qs, concat mss)




-- | Generate variables as placeholders for any regular patterns, in order
-- to place their evaluation elsewhere. We must likewise move the evaluation
-- of Tags because attribute lookups are force evaluation.
renameRPats :: [Pat ()] -> [(Pat (), [NameBind ()])]
renameRPats ps = runRename $ mapM renameRP ps

renameRP :: Pat () -> RN (Pat (), [NameBind ()])
renameRP p = case p of
    -- We must rename regular patterns and Tag expressions
    PRPat _ _           -> rename p
    PXTag _ _ _ _ _   -> rename p
    PXETag _ _ _ _    -> rename p
    -- The rest of the rules simply try to rename regular patterns in
    -- their immediate subpatterns.
    PInfixApp l p1 n p2 -> rename2pat p1 p2
                                (\p1 p2 -> PInfixApp l p1 n p2)
                                renameRP
    PApp l n ps         -> renameNpat ps (PApp l n) renameRP
    PTuple l bx ps      -> renameNpat ps (PTuple l bx) renameRP
    PList l ps          -> renameNpat ps (PList l) renameRP
    PParen l p          -> rename1pat p (PParen l) renameRP
    PRec l n pfs        -> renameNpat pfs (PRec l n) renameRPf
    PAsPat l n p        -> rename1pat p (PAsPat l n) renameRP
    PIrrPat l p         -> rename1pat p (PIrrPat l) renameRP
    PXPatTag l p        -> rename1pat p (PXPatTag l) renameRP
    PatTypeSig l p t  -> rename1pat p (\p -> PatTypeSig l p t) renameRP
    _                   -> return (p, [])
  where renameRPf :: PatField () -> RN (PatField (), [NameBind ()])
        renameRPf (PFieldPat l n p) = rename1pat p (PFieldPat l n) renameRP
        renameRPf pf              = return (pf, [])

        renameAttr :: PXAttr () -> RN (PXAttr (), [NameBind ()])
        renameAttr (PXAttr l s p) = rename1pat p (PXAttr l s) renameRP

        rename :: Pat () -> RN (Pat (), [NameBind ()])
        rename p = do -- Generate a fresh variable
              n <- genVarName
              -- ... and return that, along with the association of
              -- the variable with the old pattern
              return (pvar n, [(n,p)])

-- | Rename declarations appearing in @let@s or @where@ clauses.
renameLetDecls :: [Decl ()] -> [Decl ()]
renameLetDecls ds =
    let -- Rename all regular patterns bound in pattern bindings.
        (ds', smss) = unzip $ runRename $ mapM renameLetDecl ds
        -- ... and then generate declarations for the associations
        gs = map (\(n,p) -> mkDecl (n,p)) (concat smss)
        -- ... which should be added to the original list of declarations.
     in ds' ++ gs

  where renameLetDecl :: Decl () -> RN (Decl (), [(Name (), Pat ())])
        renameLetDecl d = case d of
            -- We need only bother about pattern bindings.
            PatBind l pat rhs decls -> do
                -- Rename any regular patterns that appear in the
                -- pattern being bound.
                (p, ms) <- renameRP pat
                let sms = map (\(n,p) -> (n, p)) ms
                return $ (PatBind l p rhs decls, sms)
            _ -> return (d, [])

-- | Move irrefutable regular patterns into a @let@-expression instead,
-- to make sure that the semantics of @~@ are preserved.
renameIrrPats :: [Pat ()] -> [(Pat (), [NameBind ()])]
renameIrrPats ps = runRename (mapM renameIrrP ps)

renameIrrP :: Pat () -> RN (Pat (), [(Name (), Pat ())])
renameIrrP p = case p of
    -- We should rename any regular pattern appearing
    -- inside an irrefutable pattern.
    PIrrPat l p     -> do (q, ms) <- renameRP p
                          return $ (PIrrPat l q, ms)
    -- The rest of the rules simply try to rename regular patterns in
    -- irrefutable patterns in their immediate subpatterns.
    PInfixApp l p1 n p2 -> rename2pat p1 p2
                                (\p1 p2 -> PInfixApp l p1 n p2)
                                renameIrrP
    PApp l n ps         -> renameNpat ps (PApp l n) renameIrrP
    PTuple l bx ps      -> renameNpat ps (PTuple l bx) renameIrrP
    PList l ps          -> renameNpat ps (PList l) renameIrrP
    PParen l p          -> rename1pat p (PParen l) renameIrrP
    PRec l n pfs        -> renameNpat pfs (PRec l n) renameIrrPf
    PAsPat l n p        -> rename1pat p (PAsPat l n) renameIrrP
    PatTypeSig l p t  -> rename1pat p (\p -> PatTypeSig l p t) renameIrrP
    -- Hsx
    PXTag l n attrs mat ps -> do (attrs', nss) <- fmap unzip $ mapM renameIrrAttr attrs
                                 (mat', ns1) <- case mat of
                                                   Nothing -> return (Nothing, [])
                                                   Just at -> do (at', ns) <- renameIrrP at
                                                                 return (Just at', ns)
                                 (q, ns) <- renameNpat ps (PXTag l n attrs' mat') renameIrrP
                                 return (q, concat nss ++ ns1 ++ ns)
    PXETag l n attrs mat  -> do (as, nss) <- fmap unzip $ mapM renameIrrAttr attrs
                                (mat', ns1) <- case mat of
                                                  Nothing -> return (Nothing, [])
                                                  Just at -> do (at', ns) <- renameIrrP at
                                                                return (Just at', ns)
                                return $ (PXETag l n as mat', concat nss ++ ns1)
    PXPatTag l p            -> rename1pat p (PXPatTag l) renameIrrP
    -- End Hsx

    _                       -> return (p, [])

  where renameIrrPf :: PatField () -> RN (PatField (), [NameBind ()])
        renameIrrPf (PFieldPat l n p) = rename1pat p (PFieldPat l n) renameIrrP
        renameIrrPf pf = return (pf, [])

        renameIrrAttr :: PXAttr () -> RN (PXAttr (), [NameBind ()])
        renameIrrAttr (PXAttr l s p) = rename1pat p (PXAttr l s) renameIrrP

-----------------------------------------------------------------------------------
-- Transforming Patterns: the real stuff

-- | Transform several patterns in the same context, thereby
-- generating any code for matching regular patterns.
transformPatterns :: [Pat ()] -> HsxM ([Pat ()], [Guard ()], [Guard ()], [Decl ()])
transformPatterns ps = runTr (trPatterns ps)

---------------------------------------------------
-- The transformation monad

type State = (Int, Int, Int, [Guard ()], [Guard ()], [Decl ()])

newtype Tr a = Tr (State -> HsxM (a, State))

instance Applicative Tr where
  pure  = return
  (<*>) = ap

instance Monad Tr where
 return a = Tr $ \s -> return (a, s)
 (Tr f) >>= k = Tr $ \s ->
          do (a, s') <- f s
             let (Tr f') = k a
             f' s'

instance Functor Tr where
 fmap f tra = tra >>= (return . f)

liftTr :: HsxM a -> Tr a
liftTr hma = Tr $ \s -> do a <- hma
                           return (a, s)

initState = initStateFrom 0 0

initStateFrom k l = (0, k, l, [], [], [])

runTr :: Tr a -> HsxM (a, [Guard ()], [Guard ()], [Decl ()])
runTr (Tr f) = do (a, (_,_,_,gs1,gs2,ds)) <- f initState
                  return (a, reverse gs1, reverse gs2, reverse ds)


runTrFromTo :: Int -> Int -> Tr a -> HsxM (a, [Guard ()], [Guard ()], [Decl ()], Int, Int)
runTrFromTo k l (Tr f) = do (a, (_,k',l',gs1,gs2,ds)) <- f $ initStateFrom k l
                            return (a, reverse gs1, reverse gs2, reverse ds, k', l')


-- manipulating the state
getState :: Tr State
getState = Tr $ \s -> return (s,s)

setState :: State -> Tr ()
setState s = Tr $ \_ -> return ((),s)

updateState :: (State -> (a,State)) -> Tr a
updateState f = do s <- getState
                   let (a,s') = f s
                   setState s'
                   return a

-- specific state manipulating functions
pushGuard :: Pat () -> Exp () -> Tr ()
pushGuard p e = updateState $ \(n,m,a,gs1,gs2,ds) -> ((),(n,m,a,gs1,(p,e):gs2,ds))

pushDecl :: Decl () -> Tr ()
pushDecl d = updateState $ \(n,m,a,gs1,gs2,ds) -> ((),(n,m,a,gs1,gs2,d:ds))

pushAttrGuard :: Pat () -> Exp () -> Tr ()
pushAttrGuard p e = updateState $ \(n,m,a,gs1,gs2,ds) -> ((),(n,m,a,(p,e):gs1,gs2,ds))

genMatchName :: Tr (Name ())
genMatchName = do k <- updateState $ \(n,m,a,gs1,gs2,ds) -> (n,(n+1,m,a,gs1,gs2,ds))
                  return $ Ident () $ "harp_match" ++ show k

genPatName :: Tr (Name ())
genPatName = do k <- updateState $ \(n,m,a,gs1,gs2,ds) -> (m,(n,m+1,a,gs1,gs2,ds))
                return $ Ident () $ "harp_pat" ++ show k

genAttrName :: Tr (Name ())
genAttrName = do k <- updateState $ \(n,m,a,gs1,gs2,ds) -> (m,(n,m,a+1,gs1,gs2,ds))
                 return $ Ident ()  $ "hsx_attrs" ++ show k


setHarpTransformedT, setXmlTransformedT :: Tr ()
setHarpTransformedT = liftTr setHarpTransformed
setXmlTransformedT  = liftTr setXmlTransformed


-------------------------------------------------------------------
-- Some generic functions for computations in the Tr monad. Could
-- be made even more general, but there's really no point right now...

tr1pat :: a -> (b -> c) -> (a -> Tr b) -> Tr c
tr1pat p f tr = do q <- tr p
                   return $ f q

tr2pat :: a -> a -> (b -> b -> c) -> (a -> Tr b) -> Tr c
tr2pat p1 p2 f tr = do q1 <- tr p1
                       q2 <- tr p2
                       return $ f q1 q2

trNpat :: [a] -> ([b] -> c) -> (a -> Tr b) -> Tr c
trNpat ps f tr = do qs <- mapM tr ps
                    return $ f qs

-----------------------------------------------------------------------------
-- The *real* transformations
-- Transforming patterns

-- | Transform several patterns in the same context
trPatterns :: [Pat ()] -> Tr [Pat ()]
trPatterns = mapM trPattern

-- | Transform a pattern by traversing the syntax tree.
-- A regular pattern is translated, other patterns are
-- simply left as is.
trPattern :: Pat () -> Tr (Pat ())
trPattern p = case p of
    -- This is where the fun starts. =)
    -- Regular patterns must be transformed of course.
    PRPat _ rps -> do
        -- First we need a name for the placeholder pattern.
        n <- genPatName
        -- A top-level regular pattern is a sequence in linear
        -- context, so we can simply translate it as if it was one.
        (mname, vars, _) <- trRPat True (RPSeq () rps)
        -- Generate a top level declaration.
        topmname <- mkTopDecl mname vars
        -- Generate a pattern guard for this regular pattern,
        -- that will match the generated declaration to the
        -- value of the placeholder, and bind all variables.
        mkGuard vars topmname n
        -- And indeed, we have made a transformation!
        setHarpTransformedT
        -- Return the placeholder pattern.
        return $ pvar n
    -- Tag patterns should be transformed
    PXTag _ name attrs mattr cpats -> do
        -- We need a name for the attribute list, if there are lookups
        an <- case (mattr, attrs) of
                -- ... if there is one already, and there are no lookups
                -- we can just return that
                (Just ap, []) -> return $ ap
                      -- ... if there are none, we dont' care
                (_, []) -> return wildcard
                (_, _)  -> do -- ... but if there are, we want a name for that list
                              n <- genAttrName
                              -- ... we must turn attribute lookups into guards
                              mkAttrGuards n attrs mattr
                              -- ... and we return the pattern
                              return $ pvar n
        -- ... the pattern representing children should be transformed
        cpat' <- case cpats of
                  -- ... it's a regular pattern, so we can just go ahead and transform it
                  (p@(PXRPats _ _)):[] -> trPattern p
                  -- ... it's an ordinary list, so we first wrap it up as such
                  _                    -> trPattern (PList () cpats)
        -- ...  we have made a transformation and should report that
        setHarpTransformedT
        -- ... and we return a Tag pattern.
        let (dom, n) = xNameParts name
        return $ metaTag dom n an cpat'
    -- ... as should empty Tag patterns
    PXETag _ name attrs mattr -> do
        -- We need a name for the attribute list, if there are lookups
        an <- case (mattr, attrs) of
                -- ... if there is a pattern already, and there are no lookups
                -- we can just return that
                (Just ap, []) -> return $ ap
                      -- ... if there are none, we dont' care
                (_, []) -> return wildcard
                (_, _)  -> do -- ... but if there are, we want a name for that list
                              n <- genAttrName
                              -- ... we must turn attribute lookups into guards
                              mkAttrGuards n attrs mattr
                              -- ... and we return the pattern
                              return $ pvar n
        -- ...  we have made a transformation and should report that
        setHarpTransformedT
        -- ... and we return an ETag pattern.
        let (dom, n) = xNameParts name
        return $ metaTag dom n an peList
    -- PCDATA patterns are strings in the xml datatype.
    PXPcdata _ st -> setHarpTransformedT >> (return $ metaPcdata st)
    -- XML comments are likewise just treated as strings.
    PXPatTag _ p -> setHarpTransformedT >> trPattern p
    -- Regular expression patterns over children should be translated
    -- just like PRPat.

    PXRPats l rps -> trPattern $ PRPat l rps
    -- Transforming any other patterns simply means transforming
    -- their subparts.
    PViewPat l e p       -> do
        e' <- liftTr $ transformExpM e
        tr1pat p (PViewPat l e') trPattern
    PVar _ _             -> return p
    PLit _ _ _           -> return p
    PInfixApp l p1 op p2 -> tr2pat p1 p2 (\p1 p2 -> PInfixApp l p1 op p2) trPattern
    PApp l n ps          -> trNpat ps (PApp l n) trPattern
    PTuple l bx ps       -> trNpat ps (PTuple l bx) trPattern
    PList l ps           -> trNpat ps (PList l) trPattern
    PParen l p           -> tr1pat p (PParen l) trPattern
    PRec l n pfs         -> trNpat pfs (PRec l n) trPatternField
    PAsPat l n p         -> tr1pat p (PAsPat l n) trPattern
    PWildCard l          -> return p
    PIrrPat l p          -> tr1pat p (PIrrPat l) trPattern
    PatTypeSig l p t   -> tr1pat p (\p -> PatTypeSig l p t) trPattern
    PQuasiQuote _ _ _    -> return p
    PBangPat l p         -> tr1pat p (PBangPat l) trPattern
    PNPlusK _ _ _        -> return p

  where -- Transform a pattern field.
    trPatternField :: PatField () -> Tr (PatField ())
    trPatternField (PFieldPat l n p) =
        tr1pat p (PFieldPat l n) trPattern
    trPatternField p = return p

    -- | Generate a guard for looking up xml attributes.
    mkAttrGuards :: Name () -> [PXAttr ()] -> Maybe (Pat ()) -> Tr ()
    mkAttrGuards attrs [PXAttr _ n q] mattr = do
        -- Apply lookupAttr to the attribute name and
        -- attribute set
        let rhs = metaExtract n attrs
            -- ... catch the result
            pat = metaPJust q
            -- ... catch the remainder list
            rml = case mattr of
                   Nothing -> wildcard
                   Just ap -> ap
        -- ... and add the generated guard to the store.
        pushAttrGuard (pTuple [pat, rml]) rhs

    mkAttrGuards attrs ((PXAttr _ a q):xs) mattr = do
        -- Apply lookupAttr to the attribute name and
        -- attribute set
        let rhs = metaExtract a attrs
            -- ... catch the result
            pat = metaPJust q
        -- ... catch the remainder list
        newAttrs <- genAttrName
        -- ... and add the generated guard to the store.
        pushAttrGuard (pTuple [pat, pvar newAttrs]) rhs
        -- ... and finally recurse
        mkAttrGuards newAttrs xs mattr

    -- | Generate a declaration at top level that will finalise all
    -- variable continuations, and then return all bound variables.
    mkTopDecl :: Name () -> [Name ()] -> Tr (Name ())
    mkTopDecl mname vars =
        do -- Give the match function a name
           n <- genMatchName
           -- Create the declaration and add it to the store.
           pushDecl $ topDecl n mname vars
           -- Return the name of the match function so that the
           -- guard that will be generated can call it.
           return n

    topDecl :: Name () -> Name () -> [Name ()] -> Decl ()
    topDecl n mname vs =
        let pat  = pTuple [wildcard, pvarTuple vs]      -- (_, (foo, bar, ...))
            g    = var mname                            -- harp_matchX
            a    = genStmt pat g                        -- (_, (foo, ...)) <- harp_matchX
            vars = map (\v -> app (var v) eList) vs     -- (foo [], bar [], ...)
            b    = qualStmt $ metaReturn $ tuple vars   -- return (foo [], bar [], ...)
            e    = doE [a,b]                            -- do (...) <- harp_matchX
                                                        --    return (foo [], bar [], ...)
         in nameBind n e                                -- harp_matchY = do ....

    -- | Generate a pattern guard that will apply the @runMatch@
    -- function on the top-level match function and the input list,
    -- thereby binding all variables.
    mkGuard :: [Name ()] -> Name () -> Name () -> Tr ()
    mkGuard vars mname n = do
        let tvs = pvarTuple vars                        -- (foo, bar, ...)
            ge  = appFun runMatchFun [var mname, var n] -- runMatch harp_matchX harp_patY
        pushGuard (pApp just_name [tvs]) ge             -- Just (foo, bar, ...) , runMatch ...

--------------------------------------------------------------------------------
-- Transforming regular patterns

-- | A simple datatype to annotate return values from sub-patterns
data MType = S         -- Single element
           | L MType       -- List of ... , (/  /), *, +
           | E MType MType -- Either ... or ... , (  |  )
           | M MType       -- Maybe ... , ?


-- When transforming a regular sub-pattern, we need to know the
-- name of the function generated to match it, the names of all
-- variables it binds, and the type of its returned value.
type MFunMetaInfo l = (Name l, [Name l], MType)


-- | Transform away a regular pattern, generating code
-- to replace it.
trRPat :: Bool -> RPat () -> Tr (MFunMetaInfo ())
trRPat linear rp = case rp of
    -- For an ordinary Haskell pattern we need to generate a
    -- base match function for the pattern, and a declaration
    -- that lifts that function into the matcher monad.
    RPPat _ p -> mkBaseDecl linear p

      where
        -- | Generate declarations for matching ordinary Haskell patterns
        mkBaseDecl :: Bool -> Pat () -> Tr (MFunMetaInfo ())
        mkBaseDecl linear p = case p of
            -- We can simplify a lot if the pattern is a wildcard or a variable
            PWildCard _ -> mkWCMatch
            PVar _ v    -> mkVarMatch linear v
            -- ... and if it is an embedded pattern tag, we can just skip it
            PXPatTag _ q -> mkBaseDecl linear q

            -- ... otherwise we'll have to take the long way...
            p           -> do -- First do a case match on a single element
                              (name, vars, _) <- mkBasePat linear p
                              -- ... apply baseMatch to the case matcher to
                              -- lift it into the matcher monad.
                              newname <- mkBaseMatch name
                              -- ... and return the meta-info gathered.
                              return (newname, vars, S)

        -- | Generate a basic function that cases on a single element,
        -- returning Just (all bound variables) on a match, and
        -- Nothing on a mismatch.
        mkBasePat :: Bool -> Pat () -> Tr (MFunMetaInfo ())
        mkBasePat b p =
         do -- First we need a name...
           n <- genMatchName
           -- ... and then we need to know what variables that
           -- will be bound by this match.
           let vs = gatherPVars p
           -- ... and then we can create and store away a casing function.
           basePatDecl b n vs p >>= pushDecl
           return (n, vs, S)

        -- | Generate a basic casing function for a given pattern.
        basePatDecl :: Bool -> Name () -> [Name ()] -> Pat () -> Tr (Decl ())
        basePatDecl linear f vs p = do
         -- We can use the magic variable harp_a since nothing else needs to
         -- be in scope at this time (we could use just a, or foo, or whatever)
         let a = Ident () $ "harp_a"
         -- ... and we should case on that variable on the right-hand side.
         rhs <- baseCaseE linear p a vs    -- case harp_a of ...
         -- The result is a simple function with one paramenter and
         -- the right-hand side we just generated.
         return $ simpleFun f a rhs
           where baseCaseE :: Bool -> Pat () -> Name () -> [Name ()] -> Tr (Exp ())
                 baseCaseE b p a vs = do
                    -- First the alternative if we actually
                    -- match the given pattern
                    let alt1 = alt p                  -- foo -> Just (mf foo)
                                (app (con just_name) $
                                 tuple (map (retVar b) vs))
                        -- .. and finally an alternative for not matching the pattern.
                        alt2 = alt wildcard (con nothing_name)        -- _ -> Nothing
                        -- ... and that pattern could itself contain regular patterns
                        -- so we must transform away these.
                    alt1' <- liftTr $ transformAlt alt1
                    return $ caseE (var a) [alt1', alt2]
                 retVar :: Bool -> Name () -> Exp ()
                 retVar linear v
                    -- if bound in linear context, apply const
                    | linear    = metaConst (var v)
                    -- if bound in non-linear context, apply (:)
                    | otherwise = app consFun (var v)

    -- For guarded base patterns, we want to do the same as for unguarded base patterns,
    -- only with guards (doh).
    RPGuard _ p gs -> mkGuardDecl linear p gs

     where mkGuardDecl :: Bool -> Pat () -> [Stmt ()] -> Tr (MFunMetaInfo ())
           mkGuardDecl linear p gs = case p of
                -- If it is an embedded pattern tag, we want to skip it
                PXPatTag _ q -> mkGuardDecl linear q gs

                -- ... otherwise we'll want to make a base pattern
                p           -> do -- First do a case match on a single element
                      (name, vars, _) <- mkGuardPat linear p gs
                      -- ... apply baseMatch to the case matcher to
                      -- lift it into the matcher monad.
                      newname <- mkBaseMatch name
                      -- ... and return the meta-info gathered.
                      return (newname, vars, S)

           -- | Generate a basic function that cases on a single element,
           -- returning Just (all bound variables) on a match, and
           -- Nothing on a mismatch.
           mkGuardPat :: Bool -> Pat () -> [Stmt ()] -> Tr (MFunMetaInfo ())
           mkGuardPat b p gs =
                do -- First we need a name...
                   n <- genMatchName
                   -- ... and then we need to know what variables that
                   -- will be bound by this match.
                   let vs = gatherPVars p ++ concatMap gatherStmtVars gs
                   -- ... and then we can create and store away a casing function.
                   guardPatDecl b n vs p gs >>= pushDecl
                   return (n, vs, S)

           -- | Generate a basic casing function for a given pattern.
           guardPatDecl :: Bool -> Name () -> [Name ()] -> Pat () -> [Stmt ()] -> Tr (Decl ())
           guardPatDecl linear f vs p gs = do
                -- We can use the magic variable harp_a since nothing else needs to
                -- be in scope at this time (we could use just a, or foo, or whatever)
                let a = Ident () $ "harp_a"
                -- ... and we should case on that variable on the right-hand side.
                rhs <- guardedCaseE linear p gs a vs  -- case harp_a of ...
                -- The result is a simple function with one parameter and
                -- the right-hand side we just generated.
                return $ simpleFun f a rhs
              where guardedCaseE :: Bool -> Pat () -> [Stmt ()] -> Name () -> [Name ()] -> Tr (Exp ())
                    guardedCaseE b p gs a vs = do
                        -- First the alternative if we actually
                        -- match the given pattern
                        let alt1 = altGW p gs                 -- foo -> Just (mf foo)
                                    (app (con just_name) $
                                    tuple (map (retVar b) vs)) (binds [])
                            -- .. and finally an alternative for not matching the pattern.
                            alt2 = alt wildcard (con nothing_name)        -- _ -> Nothing
                            -- ... and that pattern could itself contain regular patterns
                            -- so we must transform away these.
                        alt1' <- liftTr $ transformAlt alt1
                        return $ caseE (var a) [alt1', alt2]
                    retVar :: Bool -> Name () -> Exp ()
                    retVar linear v
                        -- if bound in linear context, apply const
                        | linear    = metaConst (var v)
                        -- if bound in non-linear context, apply (:)
                        | otherwise = app consFun (var v)

    -- For a sequence of regular patterns, we should transform all
    -- sub-patterns and then generate a function for sequencing them.
    RPSeq _ rps -> do
        nvts <- mapM (trRPat linear) rps
        mkSeqDecl nvts

      where
        -- | Generate a match function for a sequence of regular patterns,
        -- flattening any special sub-patterns into normal elements of the list
        mkSeqDecl :: [MFunMetaInfo ()] -> Tr (MFunMetaInfo ())
        mkSeqDecl nvts = do
            -- First, as always, we need a name...
            name <- genMatchName
            let -- We need a generating statement for each sub-pattern.
                (gs, vals) = unzip $ mkGenExps 0 nvts     -- (harp_valX, (foo, ...)) <- harp_matchY
                -- Gather up all variables from all sub-patterns.
                vars    = concatMap (\(_,vars,_) -> vars) nvts
                -- ... flatten all values to simple lists, and concatenate
                -- the lists to a new return value
                fldecls = flattenVals vals                  -- harp_valXf = $flatten harp_valX
                                                            -- harp_ret = foldComp [harp_val1f, ...]
                -- ... return the value along with all variables
                ret     = qualStmt $ metaReturn $           -- return (harp_ret, (foo, .....))
                            tuple [var retname, varTuple vars]
                -- ... do all these steps in a do expression
                rhs     = doE $ gs ++                       -- do (harp_valX, (foo, ...)) <- harpMatchY
                            [letStmt fldecls, ret]          --    let harp_valXf = $flatten harp_valX
                                                            --    return (harp_ret, (foo, .....))
            -- ... bind it to its name, and add the declaration
            -- to the store.
            pushDecl $ nameBind name rhs                    -- harp_matchZ = do ....
            -- The return value of a sequence is always a list of elements.
            return (name, vars, L S)

        -- | Flatten values of all sub-patterns into normal elements of the list
        flattenVals :: [(Name (), MType)] -> [Decl ()]
        flattenVals nts =
            let -- Flatten the values of all sub-patterns to
                -- lists of elements
                (nns, ds) = unzip $ map flVal nts
                -- ... and concatenate their results.
                ret       = nameBind retname $ app
                              (paren $ app foldCompFun
                                (listE $ map var nns)) $ eList
             in ds ++ [ret]

        flVal :: (Name (), MType) -> (Name (), Decl ())
        flVal (name, mt) =
            let -- We reuse the old names, we just extend them a bit.
                newname = extendVar name "f"    -- harp_valXf
                -- Create the appropriate flattening function depending
                -- on the type of the value
                f       = flatten mt
                -- ... apply it to the value and bind it to its new name.
             in (newname, nameBind newname $  -- harp_valXf = $flatten harp_valX
                    app f (var name))

        -- | Generate a flattening function for a given type structure.
        flatten :: MType -> Exp ()
        flatten S = consFun                         -- (:)
        flatten (L mt) =
            let f = flatten mt
                r = paren $ metaMap [f]
             in paren $ foldCompFun `metaComp` r    -- (foldComp . (map $flatten))
        flatten (E mt1 mt2) =
            let f1 = flatten mt1
                f2 = flatten mt2
             in paren $ metaEither f1 f2            -- (either $flatten $flatten)
        flatten (M mt) =
            let f = flatten mt
             in paren $ metaMaybe idFun f           -- (maybe id $flatten)

    -- For accumulating as-patterns we should transform the subpattern, and then generate
    -- a declaration that supplies the value to be bound to the variable in question.
    -- The variable should be bound non-linearly.
    RPCAs _ v rp -> do
        -- Transform the subpattern
        nvt@(name, vs, mt) <- trRPat linear rp
        -- ... and create a declaration to bind its value.
        n <- mkCAsDecl nvt
        -- The type of the value is unchanged.
        return (n, (v:vs), mt)

      where
        -- | Generate a declaration for a \@: binding.
        mkCAsDecl :: MFunMetaInfo () -> Tr (Name ())
        mkCAsDecl = asDecl $ app consFun    -- should become lists when applied to []

    -- For ordinary as-patterns we should transform the subpattern, and then generate
    -- a declaration that supplies the value to be bound to the variable in question.
    -- The variable should be bound linearly.
    RPAs _ v rp
        | linear ->
             do -- Transform the subpattern
                nvt@(name, vs, mt) <- trRPat linear rp
                -- ... and create a declaration to bind its value
                n <- mkAsDecl nvt
                -- The type of the value is unchanged.
                return (n, (v:vs), mt)
        -- We may not use an @ bind in non-linear context
        | otherwise -> case v of
                Ident () n -> fail $ "Attempting to bind variable "++n++
                      " inside the context of a numerable regular pattern"
                _         -> fail $ "This should never ever ever happen... how the #% did you do it??!?"

      where
        -- | Generate a declaration for a \@ binding.
        mkAsDecl :: MFunMetaInfo () -> Tr (Name ())
        mkAsDecl = asDecl metaConst     -- should be constant when applied to []

    -- For regular patterns, parentheses have no real meaning
    -- so at this point we can just skip them.
    RPParen _ rp -> trRPat linear rp

    -- For (possibly non-greedy) optional regular patterns we need to
    -- transform the subpattern, and the generate a function that can
    -- choose to match or not to match, that is the question...
    RPOp _ rp (RPOpt _)->
        do -- Transform the subpattern
           nvt <- trRPat False rp
           -- ... and create a declaration that can optionally match it.
           mkOptDecl False nvt
    -- ... similarly for the non-greedy version.
    RPOp _ rp (RPOptG _) ->
        do -- Transform the subpattern
           nvt <- trRPat False rp
           -- ... and create a declaration that can optionally match it.
           mkOptDecl True nvt

    -- For union patterns, we should transform both subexpressions,
    -- and generate a function that chooses between them.
    RPEither _ rp1 rp2 ->
        do -- Transform the subpatterns
           nvt1 <- trRPat False rp1
           nvt2 <- trRPat False rp2
           -- ... and create a declaration that can choose between them.
           mkEitherDecl nvt1 nvt2
        -- Generate declarations for either patterns, i.e. ( | )
      where mkEitherDecl :: MFunMetaInfo () -> MFunMetaInfo () -> Tr (MFunMetaInfo ())
            mkEitherDecl nvt1@(_, vs1, t1) nvt2@(_, vs2, t2) = do
                -- Eine namen, bitte!
                n <- genMatchName
                let -- Generate generators for the subpatterns
                    (g1, v1) = mkGenExp nvt1
                    (g2, v2) = mkGenExp nvt2          -- (harp_valX, (foo, bar, ...)) <- harp_matchY
                    -- ... gather all variables from both sides
                    allvs = vs1 `union` vs2
                    -- ... some may be bound on both sides, so we
                    -- need to check which ones are bound on each,
                    -- supplying empty value for those that are not
                    vals1 = map (varOrId vs1) allvs
                    vals2 = map (varOrId vs2) allvs
                    -- ... apply either Left or Right to the returned value
                    ret1  = metaReturn $ tuple          -- return (Left harp_val1, (foo, id, ...))
                                [app (con left_name)
                                 (var v1), tuple vals1]
                    ret2  = metaReturn $ tuple          -- return (Right harp_val2, (id, bar, ...))
                                [app (con right_name)
                                 (var v2), tuple vals2]
                    -- ... and do all these things in do-expressions
                    exp1  = doE [g1, qualStmt ret1]
                    exp2  = doE [g2, qualStmt ret2]
                    -- ... and choose between them using the choice (+++) operator.
                    rhs   = (paren exp1) `metaChoice`       -- (do ...) +++
                            (paren exp2)            --  (do ...)
                -- Finally we create a declaration for this function and
                -- add it to the store.
                pushDecl $ nameBind n rhs         -- harp_matchZ = (do ...) ...
                -- The type of the returned value is Either the type of the first
                -- or the second subpattern.
                return (n, allvs, E t1 t2)

            varOrId :: [Name ()] -> Name () -> Exp ()
            varOrId vs v = if v `elem` vs   -- the variable is indeed bound in this branch
                            then var v      -- ... so it should be added to the result
                            else idFun      -- ... else it should be empty.

    -- For (possibly non-greedy) repeating regular patterns we need to transform the subpattern,
    -- and then generate a function to handle many matches of it.
    RPOp _ rp (RPStar _) ->
        do -- Transform the subpattern
           nvt <- trRPat False rp
           -- ... and create a declaration that can match it many times.
           mkStarDecl False nvt
    -- ... and similarly for the non-greedy version.

    RPOp _ rp (RPStarG _) ->
        do -- Transform the subpattern
           nvt <- trRPat False rp
           -- ... and create a declaration that can match it many times.
           mkStarDecl True nvt

    -- For (possibly non-greedy) non-empty repeating patterns we need to transform the subpattern,
    -- and then generate a function to handle one or more matches of it.
    RPOp _ rp (RPPlus _) ->
        do -- Transform the subpattern
           nvt <- trRPat False rp
           -- ... and create a declaration that can match it one or more times.
           mkPlusDecl False nvt

    -- ... and similarly for the non-greedy version.
    RPOp _ rp (RPPlusG _) ->
        do -- Transform the subpattern
           nvt <- trRPat False rp
           -- ... and create a declaration that can match it one or more times.
           mkPlusDecl True nvt

  where -- These are the functions that must be in scope for more than one case alternative above.

    -- | Generate a declaration for matching a variable.
    mkVarMatch :: Bool -> Name () -> Tr (MFunMetaInfo ())
    mkVarMatch linear v = do
            -- First we need a name for the new match function.
            n <- genMatchName
            -- Then we need a basic matching function that always matches,
            -- and that binds the value matched to the variable in question.
            let e = paren $ lamE [pvar v] $       -- (\v -> Just (mf v))
                              app (con just_name)
                              (paren $ retVar linear v)
            -- Lift the function into the matcher monad, and bind it to its name,
            -- then add it the declaration to the store.
            pushDecl $ nameBind n $
                          app baseMatchFun e    -- harp_matchX = baseMatch (\v -> Just (mf v))
            return (n, [v], S)          -- always binds v and only v

          where retVar :: Bool -> Name () -> Exp ()
                retVar linear v
                    -- if bound in linear context, apply const
                    | linear    = metaConst (var v)
                    -- if bound in non-linear context, apply (:)
                    | otherwise = app consFun (var v)

    -- | Generate a declaration for matching a wildcard
    mkWCMatch :: Tr (MFunMetaInfo ())
    mkWCMatch = do
            -- First we need a name...
            n <- genMatchName
            -- ... and then a function that always matches, discarding the result
            let e = paren $ lamE [wildcard] $     -- (\_ -> Just ())
                                app (con just_name) (unit_con ())
            -- ... which we lift, bind, and add to the store.
            pushDecl $ nameBind n $       -- harp_matchX = baseMatch (\_ -> Just ())
                         app baseMatchFun e
            return (n, [], S)   -- no variables bound, hence []

    -- | Gather up the names of all variables in a pattern,
    -- using a simple fold over the syntax structure.
    gatherPVars :: Pat () -> [Name ()]
    gatherPVars p = case p of
            PVar _ v             -> [v]
            PInfixApp _ p1 _ p2  -> gatherPVars p1 ++
                                         gatherPVars p2
            PApp _ _ ps          -> concatMap gatherPVars ps
            PTuple _ _ ps        -> concatMap gatherPVars ps
            PList _ ps           -> concatMap gatherPVars ps
            PParen _ p           -> gatherPVars p
            PRec _ _ pfs         -> concatMap help pfs
                where help (PFieldPat _ _ p) = gatherPVars p
                      help _                 = []
            PAsPat _ n p         -> n : gatherPVars p
            PWildCard _          -> []
            PIrrPat _ p          -> gatherPVars p
            PatTypeSig _ p _     -> gatherPVars p
            PRPat _ rps          -> concatMap gatherRPVars rps
            PXTag _ _ attrs mattr cps ->
                concatMap gatherAttrVars attrs ++ concatMap gatherPVars cps ++
                    case mattr of
                     Nothing -> []
                     Just ap -> gatherPVars ap
            PXETag _ _ attrs mattr ->
                concatMap gatherAttrVars attrs ++
                    case mattr of
                     Nothing -> []
                     Just ap -> gatherPVars ap
            PXPatTag _ p         -> gatherPVars p
            _                    -> []

    gatherRPVars :: RPat () -> [Name ()]
    gatherRPVars rp = case rp of
            RPOp _ rq _        -> gatherRPVars rq
            RPEither _ rq1 rq2 -> gatherRPVars rq1 ++ gatherRPVars rq2
            RPSeq _ rqs        -> concatMap gatherRPVars rqs
            RPCAs _ n rq       -> n : gatherRPVars rq
            RPAs _ n rq        -> n : gatherRPVars rq
            RPParen _ rq       -> gatherRPVars rq
            RPGuard _ q gs     -> gatherPVars q ++ concatMap gatherStmtVars gs
            RPPat _ q          -> gatherPVars q

    gatherAttrVars :: PXAttr () -> [Name ()]
    gatherAttrVars (PXAttr _ _ p) = gatherPVars p

    gatherStmtVars :: Stmt () -> [Name ()]
    gatherStmtVars gs = case gs of
            Generator _ p _ -> gatherPVars p
            _               -> []

    -- | Generate a match function that lift the result of the
    -- basic casing function into the matcher monad.
    mkBaseMatch :: Name () -> Tr (Name ())
    mkBaseMatch name =
            do -- First we need a name...
               n <- genMatchName
               -- ... to which we bind the lifting function
               pushDecl $ baseMatchDecl n name
               -- and then return for others to use.
               return n

    -- | Generate a declaration for the function that lifts a simple
    -- casing function into the matcher monad.
    baseMatchDecl :: Name () -> Name () -> Decl ()
    baseMatchDecl newname oldname =
            -- Apply the lifting function "baseMatch" to the casing function
            let e = app baseMatchFun (var oldname)
                -- ... and bind it to the new name.
             in nameBind newname e        -- harp_matchX = baseMatch harp_matchY

    -- | Generate the generators that call sub-matching functions, and
    -- annotate names with types for future flattening of values.
    -- Iterate to enable gensym-like behavior.
    mkGenExps :: Int -> [MFunMetaInfo ()] -> [(Stmt (), (Name (), MType))]
    mkGenExps _ [] = []
    mkGenExps k ((name, vars, t):nvs) =
        let valname = mkValName k                           -- harp_valX
            pat     = pTuple [pvar valname, pvarTuple vars] -- (harp_valX, (foo, bar, ...))
            g       = var name
         in (genStmt pat g, (valname, t)) :               -- (harp_valX, (foo, ...)) <- harp_matchY
                mkGenExps (k+1) nvs

    -- | Create a single generator.
    mkGenExp :: MFunMetaInfo () -> (Stmt (), Name ())
    mkGenExp nvt = let [(g, (name, _t))] = mkGenExps 0 [nvt]
                   in (g, name)

    -- | Generate a single generator with a call to (ng)manyMatch,
    -- and an extra variable name to use after unzipping.
    mkManyGen :: Bool -> Name () -> Stmt ()
    mkManyGen greedy mname =
        -- Choose which repeater function to use, determined by greed
        let mf  = if greedy then gManyMatchFun else manyMatchFun
         -- ... and create a generator that applies it to the
         -- matching function in question.
         in genStmt (pvar valsvarsname) $
            app mf (var mname)

    -- | Generate declarations for @: and @ bindings.
    asDecl :: (Exp () -> Exp ()) -> MFunMetaInfo () -> Tr (Name ())
    asDecl mf nvt@(_, vs, _) = do
        -- A name, if you would
        n <- genMatchName                                -- harp_matchX
        let -- Generate a generator for matching the subpattern
            (g, val) = mkGenExp nvt                      -- (harp_valY, (foo, ...)) <- harp_matchZ
            -- ... fix the old variables
            vars     = map var vs                        -- (apa, bepa, ...)
            -- ... and return the generated value, along with the
            -- new set of variables which is the old set prepended
            -- by the variable currently being bound.
            ret = qualStmt $ metaReturn $ tuple          -- return (harp_valY, ($mf harp_valY, apa, ...))
                [var val, tuple $ mf (var val) : vars]   -- mf in the line above is what separates
                                                         -- @: ((:)) from @ (const)
        -- Finally we create a declaration for this function and
        -- add it to the store.
        pushDecl $ nameBind n $ doE [g, ret]             -- harp_matchX = do ...
        return n

    -- | Generate declarations for optional patterns, ? and #?.
    -- (Unfortunally we must place this function here since both variations
    -- of transformations of optional patterns should be able to call it...)
    mkOptDecl :: Bool -> MFunMetaInfo () -> Tr (MFunMetaInfo ())
    mkOptDecl greedy nvt@(_, vs, t) = do
        -- Un nome, s'il vouz plaît.
        n <- genMatchName
        let -- Generate a generator for matching the subpattern
            (g, val) = mkGenExp nvt                 -- (harp_valX, (foo, bar, ...)) <- harp_matchY
            -- ... and apply a Just to its value
            ret1 = metaReturn $ tuple               -- return (Just harp_val1, (foo, bar, ...))
                    [app (con just_name)
                     (var val), varTuple vs]
            -- ... and do those two steps in a do-expression
            exp1 = doE [g, qualStmt ret1]           -- do ....
            -- For the non-matching branch, all the variables should be empty
            ids  = map (const idFun) vs             -- (id, id, ...)
            -- ... and the value should be Nothing.
            ret2 = metaReturn $ tuple               -- return (Nothing, (id, id, ...))
                    [con nothing_name, tuple ids]   -- i.e. no vars were bound
            -- The order of the arguments to the choice (+++) operator
            -- is determined by greed...
            mc   = if greedy
                    then metaChoice        -- standard order
                    else (flip metaChoice) -- reversed order
            -- ... and then apply it to the branches.
            rhs  = (paren exp1) `mc`                -- (do ....) +++
                    (paren ret2)                    --  (return (Nothing, .....))
        -- Finally we create a declaration for this function and
        -- add it to the store.
        pushDecl $ nameBind n rhs                   -- harp_matchZ = (do ....) +++ (return ....)
        -- The type of the returned value will be Maybe the type
        -- of the value of the subpattern.
        return (n, vs, M t)

    -- | Generate declarations for star patterns, * and #*
    -- (Unfortunally we must place this function here since both variations
    -- of transformations of repeating patterns should be able to call it...)
    mkStarDecl :: Bool -> MFunMetaInfo () -> Tr (MFunMetaInfo ())
    mkStarDecl greedy (mname, vs, t) = do
        -- Ett namn, tack!
        n <- genMatchName
        let -- Create a generator that matches the subpattern
            -- many times, either greedily or non-greedily
            g = mkManyGen greedy mname
            -- ... and unzip the result, choosing the proper unzip
            -- function depending on the number of variables returned.
            metaUnzipK = mkMetaUnzip (length vs)
            -- ... first unzip values from variables
            dec1    = patBind (pvarTuple [valname, varsname])
                    (metaUnzip $ var valsvarsname)
            -- ... and then unzip the variables
            dec2    = patBind (pvarTuple vs)
                    (metaUnzipK $ var varsname)
            -- ... fold all the values for variables
            retExps = map ((app foldCompFun) . var) vs
            -- ... and return value and variables
            ret     = metaReturn $ tuple $
                    [var valname, tuple retExps]
        -- Finally we need to generate a function that does all this,
        -- using a let-statement for the non-monadic stuff and a
        -- do-expression to wrap it all in.
        pushDecl $ nameBind n $
            doE [g, letStmt [dec1, dec2], qualStmt ret]
        -- The type of the returned value is a list ([]) of the
        -- type of the subpattern.
        return (n, vs, L t)

    -- | Generate declarations for plus patterns, + and #+
    -- (Unfortunally we must place this function here since both variations
    -- of transformations of non-empty repeating patterns should be able to call it...)
    mkPlusDecl :: Bool -> MFunMetaInfo () -> Tr (MFunMetaInfo ())
    mkPlusDecl greedy nvt@(mname, vs, t) = do
        -- and now I've run out of languages...
        n <- genMatchName
        let k = length vs
            -- First we want a generator to match the
            -- subpattern exactly one time
            (g1, val1) = mkGenExp nvt                       -- (harp_valX, (foo, ...)) <- harpMatchY
            -- ... and then one that matches it many times.
            g2         = mkManyGen greedy mname             -- harp_vvs <- manyMatch harpMatchY
            -- ... we want to unzip the result, using
            -- the proper unzip function
            metaUnzipK = mkMetaUnzip k
            -- ... first unzip values from variables
            dec1    = patBind                               -- (harp_vals, harp_vars) = unzip harp_vvs
                        (pvarTuple [valsname, varsname])
                        (metaUnzip $ var valsvarsname)
            -- .. now we need new fresh names for variables
            -- since the ordinary ones are already taken.
            vlvars  = genNames "harp_vl" k
            -- ... and then we can unzip the variables
            dec2    = patBind (pvarTuple vlvars)            -- (harp_vl1, ...) = unzipK harp_vars
                        (metaUnzipK $ var varsname)
            -- .. and do the unzipping in a let-statement
            letSt   = letStmt [dec1, dec2]
            -- ... fold variables from the many-match,
            -- prepending the variables from the single match
            retExps = map mkRetFormat $ zip vs vlvars       -- foo . (foldComp harp_vl1), ...
            -- ... prepend values from the single match to
            -- those of the many-match.
            retVal  = (var val1) `metaCons`
                        (var valsname)                      -- harp_valX : harp_vals
            -- ... return all values and variables
            ret     = metaReturn $ tuple $                  -- return (harp_valX:harpVals,
                        [retVal, tuple retExps]             --   (foo . (...), ...))
            -- ... and wrap all of it in a do-expression.
            rhs     = doE [g1, g2, letSt, qualStmt ret]
        -- Finally we create a declaration for this function and
        -- add it to the store.
        pushDecl $ nameBind n rhs
        -- The type of the returned value is a list ([]) of the
        -- type of the subpattern.
        return (n, vs, L t)

      where mkRetFormat :: (Name (), Name ()) -> Exp ()
            mkRetFormat (v, vl) =
                -- Prepend variables using function composition.
                (var v) `metaComp`
                  (paren $ (app foldCompFun) $ var vl)

--------------------------------------------------------------------------
-- HaRP-specific functions and ids

-- | Functions and ids from the @Match@ module,
-- used in the generated matching functions
runMatchFun, baseMatchFun, manyMatchFun, gManyMatchFun :: Exp ()
runMatchFun = match_qual runMatch_name
baseMatchFun = match_qual baseMatch_name
manyMatchFun = match_qual manyMatch_name
gManyMatchFun = match_qual gManyMatch_name

runMatch_name, baseMatch_name, manyMatch_name, gManyMatch_name :: Name ()
runMatch_name   = Ident () "runMatch"
baseMatch_name  = Ident () "baseMatch"
manyMatch_name  = Ident () "manyMatch"
gManyMatch_name = Ident () "gManyMatch"

match_mod, match_qual_mod :: ModuleName ()
match_mod      = ModuleName () "Harp.Match"
match_qual_mod = ModuleName () "HaRPMatch"

match_qual :: Name () -> Exp ()
match_qual = qvar match_qual_mod

choiceOp :: QOp ()
choiceOp = QVarOp () $ Qual () match_qual_mod choice

appendOp :: QOp ()
appendOp = QVarOp () $ UnQual () append

-- foldComp = foldl (.) id, i.e. fold by composing
foldCompFun :: Exp ()
foldCompFun = match_qual $ Ident () "foldComp"

mkMetaUnzip :: Int -> Exp () -> Exp ()
mkMetaUnzip k | k <= 7 = let n = "unzip" ++ show k
                          in (\e -> matchFunction n [e])
              | otherwise =
                   let vs      = genNames "x" k
                       lvs     = genNames "xs" k
                       uz      = name $ "unzip" ++ show k
                       ys      = name "ys"
                       xs      = name "xs"
                       alt1    = alt peList $ tuple $ replicate k eList   -- [] -> ([], [], ...)
                       pat2    = (pvarTuple vs) `metaPCons` (pvar xs)       -- (x1, x2, ...)
                       ret2    = tuple $ map appCons $ zip vs lvs           -- (x1:xs1, x2:xs2, ...)
                       rhs2    = app (var uz) (var xs)                      -- unzipK xs
                       dec2    = patBind (pvarTuple lvs) rhs2             -- (xs1, xs2, ...) = unzipK xs
                       exp2    = letE [dec2] ret2
                       alt2    = alt pat2 exp2
                       topexp  = lamE [pvar ys] $ caseE (var ys) [alt1, alt2]
                       topbind = nameBind uz topexp
                    in app (paren $ letE [topbind] (var uz))
  where appCons :: (Name (), Name ()) -> Exp ()
        appCons (x, xs) = metaCons (var x) (var xs)

matchFunction :: String -> [Exp ()] -> Exp ()
matchFunction s es = mf s (reverse es)
  where mf s []     = match_qual $ Ident () s
        mf s (e:es) = app (mf s es) e

-- | Some 'magic' gensym-like functions, and functions
-- with related functionality.
retname :: Name ()
retname = name "harp_ret"

varsname :: Name ()
varsname = name "harp_vars"

valname :: Name ()
valname = name "harp_val"

valsname :: Name ()
valsname = name "harp_vals"

valsvarsname :: Name ()
valsvarsname = name "harp_vvs"

mkValName :: Int -> Name ()
mkValName k = name $ "harp_val" ++ show k

extendVar :: Name () -> String -> Name ()
extendVar (Ident l n) s = Ident l $ n ++ s
extendVar n _ = n

xNameParts :: XName () -> (Maybe String, String)
xNameParts n = case n of
                XName _ s      -> (Nothing, s)
                XDomName _ d s -> (Just d, s)

---------------------------------------------------------
-- meta-level functions, i.e. functions that represent functions,
-- and that take arguments representing arguments... whew!

metaReturn, metaConst, metaUnzip :: Exp () -> Exp ()
metaReturn e = metaFunction "return" [e]
metaConst e  = metaFunction "const" [e]
metaUnzip e  = metaFunction "unzip" [e]

metaEither, metaMaybe :: Exp () -> Exp () -> Exp ()
metaEither e1 e2 = metaFunction "either" [e1,e2]
metaMaybe e1 e2 = metaFunction "maybe" [e1,e2]

metaConcat, metaMap :: [Exp ()] -> Exp ()
metaConcat es = metaFunction "concat" [listE es]
metaMap       = metaFunction "map"

metaAppend :: Exp () -> Exp () -> Exp ()
metaAppend l1 l2 = infixApp l1 appendOp l2

-- the +++ choice operator
metaChoice :: Exp () -> Exp () -> Exp ()
metaChoice e1 e2 = infixApp e1 choiceOp e2

metaPCons :: Pat () -> Pat () -> Pat ()
metaPCons p1 p2 = PInfixApp () p1 cons p2

metaCons, metaComp :: Exp () -> Exp () -> Exp ()
metaCons e1 e2 = infixApp e1 (QConOp () cons) e2
metaComp e1 e2 = infixApp e1 (op fcomp) e2

metaPJust :: Pat () -> Pat ()
metaPJust p = pApp just_name [p]

metaPNothing :: Pat ()
metaPNothing = pvar nothing_name

metaPMkMaybe :: Maybe (Pat ()) -> Pat ()
metaPMkMaybe mp = case mp of
    Nothing -> metaPNothing
    Just p  -> pParen $ metaPJust p

metaJust :: Exp () -> Exp ()
metaJust e = app (con just_name) e

metaNothing :: Exp ()
metaNothing = con nothing_name

metaMkMaybe :: Maybe (Exp ()) -> Exp ()
metaMkMaybe me = case me of
    Nothing -> metaNothing
    Just e  -> paren $ metaJust e

---------------------------------------------------
-- some other useful functions at abstract level
consFun, idFun :: Exp ()
consFun = Con () cons
idFun = function "id"

con :: Name () -> Exp ()
con = Con () . UnQual ()

cons :: QName ()
cons = Special () (Cons ())

fcomp, choice, append :: Name ()
fcomp = Symbol () "."
choice = Symbol () "+++"
append = Symbol () "++"

just_name, nothing_name, left_name, right_name :: Name ()
just_name    = Ident () "Just"
nothing_name = Ident () "Nothing"
left_name    = Ident () "Left"
right_name   = Ident () "Right"

------------------------------------------------------------------------
-- Help functions for meta programming xml

{- No longer used.
hsx_data_mod :: ModuleName
hsx_data_mod = ModuleName "HSP.Data"

-- Also no longer used, literal PCDATA should be considered a string.
-- | Create an xml PCDATA value
metaMkPcdata :: String -> Exp
metaMkPcdata s = metaFunction "pcdata" [strE s]
-}

-- | Create an xml tag, given its domain, name, attributes and
-- children.
metaGenElement :: XName () -> [Exp ()] -> Maybe (Exp ()) -> [Exp ()] -> Exp ()
metaGenElement name ats mat cs =
    let (d,n) = xNameParts name
        ne    = tuple [metaMkMaybe $ fmap (metaFromStringLit . strE) d, metaFromStringLit $ strE n]
        m = maybe id (\x y -> paren $ y `metaAppend` (metaMap [argAsAttr, x])) mat
        attrs = m $ listE $ map metaAsAttr ats
     in metaFunction "genElement" [ne, attrs, listE cs]

-- | Create an empty xml tag, given its domain, name and attributes.
metaGenEElement :: XName () -> [Exp ()] -> Maybe (Exp ()) -> Exp ()
metaGenEElement name ats mat =
    let (d,n) = xNameParts name
        ne    = tuple [metaMkMaybe $ fmap (metaFromStringLit . strE) d, metaFromStringLit $ strE n]
        m = maybe id (\x y -> paren $ y `metaAppend` (metaMap [argAsAttr, x])) mat
        attrs = m $ listE $ map metaAsAttr ats
     in metaFunction "genEElement" [ne, attrs]

-- | Create an attribute by applying the overloaded @asAttr@
metaAsAttr :: Exp () -> Exp ()
metaAsAttr e@(Lit _ (String _ _ _)) = metaFunction "asAttr" [metaFromStringLit e] -- [ExpTypeSig noLoc e (TyCon (UnQual (Ident "Text")))]
metaAsAttr e = metaFunction "asAttr" [e]

argAsAttr :: Exp ()
argAsAttr = var $ name "asAttr"

-- | Create a property from an attribute and a value.
metaAssign :: Exp () -> Exp () -> Exp ()
metaAssign e1 e2 = infixApp e1 assignOp e2
  where assignOp = QConOp () $ UnQual () $ Symbol () ":="

-- | Make xml out of some expression by applying the overloaded function
-- @asChild@.
metaAsChild :: Exp () -> Exp ()
metaAsChild e = metaFunction "asChild" [paren e]

-- | convert a 'String' literal to lazy 'Text' by calling a function named 'fromStringLit'
metaFromStringLit :: Exp () -> Exp ()
metaFromStringLit e = metaFunction "fromStringLit" [e]

-- TODO: We need to fix the stuff below so pattern matching on XML could also be overloaded.
-- Right now it only works on HSP XML, or anything that is syntactically identical to it.

-- | Lookup an attribute in the set of attributes.
metaExtract :: XName () -> Name () -> Exp ()
metaExtract name attrs =
    let (d,n) = xNameParts name
        np    = tuple [metaMkMaybe $ fmap strE d, strE n]
     in metaFunction "extract" [np, var attrs]

-- | Generate a pattern under the Tag data constructor.
metaTag :: (Maybe String) -> String -> Pat () -> Pat () -> Pat ()
metaTag dom name ats cpat =
    let d = metaPMkMaybe $ fmap strP dom
        n = pTuple [d, strP name]
     in metaConPat "Element" [n, ats, cpat]

-- | Generate a pattern under the PCDATA data constructor.
metaPcdata :: String -> Pat ()
metaPcdata s = metaConPat "CDATA" [strP s]

metaMkName :: XName () -> Exp ()
metaMkName n = case n of
    XName _ s      -> metaFromStringLit (strE s)
    XDomName _ d s -> tuple [metaFromStringLit $ strE d, metaFromStringLit $ strE s]

--    XName s      -> textTypeSig (strE s)
--    XDomName d s -> tuple [textTypeSig $ strE d, textTypeSig $ strE s]
--    where
--      textTypeSig e = ExpTypeSig noLoc e (TyCon (UnQual (Ident "Text")))


