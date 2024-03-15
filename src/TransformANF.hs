{-# language Strict, LambdaCase, BlockArguments, OverloadedStrings #-}
{-# options_ghc -Wincomplete-patterns #-}

module TransformANF where

import Data.Maybe
import Data.List
import Debug.Trace
import qualified InputIR as I
import InputIR (Ty(..), CompTy(..), ValTy(..))
import qualified OutputIR as O

-- Semantic difference between join points and ValTy let expressions TODO
-- are case condition the only-non-tail thing in the language?

-- Since whether a let binding is a join point or should be lifted is determined
-- at call-site we can get the need for both a lambda-lift and a join should
-- prioritize -> lambda-lift?

type Name   = String

fresh :: [Name] -> Name -> Name
fresh ns x | elem x ns = fresh ns (x ++ "'")
           | otherwise = x

data Val
  = Top Name     -- top vars
  | Local Name   -- local var
  | Fix Name

  | Lam Name ValTy (Val -> Val)
  | SApp Val [Val]

  -- | Tuple [Tm]
  -- | Proj Tm Int  -- field projection for tuples

  | BoolLit Bool
  | CaseBool Ty Val Val Val  -- ret ty, scrutinee, True case, False case

  | IntLit Int
  | IPlus Val Val
  -- insert Int operations to taste

  -- | Nil
  -- | Cons Tm Tm
  -- | CaseList Ty Tm Tm (Name, Name, Tm) -- ret ty, scrutinee, nilcase, conscase

  | Let Name Ty Val (Val -> Val)     -- let x : ty := def; body
  | LetRec Name Ty (Val -> Val) (Val -> Val)  -- letrec

data IR
  = ITop Name     -- top vars
  | ILocal Name   -- local var

  | ILam Name ValTy IR
  | ISApp IR [IR]

  -- | Tuple [Tm]
  -- | Proj Tm Int  -- field projection for tuples

  | IBoolLit Bool
  | ICaseBool ValTy IR IR IR  -- ret ty, scrutinee, True case, False case

  | IIntLit Int
  | IIPlus IR IR
  -- insert Int operations to taste

  -- | Nil
  -- | Cons Tm Tm
  -- | CaseList Ty Tm Tm (Name, Name, Tm) -- ret ty, scrutinee, nilcase, conscase

  | ILet Name Ty IR IR     -- let x : ty := def; body
  | ILetRec Name Ty IR IR  -- letrec
  | IFix Name
  deriving (Show)

data Def = Def Name Ty [Name] IR
  deriving (Show)

type Top = [Def]

type Global = [(Name, Ty)]

type Env = [(Name, (Val, Ty))]

eval :: Env -> Global -> I.Tm -> Val
eval e g etm = case etm of
  I.Top n -> Top n
  I.Local n -> lookupVal n e
  I.Lam n t tm -> Lam n t (\v -> eval ((n, (v,ValTy t)):e) g tm)
  I.App tm1 tm2 -> evalApps e g tm1 [eval e g tm2]

  --  | Tuple [Tm]
  --  | Proj Tm Int  -- field projection for tuples

  I.BoolLit b -> BoolLit b
  I.CaseBool t tmc tmt tmf -> vcaseB t (eval e g tmc) (eval e g tmt) (eval e g tmf)
  I.IntLit i -> IntLit i
  I.IPlus i1 i2 -> vlet "" (ValTy VTInt) (eval e g i1) \a -> vlet "" (ValTy VTInt) (eval e g i2) \b -> IPlus a b
  -- insert Int operations to taste

  --  | Nil
  --  | Cons Tm Tm
  --  | CaseList Ty Tm Tm (Name, Name, Tm) -- ret ty, scrutinee, nilcase, conscase

  I.Let n t tm1 tm2 -> vlet n t (eval e g tm1) \v -> eval ((n, (v,t)):e) g tm2
  I.LetRec n t tm1 tm2 -> vletrec n t (\v -> eval ((n, (v,t)):e) g tm1) \v -> eval ((n, (v,t)):e) g tm2
    -- TODO is this safe?


unwrapTy :: Ty -> [Ty]
unwrapTy (ValTy _) = []
unwrapTy (CompTy (CTFunStop v1 _)) = [(ValTy v1)]
unwrapTy (CompTy (CTFunMore v1 ct)) = (ValTy v1) : unwrapTy (CompTy ct)

etaExp :: Ty -> Val -> Val
etaExp tp val = go tp []
  where
    go :: Ty -> [Val] -> Val
    go t@(ValTy _) [] = val
    go t@(CompTy (CTFunStop v1 _)) args = Lam "" v1 (\v -> vapps val (unwrapTy tp) (reverse args))
    go t@(CompTy (CTFunMore v1 ct)) args = Lam "'" v1 (\v -> go (CompTy ct) (v:args))

vcaseB :: Ty -> Val -> Val -> Val -> Val
vcaseB tt@(ValTy VTBool) (BoolLit b) v2 v3 = if b then v2 else v3
-- vcaseB tt@(ValTy t) (CaseBool _ v1' v2' v3') v2 v3 = vcaseB tt v1' (vcaseB tt v2' v2 v3) (vcaseB tt v3' v2 v3) -- TODO case-of-case ?
vcaseB tt@(ValTy t) v1 v2 v3 = vlet "" (ValTy VTBool) v1 \vc -> CaseBool tt vc (vlet "" tt v2 id) (vlet "" tt v3 id)
vcaseB t v1 v2 v3 = CaseBool t v1 v2 v3



vapps :: Val -> [Ty] -> [Val] -> Val
vapps t [] [] = t
vapps t tss@(ty:ts) uss@(u:us) = case t of
  Lam n ty f -> case u of
    (Local _) -> vapps (f u) ts us
    (BoolLit _) -> vapps (f u) ts us
    (IntLit _) -> vapps (f u) ts us
    _ -> letBind (SApp t []) uss tss
  Let x ty t v -> vlet x ty t \y -> vapps (v y) tss uss
  LetRec x ty t v -> vletrec x ty t \y -> vapps (v y) tss uss
  CaseBool t@(CompTy _) vb vt vf -> vcaseB (foldl appTy t tss) vb (vapps vt tss uss) (vapps vf tss uss)
  SApp _ _ -> letBind t uss tss
  _          -> letBind (SApp t []) uss tss
  where
    letBind v [] _ = v
    letBind (SApp t vals) (v:vs) (ty:ts) = vlet "" ty v \v -> letBind (SApp t (vals ++ [v])) vs ts-- TODO ugly
    appTy :: Ty -> Ty -> Ty
    appTy (CompTy (CTFunStop v1 v2)) _ = ValTy v2
    appTy (CompTy (CTFunMore v1 ct)) _ = CompTy ct

vlet :: Name -> Ty -> Val -> (Val -> Val) -> Val
vlet x ty t u = case t of
  Local _        -> u t
  BoolLit _        -> u t
  IntLit _        -> u t
  Let x' ty' t' u' -> vlet x' ty' t' \x' -> vlet x ty (u' x') u
  LetRec x' ty' t' u' -> vletrec x' ty' t' \x' -> vlet x ty (u' x') u
  t             -> Let x ty (etaExp ty t) u

vletrec :: Name -> Ty -> (Val -> Val) -> (Val -> Val) -> Val
vletrec x ty t u = case t (Local x) of
  Local n  | n /= x -> u (Local n)
  Let x' ty' t' u' -> vlet x' ty' t' \x' -> vlet x ty (u' x') u
  LetRec x' ty' t' u' -> vletrec x' ty' t' \x' -> vlet x ty (u' x') u
  _             -> LetRec x ty (\v -> etaExp ty (t v)) u

evalApps :: Env -> Global -> I.Tm -> [Val] ->Val
evalApps env g t [] = eval env g t
evalApps env g t args@(arg:argss) = case t of
   I.Lam n t tm -> vapps (Lam n t (\v -> evalApps ((n, (v,ValTy t)):env) g tm (argss))) [ValTy t] [arg]
   I.Let n t tm1 tm2 -> vlet n t (eval env g tm1) \v -> evalApps ((n, (v,t)):env) g tm2 args
   I.LetRec n t tm1 tm2 -> vletrec n t (\v -> eval ((n, (v,t)):env) g tm1) \v -> evalApps ((n, (v,t)):env) g tm2 args
   I.CaseBool ty _ _ _  -> vapps (eval env g t) (unwrapTy ty) args
   I.Local n     -> vapps (lookupVal n env) types args
     where
       types = unwrapTy $ lookupType n env
   I.Top n -> vapps (Top n) (unwrapTy $ fromJust $ lookup n g) args
   I.App t u -> evalApps env g t (eval env g u : args)

lookupType :: Name -> [(Name,(Val, Ty))] -> Ty
lookupType n = snd . fromJust . lookup n

lookupVal :: Name -> [(Name,(Val, Ty))] -> Val
lookupVal n = fst . fromJust . lookup n

quote :: [Name] -> Val -> IR
quote ns = \case
  Top n -> ITop n
  Fix n -> IFix n
  Local x     -> ILocal x
  SApp t us   -> ISApp (quote ns t) (fmap (quote ns) us)
  Lam x ty t   -> let x' = fresh ns x in ILam x' ty (quote (x':ns) (t (Local x')))
  Let x ty t u -> let x' = fresh ns x in ILet x' ty (quote ns t) (quote (x':ns) (u (Local x')))
  LetRec x ty t u -> let x' = fresh ns x in ILetRec x' ty (quote (x':ns) (t (Fix x'))) (quote (x':ns) (u (Local x')))

  --  | Tuple [Tm]
  --  | Proj Tm Int  -- field projection for tuples

  BoolLit b -> IBoolLit b
  CaseBool (ValTy t) tmc tmt tmf -> ICaseBool t (quote ns tmc) (quote ns tmt) (quote ns tmf)
  IntLit i -> IIntLit i
  IPlus i1 i2 -> IIPlus (quote ns i1) (quote ns i2)
  -- insert Int operations to taste

  --  | Nil
  --  | Cons Tm Tm
  --  | CaseList Ty Tm Tm (Name, Name, Tm) -- ret ty, scrutinee, nilcase, conscase


-- transform closed terms
transform :: Global -> (Name, Ty,  I.Tm) -> Def
transform g (n,ty,tm) = Def n ty nms (quote nms $ (\v -> vapps v tps (fmap Local nms)) $ eval [] g tm)
  where
    tps = unwrapTy ty
    nms = fmap show $ take (length tps) [1..]


-- transformProgram :: I.Program -> I.Program
-- transformProgram p = fmap (\case (n,ty,tm) -> (n,ty, transform tyMap tm)) p
--   where
--     tyMap = fmap (\case (n,ty,tm) -> (n,ty)) p

-- coerceToOutput :: I.Program -> O.Top
-- coerceToOutput = undefined

-- -- Examples
-- --------------------------------------------------------------------------------

-- instance IsString I.Tm where
--   fromString = I.Local

-- ($$) = App
-- infixl 2 $$

-- p1 = I.Let "zero" (I.Lam "s" $ I.Lam "z" $ "z") $
--      (Lam "x" "x") $$ "zero" -- beta redex of varible (gets reduced)

-- p2 = I.Let "zero" (I.Lam "s" $ I.Lam "z" $ "z") $
--      (I.Let "id" (I.Lam "x" "x") "id") $$ "zero"  -- push app inside let

-- p3 = (I.Lam "x" "x") $$ (I.Lam "s" $ I.Lam "z" $ "z") -- beta-redex of non-variable (turns into let)

-- p4 = I.Let "zero" (I.Let "id" (I.Lam "x" "x") (I.Lam "s" $ I.Lam "z" "z")) "zero" -- let-of-let

{-
We want to apply the following transformations everywhere

  let x = y in t   -- where y is a variable
  ~>
  t[y/x]

  let x = (let y = t in u) in v
  ~>
  let y = t in let x = u in v

  (let x = t in u) v
  ~>
  let x = t in u v

  (\x -> t) u
  ~>
  let x = u in t

This is possible purely with NbE in an efficient and very easy way, where we
don't have to manually handle variables and substitution. Instead, every binder
looks like a plain function.

nThe main limitation is that we can't look "under" a binder to check whether its
body is of a particular form. However, many things can be implemented without
looking under binders, and we can work around some limitations by including more
information in values and Val binders (which I don't demonstrate here).
-}
