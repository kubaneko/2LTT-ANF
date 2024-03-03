{-# language Strict, LambdaCase, BlockArguments, OverloadedStrings #-}
{-# options_ghc -Wincomplete-patterns #-}

module TransformANF where

import Data.Maybe
import Debug.Trace
import qualified InputIR as I
import InputIR (Ty(..), CompTy(..), ValTy(..))
import qualified OutputIR as O

type Name   = String

fresh :: [Name] -> Name -> Name
fresh ns x | elem x ns = fresh ns (x ++ "'")
           | otherwise = x

data Val
  = Top Name     -- top vars
  | Local Name   -- local var

  | Lam Name ValTy (Val -> Val)
  | App Val Val

  -- | Tuple [Tm]
  -- | Proj Tm Int  -- field projection for tuples

  | BoolLit Bool
  | CaseBool ValTy Val Val Val  -- ret ty, scrutinee, True case, False case

  | IntLit Int
  | IPlus Val Val
  -- insert Int operations to taste

  -- | Nil
  -- | Cons Tm Tm
  -- | CaseList Ty Tm Tm (Name, Name, Tm) -- ret ty, scrutinee, nilcase, conscase

  | Let Name Ty Val (Val -> Val)     -- let x : ty := def; body
  | LetRec Name Ty Val (Val -> Val)  -- letrec

type Program = [(Name, Ty)]

type Env = [(Name, Val)]

eval :: Env -> Program -> I.Tm -> Val
eval e g etm = case etm of
  I.Top n -> etaExp n g
  I.Local n -> fromJust $ lookup n e
  I.Lam n t tm -> Lam n t (\v -> eval ((n, v):e) g tm)
  I.App tm1 tm2 -> vapp (eval e g tm1) (eval e g tm2)

  --  | Tuple [Tm]
  --  | Proj Tm Int  -- field projection for tuples

  I.BoolLit b -> BoolLit b
  I.CaseBool t tmc tmt tmf -> vcaseB t (eval e g tmc) (eval e g tmt) (eval e g tmf)
  I.IntLit i -> IntLit i
  I.IPlus i1 i2 -> IPlus (anfLift (eval e g i1) (ValTy VTInt)) (anfLift (eval e g i2) (ValTy VTInt))
  -- insert Int operations to taste

  --  | Nil
  --  | Cons Tm Tm
  --  | CaseList Ty Tm Tm (Name, Name, Tm) -- ret ty, scrutinee, nilcase, conscase

  I.Let n t tm1 tm2 -> vlet n t (eval e g tm1) \v -> eval ((n, v):e) g tm2
  I.LetRec n t tm1 tm2 -> vlet n t (eval ((n, Local n):e) g tm1) \v -> eval ((n, v):e) g tm2

isAtom a = case a of
  (Local _) -> True
  (BoolLit _) -> True
  (IntLit _) -> True
  _ -> False

etaExp :: String -> Program -> Val
etaExp n g = go tp (Top n)
  where
    tp = fromJust $ lookup n g
    go :: Ty -> Val -> Val
    go t@(ValTy _) p = p
    go t@(CompTy (CTFunStop v1 _)) p = trace "as" $ Lam "" v1 (\v -> vapp p v)
    go t@(CompTy (CTFunMore v1 ct)) p = trace "ass" $ Lam "'" v1 (\v -> go (CompTy ct) (vapp p v))

-- TODO fresh does not make sense here
vcaseB :: Ty -> Val -> Val -> Val -> Val
vcaseB (ValTy t) tm1 tm2 tm3 = CaseBool t (anfLift tm1 (ValTy VTBool)) tm2 tm3
vcaseB (CompTy (CTFunStop v1 v2)) tm1 tm2 tm3 =
  Lam "" v1 (\v -> CaseBool v2 (anfLift tm1 (ValTy VTBool)) (vapp tm2 v) (vapp tm3 v))
vcaseB (CompTy (CTFunMore v1 ct)) tm1 tm2 tm3 =
  Lam "" v1 (\v -> vcaseB (CompTy ct) tm1 (vapp tm2 v) (vapp tm3 v))

anfLift tm t = if isAtom tm then tm else Let "" t tm id

vapp :: Val -> Val -> Val
vapp t u = case t of
  Let x ty t v -> vlet x ty t \x -> vapp (v x) u
  LetRec x ty t v -> vlet x ty t \x -> vapp (v x) u
  Lam x ty f -> f u
  t          -> App t u

vlet :: Name -> Ty -> Val -> (Val -> Val) -> Val
vlet x ty t u = case t of
  Local _        -> u t
  Let x' ty' t' u' -> vlet x' ty' t' \x' -> vlet x ty (u' x') u
  t             -> case (u (Local x))


quote :: [Name] -> Val -> I.Tm
quote ns = \case
  Top n -> I.Top n
  Local x     -> I.Local x
  App t u   -> I.App (quote ns t) (quote ns u)
  Lam x ty t   -> let x' = fresh ns x in I.Lam x' ty (quote (x':ns) (t (Local x')))
  Let x ty t u -> let x' = fresh ns x in I.Let x' ty (quote ns t) (quote (x':ns) (u (Local x')))
  LetRec x ty t u -> let x' = fresh ns x in I.Let x' ty (quote ns t) (quote (x':ns) (u (Local x')))

  --  | Tuple [Tm]
  --  | Proj Tm Int  -- field projection for tuples

  BoolLit b -> I.BoolLit b
  CaseBool t tmc tmt tmf -> I.CaseBool (ValTy t) (quote ns tmc) (quote ns tmt) (quote ns tmf)
  IntLit i -> I.IntLit i
  IPlus i1 i2 -> I.IPlus (quote ns i1) (quote ns i2)
  -- insert Int operations to taste

  --  | Nil
  --  | Cons Tm Tm
  --  | CaseList Ty Tm Tm (Name, Name, Tm) -- ret ty, scrutinee, nilcase, conscase


-- transform closed terms
transform :: Program -> I.Tm -> I.Tm
transform g = quote [] . eval [] g

transformProgram :: I.Program -> I.Program
transformProgram p = fmap (\case (n,ty,tm) -> (n,ty, transform tyMap tm)) p
  where
    tyMap = fmap (\case (n,ty,tm) -> (n,ty)) p

coerceToOutput :: I.Program -> O.Top
coerceToOutput = undefined

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
