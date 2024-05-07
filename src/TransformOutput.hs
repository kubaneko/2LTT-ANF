module TransformOutput where

import qualified TransformANF as T
import TransformANF (Val(..))

data ValTy  = VTInt | VTList ValTy | VTTuple [ValTy] | VTBool
data CompTy = CTFun [ValTy] ValTy  -- nonempty list of args
data Ty     = ValTy ValTy | CompTy CompTy

type Name = String

data Jump = MkJump Name [ValO]

data FunExp = FunExp [Name] ValO

data ValO
  = NewTop Def (ValO -> ValO)
  -- Nil
  -- | Cons Atom Atom
  -- | Tuple [Atom]
  | ATrue
  | AFalse
  -- | ANil
  | AIntLit Int
  -- | ATt              -- nullary tuple value
  | LocalVar Name    -- (can only refer to a let binder)
  | TopCall Name [ValO]
  | Jump Jump
  | Let Name ValTy ValO (ValO -> ValO)
  | Join Name CompTy FunExp (ValO -> ValO)
  | JoinRec Name CompTy FunExp (ValO -> ValO)
  | CaseBool ValO Jump Jump
  -- | CaseList Atom Jump (Name, Name, Jump)

data Def =
  ValDef Name ValTy ValO
  | FunDef Name CompTy FunExp

type Top = [Def]

defaultBinding :: Binding
defaultBinding = Loc Unused

data Binding = Loc BindingType | Arg | Rec BindingType BindingType
data BindingType = Tail | NTail | Unused

mergeBindings :: Binding -> Binding -> Binding
mergeBindings _ Arg = Arg
mergeBindings (Loc loc1 fix1) (Loc loc1 fix1) = Loc (mergeT loc1 loc2) (mergeT fix1 fix2)
  where
    mergeT b1 Unused = b1
    mergeT Unused b2 = b2
    mergeT Tail Tail = Tail
    mergeT _  _ = NTail

evalDef :: R.Def -> Def

eval :: Val -> BindingM Def
eval e g etm = case etm of
  Top n -> return $ TopCall n []
  Local n ->
  Fix n ->
  Lam n vt f
  SApp v vs
  -- | Tuple [Tm]
  -- | Proj Tm Int  -- field projection for tuples
  BoolLit b
  CaseBool Ty Val Val Val  -- ret ty, scrutinee, True case, False case

  IntLit Int
  IPlus Val Val
  -- insert Int operations to taste

  -- | Nil
  -- | Cons Tm Tm
  -- | CaseList Ty Tm Tm (Name, Name, Tm) -- ret ty, scrutinee, nilcase, conscase

  Let Name Ty Val (Val -> Val)     -- let x : ty := def; body
  LetRec Name CompTy (Val -> Val) (Val -> Val)  -- letrec
