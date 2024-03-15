{-# language Strict #-}
{-# options_ghc -funbox-strict-fields #-}

module OutputIR where

data ValTy  = VTInt | VTList ValTy | VTTuple [ValTy] | VTBool
data CompTy = CTFun [ValTy] ValTy  -- nonempty list of args
data Ty     = ValTy ValTy | CompTy CompTy

type Name = Int

data Atom
  = ATrue
  | AFalse
  -- | ANil
  -- | AIntLit Int
  -- | ATt              -- nullary tuple value
  | LocalVar Name    -- (can only refer to a let binder)

data Jump = MkJump Name [Atom]

-- constructor application or function call
data ValExp
  = -- Nil
  -- | Cons Atom Atom
  -- | Tuple [Atom]
  TopCall Name [Atom]
  | Jump Jump

data FunExp = FunExp [Name] Program

data Program
  = Let Name ValTy ValExp Program
  | Join Name CompTy FunExp Program
  | JoinRec Name CompTy FunExp Program
  | Ret ValExp
  | Atom Atom
  | CaseBool Atom Jump Jump
  -- | CaseList Atom Jump (Name, Name, Jump)

data Def =
  ValDef Name ValTy Program
  | FunDef Name CompTy FunExp

type Top = [Def]
