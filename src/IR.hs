module IR where

data Def = Def Name Ty [Name] IR
  deriving (Show)

type Top = [Def]

data IR
  = ITop Name     -- top vars
  | ILocal Name   -- local var

  | ILam Binding Name ValTy IR
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

  | ILet Binding Name Ty IR IR     -- let x : ty := def; body
  | ILetRec Binding Name CompTy IR IR  -- letrec
  | IFix Name
  deriving (Show)
