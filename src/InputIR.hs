
{-# language Strict #-}

-- Simplifications

{-
  INPUT:
    No side effects, inductive types: Lists, Bool, primitive Int + primops, Tuple types
    - curried function types, arbitrary local let-s
    - case split for lists, Bool can target ValTy, CompTy
    - let, letrec
    - beta-redexes in program are allowed: (λ x. x) e
    Program is a sequence of top defs


  OUTPUT:
    ValTy is same
    - CompTy is fixed arity of ([ValTy] -> ValTy)
    - No partial applications
    - case splits can only target ValTy
    - local let for ValTy
    - local join/joinrec for CompTy  (local definition, for functions which are
                                      only tail-called)
    - ANF
    - Program is a sequence of top defs
      (only way to make a non-tail call is to call a top-level function)

   TRANSLATION SUMMARY
    - eta-expansion of calls to arity

    - case commutation to eliminate cases returning in CompTy

        f = λ b. (case b of
          True  -> λ x. x
          False -> λ y. y)

        f = λ b. λ x. (case b of True → λ x. x; False → λ x. x) x

        f = λ b. λ x. (case b of True → (λ x. x) x; False → (λ x. x) x)

        f = λ b. λ x. (case b of True → x; False → x)

    - local lets:
      - if some local function is non-tail-called: lift it top-level
      - if some local function is only tail-called: becomes a join

-}

module InputIR where

data ValTy  = VTInt | VTBool --  | VTList ValTy | VTTuple [ValTy]
  deriving (Show)
data CompTy = CTFunStop ValTy ValTy | CTFunMore ValTy CompTy
  deriving (Show)
data Ty     = ValTy ValTy | CompTy CompTy
  deriving (Show)

type Name   = String

data Tm
  = Top Name     -- top vars
  | Local Name   -- local var

  | Lam Name ValTy Tm
  | App Tm Tm

  -- | Tuple [Tm]
  -- | Proj Tm Int  -- field projection for tuples

  | BoolLit Bool
  | CaseBool Ty Tm Tm Tm  -- ret ty, scrutinee, True case, False case

  | IntLit Int
  | IPlus Tm Tm
  -- insert Int operations to taste

  -- | Nil
  -- | Cons Tm Tm
  -- | CaseList Ty Tm Tm (Name, Name, Tm) -- ret ty, scrutinee, nilcase, conscase

  | Let Name Ty Tm Tm     -- let x : ty := def; body
  | LetRec Name CompTy Tm Tm  -- letrec
  deriving (Show)

type Program = [(Name, Ty, Tm)]
