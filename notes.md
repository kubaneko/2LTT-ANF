# Notes 2

   TRANSLATION SUMMARY
    - eta-expansion of calls to arity

    - case commutation to eliminate cases returning in CompTy

        f = λ b. (case b of
          True  -> λ x. x
          False -> λ y. y)

        f = λ b. λ x. (case b of True → λ x. x; False → λ x. x) x

        f = λ b. λ x. (case b of True → (λ x. x) x; False → (λ x. x) x)

        f = λ b. λ x. (case b of True → x; False → x)

-- Conditions Saturation, TailCall, TopLevel f
(f a b c) d -> join 1,2,3,4 (TopCall f 1 2 3 4) (let k=a in (let l=b in (... Jump k l ... )))

Put lets into lambdas

SApp ?

Typed Env?

Where to outline stuff (problem with accessing env)

LetRec - what to do with eval since it can contain refs to itself (add (n, Local n)) to the enviroment

LetRec how to change vlet

Probably need to lift all Non-triv Lets/LetRecs out Modulo jumps



What are costs of mono-transf
    - probably should eta-Exp The Tm and bind it to Local defs
    - State for - allGlobals, Tail-call, Jumps?

If I get to Lam in tail -> Join otherwise - lift
Similar for Lets

Why is there jump in Case

Any reason for Top being recursive

Case-of-case???
