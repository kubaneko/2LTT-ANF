# Notes for the project

- De bruijn

Required transformations

ANF requires name generation while evaluating?

VApp f - if f is VLam or VGlob then we transform to VApp Join newName f

We propagate Joins,Lets up

When we get VApp f u and u is not Atomic we let-bind it

We need to track number of arguments in Intermediate representation of Jump - if we get to another decrement it.


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
