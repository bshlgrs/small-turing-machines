# buck-laconic


ghc -O2 -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes src/Friedman.hs

todo:

- write the translation from Core to lambda calculus.
  - This mostly involves changing Core primitives in pretty easy ways. Eg:
    - `let x = y in z` becomes `App (Abs x z) y`
    - Case statements turn into function calls on the object being matched (which can be a bool)
  - There's some trickiness here about generic functions
  - Also there's a bit of subtlety in turning a library into a single lambda expression
- Translate lambda calculus to SKI.
  - I think I've done this before; it's not very hard.
  - I'm worried that it gives you a quadratic increase in program size. But I think there might be ways of getting around that. There are papers about making it only n log(n), which I think is the best you can do.
  - I could experiment with inlining perhaps?
    - or I could have the compiler evaluate every subtree if that would make it smaller?
  - Maybe another combinator calculus is more efficient
- Implement an SKI interpreter on a TM.
- Figure out how to do the TM introspection thing to encode the SKI program efficiently.
