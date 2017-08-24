**(this is a work in progress, none of what is described below is necessarily implemented yet)**

Island
===

Island extends [vinyl](https://hackage.haskell.org/package/vinyl) to support more shapes in addition to vinyl's flat records. In particular, you can use Island with any existing Plain Old Algebraic Datatype (POAD), you don't need to rewrite your codebase to use our fancy types everywhere.

Shapes
---

Here are the shapes we plan to support:

1. `Rec :: HList t -> (t -> *) -> *` (record)  
    The classic vinyl shape, indexed by an HList and the Functor of your choice, usually Identity. Any product POAD corresponds to a Rec containing the same fields as the POAD, except each field is wrapped in the chosen Functor.
2. `CoRec :: HList t -> (t -> *) -> *` (co-record)  
    A more recent vinyl shape, also indexed by an HList and a Functor. Any sum POAD corresponds to a CoRec containing the argument of one of the constructors, except the argument is wrapped in the chosen Functor. It can also be used to represent one of the fields of a product POAD. And a Rec can be used to represent all of the arguments of all the constructors of a sum POAD!
3. `RecTree :: HTree t -> (t -> *) -> *` (record tree)  
    This time the index is a tree of types, not a list. This is useful when some of your record's fields are also records, and you want to work on all the non-record fields at the leaves. As usual, those leaves are wrapped in the chosen Functor.
4. `CoRecTree :: HTree t -> (t -> *) -> *` (co-record tree)  
    You've guessed it, this is useful when some of your constructors have arguments which are also sums. A single leaf alternative is wrapped in the chosen Functor. Can also be used to represent a single leaf field in a record of records. And RecTree can be used to represent all the leaf alternatives of a sum of sums.
5. `Alg :: ? tP tS tL -> (tP -> *) -> (tS -> *) -> (tL -> *) -> *` (algebraic)  
    This part of the API isn't nailed down yet, but the idea is that we want to be able to talk about a record with a sum field where one of the constructors has a record argument. One idea here is that you might want to provide three Functors: one which says what to do with the products, one which says what to do with the sums, and one which says what to do with the leaves.
6. `CoAlg :: ? tP tS tL -> (tP -> *) -> (tS -> *) -> (tL -> *) -> *` (co-algebraic)  
    Whatever we end up deciding for Alg, given the other shapes in this list we will probably want an instantiation in which all the products become sums and vice-versa.

Operations
---

Here are the operations we plan to support:

    _Rec       :: IsProduct     poad => Iso' poad (Rec       (Fields poad))
    _CoRec     :: IsSum         poad => Iso' poad (CoRec     (Fields poad))
    _RecTree   :: IsProductTree poad => Iso' poad (RecTree   (FieldsTree poad))
    _CoRecTree :: IsSumTree     poad => Iso' poad (CoRecTree (FieldsTree poad))

    lensPath  :: Path ta a -> Lens'  (RecTree   ta) a
    prismPath :: Path ta a -> Prism' (CoRecTree ta) a

where `Path` looks like

    Proxy @("lowerRight" :-> "x" :-> Int) :: Path ('Branch '[ "upperLeft" :-> ..., "lowerRight" :-> 'Branch '["x" :-> 'Leaf Int, ...] ]) Int

The optics for Rec and CoRec should already be provided by vinyl.

Other than that, we plan to add whatever is the equivalent of `fmap`, `zip` and `traverse` for each shape:

    hoistRec       :: (forall x. f x -> g x) -> Rec          f as -> Rec          g as
    hoistCoRec     :: (forall x. f x -> g x) -> CoRec        f as -> CoRec        g as
    hoistRecTree   :: (forall x. f x -> g x) -> RecTree      f ta -> RecTree      g ta
    hoistCoRecTree :: (forall x. f x -> g x) -> CoRecTree    f ta -> CoRecTree    g ta

    zipRecsWith     :: (forall x. f x -> g x -> h x) -> Rec     f as -> Rec     g as -> Rec     h as
    zipRecTreesWith :: (forall x. f x -> g x -> h x) -> RecTree f ta -> RecTree g ta -> RecTree h ta

    traverseRec       :: Applicative h => (forall x . f x -> h (g x)) -> Rec       f as -> h (Rec       g as)
    traverseCoRec     :: Applicative h => (forall x . f x -> h (g x)) -> CoRec     f as -> h (CoRec     g as)
    traverseRecTree   :: Applicative h => (forall x . f x -> h (g x)) -> RecTree   f ta -> h (RecTree   g ta)
    traverseCoRecTree :: Applicative h => (forall x . f x -> h (g x)) -> CoRecTree f ta -> h (CoRecTree g ta)

From those, it should be easy to implement folds, etc.

We will of course also implement operations for Alg and CoAlg, as soon as we figure out what those shapes look like :)


FAQ
---

Q: Why "Island"?  
A: Vinyl is a wordplay on the musical meaning of the word "record". I want to make a variant of Vinyl based on trees instead of lists. There is a music publishing company called "[Island Records](https://en.wikipedia.org/wiki/Island_Records)" and their logo is a tree.
