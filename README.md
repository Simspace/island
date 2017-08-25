**(this is a work in progress, none of what is described below is necessarily implemented yet)**

Island
===

Island extends [vinyl](https://hackage.haskell.org/package/vinyl) to support more shapes in addition to vinyl's flat records. In particular, you can use Island with any existing Plain Old Algebraic Datatype (POAD), you don't need to rewrite your codebase to use our fancy types everywhere.

Shapes
---

Here are the shapes we plan to support:

1.  `Rec :: (t -> *) -> [t] -> *` (record)  
    The classic vinyl shape, indexed by the Functor of your choice, usually Identity, and a type-level list of `t`s, usually types. Any product POAD corresponds to a Rec containing the same fields as the POAD, except each field is wrapped in the chosen Functor.
2.  `CoRec :: (t -> *) -> [t] -> *` (co-record)  
    A more recent vinyl shape, also indexed by a Functor and a type-level list. Any sum POAD corresponds to a CoRec containing the fields of one of the constructors, except the tuple of fields is wrapped in the chosen Functor. It can also be used to represent one of the fields of a product POAD. Conversely, a Rec can be used to represent all of the fields of all the constructors of a sum POAD.
3.  `data Tree a = TreeLeaf a | TreeBranch [Tree a]`  
    `RecTree :: (t -> *) -> Tree t -> *` (record tree)  
    This time the index is a tree of types, not a list. This is useful when some of your record's fields are also records, and you want to work on all the non-record fields at the leaves. As usual, those leaves are wrapped in the chosen Functor.
4.  `CoRecTree :: (t -> *) -> Tree t -> *` (co-record tree)  
    You've guessed it, this is useful when some of your sum's constructors have a single field which is also a sum. A single leaf field is wrapped in the chosen Functor. Can also be used to represent a single leaf field in a record of records. And RecTree can be used to represent all the leaf fields of a sum of sums.
5.  `RecTrie :: (Tree t -> *) -> Tree t -> *` (record trie)  
    All the shapes up to now store their information at the leaves, but this doesn't have to be the case. This shape stores a piece of information at each internal node of the tree, and also at each leaf as usual. The Tree constructor can be used to distinguish those two cases so that different types of information can be stored in those different places.
6.  `CoRecTrie :: (Tree t -> *) -> Tree t -> *` (co-record trie)  
    Either points to an internal node, or to a leaf.
7.  `data SP = Sum | Product`  
    `data Trie e a = TrieLeaf a | TrieBranch e [Trie e a]`  
    `AlgTree :: Trie SP t -> (t -> *) -> *` (algebraic tree)  
    Each node in the tree is either a product or a sum, so the sub-nodes of a product can be sums and vice-versa. This is useful when your POAD isn't a product of products all the way down or a sum of sums all the way down, but you still want to be able to work on the leaves.
8.  `TwistedAlgTree :: (SP -> SP) -> (t -> *) -> Trie SP t -> *` (twisted algebraic tree)  
    You can make your own CoAlgTree by providing a type-level function which flips the SP constructor. Or, by providing a constant function, you can use products or sums everywhere. This is useful if you want to talk about every possible leaf, or point to one possible leaf.
9.  `TwistedAlgTrie :: (SP -> SP) -> (Trie SP t -> *) -> Trie SP t -> *` (twisted algebraic trie)  
    As before, the Trie constructor can be used to distinguish between internal nodes and leaves. In the case of an internal node, the SP constructor can be used to distinguish products and sums, so that different types of information can be stored in each case.  
    There is no AlgTrie, since it can easily be obtained by using the identity function for the `ST -> ST` index.
10. `Structured :: (tP -> *) -> (tS -> *) -> (tL -> *) -> ? tP tS tL -> *`  
    This part of the API isn't nailed down yet, but the idea is that we want to be able to talk about a record with a sum field where one of the constructors has a record field. One idea here is that you might want to provide three Functors: one which says what to do with the products, one which says what to do with the sums, and one which says what to do with the leaves.
11. `CoStructured :: (tP -> *) -> (tS -> *) -> (tL -> *) -> ? tP tS tL -> *`  
    Whatever we end up deciding for Structured, given the other shapes in this list we will probably want an instantiation in which all the products become sums and vice-versa.

Operations
---

Here are the operations we plan to support:

    _Rec            :: IsProduct     poad => Iso' poad (Rec               Identity     (Fields     poad))
    _CoRec          :: IsSum         poad => Iso' poad (CoRec             Identity     (Fields     poad))
    _RecTree        :: IsProductTree poad => Iso' poad (RecTree           Identity     (FieldsTree poad))
    _CoRecTree      :: IsSumTree     poad => Iso' poad (CoRecTree         Identity     (FieldsTree poad))
    _RecTrie        :: IsProductTree poad => Iso' poad (RecTrie           OnlyLeaf     (FieldsTree poad))
    _CoRecTrie      :: IsSumTree     poad => Iso' poad (CoRecTrie         OnlyLeaf     (FieldsTree poad))
    _AlgTree        :: IsAlgTree     poad => Iso' poad (AlgTree           Identity     (FieldsTrie poad))
    _TwistedAlgTree :: IsAlgTree     poad => Iso' poad (TwistedAlgTree Id OnlyLeaf     (FieldsTrie poad))
    _TwistedAlgTrie :: IsAlgTree     poad => Iso' poad (TwistedAlgTrie Id OnlyTrieLeaf (FieldsTrie poad))

    lensRec                 :: LeafPath  as  a -> Lens'      (Rec                f as) (f a)
    prismCoRec              :: LeafPath  as  a -> Prism'     (CoRec              f as) (f a)
    lensRecTree             :: LeafPath  ta  a -> Lens'      (RecTree            f ta) (f a)
    prismCoRecTree          :: LeafPath  ta  a -> Prism'     (CoRecTree          f ta) (f a)
    lensRecTrie             :: InnerPath ta tb -> Lens'      (RecTrie            f ta) (f tb)
    prismCoRecTrie          :: InnerPath ta tb -> Prism'     (CoRecTrie          f ta) (f tb)
    traversalAlgTree        :: LeafPath  ta tb -> Traversal' (AlgTree            f ta) (f tb)
    traversalTwistedAlgTree :: LeafPath  ta tb -> Traversal' (TwistedAlgTree fSP f ta) (f tb)
    traversalTwistedAlgTrie :: InnerPath ta tb -> Traversal' (TwistedAlgTrie fSP f ta) (f tb)

where LeafPath looks like

    Proxy @("lowerRight" :-> "x" :-> Int) :: LeafPath ('TreeBranch '[ "upperLeft" :-> ..., "lowerRight" :-> 'TreeBranch '["x" :-> 'TreeLeaf Int, ...] ]) Int

and InnerPath is a like LeafPath but it can stop at internal nodes as well, it doesn't have to reach a leaf.

The optics for Rec and CoRec are already be provided by vinyl, but are re-exported under this new name for completeness.

Since those shapes are holding values of different types at the leaves, they cannot have a Functor instance. Since all the values are wrapped by the same `f`, however, we can give them a [Functor1](https://www.stackage.org/haddock/lts-8.19/type-combinators-0.2.4.3/Type-Class-Higher.html#t:Functor1) instance, and also [Foldable1](https://www.stackage.org/haddock/lts-8.19/type-combinators-0.2.4.3/Type-Class-Higher.html#t:Foldable1) and [Traversable1](https://www.stackage.org/haddock/lts-8.19/type-combinators-0.2.4.3/Type-Class-Higher.html#t:Traversable1).

    class Functor1 s where
      map1 :: (forall x. f x -> g x) -> s f a -> s g a

    class Foldable1 s where
      foldMap1 :: Monoid m => (forall x. f x -> m) -> s f a -> m

    class Traversable1 s where
      traverse1 :: Applicative m => (forall x. f x -> m (g x)) -> s f a -> m (s g a)

The product shapes, namely Rec, RecTree, RecTrie, and the twisted shapes which use `Const 'Product`, can also be given an Applicative1 instance.

    class Applicative1 s where
      pure1 :: (forall x. f x) -> s f a
      ap1   :: (forall x. f x -> g x -> h x) -> s f a -> s g a -> s h a

We will of course also implement operations for Structured and CoStructured, as soon as we figure out what those shapes look like :)


FAQ
---

Q: Why "Island"?  
A: Vinyl is a wordplay on the musical meaning of the word "record". I want to make a variant of Vinyl based on trees instead of lists. There is a music publishing company called "[Island Records](https://en.wikipedia.org/wiki/Island_Records)" and their logo is a tree.
