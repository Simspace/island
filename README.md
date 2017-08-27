**(this is a work in progress, none of what is described below is necessarily implemented yet)**

Island
===

Island extends [vinyl](https://hackage.haskell.org/package/vinyl) to support tree shapes in addition to vinyl's flat records and co-records. Don't worry, you can use Island with your existing Plain Old Algebraic Datatype (POAD), you don't need to rewrite your codebase to use our fancy types everywhere.

Vinyl Recap
---

Vinyl's `HList` is a generalized tuple with N fields. It is parameterized by a list of types `ts`. If `fold` worked at the type level, we could define it as follows.

    type HList ts = fold (,) () ts

Vinyl's `Rec` is a generalized tuple with N fields, and its `CoRec` is a generalized Either with N alternatives. We will use "shape" as a generic term for either `Rec` or `CoRec`, and the word "pieces" as a generic term for that shape's fields or alternatives, as appropriate. In addition to `ts`, those two shapes have an extra type parameter `f` which they apply to every piece. If `fmap` also worked at the type level, we could define them as follows.

    type Rec   f ts = fold (,)    ()   (fmap f ts)
    type CoRec f ts = fold Either Void (fmap f ts)

This extra `f` makes it possible to manipulate all the pieces uniformly even though they have different types. For example, we cannot use `fmap show` to convert an `HList '[Int, Double]` into an `HList '[String, String]`, but we can use `rmap` to convert a `Rec (Dict Show) '[Int, Double]` into a `Rec (Const String) '[Int, Double]`.

    -- |
    -- >>> fmap show xs
    -- error: Couldn't match kind [*] with *
    xs :: HList '[Int, Double]
    xs = 42 :& 0.5 :& RNil

    -- |
    -- >>> rmap (\case {Dict x -> Const (show x)}) ys
    -- Const "42" :& Const "0.5" :& RNil
    ys :: Rec (Dict Show) '[Int, Double]
    ys = Dict 42 :& 0.5 :& RNil

Vinyl Limitations
---

Suppose we're writing a color picker.

    type RGB f = Rec f '[Word8, Word8, Word8]  -- [red, green, blue]

Our color picker might have a color wheel, plus individual sliders and textboxes for each color component. One thing we might be interested in is whether each component has been changed or not. We can represent this information using a value of type `RGB (Const Bool)`.

So far so good. Now suppose we want to create an editor for gradients.

    type Gradient f = Rec f '[RGB f, RGB f, Bool]  -- [start color, end color, isCircular]

We are again interested in the changed fields, and it seems like we should be able to represent this information using a value of type `Gradient (Const Bool)`. But can we?

    type RGB      (Const Bool) = ( Const Bool Word8
                                 , Const Bool Word8
                                 , Const Bool Word8
                                 )
    type Gradient (Const Bool) = ( Const Bool (RGB (Const Bool))
                                 , Const Bool (RGB (Const Bool))
                                 , Const Bool Bool
                                 )

Unfortunately, the `Const Bool` around each `RGB (Const Bool)` means we'll only have a single `Bool` describing each `RGB` field, instead of the three `Bool`s per `RGB` field we would like to have. This is because Vinyl is designed to work on flat records whose data is in their immediate fields, not on records whose fields contain nested records.

Vinyl allows us to work on pieces uniformly, as long as those pieces are the immediate children of a record or a sum. Island improves upon Vinyl by providing new shapes based on a tree of types instead of a flat list of types. This allows us to uniformly manipulate the `Word8`s and the `Bool` even though they live at different depths.

Shapes
---

Here are the shapes we plan to support:

1.  `Rec :: (t -> *) -> [t] -> *` (record)  
    The classic vinyl shape, indexed by the Functor of your choice, usually Identity, and a type-level list of `t`s, usually types. Any product POAD corresponds to a `Rec` containing the same fields as the POAD, except each field is wrapped in the chosen Functor.
2.  `CoRec :: (t -> *) -> [t] -> *` (co-record)  
    A more recent vinyl shape, also indexed by a Functor and a type-level list. Any sum POAD corresponds to a `CoRec` containing the fields of one of the constructors, except the tuple of fields is wrapped in the chosen Functor. It can also be used to represent one of the fields of a product POAD. Conversely, a `Rec` can be used to represent all of the fields of all the constructors of a sum POAD.
3.  `data Tree a = TreeLeaf a | TreeBranch [Tree a]`  
    `RecTree :: (t -> *) -> Tree t -> *` (record tree)  
    This time the index is a tree of types, not a list. This is useful when some of your record's fields are also records, and you want to work on all the non-record fields at the leaves. As usual, those leaves are wrapped in the chosen Functor.
4.  `CoRecTree :: (t -> *) -> Tree t -> *` (co-record tree)  
    You've guessed it, this is useful when some of your sum's constructors have a single field which is also a sum. A single leaf field is wrapped in the chosen Functor. Can also be used to represent a single leaf field in a record of records. And `RecTree` can be used to represent all the leaf fields of a sum of sums.
5.  `RecTrie :: (Tree t -> *) -> Tree t -> *` (record trie)  
    All the shapes up to now store their information at the leaves, but this doesn't have to be the case. This shape stores a piece of information at each internal node of the tree, and also at each leaf as usual. The `Tree` constructor can be used to distinguish those two cases so that different types of information can be stored in those different places.
6.  `CoRecTrie :: (Tree t -> *) -> Tree t -> *` (co-record trie)  
    Either points to an internal node, or to a leaf.
7.  `data SP = Sum | Product`  
    `data Trie e a = TrieLeaf a | TrieBranch e [Trie e a]`  
    `AlgTree :: Trie SP t -> (t -> *) -> *` (algebraic tree)  
    Each node in the tree is either a product or a sum, so the sub-nodes of a product can be sums and vice-versa. This is useful when your POAD isn't a product of products all the way down or a sum of sums all the way down, but you still want to be able to work on the leaves.
8.  `TwistedAlgTree :: (SP -> SP) -> (t -> *) -> Trie SP t -> *` (twisted algebraic tree)  
    You can make your own `CoAlgTree` by providing a type-level function which flips the `SP` constructor. Or, by providing a constant function, you can use products or sums everywhere. This is useful if you want to talk about every possible leaf, or point to one possible leaf.
9.  `TwistedAlgTrie :: (SP -> SP) -> (Trie SP t -> *) -> Trie SP t -> *` (twisted algebraic trie)  
    As before, the `Trie` constructor can be used to distinguish between internal nodes and leaves. In the case of an internal node, the `SP` constructor can be used to distinguish products and sums, so that different types of information can be stored in each case.  
    There is no `AlgTrie`, since it can easily be obtained by using the identity function for the `ST -> ST` index.

POADs
---

Here are the conversions we plan to support:

    _Rec            :: IsProduct     poad => Iso' poad (Rec               Identity     (Fields     poad))
    _CoRec          :: IsSum         poad => Iso' poad (CoRec             Identity     (Fields     poad))
    _RecTree        :: IsProductTree poad => Iso' poad (RecTree           Identity     (FieldsTree poad))
    _CoRecTree      :: IsSumTree     poad => Iso' poad (CoRecTree         Identity     (FieldsTree poad))
    _RecTrie        :: IsProductTree poad => Iso' poad (RecTrie           OnlyLeaf     (FieldsTree poad))
    _CoRecTrie      :: IsSumTree     poad => Iso' poad (CoRecTrie         OnlyLeaf     (FieldsTree poad))
    _AlgTree        :: IsAlgTree     poad => Iso' poad (AlgTree           Identity     (FieldsTrie poad))
    _TwistedAlgTree :: IsAlgTree     poad => Iso' poad (TwistedAlgTree Id OnlyLeaf     (FieldsTrie poad))
    _TwistedAlgTrie :: IsAlgTree     poad => Iso' poad (TwistedAlgTrie Id OnlyTrieLeaf (FieldsTrie poad))

Type Classes
---

Since those shapes are holding values of different types at the leaves, they cannot have a Functor instance. Since all the values are wrapped by the same `f`, however, we can give them a [`Functor1`](https://www.stackage.org/haddock/lts-8.19/type-combinators-0.2.4.3/Type-Class-Higher.html#t:Functor1) instance, and also [`Foldable1`](https://www.stackage.org/haddock/lts-8.19/type-combinators-0.2.4.3/Type-Class-Higher.html#t:Foldable1) and [`Traversable1`](https://www.stackage.org/haddock/lts-8.19/type-combinators-0.2.4.3/Type-Class-Higher.html#t:Traversable1).

    class Functor1 s where
      map1 :: (forall x. f x -> g x) -> s f a -> s g a

    class Foldable1 s where
      foldMap1 :: Monoid m => (forall x. f x -> m) -> s f a -> m

    class Traversable1 s where
      traverse1 :: Applicative m => (forall x. f x -> m (g x)) -> s f a -> m (s g a)

The product shapes, namely `Rec`, `RecTree`, `RecTrie`, and the twisted shapes which use `Const 'Product`, can also be given an `Applicative1` instance.

    class Applicative1 s where
      pure1 :: (forall x. f x) -> s f a
      ap1   :: (forall x. f x -> g x -> h x) -> s f a -> s g a -> s h a

The other shapes all have a corresponding product shape into which they can be cast in order to use `Applicative1`.

    class Productizable s where
      type Productized s
      productize :: (forall x. Maybe (f x) -> g x) -> s f a -> Productized s g a

Paths
---

Each shape has a number of positions at which they may hold some information. A `Path` describes one such position using a sequence of field names from the root of the data structure.

    class Structured1 s where
      type Path s
      type Optic s
      withPath :: (forall x. Path s x -> f x -> g x) -> s f a -> s g a
      atPath   :: Path s b -> Optic s (s f a) (f b)

A `Path` can be converted into an `Optic` pointing at the value inside the corresponding position. This `Optic` must be at least as powerful as a `Traversal'`, and can be stronger if the shape guarantees that the position is always present or that a single value at that position is sufficient to reconstruct the entire shape. In particular, the product shapes use a `Lens'`, while the sum shapes use a `Prism'`.

The `Path` typically consists of a sequence of `PathSegment`s, each of which corresponds to a field name.

    class Structured s where
      type PathSegment s a
      type Optic' s
      atPathSegment :: PathSegment s a -> Optic' s s a

FAQ
---

Q: Why "Island"?  
A: Vinyl is a wordplay on the musical meaning of the word "record". I want to make a variant of Vinyl based on trees instead of lists. There is a music publishing company called "[Island Records](https://en.wikipedia.org/wiki/Island_Records)" and their logo is a tree.
