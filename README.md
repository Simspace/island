**(this is a work in progress, none of what is described below is necessarily implemented yet)**

Island
===

Island is a collection of generic types and algorithms which are intended to work on your existing POADs (Plain Old Algebraic Datatypes). The intention is to make it easy for you to refactor your data model to match your changing requirements, without having to worry about having to reimplement the following algorithms each time you make a change.

* a generic type for patches, a generic diff algorithm to obtain them, and a generic patching algorithm to apply them
* a generic type for pointing at a deeply-nested field, to locate an error for example

---

For a given set of business types of some application, this kernel provides
*   First class updates
    *   Can take apply an update to an existing value to get a new value
*   First class differences
    *   Can take two values and generate a list of their differences
*   Validation / First class error locations
    *   Given some rules, is a given value 'valid'? If not, generate a list of error locations
*   Reverting diffs 
    *   Given a diff, create an update that would remove the difference from the second value to the first
*   Incremental diffs (Maybe)
    *   If we have the differences between two values, and we receive an update to the second value, can we update the differences without having to recompute them all?
    *   This may be hard or infeasable to do generically
*   Serialization
    *   Values, updates, differences, and error locations can all be serialized
*   Pretty printing
    *   Values, updates, differences, and error locations can all be printed in a user friendly way

FAQ
---

Q: Why "island"?  
A: It's a long story. The original design for Island was based on the [vinyl](http://hackage.haskell.org/package/vinyl) library, whose name is itself a wordplay on the musical meaning of the word "record". Since our design was based on type-level trees instead of vinyl's type-level lists, it made sense to pick a music-themed name related to trees. And there is a music publishing company called "[Island Records](https://en.wikipedia.org/wiki/Island_Records)" whose logo is a tree. Later on the design changed, but the name stuck.  
Then, the design changed again when we realized that we could provide a much simpler API by providing separate implementations for individual features instead of one super-generic implementation subsuming many features. So you could say that island now provides several islands of isolated functionalities instead of a single unwieldy continent!
