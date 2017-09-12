**(this is a work in progress, none of what is described below is necessarily implemented yet)**

Island
===

FAQ
---

Q: Why "island"?  
A: It's a long story. The original design for Island was based on the [vinyl](http://hackage.haskell.org/package/vinyl) library, whose name is itself a wordplay on the musical meaning of the word "record". Since our design was based on type-level trees instead of vinyl's type-level lists, it made sense to pick a music-themed name related to trees. And there is a music publishing company called "[Island Records](https://en.wikipedia.org/wiki/Island_Records)" whose logo is a tree. Later on the design changed, but the name stuck.  
Then, the design changed again when we realized that we could provide a much simpler API by providing separate implementations for individual features instead of one super-generic implementation subsuming many features. So you could say that island now provides several islands of isolated functionalities instead of a single unwieldy continent!
