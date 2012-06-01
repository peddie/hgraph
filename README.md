hgraph
===========

hgraph is a Haskell library for heterogeneous, fully statically typed
graphs whose nodes can perform computations on the node data of their
predecessors.

Installation
------------

        git clone git://github.com/peddie/hgraph.git
        cd hgraph
        cabal install

Features
------------

-   a heterogeneously typed `HList` akin to Oleg's one, but this one
    uses associated type families rather than functional dependencies
    and undecideable instances.  These have a convenient type operator
    syntax.

-   an `HGraph` class for heterogenous, statically typed directed
    graphs, and an `HGraphApply` subclass for acyclic `HGraph`s whose
    nodes perform computations on the node data of their predecessors.

-   two example `HGraphApply` instances, one built directly on `HList`s
    and the other built on a new `HNode` data type.

-   `Data` and `Typeable` instances for everything, so you can scrap
    your boilerplate and your zippers.

Misfeatures and Bugs
------------

-   No graph algorithms yet; all you can do is build and inspect graphs
    and apply graph nodes.  You can't apply entire graphs yet either.

-   No unit tests yet.

-   Type errors are, for the most part, intimidating and inscrutable.

-   The library doesn't implement everything in Oleg's `HList` yet,
    although most of it should be straightforward.

-   The typechecker gets upset about ambiguity, so you have to tell it
    the types of literals and of some polymorphic functions.  I expect
    this to be less of a problem when the graph has all your own types.
