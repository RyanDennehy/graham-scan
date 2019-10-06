# graham-scan
[Graham Scan on Wikipedia](https://en.wikipedia.org/wiki/Graham_scan)

The Graham Scan algorithm is a way to find a convex hull given a set of points. Refer to the Wikipedia page for a detailed description of a convex hull, but as a simple explanation: imagine there are a set of pegs sticking out of a board, and imagine that you stretch a rubber band around the outside of all the pegs.

I am implementing the Graham Scan algorithm in Haskell as a way of learning the language. Implementing the Graham Scan algorithm is given as an exercise at the end of Chapter 3 of Real World Haskell by Bryan O'Sullivan, John Goerzen, and Don Stewart (© 2009). This is an abrupt spike in complexity from the previous questions which are still easing the reader into the language, but my curiosity was piqued.

The Haskell code is a <pre>cabal</pre> project, so it should be runnable by navigating to the haskell/ directory in a terminal and entering <pre>cabal run</pre>. Unit tests will require additional packages.

After Haskell I may implement the algorithm in other languages.
