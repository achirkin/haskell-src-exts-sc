# haskell-src-exts-sc

The library generates code from `haskell-src-exts` AST.
The procedure is as follows:

  1. pretty-print AST generated by `haskell-src-exts`
  2. parse the generated code to get `SrcSpanInfo` for each node
  3. combine AST annotated with comments and AST annotated with `SrcSpanInfo`
  4. insert (non-empty) comments into each node, updating `SrcSpanInfo` of all nodes
  5. profit!

As you can see, the algorithm is nor remarkably fast, because it prints and parses code
and modifies `SrcSpanInfo` of all nodes in an AST on each comment insertion.
On the good side, it is quite flexible and compatible with many versions of `haskell-src-exts`.
Performance is also acceptable if you don't need to invoke it every millisecond.