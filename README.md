# Using Z3 from Haskell

Project `cobaHaskellz3`. This simple project is intended to show some examples of using Haskell Z3-binding. Using this binding allows you to use Z3 SMT Solver from Haskell.

  * The binding is available in Hackage: https://hackage.haskell.org/package/z3
  * Z3-theorem prover has a page here: https://github.com/Z3Prover/z3

To build `cobaHaskellz3` you can just do `cabal build`, which will also trigger the installation of the Z3-binding, if it is not installed yet.

The examples are shown  the module `CobaZ3`. It includes two functions for
checking the satisfiability and validity of a formula. For a demo:

   * Do `cabal repl`. This will load the module `CobaZ3`. From there, just run `test`.

More examples can be found in Hackage-page of Z3 Haskell-binding. Check this location: https://github.com/IagoAbal/haskell-z3/tree/master/examples

#### Installing Z3-binding

* You need the theorem prover Z3 itself. Check its Z3-binding Hackage-page (see above). The binding may not support the latest version of Z3. E.g. the version of Z3-binding that I use only support Z3 version 4.8.x. Install the appropriate version of Z3.

* Then you can install Z3-binding. Check its Hackage-page for instructions how to do this.
