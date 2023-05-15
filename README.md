libcspm
========

This library provides a [FDR](http://www.fsel.com/software.html)-compliant
parser, type checker and (experimental) evaluator for machine CSP files.

There is also a program, `cspmchecker`, that makes use of this library to
provide command line type checking.

Installation
------------

The simplest method is to install via [Hackage](http://hackage.haskell.org/).
`cabal install cspmchecker` will install cspmchecker and its dependencies.

Otherwise, if you obtain a source distribution then the following commands 
will install `libcspm`.

    cabal configure
    cabal build
    cabal install

To install `cspmchecker`, firstly install `libcspm` as above, then change
directory to `cspmchecker` and run the following commands.

    cabal configure
    cabal build
    cabal install

This should make `cspmchecker` available from your command line shell
(if not check that the location `cabal` installs binaries to is on your 
`$PATH`).

Usage of cspmchecker
----------------------

From a command line shell simply do `cspmchecker file.csp` to type check
the files. Any error messages will be printed out. For example:

    $ cspmchecker ucsexamples/chapter04/abp.csp 
    Checking ucsexamples/chapter04/abp.csp.....
    ucsexamples/chapter04/abp.csp:80:36-39:
        Couldn't match expected type Int.Int with actual type Int
        In the expression: bit
        In the expression: ack == bit
        In the expression: (ack == bit)

Documentation of the type system is forthcoming.

Usage of libcspm
----------------

See http://hackage.haskell.org/package/libcspm for documentation.

Testing
-------

To test `libcspm` run the following commands.

    cabal configure --enable-tests
    cabal build
    cabal test

Bug Reports
-----------

Please files bug reports at https://github.com/tomgr/libcspm/issues. Please
provide a minimal example script that exhibits the error (if possible).
