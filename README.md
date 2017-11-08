# ghc-make [![Hackage version](https://img.shields.io/hackage/v/ghc-make.svg?label=Hackage)](https://hackage.haskell.org/package/ghc-make) [![Linux Build Status](https://img.shields.io/travis/ndmitchell/ghc-make.svg?label=Linux%20build)](https://travis-ci.org/ndmitchell/ghc-make)

An alternative to `ghc --make` which supports parallel compilation of modules and runs faster when nothing needs compiling.

#### How do I use it?

Install `ghc-make` (`cabal update && cabal install ghc-make`). Then replace your calls to `ghc my -arguments` with `ghc-make my -arguments`. Almost all arguments and flags supported by `ghc` are supported by `ghc-make` - it is intended as a drop-in replacement.

#### What should I see?

Imagine you have a script that runs `ghc --make MyCode && ./MyCode` and that running `ghc --make` when nothing needs compiling takes 5 seconds (I have projects that take as long as 23 seconds). If you switch to `ghc-make MyCode && ./MyCode` then when nothing needs compiling it will take almost no time (less than 0.2 seconds). If things need compiling it will take the compilation time plus the time with `ghc --make` when nothing needs compiling (in this example, 5 seconds extra). If the source changes on less than half the executions you will see a speedup.

The `ghc-make` program produces a handful of metadata files which are stored with the `.ghc-make` prefix. These files will be placed in the current directory, or the `-hidir`/`-odir` directory if specified.

#### How do I turn on parallel module compilation?

Pass `-j4` to build using 4 cores. In my experience you usually need a parallel factor of 3x to match `ghc --make` on a single core, since `ghc --make` does a lot of caching that is unavailable to `ghc-make`.

To use `ghc-make` with Cabal, try `cabal build --with-ghc=ghc-make --ghc-options=-j4`. (This technique is due to the [`ghc-parmake`](https://github.com/23Skidoo/ghc-parmake) project, which also does parallel `ghc --make` compiles.)

#### What GHC features are unsupported?

Anything not captured by `ghc -M` will not be tracked, including dependencies registered by Template Haskell and `#include` files.

#### Why is it faster?

When GHC does a compilation check it runs any preprocessors and parses the Haskell files, which can be slow. When `ghc-make` does a compilation check it reads a list of file names and modification times from a database and checks the times still match, and if they do, it does nothing.

#### Why is it slower?

When things have changed `ghc-make` also runs `ghc-pkg list` and `ghc -M` to get a list of dependencies. To produce that list, GHC has to run any preprocessors and parse the Haskell files. If GHC was able to produce the dependencies while building (as `gcc` is able to do) then `ghc-make` would never be noticeably slower.

#### How is it implemented?

This program uses the [Shake library](https://github.com/ndmitchell/shake#readme) for dependency tracking and `ghc --make` for building.

To pass options to the underlying Shake build system prefix them with `--shake`, for example `--shake--report=-` will write a profile report to stdout and `--shake--help` will list the available Shake options.

#### Should GHC just use Shake directly?

Should _large and important project_ use _authors pet library_? Yes, of course :smiley:. If `ghc --make` used Shake it is likely their builds with no recompilation would be just as fast as `ghc-make`, and they could take advantage of parallel compilation with no additional overhead. However, integrating Shake into such a large code base would be a lot of work - perhaps you should offer to help the GHC team?
