ghc-make
========

[![Build Status](https://travis-ci.org/ndmitchell/ghc-make.png)](https://travis-ci.org/ndmitchell/ghc-make)

A version of `ghc --make` which completes faster if there is nothing to rebuild.

#### How do I use it?

Install `ghc-make` (`cabal update && cabal install ghc-make`). Then replace your calls to `ghc --make _my -arguments_` with `ghc-make _my -arguments_` (you can still pass `--make` to `ghc-make`, but it is unnecessary). All arguments and flags supported by `ghc --make` are supported by `ghc-make` - it is intended as a drop in replacement.

#### What should I see?

Imagine you have a script that runs `ghc --make MyCode && ./MyCode` and that running `ghc --make` when there is nothing to do takes 5 seconds (I have projects that are as high as 23 seconds). If you switch to `ghc-make MyCode && ./MyCode` then when there is nothing to do it will take almost no time (0.2 seconds would be on the high side). If things need recompiling it will take the recompilation time plus the time for `ghc --make` to do nothing (so 5 seconds extra).

#### What GHC features are unsupported?

Template Haskell files that register additional file dependencies during execution these will not cause `ghc-make` to rebuild. More generally, anything not captured by `ghc -M` will not cause a rebuild.

#### Why is it faster?

When GHC does a compilation check it runs any preprocessors and parses the Haskell files, which can be slow. When `ghc-make` does a compilation check it reads a list of file/modification times from a database then checks the files have the same times, if they do, it does nothing.

#### Why is it slower?

When `ghc-make` has to do a build it also runs `ghc -M` to generate a makefile so it has a current list of all dependencies. To produce that list, GHC has to run all preprocessors and parse all Haskell files. If GHC was able to produce a makefile while building (as `gcc` is able to do) then `ghc-make` would only ever be very slightly slower.

#### Does `ghc-make` provide parallel builds?

No, but [ghc-parmake](http://hackage.haskell.org/package/ghc-parmake) does. However, generally I find you need a parallel factor of 3x to match `ghc --make` on a single core, since `ghc --make` does a lot of caching that is unavailable to `ghc-parmake`.

#### How it it implemented?

The majority of the work is performed by the [Shake library](https://github.com/ndmitchell/shake), and everything is expressed as standard build-system dependencies.

#### Should `ghc --make` just switch to Shake directly?

If `ghc --make` used Shake it is likely their nothing to do builds would be just as fast as `ghc-make`, and with additional work they could take advantage of parallel compilation. Of course, given a large existing code base, it could be a complex project.
