ghc-make
========

[![Build Status](https://travis-ci.org/ndmitchell/ghc-make.png)](https://travis-ci.org/ndmitchell/ghc-make)

A version of ghc --make which runs faster when there is nothing to rebuild.

#### How do I use it?

Install `ghc-make` (`cabal update && cabal install ghc-make`). Then replace your calls to `ghc --make my arguments` with `ghc-make my arguments` (you can still pass `--make` to `ghc-make`, but it is unnecessary). All arguments and flags GHC supports are supported by `ghc-make` - it is intended as a drop in replacement.

#### What should I see?

If a zero build takes _Z_ seconds, then a build which requires compilation will take an additional _Z_ seconds, and a build which requires no compilation will take almost no time at all (saving you _Z_ seconds).

More concretely, imagine I have a script that runs `ghc --make MyCode && ./MyCode`. Most of the time the source does not change. Running `ghc --make` when there is nothing to do takes 5 seconds (I have projects that are as high as 23 seconds). If I switch to `ghc-make MyCode && ./MyCode` then when there are no changes the compilation takes a mere 0.21s (of which 0.18s is process spawning overhead). If there are changes to be made, it will take recompilation time plus an additional 5 seconds.

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
