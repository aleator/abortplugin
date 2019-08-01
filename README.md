# AbortPlugin

This GHC plugin allows compilation to abort with an
error if any non-whitelisted extension, or import is used.

# Usage

This is a ghc plugin. The general pattern of usage is to

1. Add `AbortPlugin` as a dependency to your project. 
2. Create a limitations file in style of the [the default one](Limitations.dhall), which contains whitelists of allowed imports and modules.
3. Invoke GHC with

         ghc -- -Wall -fplugin=AbortPlugin -fplugin-opt=AbortPlugin:Limitations.dhall YourFile.hs    

# Limitations

This does **not** provide compilation safety! GHC allows options
such as `-F -pgmF ./something_nasty` which cannot be intercepted by
a compiler plugin.

A modicum of compiler safety can be obtained by careful
whitelisting of extensions and modules and checking that the
source file nor any of it's transitive imports does not contain
the string `OPTIONS_GHC` (or any upper/lowercase permutation of
it).
