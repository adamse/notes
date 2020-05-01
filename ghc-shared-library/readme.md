# Creating a shared library with GHC


I'm writing a plugin for a C++ application in GHC Haskell. The application
uses shared library (.so's) for plugins. This is a fairly minimal example of
how you can create a shared library from Haskell code using GHC.


Let's start with a bit of Haskell, the plugin will have an initialisation
function, for simplicity this is all the plugin will do.


```haskell
-- Plugin.hs
{-# language ForeignFunctionInterface #-}
module Plugin (init) where

initialise :: IO ()
initialise = do
  putStrLn "Initialising plugin in Haskell."
  putStrLn "... doing some work :)"
  putStrLn "Done initialising plugin in Haskell."

foreign export ccall initialise :: IO ()
```


We compile this module with GHC: `ghc -c -dynamic -fPIC Plugin.hs`.
`-dynamic` tells GHC generate code for dynamic linking and `-fPIC` tells GHC
to generate position independent code that is suitable to put into a shared
library.


Compilation produces two files of interest to us: `Plugin.o` containing the
compiled code and `Plugin_stub.h` containing a C declaration for calling the
`foreign export` of `initialise`:


```c
// Plugin_stub.h
#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
extern void initialise(void);
#ifdef __cplusplus
}
#endif
```


To make the plugin self contained and allowing the plugin host to be GHC
agnostic we create a simple C wrapper to initialise the GHC runtime and call
our Haskell code. We also include a cleanup function to allow the GHC rts to
clean up after itself:


```c
// plugin_wrapper.c

#include <stdio.h>

// functions to interact with GHC Haskell from C
#include "HsFFI.h"

#include "Plugin_stub.h"

void plugin_init() {
  int argc = 0;
  char* argv[] = {NULL};
  char** argp = argv;
  hs_init(&argc, &argp);
  initialise();
}

void plugin_deinit() {
  hs_exit();
}
```


We compile this using GHC: `ghc -c plugin_wrapper.c`. It might seem a bit
strange to compile C code with GHC, but it makes life easier as GHC knows how
to call your C compiler with the right flags to find the `HsFFI.h` include.


Finally we can tie this together into our shared library: `ghc -c -dynamic
-shared -lHSrts_thr-ghc8.8.3 Plugin.o plugin_wrapper.o -o plugin.so`. Here's
what this means:


- `-dynamic`: link against shared libraries (ie use the .so for base)
- `-shared`: produce a shared library as output
- `-lHSrts_thr-ghc8.8.3`: when producing a shared library GHC doesn't link the RTS by default.
    Since we are creating a shared library that is supposed to stand by
    itself we need to explicitly tell GHC to link the RTS, we choose to use
    the threaded runtime here.


To validate our work we write a simple host process which uses `dlopen` to
load the shared library and `dlsym` to find the initialisation and
deinititialisation functions (see [`host.c`][host.c] for the code).

And we have successfully created a shared library from our Haskell module:

```
$ ./host ./plugin.so
Opening object...
Loading symbols...
Calling plugin_init...
Initialising plugin in Haskell.
... doing some work :)
Done initialising plugin in Haskell.
Called plugin_init.
Calling plugin_deinit...
Called plugin_deinit.
```

This [repository][] contains the complete code and a Makefile tying it all together.

[host.c]: host.c
[repository]: https://github.com/adamse/notes/tree/master/ghc-shared-library