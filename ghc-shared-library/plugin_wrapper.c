// plugin.c

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