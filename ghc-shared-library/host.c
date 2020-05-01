#include <stdio.h>
#include <dlfcn.h>

int main(int argc, char* argv[]) {

  if (argc < 2) {
    printf("Usage: %s ./plugin.so", argv[0]);
    return 0;
  }

  printf("Opening object...\n");
  void* obj = dlopen(argv[1], RTLD_NOW);

  if (!obj) {
    printf("Failed to open object.\n");
    return 1;
  }

  printf("Loading symbols...\n");
  void* plugin_init_fun = dlsym(obj, "plugin_init");

  if (!plugin_init_fun) {
    printf("Failed to find plugin_init.\n");
    return 2;
  }

  void* plugin_deinit_fun = dlsym(obj, "plugin_deinit");

  if (!plugin_deinit_fun) {
    printf("Failed to find plugin_deinit.\n");
    return 2;
  }

  printf("Calling plugin_init...\n");
  ((void (*)())plugin_init_fun)();
  printf("Called plugin_init.\n");

  printf("Calling plugin_deinit...\n");
  ((void (*)())plugin_deinit_fun)();
  printf("Called plugin_deinit.\n");

  if (dlclose(obj) != 0) {
    printf("Failed to close object.\n");
    return 3;
  }

  return 0;
}