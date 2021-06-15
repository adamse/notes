#include <stdio.h>

typedef void * CB(void);

void * u(CB * cb) {
  printf("calling cb\n");
  void* res = cb();
  if (res != NULL) {
    // exception
    return res;
  }

  printf("cb success\n");
  return NULL;
}
