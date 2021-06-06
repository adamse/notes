#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>

#define T int

struct Vec_ {
  T * data;
  int len;
  int cap;
};

typedef struct Vec_ Vec;

Vec * vec_new(int capacity) {
  int cap = capacity > 0 ? capacity : 1;

  T * data = (T *)calloc(cap, sizeof(T));
  if (!data) return NULL;

  Vec * vec = malloc(sizeof(Vec));
  if (!vec) return NULL;

  vec->data = data;
  vec->len = 0;
  vec->cap = cap;
  return vec;
}

void vec_free(Vec * vec) {
  free(vec->data);
  free(vec);
}

int vec_len(Vec * vec) {
  return vec->len;
}


bool vec_grow(Vec * vec) {
  int cap = vec->cap;
  int newcap = cap * 2;
  T * newdata = (T *)reallocarray(vec->data, newcap, sizeof(T));
  if (!newdata) {
    return false;
  }
  vec->cap = newcap;
  vec->data = newdata;
  return true;
}

bool vec_push(Vec * vec, T val) {
  if (vec->len + 1 > vec->cap) {
    if (!vec_grow(vec)) {
      return false;
    }
  }
  vec->data[vec->len] = val;
  vec->len += 1;
}

bool vec_pop(Vec * vec, T * res) {
  if (vec->len < 1) {
    return false;
  }

  vec->len -= 1;
  *res = vec->data[vec->len];
  return true;
}

bool vec_get(Vec * vec, int i, T * out) {
  if (i >= vec->len) {
    return false;
  }
  *out = vec->data[i];
  return true;
}

bool vec_set(Vec * vec, int i, T val) {
  if (i >= vec->len) {
    return false;
  }
  vec->data[i] = val;
  return true;
}

bool vec_resize(Vec * vec, int new_len, T val) {
  if (new_len <= vec->len) {
    vec->len = new_len;
    return true;
  }

  while (vec->cap < new_len) {
    if (!vec_grow(vec)) {
      return false;
    }
  }
  for (int i = vec->len; i < new_len; i++) {
    vec->data[i] = val;
  }
  vec->len = new_len;
  return true;
}

#include <assert.h>
#include <stdio.h>

void test1() {
  Vec * v = vec_new(0);
  assert(v->cap == 1);
  assert(v->len == 0);
  vec_free(v);
  printf("test1 ok\n");
}

void test2() {
  Vec * v = vec_new(1);
  assert(v->cap == 1);
  assert(v->len == 0);
  vec_push(v, 1234);
  assert(v->cap == 1);
  assert(v->len == 1);
  T o;
  bool ok;
  ok = vec_pop(v, &o);
  assert(ok == true);
  assert(o == 1234);
  ok = vec_pop(v, &o);
  assert(ok == false);
  vec_free(v);
  printf("test2 ok\n");
}

void test3() {
  Vec * v = vec_new(1);
  vec_push(v, 1234);
  vec_push(v, 2345);
  assert(v->cap == 2);
  assert(v->len == 2);
  vec_push(v, 3456);
  assert(v->cap == 4);
  assert(v->len == 3);

  T o;
  bool ok;

  // can get what we put in
  ok = vec_get(v, 0, &o);
  assert(ok);
  assert(o == 1234);
  ok = vec_get(v, 1, &o);
  assert(ok);
  assert(o == 2345);
  ok = vec_get(v, 2, &o);
  assert(ok);
  assert(o == 3456);

  // out of bounds
  ok = vec_get(v, 3, &o);
  assert(!ok);
  ok = vec_get(v, 500, &o);
  assert(!ok);

  // can pop what we pushed
  ok = vec_pop(v, &o);
  assert(ok);
  assert(o == 3456);
  ok = vec_pop(v, &o);
  assert(ok);
  assert(o == 2345);
  ok = vec_pop(v, &o);
  assert(ok);
  assert(o == 1234);

  assert(v->cap == 4);
  assert(v->len == 0);

  vec_free(v);
  printf("test3 ok\n");
}

void test4() {
  Vec * v = vec_new(4);
  assert(vec_len(v) == 0);
  vec_resize(v, 0, 0);
  assert(vec_len(v) == 0);

  bool ok = vec_resize(v, 10, 1);
  assert(ok);
  assert(vec_len(v) == 10);
  for (int i = 0; i < 10; i++) {
    int o;
    ok = vec_get(v, i, &o);
    assert(ok);
    assert(o == 1);
  }

  ok = vec_resize(v, 0, 2);
  assert(ok);
  assert(vec_len(v) == 0);

  vec_free(v);
  printf("test4 ok\n");
}

int main(int argc, char ** argv) {
  test1();
  test2();
  test3();
  test4();
}
