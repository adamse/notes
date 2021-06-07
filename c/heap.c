#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>

// define element type
#define T int
// compare two Ts
#define heap_cmp(a, b) ((a) < (b))
// define to allow growth
#define HEAP_ALLOW_GROW

// for heap_dump
#define HEAP_VAL_FMT "%d"
#define HEAP_VAL_ACCESS(x) (x)

void heap_swap(T * a, T * b) {
  T tmp = *a;
  *a = *b;
  *b = tmp;
}

struct Heap_ {
  T * data;
  int len;
  int cap;
};

typedef struct Heap_ Heap;

#define heap_parent_idx(i) (((i) - 1) / 2)
#define heap_left_child_idx(i) (2 * (i) + 1)
#define heap_right_child_idx(i) (2 * (i) + 2)

Heap * heap_new(int capacity) {
  int cap = capacity > 0 ? capacity : 1;

  T * data = (T *)calloc(cap, sizeof(T));
  if (!data) return NULL;

  Heap * h = malloc(sizeof(Heap));
  if (!h) return NULL;

  h->data = data;
  h->len = 0;
  h->cap = cap;
  return h;
}

void heap_clear(Heap * h) {
  h->len = 0;
}

void heap_free(Heap * h) {
  free(h->data);
  free(h);
}

bool heap_grow(Heap * h) {
  int newcap = h->cap * 2;
  T * newdata = (T *)reallocarray(h->data, newcap, sizeof(T));
  if (newdata == NULL) {
    return false;
  }
  h->cap = newcap;
  h->data = newdata;
  return true;
}

void heap_dump(Heap * h) {
  printf("H: (%d/%d) ", h->len, h->cap);
  for (int i = 0; i < h->len; i++) {
    printf(HEAP_VAL_FMT ", ", HEAP_VAL_ACCESS(h->data[i]));
  }
  printf("\n");
}

bool heap_empty(Heap * h) {
  return h->len == 0;
}

void heap_fixup(Heap * h, int hole, T val) {
  while (hole != 0) {
    int parent_idx = heap_parent_idx(hole);
    T parent = h->data[parent_idx];

    if (heap_cmp(parent, val)) {
      // found the right place
      break;
    } else {
      // push parent down
      h->data[hole] = parent;
      hole = parent_idx;
    }
  }

  // write new value to it's right place
  h->data[hole] = val;
}

void heap_fixdown(Heap * h, int pos) {
  while (1) {
    int left = heap_left_child_idx(pos);
    int right = heap_right_child_idx(pos);

    int smallest = pos;

    if (left < h->len && heap_cmp(h->data[left], h->data[pos])) {
      smallest = left;
    }

    if (right < h->len && heap_cmp(h->data[right], h->data[smallest])) {
      smallest = right;
    }

    if (smallest != pos) {
      heap_swap(h->data + pos, h->data + smallest);
      pos = smallest;
    } else {
      break;
    }
  }
}

bool heap_insert(Heap * h, T val) {
  if (h->len == h->cap) {
#if defined(HEAP_ALLOW_GROW)
    if (!heap_grow(h)) {
      return false;
    }
#else
    return false;
#endif
  }
  int hole = h->len;
  h->len += 1;
  heap_fixup(h, hole, val);
  return true;
}

// peek at first element
bool heap_peek(Heap * h, T * out) {
  if (h->len == 0) {
    return false;
  }

  if (out) {
    *out = h->data[0];
  }
  return true;
}

// extract first element
bool heap_extract(Heap * h, T * out) {
  if (h->len == 0) {
    return false;
  }
  if (out) {
    *out = h->data[0];
  }
  h->data[0] = h->data[h->len - 1];
  h->len -= 1;
  heap_fixdown(h, 0);
  return true;
}

// fused extract and insert
bool heap_replace(Heap * h, T in, T * out) {
  if (h->len == 0) {
    return false;
  }
  if (out) {
    *out = h->data[0];
  }
  h->data[0] = in;
  heap_fixdown(h, 0);
  return true;
}

#include <assert.h>
#include <stdio.h>

void test1() {
  bool ok;
  T o;
  Heap * h = heap_new(2);
  assert(h->len == 0);

  for (int i = 0; i < 12; i++) {
    assert(heap_insert(h, 12 - i));
  }

  for (int i = 0; i < 12; i++) {
    assert(heap_extract(h, &o));
    assert(o == i + 1);
  }

  heap_clear(h);

  ok = heap_peek(h, &o);
  assert(!ok);

  ok = heap_insert(h, 2);
  assert(ok);

  ok = heap_peek(h, &o);
  assert(ok);
  assert(o == 2);

  ok = heap_insert(h, 1);
  assert(ok);

  ok = heap_peek(h, &o);
  assert(ok);
  assert(o == 1);

  ok = heap_extract(h, &o);
  assert(ok);
  assert(o == 1);

  ok = heap_extract(h, &o);
  assert(ok);
  assert(o == 2);

  heap_free(h);
  printf("test1 ok\n");
}

void test2() {
  Heap * h = heap_new(10);
  assert(h != NULL);

  assert(heap_insert(h, 4));
  assert(heap_insert(h, 3));
  assert(heap_insert(h, 2));
  assert(heap_insert(h, 1));

  T o;

  assert(heap_replace(h, 5, &o));
  assert(o == 1);
  assert(heap_extract(h, &o));
  assert(o == 2);
  assert(heap_insert(h, 1));
  assert(heap_extract(h, &o));
  assert(o == 1);

  heap_free(h);
  printf("test2 ok\n");
}

int main(int argc, char ** argv) {
  test1();
  test2();
}
