#include "zmc_internal.h"

// Purpose: Small shared utilities for zmc (error handling, allocation helpers, byte buffer, file IO).

void die(const char* msg) {
  fprintf(stderr, "zmc: %s\n", msg);
  exit(2);
}

void* xmalloc(size_t n) {
  void* p = malloc(n);
  if (!p) die("out of memory");
  return p;
}

void* xrealloc(void* p, size_t n) {
  void* q = realloc(p, n);
  if (!q) die("out of memory");
  return q;
}

void bb_init(ByteBuf* b) {
  b->data = NULL;
  b->len = 0;
  b->cap = 0;
}

void bb_free(ByteBuf* b) {
  free(b->data);
  b->data = NULL;
  b->len = 0;
  b->cap = 0;
}

void bb_reserve(ByteBuf* b, size_t need) {
  if (need <= b->cap) return;
  size_t new_cap = b->cap ? b->cap : 16;
  while (new_cap < need) new_cap *= 2;
  b->data = (uint8_t*)xrealloc(b->data, new_cap);
  b->cap = new_cap;
}

void bb_push(ByteBuf* b, uint8_t v) {
  if (b->len + 1 > b->cap) bb_reserve(b, b->len + 1);
  b->data[b->len++] = v;
}

char* read_entire_file(const char* path, size_t* out_len) {
  FILE* f = fopen(path, "rb");
  if (!f) {
    fprintf(stderr, "zmc: failed to open '%s': %s\n", path, strerror(errno));
    exit(2);
  }

  if (fseek(f, 0, SEEK_END) != 0) {
    fprintf(stderr, "zmc: fseek failed for '%s'\n", path);
    exit(2);
  }
  long end = ftell(f);
  if (end < 0) {
    fprintf(stderr, "zmc: ftell failed for '%s'\n", path);
    exit(2);
  }
  if (fseek(f, 0, SEEK_SET) != 0) {
    fprintf(stderr, "zmc: fseek failed for '%s'\n", path);
    exit(2);
  }

  size_t n = (size_t)end;
  char* buf = (char*)xmalloc(n + 1);
  size_t got = fread(buf, 1, n, f);
  if (got != n) {
    fprintf(stderr, "zmc: failed to read '%s'\n", path);
    exit(2);
  }
  buf[n] = '\0';
  fclose(f);

  if (out_len) *out_len = n;
  return buf;
}
