#include "zmc_internal.h"

// Purpose: Small shared utilities for zmc (error handling, allocation helpers, byte buffer, file IO).

static int g_zmc_trap_errors = 0;
static jmp_buf g_zmc_trap_jmp;

static uint8_t* g_zmc_err = NULL;
static uint32_t g_zmc_err_len = 0;

static void zmc_error_setf(const char* fmt, va_list ap) {
  char tmp[2048];
  int n = vsnprintf(tmp, sizeof(tmp), fmt, ap);
  if (n < 0) {
    zmc_error_clear();
    return;
  }
  if ((size_t)n >= sizeof(tmp)) n = (int)sizeof(tmp) - 1;

  zmc_error_clear();
  g_zmc_err = (uint8_t*)malloc((size_t)n + 1);
  if (!g_zmc_err) return;
  memcpy(g_zmc_err, tmp, (size_t)n);
  g_zmc_err[n] = 0;
  g_zmc_err_len = (uint32_t)n;
}

void zmc_trap_set(int enabled) { g_zmc_trap_errors = enabled ? 1 : 0; }
int zmc_trap_active(void) { return g_zmc_trap_errors; }
jmp_buf* zmc_trap_jmp(void) { return &g_zmc_trap_jmp; }

uint32_t zmc_error_ptr(void) { return g_zmc_err ? (uint32_t)(uintptr_t)g_zmc_err : 0u; }
uint32_t zmc_error_len(void) { return g_zmc_err_len; }
void zmc_error_clear(void) {
  free(g_zmc_err);
  g_zmc_err = NULL;
  g_zmc_err_len = 0;
}

ZMC_NORETURN void die(const char* msg) {
  zmc_failf("zmc: %s", msg);
}

ZMC_NORETURN void zmc_failf(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  zmc_error_setf(fmt, ap);
  va_end(ap);

  if (g_zmc_trap_errors) {
    longjmp(g_zmc_trap_jmp, 1);
  }

  if (g_zmc_err && g_zmc_err_len) {
    fwrite(g_zmc_err, 1, g_zmc_err_len, stderr);
    fputc('\n', stderr);
  } else {
    fprintf(stderr, "zmc: error\n");
  }
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

void bb_write(ByteBuf* b, const void* src, size_t n) {
  if (!b || !src || n == 0) return;
  bb_reserve(b, b->len + n);
  memcpy(b->data + b->len, src, n);
  b->len += n;
}

void bb_write_str(ByteBuf* b, const char* s) {
  if (!s) return;
  bb_write(b, s, strlen(s));
}

void bb_printf(ByteBuf* b, const char* fmt, ...) {
  if (!b || !fmt) return;

  va_list ap;
  va_start(ap, fmt);
  char tmp[2048];
  int n = vsnprintf(tmp, sizeof(tmp), fmt, ap);
  va_end(ap);

  if (n < 0) return;
  if ((size_t)n < sizeof(tmp)) {
    bb_write(b, tmp, (size_t)n);
    return;
  }

  // Slow path: dynamically format into heap buffer.
  size_t need = (size_t)n + 1;
  char* heap = (char*)xmalloc(need);
  va_start(ap, fmt);
  int n2 = vsnprintf(heap, need, fmt, ap);
  va_end(ap);
  if (n2 > 0) bb_write(b, heap, (size_t)n2);
  free(heap);
}

char* read_entire_file(const char* path, size_t* out_len) {
  FILE* f = fopen(path, "rb");
  if (!f) {
    zmc_failf("zmc: failed to open '%s': %s", path, strerror(errno));
  }

  if (fseek(f, 0, SEEK_END) != 0) {
    zmc_failf("zmc: fseek failed for '%s'", path);
  }
  long end = ftell(f);
  if (end < 0) {
    zmc_failf("zmc: ftell failed for '%s'", path);
  }
  if ((unsigned long)end > (unsigned long)(SIZE_MAX - 1)) {
    zmc_failf("zmc: file too large '%s'", path);
  }
  if (fseek(f, 0, SEEK_SET) != 0) {
    zmc_failf("zmc: fseek failed for '%s'", path);
  }

  size_t n = (size_t)end;
  char* buf = (char*)malloc(n + 1);
  if (!buf) die("out of memory");
  size_t got = fread(buf, 1, n, f);
  if (got != n) {
    zmc_failf("zmc: failed to read '%s'", path);
  }
  buf[got] = '\0';
  fclose(f);

  if (out_len) *out_len = n;
  return buf;
}
