#include "zmc_internal.h"

// Purpose: Lexer/tokenizer for ZManLang source code.

static bool is_alpha_(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static bool is_alnum_(char c) {
  return is_alpha_(c) || (c >= '0' && c <= '9');
}

void lex_init(Lexer* lx, const char* src, size_t len) {
  lx->src = src;
  lx->len = len;
  lx->i = 0;
}

static void skip_ws_and_comments(Lexer* lx) {
  for (;;) {
    while (lx->i < lx->len) {
      char c = lx->src[lx->i];
      if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
        lx->i++;
        continue;
      }
      break;
    }

    if (lx->i + 1 < lx->len && lx->src[lx->i] == '/' && lx->src[lx->i + 1] == '/') {
      lx->i += 2;
      while (lx->i < lx->len && lx->src[lx->i] != '\n') lx->i++;
      continue;
    }

    return;
  }
}

void token_free(Token* t) {
  if (t->kind == TOK_STRING) bb_free(&t->str_bytes);
}

Token next_token(Lexer* lx) {
  skip_ws_and_comments(lx);

  Token t;
  memset(&t, 0, sizeof(t));
  t.kind = TOK_EOF;
  t.pos = lx->i;
  t.len = 0;
  bb_init(&t.str_bytes);

  if (lx->i >= lx->len) {
    t.kind = TOK_EOF;
    return t;
  }

  char c = lx->src[lx->i];

  if (c >= '0' && c <= '9') {
    size_t start = lx->i;
    uint64_t v = 0;
    while (lx->i < lx->len) {
      char d = lx->src[lx->i];
      if (d < '0' || d > '9') break;
      v = v * 10u + (uint64_t)(d - '0');
      if (v > 0xFFFFFFFFu) {
        fprintf(stderr, "zmc: integer literal too large at byte %zu\n", start);
        exit(2);
      }
      lx->i++;
    }
    t.kind = TOK_INT;
    t.pos = start;
    t.len = lx->i - start;
    t.int_u32 = (uint32_t)v;
    return t;
  }

  if (is_alpha_(c)) {
    size_t start = lx->i;
    lx->i++;
    while (lx->i < lx->len && is_alnum_(lx->src[lx->i])) lx->i++;
    t.kind = TOK_IDENT;
    t.pos = start;
    t.len = lx->i - start;

    // keywords (v0 subset)
    if (t.len == 3 && memcmp(lx->src + t.pos, "let", 3) == 0) t.kind = TOK_LET;
    else if (t.len == 5 && memcmp(lx->src + t.pos, "const", 5) == 0) t.kind = TOK_CONST;
    else if (t.len == 4 && memcmp(lx->src + t.pos, "func", 4) == 0) t.kind = TOK_FUNC;
    else if (t.len == 6 && memcmp(lx->src + t.pos, "return", 6) == 0) t.kind = TOK_RETURN;
    else if (t.len == 2 && memcmp(lx->src + t.pos, "if", 2) == 0) t.kind = TOK_IF;
    else if (t.len == 4 && memcmp(lx->src + t.pos, "else", 4) == 0) t.kind = TOK_ELSE;
    else if (t.len == 5 && memcmp(lx->src + t.pos, "while", 5) == 0) t.kind = TOK_WHILE;
    else if (t.len == 4 && memcmp(lx->src + t.pos, "true", 4) == 0) t.kind = TOK_TRUE;
    else if (t.len == 5 && memcmp(lx->src + t.pos, "false", 5) == 0) t.kind = TOK_FALSE;
    else if (t.len == 3 && memcmp(lx->src + t.pos, "and", 3) == 0) t.kind = TOK_AND;
    else if (t.len == 2 && memcmp(lx->src + t.pos, "or", 2) == 0) t.kind = TOK_OR;
    else if (t.len == 3 && memcmp(lx->src + t.pos, "not", 3) == 0) t.kind = TOK_NOT;

    return t;
  }

  if (c == ':' && lx->i + 1 < lx->len && lx->src[lx->i + 1] == '=') {
    lx->i += 2;
    t.kind = TOK_ASSIGN;
    t.len = 2;
    return t;
  }

  if (c == '!') {
    if (lx->i + 1 < lx->len && lx->src[lx->i + 1] == '=') {
      lx->i += 2;
      t.kind = TOK_NE;
      t.len = 2;
      return t;
    }
    lx->i++;
    t.kind = TOK_BANG;
    t.len = 1;
    return t;
  }

  if (c == '<') {
    size_t start = lx->i;
    lx->i++;
    if (lx->i < lx->len && lx->src[lx->i] == '=') {
      lx->i++;
      t.kind = TOK_LE;
      t.len = 2;
      t.pos = start;
      return t;
    }
    t.kind = TOK_LT;
    t.len = 1;
    t.pos = start;
    return t;
  }

  if (c == '>') {
    size_t start = lx->i;
    lx->i++;
    if (lx->i < lx->len && lx->src[lx->i] == '=') {
      lx->i++;
      t.kind = TOK_GE;
      t.len = 2;
      t.pos = start;
      return t;
    }
    t.kind = TOK_GT;
    t.len = 1;
    t.pos = start;
    return t;
  }

  if (c == '=') {
    lx->i++;
    t.kind = TOK_EQ;
    t.len = 1;
    return t;
  }

  if (c == '+') {
    lx->i++;
    t.kind = TOK_PLUS;
    t.len = 1;
    return t;
  }

  if (c == '-') {
    lx->i++;
    t.kind = TOK_MINUS;
    t.len = 1;
    return t;
  }

  if (c == '*') {
    lx->i++;
    t.kind = TOK_STAR;
    t.len = 1;
    return t;
  }

  if (c == '/') {
    lx->i++;
    t.kind = TOK_SLASH;
    t.len = 1;
    return t;
  }

  if (c == '%') {
    lx->i++;
    t.kind = TOK_PERCENT;
    t.len = 1;
    return t;
  }

  if (c == '(') {
    lx->i++;
    t.kind = TOK_LPAREN;
    t.len = 1;
    return t;
  }
  if (c == ')') {
    lx->i++;
    t.kind = TOK_RPAREN;
    t.len = 1;
    return t;
  }
  if (c == '{') {
    lx->i++;
    t.kind = TOK_LBRACE;
    t.len = 1;
    return t;
  }
  if (c == '}') {
    lx->i++;
    t.kind = TOK_RBRACE;
    t.len = 1;
    return t;
  }
  if (c == '[') {
    lx->i++;
    t.kind = TOK_LBRACK;
    t.len = 1;
    return t;
  }
  if (c == ']') {
    lx->i++;
    t.kind = TOK_RBRACK;
    t.len = 1;
    return t;
  }
  if (c == ';') {
    lx->i++;
    t.kind = TOK_SEMI;
    t.len = 1;
    return t;
  }

  if (c == ',') {
    lx->i++;
    t.kind = TOK_COMMA;
    t.len = 1;
    return t;
  }

  if (c == '"') {
    size_t start = lx->i;
    lx->i++; // opening quote

    while (lx->i < lx->len) {
      char ch = lx->src[lx->i++];
      if (ch == '"') {
        t.kind = TOK_STRING;
        t.pos = start;
        t.len = lx->i - start;
        return t;
      }
      if (ch == '\\') {
        if (lx->i >= lx->len) break;
        char esc = lx->src[lx->i++];
        switch (esc) {
          case 'n': bb_push(&t.str_bytes, (uint8_t)'\n'); break;
          case 't': bb_push(&t.str_bytes, (uint8_t)'\t'); break;
          case '\\': bb_push(&t.str_bytes, (uint8_t)'\\'); break;
          case '"': bb_push(&t.str_bytes, (uint8_t)'"'); break;
          default:
            // unknown escape: keep it literal (v0 behavior)
            bb_push(&t.str_bytes, (uint8_t)esc);
            break;
        }
      } else {
        bb_push(&t.str_bytes, (uint8_t)ch);
      }
    }

    fprintf(stderr, "zmc: unterminated string literal\n");
    exit(2);
  }

  fprintf(stderr, "zmc: unexpected character '%c' at byte %zu\n", c, lx->i);
  exit(2);
}
