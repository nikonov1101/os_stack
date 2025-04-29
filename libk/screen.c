#include "screen.h"
#include "types.h"

static volatile char* video = NULL;

static u16 term_columns = 0;
static u16 term_rows = 0;
static u16 current_column = 0;
static u16 current_row = 0;

void putc(char c) {
  *video++ = c;
  *video++ = 0x0f;  // bright white

  if (++current_column == term_columns) {
    current_column = 0;
    current_row++;
  }
}

void put_eol() {
  video += 2 * (term_columns - current_column);
  current_row++;
  current_column = 0;
}

void put_str(char* str) {
  char c;
  while ((c = *str++) != NULL) {
    putc(c);
  }
}

static const char hex_table[16] = {
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
};

void put_hex(u32 v) {
  char c;
  char stack[8] = {0};
  char sp = 0;

  while (v > 0) {
    c = (char)(0x0000000f & v);  // truncate to 4 bits
    stack[sp++] = hex_table[c];
    v = v >> 4;
  }

  putc('0');
  putc('x');
  while (sp > 0) {
    putc(stack[--sp]);
  }
}

void put_int(i32 i) {
  i32 it_was = i;
  char stack[16] = {0};
  char sp = 0;

  // corner-case
  if (i == 0) {
    putc('0');
    return;
  }

  while (i != 0) {
    i32 rem = i % 10;
    if (rem < 0) {
      rem = -rem;
    }

    i = i / 10;
    stack[sp++] = rem + '0';
  }

  if (it_was < 0) {
    stack[sp++] = '-';
  }

  while (sp > 0) {
    putc(stack[--sp]);
  }
}

// TODO(nikonov): move away to "strings.c"
i32 kstrlen(char* s) {
  char c;
  u32 len;
  while ((c = *s++) != 0) {
    if (++len > 255) {
      return -1;
    }
  }
  return len;
}

void _kprintf(char* template, int argc, int argv[]) {
  char c;
  int arg = 0;
  int tmp = NULL;

  while ((c = *template ++) != 0) {
    if (c == '\n') {
      put_eol();
      continue;
    }

    if (c == '%' && arg < argc) {
      c = *template ++;
      tmp = argv[arg++];
      switch (c) {
        case 'd':
          put_int(tmp);
          break;
        case 'u':
          put_int((u32)tmp);
          break;
        case 's':
          put_str((char*)tmp);
          break;
        case 'p':
          put_hex((u32)tmp);
          break;
        case 'x':
          put_hex((u32)tmp);
          break;
        case 'c':
          putc((char)tmp);
          break;
        case '%':
          putc('%');
          break;
        default:
          // TODO(nikonov): let's invent some sort of error/panic handler
          // error, actually
      }
      continue;
    }

    putc(c);
  }
}

void term_init(u32 vm_offset) {
  video = (char*)vm_offset;

  term_columns = *(u16*)(0x44a);
  current_row = *(u16*)(0x451);
  term_rows = 25;
  current_column = 0;

  kprintf("term_init: %ux%u\n", term_columns, term_rows);
}
