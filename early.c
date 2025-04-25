// video_mem_offset equ 0x612   ; DOUBLE WORD, cursor offset in a video mem
#define NULL 0
#define VIDEO_MEM_OFFSET 0x612  // physical address
#define VIDEO_BASE 0xB8000

#define RAM_EARLY_START 0x6000
#define RAM_EARLY_END 0x7000

typedef unsigned int u32;
typedef unsigned short int u16;
typedef signed int i32;

void globals();
void putc(char c);
void printeger(i32 i);
void print(char* str);
void htoa(u32);

u32 early_mm_init();
void* early_malloc(u32 size);
void* early_free(void* ptr);
u32 early_mm_init() {
  u32 start = RAM_EARLY_START;
  while (start != RAM_EARLY_END) {
    *(char*)start++ = 0;
  }
  return start - RAM_EARLY_START;
}

char* video = NULL;
void* term_init();

void break_point(void* any) {}

void new_line() {
  // everything calculated in a single-character spaces,
  // and not in video-memory words.
  u32 in_chars = ((u32)video - VIDEO_BASE) / 2;
  u16 width = *(u16*)(0x44a);
  u32 in_rows = 1 + (in_chars / width);
  u32 to_next_row = in_rows * width;
  u32 needed = to_next_row - in_chars;

  video += needed * 2;
  // expecting to appear at the beginning of the new line at this moment
}

void k_early(void) {
  globals();
  u32 heap_early = early_mm_init();
  term_init();
  print(" => video_base: ");
  htoa(VIDEO_BASE);
  print("; *video: ");
  htoa((u32)video);

  break_point((void*)4);
  new_line();
  print("finally, i've implemented the new_line function!");
  new_line();
  print("so i can tell you that we are in the 32-bit mode,");
  new_line();
  print("and we're talking from C!");

  for (;;) {
    break_point((void*)video);
  }
}

void globals() {
  u32 base = *(u32*)(VIDEO_MEM_OFFSET);
  u32* p = (u32*)base;

  video = (char*)(p);
}

void print(char* str) {
  char c;
  while ((c = *str++) != NULL) {
    putc(c);
  }
}

static const char hextab[16] = {
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
};

void htoa(u32 v) {
  char c;
  char stack[8] = {0};
  char sp = 0;

  while (v > 0) {
    c = (char)(0x0000000f & v);  // truncate to 4 bits
    stack[sp++] = hextab[c];
    v = v >> 4;
  }

  putc('0');
  putc('x');
  while (sp > 0) {
    putc(stack[--sp]);
  }
}

void printeger(i32 i) {
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

void putc(char c) {
  *video++ = c;
  *video++ = 0x0f;  // bright white
}

u32 term_columns = 0;
void* term_init() {
  u16 cols = *(u16*)(0x450);
  u16 rows = *(u16*)(0x451);
  u16 width = *(u16*)(0x44a);

  print("cols=");
  printeger(cols);

  print("; rows=");
  printeger(rows);

  print("; width=");
  printeger(width);

  term_columns = width;
  return 0;
}
