// video_mem_offset equ 0x612   ; DOUBLE WORD, cursor offset in a video mem
#define NULL 0
#define VIDEO_MEM_OFFSET 0x612  // physical address

#define RAM_EARLY_START 0x6000
#define RAM_EARLY_END 0x7000

typedef unsigned int u32;
typedef unsigned short int u16;
typedef signed int i32;

void globals();
void putc(char c);
void puti(i32 i);
void print(char* str);

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

void* term_init();

void k_early(void) {
  globals();
  u32 heap_early = early_mm_init();
  term_init();

  // print("hello, 32 bit mode; ");
  for (;;) {
  }
}

char* video = NULL;

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

void puti(i32 i) {
  char stack[16] = {0};
  char sp = 0;

  while (i > 0) {
    i32 rem = i % 10;
    i = i / 10;
    stack[sp++] = rem + '0';
  }
  while (sp >= 0) {
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
  puti(cols);

  print("; rows=");
  puti(rows);

  print("; width=");
  puti(width);

  term_columns = width;
  return 0;
}
