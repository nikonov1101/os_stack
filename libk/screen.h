#include "types.h"

#define VIDEO_BASE 0xB8000

void _kprintf(char *fmt, int argc, int argv[]);
#define kprintf(fmt, ...)                                           \
  (_kprintf(fmt, sizeof((int[]){__VA_ARGS__}) / sizeof(int), \
            (int[]){__VA_ARGS__}))

void term_init(u32 vm_offset);
void putc(char c);
