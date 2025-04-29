#include "libk/screen.h"
#include "libk/types.h"

#define HANDOVER_TABLE_START 0x600

// to be able to "b break_point" in gdb
void break_point(void* any) {}

struct handover_table {
  u16 boot_device;
  u16 lowmem_k;
  u32 video_mem;
};

__attribute__((section(".early32"))) void early32(void) {
  struct handover_table* t = (struct handover_table*)HANDOVER_TABLE_START;

  term_init(t->video_mem);
  kprintf("early32: started\n");
  kprintf("boot_dev: %x; low_mem: %uK\n", t->boot_device, t->lowmem_k);

  break_point((void*)1);

  for (;;) {
    break_point((void*)2);
  }
}
