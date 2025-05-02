#include "libk/screen.h"
#include "libk/types.h"
#include "x86/cpuid.h"
#include "x86/equipment.h"

#define HANDOVER_TABLE_START 0x600

struct handover_table {
  u16 boot_device;
  u16 lowmem_k;
  u32 video_mem;
  u16 equip_flags;
  u32 cpu_features1;  // cpuid, edx=1
  u32 cpu_features2;  // cpuid, ecx=1
  u8 cpu_name[48];    // cpuid, eax=0
};

// to be able to "b break_point" in gdb
void break_point(void* any){}

__attribute__((section(".early32"))) void early32(void) {
  struct handover_table* t = (struct handover_table*)HANDOVER_TABLE_START;

  term_init(t->video_mem);
  kprintf("early32: started\n");
  kprintf("boot_dev: %x; low_mem: %uK\n", t->boot_device, t->lowmem_k);

  u16 videomod = equip_flags_video_mode(t->equip_flags);
  u16 serialports = equip_flags_serial_ports(t->equip_flags);
  kprintf("video mode: %d\nserial ports: %d\n", videomod, serialports);

  cpuid_get_name((u8*)t->cpu_name);
  kprintf("cpu_model: %s\n", (int)t->cpu_name);

  cpuid_get_features(&t->cpu_features1, &t->cpu_features2);
  kprintf("cpu_features: %x %x\n", t->cpu_features1, t->cpu_features2);

  break_point((void*)1);
  kprintf("halt.");
  __asm__("hlt");
}
