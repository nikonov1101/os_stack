#include "libk/types.h"

#define cpuid(in, a, b, c, d) \
  __asm__("cpuid" : "=a"(a), "=b"(b), "=c"(c), "=d"(d) : "a"(in));

// TODO(nikonov): we need better formatting rules.
void cpuid_copy_cpu_name(u32 eax,
                         u32 ebx,
                         u32 ecx,
                         u32 edx,
                         u8* strp,
                         u32 round) {
  for (u32 i = 0; i < 4; i++) {
    *(strp + (round * 16) + i) = (eax >> 8 * i);
    *(strp + (round * 16) + i + 4) = (ebx >> 8 * i);
    *(strp + (round * 16) + i + 8) = (ecx >> 8 * i);
    *(strp + (round * 16) + i + 12) = (edx >> 8 * i);
  }
}

void cpuid_get_name(u8* cpu_name) {
  u32 eax, ebx, ecx, edx, max_eax, unused;
  cpuid(0x80000000, max_eax, unused, unused, unused);

  if (max_eax >= 0x80000004) {
    if (max_eax >= 0x80000002) {
      cpuid(0x80000002, eax, ebx, ecx, edx);
      cpuid_copy_cpu_name(eax, ebx, ecx, edx, cpu_name, 0);
    }
    if (max_eax >= 0x80000003) {
      cpuid(0x80000003, eax, ebx, ecx, edx);
      cpuid_copy_cpu_name(eax, ebx, ecx, edx, cpu_name, 1);
    }
    if (max_eax >= 0x80000004) {
      cpuid(0x80000004, eax, ebx, ecx, edx);
      cpuid_copy_cpu_name(eax, ebx, ecx, edx, cpu_name, 2);
    }
  }
}

void cpuid_get_features(u32* feat1, u32* feat2) {
  u32 edx, ecx, unused;
  cpuid(1, unused, unused, ecx, edx);
  *feat1 = edx;
  *feat2 = ecx;
}
