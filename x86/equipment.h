/* macros to work with 32 bit flag value
 * produced by int 11h in real mode   */

#define video_mode_80x25_color 1
#define video_mode_80x25_mono 2

#define equip_flags_video_mode(flags)                    \
  (((flags >> 4) & 0b11) == 0b11 ? video_mode_80x25_mono \
                                 : video_mode_80x25_color);

#define equip_flags_serial_ports(flags) ((flags >> 9) & 0b111);
