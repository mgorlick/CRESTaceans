#include <stdint.h>

#define enc_frame_width 640
#define enc_frame_height 480
#define enc_pic_width 640
#define enc_pic_height 480

#define enc_fps_numerator 30
#define enc_fps_denominator 1

#define enc_quality 48
#define enc_target_bitrate 0
#define enc_keyframe_granule_shift 6

#define FROMFMT_BYTES_PER_PIXEL 2 /* based on pixel format, e.g.,
                                   * YUYV = 4 bytes per 2 pixels */
#define TOFMT_BYTES_PER_PIXEL 1.5 /* based, on pixel format, e.g.,
                                   *I420 = 3 x 8-bit component
                                   * per pixel */
