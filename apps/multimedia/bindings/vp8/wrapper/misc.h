#include <errno.h>
#include <string.h>
#include <stdlib.h>

inline void log_err (char *msg) {
  int i = errno;
  printf ("%s: %s\n", strerror (i), msg);
}

inline int take_quarter_yuyv (const size_t size, const unsigned char *buffer, 
			      const size_t outsize, unsigned char *out,
			      const int qtr_row, const int qtr_col,
			      const int original_width, 
			      const int original_height) {

   if (qtr_row < 0 || qtr_row > 1 || qtr_col < 0 || qtr_col > 1) {
     printf ("Error: quarter coordinates must be in range (0,0) - (1,1)\n");
     return 0;
   }
   if (outsize * 4 != size) {
     printf ("Error: size should be outsize * 4, is actually %d times: %d, %d\n", size, outsize, size / outsize);
     return 0;
   }
   if ((int) size != original_width * original_height * 2) {
     printf ("Error: dimensions do not match specified buffer size, \
size was %d, should be %d for dimensions %dx%d\n",
	     size,
	     original_width * original_height * 2,
	     original_width,
	     original_height);
     return 0;
   }

   const int stride_yuyv = original_width;
   const int row_modifier = qtr_row == 0 ? 0 : original_height / 2;
   const int col_modifier = qtr_col == 0 ? 0 : original_width;
   const unsigned char *input_cursor;
   unsigned char *output_cursor;
   int row = 0;
  
  for (row = 0; row < original_height / 2; row++) {
    input_cursor = buffer + stride_yuyv * 2 * (row + row_modifier) + col_modifier;
    output_cursor = out + (stride_yuyv * row);
    memcpy (output_cursor, input_cursor, stride_yuyv);
  }

  return 1;
}
