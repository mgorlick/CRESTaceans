inline void log_err (char *msg);

int take_quarter_rgb (const size_t size, const unsigned char *buffer,
		      const size_t outsize, unsigned char *out,
		      const int qtr_row, const int qtr_col,
		      const int original_width,
		      const int original_height);
int take_quarter_yuyv (const size_t size, const unsigned char *buffer,
		       const size_t outsize, unsigned char *out,
		       const int qtr_row, const int qtr_col,
		       const int original_width,
		       const int original_height);
