/*         ______   ___    ___
 *        /\  _  \ /\_ \  /\_ \
 *        \ \ \L\ \\//\ \ \//\ \      __     __   _ __   ___
 *         \ \  __ \ \ \ \  \ \ \   /'__`\ /'_ `\/\`'__\/ __`\
 *          \ \ \/\ \ \_\ \_ \_\ \_/\  __//\ \L\ \ \ \//\ \L\ \
 *           \ \_\ \_\/\____\/\____\ \____\ \____ \ \_\\ \____/
 *            \/_/\/_/\/____/\/____/\/____/\/___L\ \/_/ \/___/
 *                                           /\____/
 *                                           \_/__/
 *
 *      BMP reader.
 *
 *      By Seymour Shlien.
 *
 *      OS/2 BMP support and BMP save function by Jonas Petersen.
 *
 *      See readme.txt for copyright information.
 */


#include <string.h>

#include "allegro5/allegro5.h"
#include "allegro5/allegro_image.h"
#include "allegro5/internal/aintern_convert.h"
#include "allegro5/internal/aintern_image.h"

#include "iio.h"


#define BIT_RGB          0
#define BIT_RLE8         1
#define BIT_RLE4         2
#define BIT_BITFIELDS    3

#define OS2INFOHEADERSIZE  12
#define WININFOHEADERSIZE  40


typedef struct BMPFILEHEADER
{
   unsigned long bfType;
   unsigned long bfSize;
   unsigned short bfReserved1;
   unsigned short bfReserved2;
   unsigned long bfOffBits;
} BMPFILEHEADER;


/* Used for both OS/2 and Windows BMP. 
 * Contains only the parameters needed to load the image 
 */
typedef struct BMPINFOHEADER
{
   unsigned long biWidth;
   signed long biHeight;
   unsigned short biBitCount;
   unsigned long biCompression;
} BMPINFOHEADER;


typedef struct WINBMPINFOHEADER
{                               /* size: 40 */
   unsigned long biWidth;
   signed long biHeight;
   unsigned short biPlanes;
   unsigned short biBitCount;
   unsigned long biCompression;
   unsigned long biSizeImage;
   unsigned long biXPelsPerMeter;
   unsigned long biYPelsPerMeter;
   unsigned long biClrUsed;
   unsigned long biClrImportant;
} WINBMPINFOHEADER;


typedef struct OS2BMPINFOHEADER
{                               /* size: 12 */
   unsigned short biWidth;
   unsigned short biHeight;
   unsigned short biPlanes;
   unsigned short biBitCount;
} OS2BMPINFOHEADER;



/* read_bmfileheader:
 *  Reads a BMP file header and check that it has the BMP magic number.
 */
static int read_bmfileheader(ALLEGRO_FILE *f, BMPFILEHEADER *fileheader)
{
   fileheader->bfType = al_fread16le(f);
   fileheader->bfSize = al_fread32le(f);
   fileheader->bfReserved1 = al_fread16le(f);
   fileheader->bfReserved2 = al_fread16le(f);
   fileheader->bfOffBits = al_fread32le(f);

   if (fileheader->bfType != 19778)
      return -1;

   if (al_feof(f) || al_ferror(f))
      return -1;

   return 0;
}



/* read_win_bminfoheader:
 *  Reads information from a BMP file header.
 */
static int read_win_bminfoheader(ALLEGRO_FILE *f, BMPINFOHEADER *infoheader)
{
   WINBMPINFOHEADER win_infoheader;

   win_infoheader.biWidth = al_fread32le(f);
   win_infoheader.biHeight = al_fread32le(f);
   win_infoheader.biPlanes = al_fread16le(f);
   win_infoheader.biBitCount = al_fread16le(f);
   win_infoheader.biCompression = al_fread32le(f);
   win_infoheader.biSizeImage = al_fread32le(f);
   win_infoheader.biXPelsPerMeter = al_fread32le(f);
   win_infoheader.biYPelsPerMeter = al_fread32le(f);
   win_infoheader.biClrUsed = al_fread32le(f);
   win_infoheader.biClrImportant = al_fread32le(f);

   infoheader->biWidth = win_infoheader.biWidth;
   infoheader->biHeight = win_infoheader.biHeight;
   infoheader->biBitCount = win_infoheader.biBitCount;
   infoheader->biCompression = win_infoheader.biCompression;

   if (al_feof(f) || al_ferror(f))
      return -1;

   return 0;
}



/* read_os2_bminfoheader:
 *  Reads information from an OS/2 format BMP file header.
 */
static int read_os2_bminfoheader(ALLEGRO_FILE *f, BMPINFOHEADER *infoheader)
{
   OS2BMPINFOHEADER os2_infoheader;

   os2_infoheader.biWidth = al_fread16le(f);
   os2_infoheader.biHeight = al_fread16le(f);
   os2_infoheader.biPlanes = al_fread16le(f);
   os2_infoheader.biBitCount = al_fread16le(f);

   infoheader->biWidth = os2_infoheader.biWidth;
   infoheader->biHeight = os2_infoheader.biHeight;
   infoheader->biBitCount = os2_infoheader.biBitCount;
   infoheader->biCompression = 0;

   if (al_feof(f) || al_ferror(f))
      return -1;

   return 0;
}



/* read_bmicolors:
 *  Loads the color palette for 1,4,8 bit formats.
 */
static void read_bmicolors(int bytes, PalEntry *pal, ALLEGRO_FILE *f,
   int win_flag)
{
   int i, j;

   for (i = j = 0; (i + 3 <= bytes && j < 256); j++) {
      pal[j].b = al_fgetc(f);
      pal[j].g = al_fgetc(f);
      pal[j].r = al_fgetc(f);

      i += 3;

      if (win_flag && i < bytes) {
         al_fgetc(f);
         i++;
      }
   }

   for (; i < bytes; i++)
      al_fgetc(f);
}



/* read_1bit_line:
 *  Support function for reading the 1 bit bitmap file format.
 */
static void read_1bit_line(int length, ALLEGRO_FILE *f, unsigned char *buf)
{
   unsigned char b[32];
   unsigned long n;
   int i, j, k;

   for (i = 0; i < length; i++) {
      j = i % 32;
      if (j == 0) {
         n = al_fread32be(f);
         for (k = 0; k < 32; k++) {
            b[31 - k] = (char)(n & 1);
            n = n >> 1;
         }
      }
      buf[i] = b[j];
   }
}



/* read_4bit_line:
 *  Support function for reading the 4 bit bitmap file format.
 */
static void read_4bit_line(int length, ALLEGRO_FILE *f, unsigned char *buf)
{
   unsigned char b[8];
   unsigned long n;
   int i, j, k;
   int temp;

   for (i = 0; i < length; i++) {
      j = i % 8;
      if (j == 0) {
         n = al_fread32le(f);
         for (k = 0; k < 4; k++) {
            temp = n & 255;
            b[k * 2 + 1] = temp & 15;
            temp = temp >> 4;
            b[k * 2] = temp & 15;
            n = n >> 8;
         }
      }
      buf[i] = b[j];
   }
}



/* read_8bit_line:
 *  Support function for reading the 8 bit bitmap file format.
 */
static void read_8bit_line(int length, ALLEGRO_FILE *f, unsigned char *buf)
{
   unsigned char b[4];
   unsigned long n;
   int i, j, k;

   for (i = 0; i < length; i++) {
      j = i % 4;
      if (j == 0) {
         n = al_fread32le(f);
         for (k = 0; k < 4; k++) {
            b[k] = (char)(n & 255);
            n = n >> 8;
         }
      }
      buf[i] = b[j];
   }
}



/* read_16bit_line:
 *  Support function for reading the 16 bit bitmap file format, doing
 *  our best to convert it down to a 256 color palette.
 */
static void read_16bit_line(int length, ALLEGRO_FILE *f, unsigned char *data)
{
   int i, w;
   unsigned char r, g, b;

   for (i = 0; i < length; i++) {
      w = al_fread16le(f);

      /* the format is like a 15-bpp bitmap, not 16bpp */
      r = _al_rgb_scale_5[(w >> 10) & 0x1f];
      g = _al_rgb_scale_5[(w >> 5) & 0x1f];
      b = _al_rgb_scale_5[w & 0x1f];

      data[0] = r;
      data[1] = g;
      data[2] = b;
      data[3] = 255;
      data += 4;
   }

   /* padding */
   i = (i * 2) % 4;
   if (i != 0) {
      while (i++ < 4)
         al_fgetc(f);
   }
}



/* read_24bit_line:
 *  Support function for reading the 24 bit bitmap file format.
 */
static void read_24bit_line(int length, ALLEGRO_FILE *f, unsigned char *data)
{
   int i;
   unsigned char c[3];
   unsigned char r, g, b;

   for (i = 0; i < length; i++) {
      al_fread(f, c, 3);
      r = c[2];
      g = c[1];
      b = c[0];

      data[0] = r;
      data[1] = g;
      data[2] = b;
      data[3] = 255;
      data += 4;
   }

   /* padding */
   i = (i * 3) % 4;
   if (i != 0) {
      while (i++ < 4)
         al_fgetc(f);
   }
}



/* read_32bit_line:
 *  Support function for reading the 32 bit bitmap file format, doing
 *  our best to convert it down to a 256 color palette.
 */
static void read_32bit_line(int length, ALLEGRO_FILE *f, unsigned char *data)
{
   int i;
   unsigned char c[4];
   unsigned char r, g, b, a;

   for (i = 0; i < length; i++) {
      al_fread(f, c, 4);
      r = c[2];
      g = c[1];
      b = c[0];
      a = c[3];

      data[0] = r;
      data[1] = g;
      data[2] = b;
      data[3] = a;
      data += 4;
   }
}



/* read_bitfields_image:
 *  For reading the bitfield compressed BMP image format.
 */
static void read_bitfields_image(ALLEGRO_FILE *f, const BMPINFOHEADER *infoheader, int bpp,
   ALLEGRO_LOCKED_REGION *lr)
{
   int k, i, line, height, dir;
   int bytes_per_pixel;
   unsigned long buffer;
   int pix;

   height = infoheader->biHeight;
   line = height < 0 ? 0 : height - 1;
   dir = height < 0 ? 1 : -1;
   height = abs(height);

   //bpp = _al_get_pixel_format_bits(bmp->format);
   bytes_per_pixel = (bpp + 1) / 8;

   for (i = 0; i < height; i++, line += dir) {
      unsigned char *data = (unsigned char *)lr->data + lr->pitch * line;

      for (k = 0; k < (int)infoheader->biWidth; k++) {

         al_fread(f, &buffer, bytes_per_pixel);

         if (bpp == 15) {
            pix = ALLEGRO_CONVERT_RGB_555_TO_ARGB_8888(buffer);
         }
         else if (bpp == 16) {
            pix = ALLEGRO_CONVERT_RGB_565_TO_ARGB_8888(buffer);
         }
         else {
            pix = ALLEGRO_CONVERT_XRGB_8888_TO_ARGB_8888(buffer);
         }

         data[2] = pix & 255;
         data[1] = (pix >> 8) & 255;
         data[0] = (pix >> 16) & 255;
         data[3] = (pix >> 24) & 255;
         data += 4;
      }

      /* padding */
      k = (k * bytes_per_pixel) % 4;
      if (k > 0) {
         while (k++ < 4)
            al_fgetc(f);
      }
   }
}


/* read_image:
 *  For reading the noncompressed BMP image format.
 */
static void read_image(ALLEGRO_FILE *f,
                       const BMPINFOHEADER *infoheader, PalEntry *pal,
                       ALLEGRO_LOCKED_REGION *lr)
{
   int i, j, line, height, dir;
   unsigned char *buf;
   unsigned char *data;

   height = infoheader->biHeight;
   line = height < 0 ? 0 : height - 1;
   dir = height < 0 ? 1 : -1;
   height = abs(height);

   buf = al_malloc(infoheader->biWidth);

   for (i = 0; i < height; i++, line += dir) {
      data = (unsigned char *)lr->data + lr->pitch * line;

      switch (infoheader->biBitCount) {

         case 1:
            read_1bit_line(infoheader->biWidth, f, buf);
            break;

         case 4:
            read_4bit_line(infoheader->biWidth, f, buf);
            break;

         case 8:
            read_8bit_line(infoheader->biWidth, f, buf);
            break;

         case 16:
            read_16bit_line(infoheader->biWidth, f, data);
            break;

         case 24:
            read_24bit_line(infoheader->biWidth, f, data);
            break;

         case 32:
            read_32bit_line(infoheader->biWidth, f, data);
            break;
      }
      if (infoheader->biBitCount <= 8) {
         for (j = 0; j < (int)infoheader->biWidth; j++) {
            data[0] = pal[buf[j]].r;
            data[1] = pal[buf[j]].g;
            data[2] = pal[buf[j]].b;
            data[3] = 255;
            data += 4;
         }
      }
   }

   al_free(buf);
}



/* read_RLE8_compressed_image:
 *  For reading the 8 bit RLE compressed BMP image format.
 */
static void read_RLE8_compressed_image(ALLEGRO_FILE *f, unsigned char *buf,
                                       const BMPINFOHEADER *infoheader)
{
   int count;
   unsigned char val;
   unsigned char val0;
   int j, pos, line;
   int eolflag, eopicflag;

   eopicflag = 0;
   line = infoheader->biHeight - 1;

   while (eopicflag == 0) {
      pos = 0;                  /* x position in bitmap */
      eolflag = 0;              /* end of line flag */

      while ((eolflag == 0) && (eopicflag == 0)) {
         count = al_fgetc(f);
         if (count == EOF)
            return;
         val = al_fgetc(f);

         if (count > 0) {       /* repeat pixel count times */
            for (j = 0; j < count; j++) {
               buf[line * infoheader->biWidth + pos] = val;
               pos++;
            }
         }
         else {
            switch (val) {

               case 0:         /* end of line flag */
                  eolflag = 1;
                  break;

               case 1:         /* end of picture flag */
                  eopicflag = 1;
                  break;

               case 2:         /* displace picture */
                  count = al_fgetc(f);
                  if (count == EOF)
                     return;
                  val = al_fgetc(f);
                  pos += count;
                  line -= val;
                  break;

               default:                      /* read in absolute mode */
                  for (j=0; j<val; j++) {
                     val0 = al_fgetc(f);
                     buf[line * infoheader->biWidth + pos] = val0;
                     pos++;
                  }

                  if (j % 2 == 1)
                     val0 = al_fgetc(f);    /* align on word boundary */

                  break;
            }
         }

         if (pos - 1 > (int)infoheader->biWidth)
            eolflag = 1;
      }

      line--;
      if (line < 0)
         eopicflag = 1;
   }
}



/* read_RLE4_compressed_image:
 *  For reading the 4 bit RLE compressed BMP image format.
 */
static void read_RLE4_compressed_image(ALLEGRO_FILE *f, unsigned char *buf,
                                       const BMPINFOHEADER *infoheader)
{
   unsigned char b[8];
   int count;
   unsigned short val0, val;
   int j, k, pos, line;
   int eolflag, eopicflag;

   eopicflag = 0;               /* end of picture flag */
   line = infoheader->biHeight - 1;

   while (eopicflag == 0) {
      pos = 0;
      eolflag = 0;              /* end of line flag */

      while ((eolflag == 0) && (eopicflag == 0)) {
         count = al_fgetc(f);
         if (count == EOF)
            return;
         val = al_fgetc(f);

         if (count > 0) {       /* repeat pixels count times */
            b[1] = val & 15;
            b[0] = (val >> 4) & 15;
            for (j = 0; j < count; j++) {
               buf[line * infoheader->biWidth + pos] = b[j % 2];
               pos++;
            }
         }
         else {
            switch (val) {

               case 0:         /* end of line */
                  eolflag = 1;
                  break;

               case 1:         /* end of picture */
                  eopicflag = 1;
                  break;

               case 2:         /* displace image */
                  count = al_fgetc(f);
                  if (count == EOF)
                     return;
                  val = al_fgetc(f);
                  pos += count;
                  line -= val;
                  break;

               default:        /* read in absolute mode */
                  for (j = 0; j < val; j++) {
                     if ((j % 4) == 0) {
                        val0 = al_fread16le(f);
                        for (k = 0; k < 2; k++) {
                           b[2 * k + 1] = val0 & 15;
                           val0 = val0 >> 4;
                           b[2 * k] = val0 & 15;
                           val0 = val0 >> 4;
                        }
                     }
                     buf[line * infoheader->biWidth + pos] = b[j % 4];
                     pos++;
                  }
                  break;
            }
         }

         if (pos - 1 > (int)infoheader->biWidth)
            eolflag = 1;
      }

      line--;
      if (line < 0)
         eopicflag = 1;
   }
}



/*  Like load_bmp, but starts loading from the current place in the ALLEGRO_FILE
 *  specified. If successful the offset into the file will be left just after
 *  the image data. If unsuccessful the offset into the file is unspecified,
 *  i.e. you must either reset the offset to some known place or close the
 *  packfile. The packfile is not closed by this function.
 */
ALLEGRO_BITMAP *_al_load_bmp_f(ALLEGRO_FILE *f)
{
   BMPFILEHEADER fileheader;
   BMPINFOHEADER infoheader;
   ALLEGRO_BITMAP *bmp;
   PalEntry pal[256];
   unsigned long biSize;
   unsigned char *buf = NULL;
   ALLEGRO_LOCKED_REGION *lr;
   int bpp;
   ASSERT(f);

   if (read_bmfileheader(f, &fileheader) != 0) {
      return NULL;
   }

   biSize = al_fread32le(f);
   if (al_feof(f) || al_ferror(f)) {
      return NULL;
   }

   if (biSize == WININFOHEADERSIZE) {
      if (read_win_bminfoheader(f, &infoheader) != 0) {
         return NULL;
      }
      if (infoheader.biCompression != BIT_BITFIELDS)
         read_bmicolors(fileheader.bfOffBits - 54, pal, f, 1);
   }
   else if (biSize == OS2INFOHEADERSIZE) {
      if (read_os2_bminfoheader(f, &infoheader) != 0) {
         return NULL;
      }
      if (infoheader.biCompression != BIT_BITFIELDS)
         read_bmicolors(fileheader.bfOffBits - 26, pal, f, 0);
   }
   else {
      return NULL;
   }

   if (infoheader.biBitCount == 24)
      bpp = 24;
   else if (infoheader.biBitCount == 16)
      bpp = 16;
   else if (infoheader.biBitCount == 32)
      bpp = 32;
   else
      bpp = 8;

   if (infoheader.biCompression == BIT_BITFIELDS) {
      unsigned long redMask = al_fread32le(f);
      unsigned long grnMask = al_fread32le(f);
      unsigned long bluMask = al_fread32le(f);

      (void)grnMask;

      if ((bluMask == 0x001f) && (redMask == 0x7C00))
         bpp = 15;
      else if ((bluMask == 0x001f) && (redMask == 0xF800))
         bpp = 16;
      else if ((bluMask == 0x0000FF) && (redMask == 0xFF0000))
         bpp = 32;
      else {
         /* Unrecognised bit masks/depth, refuse to load. */
         return NULL;
      }
   }

   bmp = al_create_bitmap(infoheader.biWidth, abs(infoheader.biHeight));
   if (!bmp) {
      return NULL;
   }

   lr = al_lock_bitmap(bmp, ALLEGRO_PIXEL_FORMAT_ABGR_8888_LE,
      ALLEGRO_LOCK_WRITEONLY);
   if (!lr) {
      al_destroy_bitmap(bmp);
      return NULL;
   }

   if (infoheader.biCompression == BIT_RLE8
       || infoheader.biCompression == BIT_RLE4) {
      buf = al_malloc(infoheader.biWidth * infoheader.biHeight);
   }

   switch (infoheader.biCompression) {

      case BIT_RGB:
         read_image(f, &infoheader, pal, lr);
         break;

      case BIT_RLE8:
         read_RLE8_compressed_image(f, buf, &infoheader);
         break;

      case BIT_RLE4:
         read_RLE4_compressed_image(f, buf, &infoheader);
         break;

      case BIT_BITFIELDS:
         read_bitfields_image(f, &infoheader, bpp, lr);
         break;

      default:
         al_unlock_bitmap(bmp);
         al_destroy_bitmap(bmp);
         bmp = NULL;
         break;
   }

   if (infoheader.biCompression == BIT_RLE8
       || infoheader.biCompression == BIT_RLE4) {
      int x, y;
      unsigned char *data;

      for (y = 0; y < infoheader.biHeight; y++) {
         data = (unsigned char *)lr->data + lr->pitch * y;
         for (x = 0; x < (int)infoheader.biWidth; x++) {
            data[0] = pal[buf[y * infoheader.biWidth + x]].r;
            data[1] = pal[buf[y * infoheader.biWidth + x]].g;
            data[2] = pal[buf[y * infoheader.biWidth + x]].b;
            data[3] = 255;
            data += 4;
         }
      }
      al_free(buf);
   }

   if (bmp) {
      al_unlock_bitmap(bmp);
   }

   return bmp;
}



/*  Like save_bmp but writes into the ALLEGRO_FILE given instead of a new file.
 *  The packfile is not closed after writing is completed. On success the
 *  offset into the file is left after the TGA file just written. On failure
 *  the offset is left at the end of whatever incomplete data was written.
 */
bool _al_save_bmp_f(ALLEGRO_FILE *f, ALLEGRO_BITMAP *bmp)
{
   int bfSize;
   int biSizeImage;
   int depth;
   int bpp;
   int filler;
   int i, j;
   int w, h;
   ALLEGRO_LOCKED_REGION *lr;
   ASSERT(f);
   ASSERT(bmp);

   w = al_get_bitmap_width(bmp);
   h = al_get_bitmap_height(bmp);

   depth = al_get_pixel_format_bits(al_get_bitmap_format(bmp));
   bpp = 24;
   filler = 3 - ((w * (bpp / 8) - 1) & 3);

   if (bpp == 8) {
      biSizeImage = (w + filler) * h;
      bfSize = (54              /* header */
                + 256 * 4       /* palette */
                + biSizeImage); /* image data */
   }
   else {
      biSizeImage = (w * 3 + filler) * h;
      bfSize = 54 + biSizeImage;        /* header + image data */
   }

   al_set_errno(0);

   /* file_header */
   al_fwrite16le(f, 0x4D42);              /* bfType ("BM") */
   al_fwrite32le(f, bfSize);              /* bfSize */
   al_fwrite16le(f, 0);                   /* bfReserved1 */
   al_fwrite16le(f, 0);                   /* bfReserved2 */

   if (bpp == 8)                /* bfOffBits */
      al_fwrite32le(f, 54 + 256 * 4);
   else
      al_fwrite32le(f, 54);

   /* info_header */
   al_fwrite32le(f, 40);                  /* biSize */
   al_fwrite32le(f, w);                   /* biWidth */
   al_fwrite32le(f, h);                   /* biHeight */
   al_fwrite16le(f, 1);                   /* biPlanes */
   al_fwrite16le(f, bpp);                 /* biBitCount */
   al_fwrite32le(f, 0);                   /* biCompression */
   al_fwrite32le(f, biSizeImage);         /* biSizeImage */
   al_fwrite32le(f, 0xB12);               /* biXPelsPerMeter (0xB12 = 72 dpi) */
   al_fwrite32le(f, 0xB12);               /* biYPelsPerMeter */

   al_fwrite32le(f, 0);                   /* biClrUsed */
   al_fwrite32le(f, 0);                   /* biClrImportant */

   lr = al_lock_bitmap(bmp, ALLEGRO_PIXEL_FORMAT_ANY, ALLEGRO_LOCK_READONLY);

   /* image data */
   for (i = h - 1; i >= 0; i--) {
      for (j = 0; j < w; j++) {
         ALLEGRO_COLOR c = al_get_pixel(bmp, j, i);
         unsigned char r, g, b;
         al_unmap_rgb(c, &r, &g, &b);
         al_fputc(f, b);
         al_fputc(f, g);
         al_fputc(f, r);
      }

      for (j = 0; j < filler; j++)
         al_fputc(f, 0);
   }

   al_unlock_bitmap(bmp);

   return al_get_errno() ? false : true;
}



ALLEGRO_BITMAP *_al_load_bmp(const char *filename)
{
   ALLEGRO_FILE *f;
   ALLEGRO_BITMAP *bmp;
   ASSERT(filename);

   f = al_fopen(filename, "rb");
   if (!f)
      return NULL;

   bmp = _al_load_bmp_f(f);

   al_fclose(f);

   return bmp;
}


bool _al_save_bmp(const char *filename, ALLEGRO_BITMAP *bmp)
{
   ALLEGRO_FILE *f;
   bool ret;
   ASSERT(filename);

   f = al_fopen(filename, "wb");
   if (!f)
      return false;

   ret = _al_save_bmp_f(f, bmp);

   al_fclose(f);

   return ret;
}

/* vim: set sts=3 sw=3 et: */
