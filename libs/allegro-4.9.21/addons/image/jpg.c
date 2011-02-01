/* libjpeg wrapper for Allegro 5 iio addon.
 * by Elias Pschernig
 */

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#ifdef ALLEGRO_HAVE_STDINT_H
#include <stdint.h>
#endif

#define BUFFER_SIZE 4096

#include "allegro5/allegro5.h"
#include "allegro5/allegro_image.h"
#include "allegro5/internal/aintern_image.h"

#include "iio.h"

#ifdef __MINGW32__
   /*
    * Differing versions of the MinGW rpcndr.h (which would be included at this
    * point) may or may not typedef `boolean', which would conflict with the
    * definition in jmorecfg.h.  The Microsoft rpcndr.h does define `boolean'.
    *
    * So tell the jpeg headers that we have our own boolean, then define a
    * macro `boolean', which will work whether the typedef exists or not.
    * Note that simply renaming `boolean' away (e.g. to jpeg_boolean) will NOT
    * work with some copies of the jpeg headers floating around either.
    */
   #define HAVE_BOOLEAN
   #define boolean int

   /* basetsd.h may define `INT32' which can conflict with a later definition
    * so rename `INT32' to something else from here on.
    */
   #define INT32 jpeg_INT32
#endif

#include <jpeglib.h>
#include <jerror.h>

struct my_src_mgr
{
   struct jpeg_source_mgr pub;
   JOCTET *buffer;
   ALLEGRO_FILE *fp;
};

struct my_dest_mgr
{
   struct jpeg_destination_mgr pub;
   JOCTET *buffer;
   ALLEGRO_FILE *fp;
};

struct my_err_mgr
{
   struct jpeg_error_mgr pub;
   jmp_buf jmpenv;
};

static void init_source(j_decompress_ptr cinfo)
{
   (void)cinfo;
}

static void init_destination(j_compress_ptr cinfo)
{
   struct my_dest_mgr *dest = (void *)cinfo->dest;
   dest->pub.next_output_byte = dest->buffer;
   dest->pub.free_in_buffer = BUFFER_SIZE;
}

static boolean fill_input_buffer(j_decompress_ptr cinfo)
{
   struct my_src_mgr *src = (void *)cinfo->src;
   src->pub.next_input_byte = src->buffer;
   src->pub.bytes_in_buffer = al_fread(src->fp, src->buffer, BUFFER_SIZE);
   return 1;
}

static boolean empty_output_buffer(j_compress_ptr cinfo)
{
   struct my_dest_mgr *dest = (void *)cinfo->dest;
   al_fwrite(dest->fp, dest->buffer, BUFFER_SIZE);
   dest->pub.next_output_byte = dest->buffer;
   dest->pub.free_in_buffer = BUFFER_SIZE;
   return 1;
}

static void skip_input_data(j_decompress_ptr cinfo, long num_bytes)
{
   struct my_src_mgr *src = (void *)cinfo->src;
   if (num_bytes <= (long)src->pub.bytes_in_buffer) {
      src->pub.next_input_byte += num_bytes;
      src->pub.bytes_in_buffer -= num_bytes;
   }
   else {
      long skip = num_bytes - src->pub.bytes_in_buffer;
      al_fseek(src->fp, skip, ALLEGRO_SEEK_CUR);
      src->pub.bytes_in_buffer = 0;
   }
}

static void term_source(j_decompress_ptr cinfo)
{
   (void)cinfo;
}

static void term_destination(j_compress_ptr cinfo)
{
   struct my_dest_mgr *dest = (void *)cinfo->dest;
   al_fwrite(dest->fp, dest->buffer, BUFFER_SIZE - dest->pub.free_in_buffer);
}


static void jpeg_packfile_src(j_decompress_ptr cinfo, ALLEGRO_FILE *fp,
                              JOCTET *buffer)
{
   struct my_src_mgr *src;

   if (!cinfo->src)
      cinfo->src =
          (*cinfo->mem->alloc_small) ((void *)cinfo, JPOOL_PERMANENT,
                                      sizeof *src);

   src = (void *)cinfo->src;
   src->pub.init_source = init_source;
   src->pub.fill_input_buffer = fill_input_buffer;
   src->pub.skip_input_data = skip_input_data;
   src->pub.resync_to_restart = jpeg_resync_to_restart;
   src->pub.term_source = term_source;
   src->pub.bytes_in_buffer = 0;
   src->buffer = buffer;
   src->fp = fp;
}

static void jpeg_packfile_dest(j_compress_ptr cinfo, ALLEGRO_FILE *fp,
                               JOCTET *buffer)
{
   struct my_dest_mgr *dest;

   if (!cinfo->dest)
      cinfo->dest =
          (*cinfo->mem->alloc_small) ((void *)cinfo, JPOOL_PERMANENT,
                                      sizeof *dest);

   dest = (void *)cinfo->dest;
   dest->pub.init_destination = init_destination;
   dest->pub.empty_output_buffer = empty_output_buffer;
   dest->pub.term_destination = term_destination;
   dest->pub.free_in_buffer = 0;
   dest->buffer = buffer;
   dest->fp = fp;
}

static void my_error_exit(j_common_ptr cinfo)
{
   struct my_err_mgr *jerr = (void *)cinfo->err;

   longjmp(jerr->jmpenv, 1);
}

/* We keep data for load_jpg_entry_helper in a structure allocated in the
 * caller's stack frame to avoid problems with automatic variables being
 * undefined after a longjmp.
 */
struct load_jpg_entry_helper_data {
   bool error;
   ALLEGRO_BITMAP *bmp;
   JOCTET *buffer;
   unsigned char *row;
};

static void load_jpg_entry_helper(ALLEGRO_FILE *fp,
   struct load_jpg_entry_helper_data *data)
{
   struct jpeg_decompress_struct cinfo;
   struct my_err_mgr jerr;
   ALLEGRO_LOCKED_REGION *lock;
   int w, h, s;

   data->error = false;

   cinfo.err = jpeg_std_error(&jerr.pub);
   jerr.pub.error_exit = my_error_exit;
   if (setjmp(jerr.jmpenv) != 0) {
      /* Longjmp'd. */
      data->error = true;
      goto longjmp_error;
   }

   data->buffer = al_malloc(BUFFER_SIZE);
   if (!data->buffer) {
      data->error = true;
      goto error;
   }

   jpeg_create_decompress(&cinfo);
   jpeg_packfile_src(&cinfo, fp, data->buffer);
   jpeg_read_header(&cinfo, true);
   jpeg_start_decompress(&cinfo);

   w = cinfo.output_width;
   h = cinfo.output_height;
   s = cinfo.output_components;

   /* Only one and three components make sense in a JPG file. */
   if (s != 1 && s != 3) {
      data->error = true;
      goto error;
   }

   data->bmp = al_create_bitmap(w, h);
   if (!data->bmp) {
      data->error = true;
      goto error;
   }

   /* Allegro's pixel format is endian independent, so that in
    * ALLEGRO_PIXEL_FORMAT_RGB_888 the lower 8 bits always hold the Blue
    * component.  On a little endian system this is in byte 0.  On a big
    * endian system this is in byte 2.
    *
    * libjpeg expects byte 0 to hold the Red component, byte 1 to hold the
    * Green component, byte 2 to hold the Blue component.  Hence on little
    * endian systems we need the opposite format, ALLEGRO_PIXEL_FORMAT_BGR_888.
    */
#ifdef ALLEGRO_BIG_ENDIAN
   lock = al_lock_bitmap(data->bmp, ALLEGRO_PIXEL_FORMAT_RGB_888,
       ALLEGRO_LOCK_WRITEONLY);
#else
   lock = al_lock_bitmap(data->bmp, ALLEGRO_PIXEL_FORMAT_BGR_888,
       ALLEGRO_LOCK_WRITEONLY);
#endif

   if (s == 3) {
      /* Colour. */
      int y;

      for (y = cinfo.output_scanline; y < h; y = cinfo.output_scanline) {
         unsigned char *out[1];
         out[0] = ((unsigned char *)lock->data) + y * lock->pitch;
         jpeg_read_scanlines(&cinfo, (void *)out, 1);
      }
   }
   else if (s == 1) {
      /* Greyscale. */
      unsigned char *in;
      unsigned char *out;
      int x, y;

      data->row = al_malloc(w);
      for (y = cinfo.output_scanline; y < h; y = cinfo.output_scanline) {
         jpeg_read_scanlines(&cinfo, (void *)&data->row, 1);
         in = data->row;
         out = ((unsigned char *)lock->data) + y * lock->pitch;
         for (x = 0; x < w; x++) {
            *out++ = *in;
            *out++ = *in;
            *out++ = *in;
            in++;
         }
      }
   }

 error:
   jpeg_finish_decompress(&cinfo);

 longjmp_error:
   jpeg_destroy_decompress(&cinfo);

   if (data->bmp) {
      if (al_is_bitmap_locked(data->bmp)) {
         al_unlock_bitmap(data->bmp);
      }
      if (data->error) {
         al_destroy_bitmap(data->bmp);
         data->bmp = NULL;
      }
   }

   al_free(data->buffer);
   al_free(data->row);
}

ALLEGRO_BITMAP *_al_load_jpg_f(ALLEGRO_FILE *fp)
{
   struct load_jpg_entry_helper_data data;

   memset(&data, 0, sizeof(data));
   load_jpg_entry_helper(fp, &data);

   return data.bmp;
}

/* See comment about load_jpg_entry_helper_data. */
struct save_jpg_entry_helper_data {
   bool error;
   JOCTET *buffer;
};

static void save_jpg_entry_helper(ALLEGRO_FILE *fp, ALLEGRO_BITMAP *bmp,
   struct save_jpg_entry_helper_data *data)
{
   struct jpeg_compress_struct cinfo;
   struct my_err_mgr jerr;
   ALLEGRO_LOCKED_REGION *lock;

   data->error = false;

   cinfo.err = jpeg_std_error(&jerr.pub);
   jerr.pub.error_exit = my_error_exit;
   if (setjmp(jerr.jmpenv)) {
      /* Longjmp'd. */
      data->error = true;
      goto longjmp_error;
   }

   data->buffer = al_malloc(BUFFER_SIZE);
   if (!data->buffer) {
      data->error = true;
      goto error;
   }

   jpeg_create_compress(&cinfo);
   jpeg_packfile_dest(&cinfo, fp, data->buffer);

   cinfo.image_width = al_get_bitmap_width(bmp);
   cinfo.image_height = al_get_bitmap_height(bmp);
   cinfo.input_components = 3;
   cinfo.in_color_space = JCS_RGB;
   jpeg_set_defaults(&cinfo);

   jpeg_start_compress(&cinfo, 1);

   /* See comment in load_jpg_entry_helper. */
#ifdef ALLEGRO_BIG_ENDIAN
   lock = al_lock_bitmap(bmp, ALLEGRO_PIXEL_FORMAT_RGB_888,
      ALLEGRO_LOCK_READONLY);
#else
   lock = al_lock_bitmap(bmp, ALLEGRO_PIXEL_FORMAT_BGR_888,
      ALLEGRO_LOCK_READONLY);
#endif

   while (cinfo.next_scanline < cinfo.image_height) {
      unsigned char *row[1];
      row[0] = ((unsigned char *)lock->data)
         + cinfo.next_scanline * lock->pitch;
      jpeg_write_scanlines(&cinfo, (void *)row, 1);
   }

 error:
   jpeg_finish_compress(&cinfo);

 longjmp_error:
   jpeg_destroy_compress(&cinfo);

   if (al_is_bitmap_locked(bmp)) {
      al_unlock_bitmap(bmp);
   }

   al_free(data->buffer);
}

bool _al_save_jpg_f(ALLEGRO_FILE *fp, ALLEGRO_BITMAP *bmp)
{
   struct save_jpg_entry_helper_data data;

   memset(&data, 0, sizeof(data));
   save_jpg_entry_helper(fp, bmp, &data);

   return !data.error;
}

ALLEGRO_BITMAP *_al_load_jpg(char const *filename)
{
   ALLEGRO_FILE *fp;
   ALLEGRO_BITMAP *bmp;

   ASSERT(filename);

   fp = al_fopen(filename, "rb");
   if (!fp)
      return NULL;

   bmp = _al_load_jpg_f(fp);

   al_fclose(fp);

   return bmp;
}

bool _al_save_jpg(char const *filename, ALLEGRO_BITMAP *bmp)
{
   ALLEGRO_FILE *fp;
   bool result;

   ASSERT(filename);
   ASSERT(bmp);

   fp = al_fopen(filename, "wb");
   if (!fp) {
      TRACE("Unable to open file %s for writing\n", filename);
      return false;
   }

   result = _al_save_jpg_f(fp, bmp);

   al_fclose(fp);

   return result;
}

/* vim: set sts=3 sw=3 et: */
