#include "allegro5/allegro5.h"
#include "allegro5/internal/aintern.h"
#include "allegro5/internal/aintern_file.h"


/* Function: al_fopen
 */
ALLEGRO_FILE *al_fopen(const char *path, const char *mode)
{
   const ALLEGRO_FILE_INTERFACE *drv = al_get_new_file_interface();
   ASSERT(path);
   ASSERT(mode);
   ASSERT(drv);

   if (drv->fi_fopen)
      return drv->fi_fopen(path, mode);
   else
      return NULL;
}


/* Function: al_fclose
 */
void al_fclose(ALLEGRO_FILE *f)
{
   if (f) {
      f->vtable->fi_fclose(f);
   }
}


/* Function: al_fread
 */
size_t al_fread(ALLEGRO_FILE *f, void *ptr, size_t size)
{
   ASSERT(f);
   ASSERT(ptr);

   return f->vtable->fi_fread(f, ptr, size);
}


/* Function: al_fwrite
 */
size_t al_fwrite(ALLEGRO_FILE *f, const void *ptr, size_t size)
{
   ASSERT(f);
   ASSERT(ptr);

   return f->vtable->fi_fwrite(f, ptr, size);
}


/* Function: al_fflush
 */
bool al_fflush(ALLEGRO_FILE *f)
{
   ASSERT(f);

   return f->vtable->fi_fflush(f);
}


/* Function: al_ftell
 */
int64_t al_ftell(ALLEGRO_FILE *f)
{
   ASSERT(f);

   return f->vtable->fi_ftell(f);
}


/* Function: al_fseek
 */
bool al_fseek(ALLEGRO_FILE *f, int64_t offset, int whence)
{
   ASSERT(f);
   /* offset can be negative */
   ASSERT(
      whence == ALLEGRO_SEEK_SET ||
      whence == ALLEGRO_SEEK_CUR ||
      whence == ALLEGRO_SEEK_END
   );

   return f->vtable->fi_fseek(f, offset, whence);
}


/* Function: al_feof
 */
bool al_feof(ALLEGRO_FILE *f)
{
   ASSERT(f);

   return f->vtable->fi_feof(f);
}


/* Function: al_ferror
 */
bool al_ferror(ALLEGRO_FILE *f)
{
   ASSERT(f);

   return f->vtable->fi_ferror(f);
}


/* Function: al_fgetc
 */
int al_fgetc(ALLEGRO_FILE *f)
{
   uint8_t c;
   ASSERT(f);

   if (al_fread(f, &c, 1) != 1) {
      return EOF;
   }

   return c;
}


/* Function: al_fputc
 */
int al_fputc(ALLEGRO_FILE *f, int c)
{
   ASSERT(f);

   if (al_fwrite(f, &c, 1) != 1) {
      return EOF;
   }

   return c;
}


/* Function: al_fread16le
 */
int16_t al_fread16le(ALLEGRO_FILE *f)
{
   unsigned char b[2];
   ASSERT(f);

   if (al_fread(f, b, 2) == 2) {
      return (((int16_t)b[1] << 8) | (int16_t)b[0]);
   }

   return EOF;
}


/* Function: al_fread32le
 */
int32_t al_fread32le(ALLEGRO_FILE *f)
{
   unsigned char b[4];
   ASSERT(f);

   if (al_fread(f, b, 4) == 4) {
      return (((int32_t)b[3] << 24) | ((int32_t)b[2] << 16) |
              ((int32_t)b[1] << 8) | (int32_t)b[0]);
   }

   return EOF;
}


/* Function: al_fwrite16le
 */
size_t al_fwrite16le(ALLEGRO_FILE *f, int16_t w)
{
   int8_t b1, b2;
   ASSERT(f);

   b1 = (w & 0xFF00) >> 8;
   b2 = w & 0x00FF;

   if (al_fputc(f, b2) == b2) {
      if (al_fputc(f, b1) == b1) {
         return 2;
      }
      return 1;
   }
   return 0;
}


/* Function: al_fwrite32le
 */
size_t al_fwrite32le(ALLEGRO_FILE *f, int32_t l)
{
   int8_t b1, b2, b3, b4;
   ASSERT(f);

   b1 = ((l & 0xFF000000L) >> 24);
   b2 = ((l & 0x00FF0000L) >> 16);
   b3 = ((l & 0x0000FF00L) >> 8);
   b4 = l & 0x00FF;

   if (al_fputc(f, b4) == b4) {
      if (al_fputc(f, b3) == b3) {
         if (al_fputc(f, b2) == b2) {
            if (al_fputc(f, b1) == b1) {
               return 4;
            }
            return 3;
         }
         return 2;
      }
      return 1;
   }
   return 0;
}


/* Function: al_fread16be
 */
int16_t al_fread16be(ALLEGRO_FILE *f)
{
   unsigned char b[2];
   ASSERT(f);

   if (al_fread(f, b, 2) == 2) {
      return (((int16_t)b[0] << 8) | (int16_t)b[1]);
   }

   return EOF;
}


/* Function: al_fread32be
 */
int32_t al_fread32be(ALLEGRO_FILE *f)
{
   unsigned char b[4];
   ASSERT(f);

   if (al_fread(f, b, 4) == 4) {
      return (((int32_t)b[0] << 24) | ((int32_t)b[1] << 16) |
              ((int32_t)b[2] << 8) | (int32_t)b[3]);
   }

   return EOF;
}


/* Function: al_fwrite16be
 */
size_t al_fwrite16be(ALLEGRO_FILE *f, int16_t w)
{
   int b1, b2;
   ASSERT(f);

   b1 = (w & 0xFF00) >> 8;
   b2 = w & 0x00FF;

   if (al_fputc(f, b1) == b1) {
      if (al_fputc(f, b2) == b2) {
         return 2;
      }
      return 1;
   }
   return 0;
}


/* Function: al_fwrite32be
 */
size_t al_fwrite32be(ALLEGRO_FILE *f, int32_t l)
{
   int b1, b2, b3, b4;
   ASSERT(f);

   b1 = ((l & 0xFF000000L) >> 24);
   b2 = ((l & 0x00FF0000L) >> 16);
   b3 = ((l & 0x0000FF00L) >> 8);
   b4 = l & 0x00FF;

   if (al_fputc(f, b1)==b1) {
      if (al_fputc(f, b2)==b2) {
         if (al_fputc(f, b3)==b3) {
            if (al_fputc(f, b4)==b4)
               return 4;
            return 3;
         }
         return 2;
      }
      return 1;
   }
   return 0;
}


/* Function: al_fgets
 */
char *al_fgets(ALLEGRO_FILE *f, char * const buf, size_t max)
{
   char *p = buf;
   int c;
   ASSERT(f);
   ASSERT(buf);

   /* Handle silly cases. */
   if (max == 0) {
      return NULL;
   }
   if (max == 1) {
      *buf = '\0';
      return buf;
   }

   /* Return NULL if already at end of file. */
   if ((c = al_fgetc(f)) == EOF) {
      return NULL;
   }

   /* Fill buffer until empty, or we reach a newline or EOF or error. */
   do {
      *p++ = c;
      max--;
      if (max == 1 || c == '\n')
         break;
      c = al_fgetc(f);
   } while (c != EOF);

   /* Return NULL on error. */
   if (c == EOF && al_ferror(f)) {
      return NULL;
   }

   /* Add null terminator. */
   ASSERT(max >= 1);
   *p = '\0';

   return buf;
}


/* Function: al_fget_ustr
 */
ALLEGRO_USTR *al_fget_ustr(ALLEGRO_FILE *f)
{
   ALLEGRO_USTR *us;
   char buf[128];

   if (!al_fgets(f, buf, sizeof(buf))) {
      return NULL;
   }

   us = al_ustr_new("");

   do {
      al_ustr_append_cstr(us, buf);
      if (al_ustr_has_suffix_cstr(us, "\n"))
         break;
   } while (al_fgets(f, buf, sizeof(buf)));

   return us;
}


/* Function: al_fputs
 */
int al_fputs(ALLEGRO_FILE *f, char const *p)
{
   size_t n;
   ASSERT(f);
   ASSERT(p);

   n = strlen(p);
   if (al_fwrite(f, p, n) != n) {
      return EOF;
   }

   return n;
}


/* Function: al_fungetc
 */
int al_fungetc(ALLEGRO_FILE *f, int c)
{
   ASSERT(f != NULL);

   return f->vtable->fi_fungetc(f, c);
}


/* Function: al_fsize
 */
int64_t al_fsize(ALLEGRO_FILE *f)
{
   ASSERT(f != NULL);

   return f->vtable->fi_fsize(f);
}


/* vim: set sts=3 sw=3 et: */
