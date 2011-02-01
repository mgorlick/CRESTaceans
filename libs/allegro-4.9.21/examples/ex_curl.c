/*
 *    Example program for Allegro library.
 *
 *    Custom file stream using cURL library.
 */

/*
 * Adapted from libcurl example fopen.c; licensed as follows:
 *---
 * Coyright (c)2003 Simtec Electronics
 *
 * Re-implemented by Vincent Sanders <vince@kyllikki.org> with extensive
 * reference to original curl example code
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


#include <stdio.h>
#include <string.h>
#ifndef WIN32
#  include <sys/time.h>
#endif
#include <stdlib.h>
#include <errno.h>

#include <curl/curl.h>
#include <allegro5/allegro5.h>
#include <allegro5/allegro_image.h>

#include "common.c"


typedef struct CURL_FILE CURL_FILE;
struct CURL_FILE {
   ALLEGRO_FILE file;
   CURL *curl;
   char *buffer;           /* buffer to store cached data */
   size_t buffer_len;      /* currently allocated buffers length */
   size_t buffer_pos;      /* end of data in buffer*/
   int still_running;      /* Is background url fetch still in progress */
};


/* forward declaration */
static ALLEGRO_FILE_INTERFACE curl_file_vtable;


/* we use a global one for convenience */
static CURLM *multi_handle;


/* curl calls this routine to get more data. */
static size_t write_callback(char *buffer, size_t size, size_t nitems,
   void *userp)
{
   CURL_FILE *cf = (CURL_FILE *)userp;
   char *newbuff;
   size_t rembuff;

   size *= nitems;

   rembuff = cf->buffer_len - cf->buffer_pos;

   if (size > rembuff) {
      /* Not enough space in buffer. */
      newbuff = realloc(cf->buffer, cf->buffer_len + size - rembuff);
      if (!newbuff) {
         fprintf(stderr, "callback buffer grow failed\n");
         size = rembuff;
      }
      else {
         /* realloc increase buffer size. */
         cf->buffer_len += size - rembuff;
         cf->buffer = newbuff;
      }
   }

   memcpy(cf->buffer + cf->buffer_pos, buffer, size);
   cf->buffer_pos += size;
   return size;
}


static ALLEGRO_FILE *curl_file_fopen(const char *path, const char *mode)
{
   CURL_FILE *cf;

   /* Only support reading. */
   if (strcmp(mode, "r") != 0 && strcmp(mode, "rb") != 0)
      return NULL;

   cf = malloc(sizeof(*cf));
   if (!cf)
      return NULL;
   memset(cf, 0, sizeof(*cf));

   cf->file.vtable = &curl_file_vtable;
   cf->curl = curl_easy_init();
   curl_easy_setopt(cf->curl, CURLOPT_URL, path);
   curl_easy_setopt(cf->curl, CURLOPT_WRITEDATA, cf);
   curl_easy_setopt(cf->curl, CURLOPT_VERBOSE, 1);
   curl_easy_setopt(cf->curl, CURLOPT_WRITEFUNCTION, write_callback);

   if (!multi_handle)
      multi_handle = curl_multi_init();

   curl_multi_add_handle(multi_handle, cf->curl);

   /* Let's start the fetch. */
   while (curl_multi_perform(multi_handle, &cf->still_running) ==
      CURLM_CALL_MULTI_PERFORM);

   if ((cf->buffer_pos == 0) && (!cf->still_running)) {
      /* If still_running is 0 now, we should return NULL. */
      curl_multi_remove_handle(multi_handle, cf->curl);
      curl_easy_cleanup(cf->curl);
      free(cf);
      cf = NULL;
   }

   return (ALLEGRO_FILE *)cf;
}


static void curl_file_fclose(ALLEGRO_FILE *f)
{
   CURL_FILE *cf = (CURL_FILE *)f;

   curl_multi_remove_handle(multi_handle, cf->curl);
   curl_easy_cleanup(cf->curl);
   if (cf->buffer)
      free(cf->buffer);
   free(cf);
}


static bool fill_buffer(CURL_FILE *cf, size_t size)
{
   fd_set fdread;
   fd_set fdwrite;
   fd_set fdexcep;
   int maxfd;
   struct timeval timeout;
   int rc;

   /* Only attempt to fill buffer if transactions still running and buffer
    * doesn't exceed required size already.
    */
   if (!cf->still_running || cf->buffer_pos > size)
      return false;

   /* Attempt to fill buffer. */
   do {
      FD_ZERO(&fdread);
      FD_ZERO(&fdwrite);
      FD_ZERO(&fdexcep);

      /* Set a suitable timeout to fail on. */
      timeout.tv_sec = 10;
      timeout.tv_usec = 0;

      /* Get file descriptors from the transfers. */
      curl_multi_fdset(multi_handle, &fdread, &fdwrite, &fdexcep, &maxfd);

      /* In a real-world program you OF COURSE check the return code of the
       * function calls, *and* you make sure that maxfd is bigger than -1
       * so that the call to select() below makes sense!
       */
      rc = select(maxfd + 1, &fdread, &fdwrite, &fdexcep, &timeout);
      switch (rc) {
         case -1:
            /* select error */
            break;

         case 0:
            break;

         default:
            /* Timeout or readable/writable sockets. */
            /* Note we *could* be more efficient and not wait for
             * CURLM_CALL_MULTI_PERFORM to clear here and check it on
             * re-entry but that gets messy.
             */
            while (curl_multi_perform(multi_handle, &cf->still_running) ==
               CURLM_CALL_MULTI_PERFORM);
            break;
      }
   } while (cf->still_running && cf->buffer_pos < size);

   return true;
}


static void use_buffer(CURL_FILE *cf, size_t size)
{
   if (cf->buffer_pos - size <= 0) {
      cf->buffer_pos = 0;
   }
   else {
      /* Move rest down make it available for later. */
      memmove(cf->buffer, cf->buffer + size, cf->buffer_pos - size);
      cf->buffer_pos -= size;
   }
}


static size_t curl_file_fread(ALLEGRO_FILE *f, void *ptr, size_t size)
{
   CURL_FILE *cf = (CURL_FILE *)f;

   fill_buffer(cf, size);
   if (!cf->buffer_pos)
      return 0;

   if (cf->buffer_pos < size)
      size = cf->buffer_pos;

   memcpy(ptr, cf->buffer, size);
   use_buffer(cf, size);
   return size;
}


static size_t curl_file_fwrite(ALLEGRO_FILE *f, const void *ptr, size_t size)
{
   (void)f;
   (void)ptr;
   (void)size;
   al_set_errno(EBADF);
   return 0;
}


static bool curl_file_fflush(ALLEGRO_FILE *f)
{
   (void)f;
   return true;
}


static int64_t curl_file_ftell(ALLEGRO_FILE *f)
{
   /* Not implemented. */
   (void)f;
   al_set_errno(ENOSYS);
   return -1;
}


static bool curl_file_fseek(ALLEGRO_FILE *f, int64_t offset, int whence)
{
   if (whence != ALLEGRO_SEEK_CUR || offset < 0) {
      /* Not implemented. */
      al_set_errno(ENOSYS);
      return false;
   }

   while (offset > 0) {
      if (al_fgetc(f) == EOF)
         break;
      offset--;
   }

   return offset == 0;
}


static bool curl_file_feof(ALLEGRO_FILE *f)
{
   CURL_FILE *cf = (CURL_FILE *)f;

   return (cf->buffer_pos == 0 && !cf->still_running);
}


static bool curl_file_ferror(ALLEGRO_FILE *f)
{
   /* Not implemented. */
   (void)f;
   return false;
}


static int curl_file_fungetc(ALLEGRO_FILE *f, int c)
{
   /* Not implemented. */
   (void)f;
   (void)c;
   al_set_errno(ENOSYS);
   return -1;
}


static off_t curl_file_fsize(ALLEGRO_FILE *f)
{
   /* Not implemented. */
   (void)f;
   al_set_errno(ENOSYS);
   return -1;
}


static ALLEGRO_FILE_INTERFACE curl_file_vtable =
{
   curl_file_fopen,
   curl_file_fclose,
   curl_file_fread,
   curl_file_fwrite,
   curl_file_fflush,
   curl_file_ftell,
   curl_file_fseek,
   curl_file_feof,
   curl_file_ferror,
   curl_file_fungetc,
   curl_file_fsize
};


static void show_image(ALLEGRO_BITMAP *bmp)
{
   ALLEGRO_EVENT_QUEUE *queue;
   ALLEGRO_EVENT event;

   queue = al_create_event_queue();
   al_register_event_source(queue, al_get_keyboard_event_source());

   while (true) {
      al_draw_bitmap(bmp, 0, 0, 0);
      al_flip_display();
      al_wait_for_event(queue, &event);
      if (event.type == ALLEGRO_EVENT_KEY_DOWN
            && event.keyboard.keycode == ALLEGRO_KEY_ESCAPE) {
         break;
      }
   }

   al_destroy_event_queue(queue);
}


int main(int argc, const char *argv[])
{
   const char *url;
   ALLEGRO_DISPLAY *display;
   ALLEGRO_BITMAP *bmp;

   if (argc > 1)
      url = argv[1];
   else
      url = "http://liballeg.org/images/logo.png";

   if (!al_init())
      return 1;
   al_init_image_addon();
   al_install_keyboard();

   display = al_create_display(640, 480);
   if (!display)
      return 1;

   curl_global_init(CURL_GLOBAL_ALL);
   al_set_new_file_interface(&curl_file_vtable);

   bmp = al_load_bitmap(url);
   if (bmp) {
      show_image(bmp);
      al_destroy_bitmap(bmp);
   }

   curl_global_cleanup();

   return 0;
}


/* vim: set sts=3 sw=3 et: */
