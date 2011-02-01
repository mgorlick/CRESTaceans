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
 *      OpenGL routines common to all OpenGL drivers.
 *
 *      By Elias Pschernig and Milan Mimica.
 *
 */

#include "allegro5/allegro5.h"
#include "allegro5/allegro_opengl.h"
#include "allegro5/internal/aintern.h"
#include "allegro5/internal/aintern_opengl.h"
#include "allegro5/internal/aintern_pixels.h"

#ifdef ALLEGRO_GP2XWIZ
#include "allegro5/internal/aintern_gp2xwiz.h"
#endif

#ifdef ALLEGRO_IPHONE
#include "allegro5/internal/aintern_iphone.h"
#endif

ALLEGRO_DEBUG_CHANNEL("opengl")

static void setup_fbo(ALLEGRO_DISPLAY *display, ALLEGRO_BITMAP *bitmap)
{
   ALLEGRO_BITMAP_OGL *ogl_bitmap;
   
   if (bitmap->parent) bitmap = bitmap->parent;
   ogl_bitmap = (void *)bitmap;

#if !defined ALLEGRO_GP2XWIZ
   if (!ogl_bitmap->is_backbuffer) {
       
      // FIXME: ...
      #ifdef ALLEGRO_IPHONE
      #define glGenFramebuffersEXT glGenFramebuffersOES
      #define glBindFramebufferEXT glBindFramebufferOES
      #define GL_FRAMEBUFFER_EXT GL_FRAMEBUFFER_OES
      #define GL_COLOR_ATTACHMENT0_EXT GL_COLOR_ATTACHMENT0_OES
      #define glCheckFramebufferStatusEXT glCheckFramebufferStatusOES
      #define glFramebufferTexture2DEXT glFramebufferTexture2DOES
      #define GL_FRAMEBUFFER_COMPLETE_EXT GL_FRAMEBUFFER_COMPLETE_OES
      #define glDeleteFramebuffersEXT glDeleteFramebuffersOES
      #define glOrtho glOrthof
      #endif

      /* When a bitmap is set as target bitmap, we try to create an FBO for it.
       */
      if (ogl_bitmap->fbo == 0 && !(bitmap->flags & ALLEGRO_FORCE_LOCKING)) {
         if (al_get_opengl_extension_list()->ALLEGRO_GL_EXT_framebuffer_object ||
            al_get_opengl_extension_list()->ALLEGRO_GL_OES_framebuffer_object) {
            glGenFramebuffersEXT(1, &ogl_bitmap->fbo);
         }
      }

      if (ogl_bitmap->fbo) {
         /* Bind to the FBO. */
         ASSERT(display->ogl_extras->extension_list->ALLEGRO_GL_EXT_framebuffer_object ||
         display->ogl_extras->extension_list->ALLEGRO_GL_OES_framebuffer_object);
         glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, ogl_bitmap->fbo);

         /* Attach the texture. */
         glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT,
            GL_TEXTURE_2D, ogl_bitmap->texture, 0);
         if (glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT) !=
            GL_FRAMEBUFFER_COMPLETE_EXT) {
            /* For some reason, we cannot use the FBO with this
             * texture. So no reason to keep re-trying, output a log
             * message and switch to (extremely slow) software mode.
             */
            ALLEGRO_ERROR("Could not use FBO for bitmap with format %s.\n",
               _al_format_name(bitmap->format));
            ALLEGRO_ERROR("*** SWITCHING TO SOFTWARE MODE ***\n");
            glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
            glDeleteFramebuffersEXT(1, &ogl_bitmap->fbo);
            ogl_bitmap->fbo = 0;
         }
         else {
            display->ogl_extras->opengl_target = ogl_bitmap;

            glViewport(0, 0, bitmap->w, bitmap->h);

            glMatrixMode(GL_PROJECTION);
            glLoadIdentity();

            glOrtho(0, bitmap->w, bitmap->h, 0, -1, 1);
         }
      }
   }
   else {
      display->ogl_extras->opengl_target = ogl_bitmap;
       
      // TODO: Might as well have a vtable entry here
      #ifdef ALLEGRO_IPHONE
      _al_iphone_setup_opengl_view(display);
      #else

      if (display->ogl_extras->extension_list->ALLEGRO_GL_EXT_framebuffer_object ||
          display->ogl_extras->extension_list->ALLEGRO_GL_OES_framebuffer_object) {
         glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
      }      

      glViewport(0, 0, display->w, display->h);

      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      /* We use upside down coordinates compared to OpenGL, so the bottommost
       * coordinate is display->h not 0.
       */
      glOrtho(0, display->w, display->h, 0, -1, 1);
      #endif
   }
#else

   ALLEGRO_DISPLAY_GP2XWIZ_OGL *wiz_disp = (ALLEGRO_DISPLAY_GP2XWIZ_OGL *)display;
   display->ogl_extras->opengl_target = ogl_bitmap;

   if (!ogl_bitmap->is_backbuffer) {
      /* FIXME: implement */
   }
   else {
      eglMakeCurrent(wiz_disp->egl_display, wiz_disp->egl_surface, wiz_disp->egl_surface, wiz_disp->egl_context); 

      glViewport(0, 0, display->w, display->h);

      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      /* We use upside down coordinates compared to OpenGL, so the bottommost
       * coordinate is display->h not 0.
       */
      glOrtho(0, display->w, display->h, 0, -1, 1);
   }
#endif
}

void _al_ogl_set_target_bitmap(ALLEGRO_DISPLAY *display, ALLEGRO_BITMAP *bitmap)
{
   ALLEGRO_BITMAP_OGL *ogl_bitmap = (void *)bitmap;
   if (bitmap->parent)
      ogl_bitmap = (void *)bitmap->parent;

   if (!bitmap->locked) {
      setup_fbo(display, bitmap);

      if (display->ogl_extras->opengl_target == ogl_bitmap) {
         _al_ogl_setup_bitmap_clipping(bitmap);
      }
   }
}


void _al_ogl_setup_bitmap_clipping(const ALLEGRO_BITMAP *bitmap)
{
   int x_1, y_1, x_2, y_2, h;
   bool use_scissor = true;

   x_1 = bitmap->cl;
   y_1 = bitmap->ct;
   x_2 = bitmap->cr_excl;
   y_2 = bitmap->cb_excl;
   h = bitmap->h;

   /* Drawing onto the sub bitmap is handled by clipping the parent. */
   if (bitmap->parent) {
      x_1 += bitmap->xofs;
      y_1 += bitmap->yofs;
      x_2 += bitmap->xofs;
      y_2 += bitmap->yofs;
      h = bitmap->parent->h;
   }

   if (x_1 == 0 &&  y_1 == 0 && x_2 == bitmap->w && y_2 == bitmap->h) {
      if (bitmap->parent) {
         /* Can only disable scissor if the sub-bitmap covers the
          * complete parent.
          */
         if (bitmap->xofs == 0 && bitmap->yofs == 0 && bitmap->w ==
            bitmap->parent->w && bitmap->h == bitmap->parent->h) {
               use_scissor = false;
            }
      }
      else
         use_scissor = false;
   }
   if (!use_scissor) {
      glDisable(GL_SCISSOR_TEST);
   }
   else {
      glEnable(GL_SCISSOR_TEST);
      /* OpenGL is upside down, so must adjust y_2 to the height. */
      glScissor(x_1, h - y_2, x_2 - x_1, y_2 - y_1);
   }
}


ALLEGRO_BITMAP *_al_ogl_get_backbuffer(ALLEGRO_DISPLAY *d)
{
   return (ALLEGRO_BITMAP *)d->ogl_extras->backbuffer;
}


bool _al_ogl_resize_backbuffer(ALLEGRO_BITMAP_OGL *b, int w, int h)
{
   int pitch;
   int bytes;
         
   pitch = w * al_get_pixel_size(b->bitmap.format);

   b->bitmap.w = w;
   b->bitmap.h = h;
   b->bitmap.pitch = pitch;
   b->bitmap.cl = 0;
   b->bitmap.ct = 0;
   b->bitmap.cr_excl = w;
   b->bitmap.cb_excl = h;

   /* There is no texture associated with the backbuffer so no need to care
    * about texture size limitations. */
   b->true_w = w;
   b->true_h = h;

   /* FIXME: lazily manage memory */
   bytes = pitch * h;
   al_free(b->bitmap.memory);
   b->bitmap.memory = al_malloc(bytes);
   memset(b->bitmap.memory, 0, bytes);

   return true;
}


ALLEGRO_BITMAP_OGL* _al_ogl_create_backbuffer(ALLEGRO_DISPLAY *disp)
{
   ALLEGRO_BITMAP_OGL *ogl_backbuffer;
   ALLEGRO_BITMAP *backbuffer;
   ALLEGRO_STATE backup;
   int format;

   al_store_state(&backup, ALLEGRO_STATE_NEW_BITMAP_PARAMETERS);

   // FIXME: _al_deduce_color_format would work fine if the display paramerers
   // are filled in, for WIZ and IPOD
#ifdef ALLEGRO_GP2XWIZ
   format = ALLEGRO_PIXEL_FORMAT_RGB_565; /* Only support display format */
#elif defined ALLEGRO_IPHONE
   format = ALLEGRO_PIXEL_FORMAT_ABGR_8888_LE;
   // TODO: This one is also supported
   //format = ALLEGRO_PIXEL_FORMAT_RGB_565;
#else
   format = _al_deduce_color_format(&disp->extra_settings);
   /* Eww. No OpenGL hardware in the world does that - let's just
    * switch to some default.
    */
   if (al_get_pixel_size(format) == 3) {
      /* Or should we use RGBA? Maybe only if not Nvidia cards? */
      format = ALLEGRO_PIXEL_FORMAT_ABGR_8888;
   }
#endif
   ALLEGRO_TRACE_CHANNEL_LEVEL("display", 1)("Format %s used for backbuffer.\n",
      _al_format_name(format));
   
   /* Now that the display backbuffer has a format, update extra_settings so
    * the user can query it back.
    */
   _al_set_color_components(format, &disp->extra_settings, ALLEGRO_REQUIRE);
   disp->backbuffer_format = format;

   al_set_new_bitmap_format(format);
   al_set_new_bitmap_flags(0);
   backbuffer = _al_ogl_create_bitmap(disp, disp->w, disp->h);
   al_restore_state(&backup);

   if (!backbuffer) {
      ALLEGRO_DEBUG("Backbuffer bitmap creation failed.\n");
      return NULL;
   }
   
   ALLEGRO_TRACE_CHANNEL_LEVEL("display", 1)(
      "Created backbuffer bitmap (actual format: %s)\n",
      _al_format_name(backbuffer->format));

   ogl_backbuffer = (ALLEGRO_BITMAP_OGL*)backbuffer;
   ogl_backbuffer->is_backbuffer = 1;
   backbuffer->display = disp;

   return ogl_backbuffer;
}


void _al_ogl_destroy_backbuffer(ALLEGRO_BITMAP_OGL *b)
{
   al_destroy_bitmap((ALLEGRO_BITMAP *)b);
}


/* vi: set sts=3 sw=3 et: */
