#include "allegro5/internal/aintern_xglx.h"
#include "allegro5/internal/aintern_bitmap.h"
#include "allegro5/internal/aintern_opengl.h"


ALLEGRO_DEBUG_CHANNEL("display")

static ALLEGRO_DISPLAY_INTERFACE xdpy_vt;



/* Helper to set up GL state as we want it. */
static void setup_gl(ALLEGRO_DISPLAY *d)
{
   ALLEGRO_OGL_EXTRAS *ogl = d->ogl_extras;

   glViewport(0, 0, d->w, d->h);

   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glOrtho(0, d->w, d->h, 0, -1, 1);

   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();

   if (ogl->backbuffer)
      _al_ogl_resize_backbuffer(ogl->backbuffer, d->w, d->h);
   else
      ogl->backbuffer = _al_ogl_create_backbuffer(d);
}



static void set_size_hints(ALLEGRO_DISPLAY *d)
{
   if (d->flags & ALLEGRO_RESIZABLE) return;

   ALLEGRO_SYSTEM_XGLX *system = (void *)al_get_system_driver();
   ALLEGRO_DISPLAY_XGLX *glx = (void *)d;
   XSizeHints *hints = XAllocSizeHints();;

   int w = d->w;
   int h = d->h;
   hints->flags = PMinSize | PMaxSize | PBaseSize;
   hints->min_width  = hints->max_width  = hints->base_width  = w;
   hints->min_height = hints->max_height = hints->base_height = h;
   XSetWMNormalHints(system->x11display, glx->window, hints);

   XFree(hints);
}



static void reset_size_hints(ALLEGRO_DISPLAY *d)
{
   ALLEGRO_SYSTEM_XGLX *system = (void *)al_get_system_driver();
   ALLEGRO_DISPLAY_XGLX *glx = (void *)d;
   XSizeHints *hints = XAllocSizeHints();;

   hints->flags = PMinSize | PMaxSize;
   hints->min_width  = 0;
   hints->min_height = 0;
   // FIXME: Is there a way to remove/reset max dimensions?
   hints->max_width  = 32768;
   hints->max_height = 32768;
   XSetWMNormalHints(system->x11display, glx->window, hints);

   XFree(hints);
}



/* Helper to set a window icon.  We use the _NET_WM_ICON property which is
 * supported by modern window managers.
 *
 * The old method is XSetWMHints but the (antiquated) ICCCM talks about 1-bit
 * pixmaps.  For colour icons, perhaps you're supposed use the icon_window,
 * and draw the window yourself?
 */
static void xdpy_set_icon(ALLEGRO_DISPLAY *d, ALLEGRO_BITMAP *bitmap)
{
   ALLEGRO_SYSTEM_XGLX *system = (ALLEGRO_SYSTEM_XGLX *)al_get_system_driver();
   ALLEGRO_DISPLAY_XGLX *glx = (ALLEGRO_DISPLAY_XGLX *)d;
   int w = al_get_bitmap_width(bitmap);
   int h = al_get_bitmap_height(bitmap);
   int data_size;
   unsigned long *data; /* Yes, unsigned long, even on 64-bit platforms! */

   data_size = 2 + w * h;
   data = al_malloc(data_size * sizeof(data[0]));

   if (data) {
      ALLEGRO_LOCKED_REGION *lr = al_lock_bitmap(bitmap,
         ALLEGRO_PIXEL_FORMAT_ANY_WITH_ALPHA, ALLEGRO_LOCK_READONLY);

      if (lr) {
         int x, y;
         ALLEGRO_COLOR c;
         unsigned char r, g, b, a;
         Atom _NET_WM_ICON;

         data[0] = w;
         data[1] = h;
         for (y = 0; y < h; y++) {
            for (x = 0; x < w; x++) {
               c = al_get_pixel(bitmap, x, y);
               al_unmap_rgba(c, &r, &g, &b, &a);
               data[2 + y*w + x] = (a << 24) | (r << 16) | (g << 8) | b;
            }
         }

         _al_mutex_lock(&system->lock);
         _NET_WM_ICON = XInternAtom(system->x11display, "_NET_WM_ICON", False);
         XChangeProperty(system->x11display, glx->window, _NET_WM_ICON,
            XA_CARDINAL, 32, PropModeReplace, (unsigned char *)data, data_size);
         _al_mutex_unlock(&system->lock);

         al_unlock_bitmap(bitmap);
      }

      al_free(data);
   }
}



static void xdpy_set_window_position(ALLEGRO_DISPLAY *display, int x, int y)
{
   ALLEGRO_SYSTEM_XGLX *system = (void *)al_get_system_driver();
   ALLEGRO_DISPLAY_XGLX *glx = (ALLEGRO_DISPLAY_XGLX *)display;
   Window root, parent, child, *children;
   unsigned int n;

   _al_mutex_lock(&system->lock);

   /* To account for the window border, we have to find the parent window which
    * draws the border. If the parent is the root though, then we should not
    * translate.
    */
   XQueryTree(system->x11display, glx->window, &root, &parent, &children, &n);
   if (parent != root) {
      XTranslateCoordinates(system->x11display, parent, glx->window,
         x, y, &x, &y, &child);
   }

   XMoveWindow(system->x11display, glx->window, x, y);
   XFlush(system->x11display);
   _al_mutex_unlock(&system->lock);
}



static void xdpy_toggle_frame(ALLEGRO_DISPLAY *display, bool onoff)
{
   ALLEGRO_SYSTEM_XGLX *system = (void *)al_get_system_driver();
   ALLEGRO_DISPLAY_XGLX *glx = (ALLEGRO_DISPLAY_XGLX *)display;
   Display *x11 = system->x11display;
   Atom hints;

   _al_mutex_lock(&system->lock);

#if 1
   /* This code is taken from the GDK sources. So it works perfectly in Gnome,
    * no idea if it will work anywhere else. X11 documentation itself only
    * describes a way how to make the window completely unmanaged, but that
    * would also require special care in the event handler.
    */
   hints = XInternAtom(x11, "_MOTIF_WM_HINTS", True);
   if (hints) {
      struct {
         unsigned long flags;
         unsigned long functions;
         unsigned long decorations;
         long input_mode;
         unsigned long status;
      } motif = {2, 0, onoff, 0, 0};
      XChangeProperty(x11, glx->window, hints, hints, 32, PropModeReplace,
         (void *)&motif, sizeof motif / 4);
   }
#endif

   _al_mutex_unlock(&system->lock);
}



static void xdpy_toggle_fullscreen_window(ALLEGRO_DISPLAY *display, bool onoff)
{
   ALLEGRO_SYSTEM_XGLX *system = (void *)al_get_system_driver();
   if (onoff == !(display->flags & ALLEGRO_FULLSCREEN_WINDOW)) {
      _al_mutex_lock(&system->lock);
      reset_size_hints(display);
      _al_xglx_toggle_fullscreen_window(display);
      display->flags ^= ALLEGRO_FULLSCREEN_WINDOW;
      set_size_hints(display);
      _al_mutex_unlock(&system->lock);
   }
}



static bool xdpy_toggle_display_flag(ALLEGRO_DISPLAY *display, int flag,
   bool onoff)
{
   switch(flag) {
      case ALLEGRO_NOFRAME:
         xdpy_toggle_frame(display, onoff);
         return true;
      case ALLEGRO_FULLSCREEN_WINDOW:
         xdpy_toggle_fullscreen_window(display, onoff);
         return true;
   }
   return false;
}



static void xdpy_destroy_display(ALLEGRO_DISPLAY *d);



/* Create a new X11 display, which maps directly to a GLX window. */
static ALLEGRO_DISPLAY *xdpy_create_display(int w, int h)
{
   ALLEGRO_SYSTEM_XGLX *system = (void *)al_get_system_driver();
   int adapter = al_get_new_display_adapter();

   _al_mutex_lock(&system->lock);

   if (adapter >= ScreenCount(system->x11display)) {
      /* Fail early, as glXGetFBConfigs may crash otherwise. */
      ALLEGRO_DEBUG("Requested display adapter more than ScreenCount.\n");
      _al_mutex_unlock(&system->lock);
      return NULL;
   }

   ALLEGRO_DISPLAY_XGLX *d = al_malloc(sizeof *d);
   ALLEGRO_DISPLAY *display = (void*)d;
   ALLEGRO_OGL_EXTRAS *ogl = al_malloc(sizeof *ogl);
   memset(d, 0, sizeof *d);
   memset(ogl, 0, sizeof *ogl);
   display->ogl_extras = ogl;

   int major, minor;
   glXQueryVersion(system->x11display, &major, &minor);
   d->glx_version = major * 100 + minor * 10;
   ALLEGRO_INFO("GLX %.1f.\n", d->glx_version / 100.f);

   display->w = w;
   display->h = h;
   display->vt = &xdpy_vt;
   display->refresh_rate = al_get_new_display_refresh_rate();
   display->flags = al_get_new_display_flags();

   // FIXME: default? Is this the right place to set this?
   display->flags |= ALLEGRO_OPENGL;

   // store our initial screen, used by fullscreen and glx visual code
   d->xscreen = adapter;
   if (d->xscreen < 0)
      d->xscreen = 0;

   d->is_mapped = false;
   _al_cond_init(&d->mapped);

   d->resize_count = 0;

   _al_xglx_config_select_visual(d);

   if (!d->xvinfo) {
      ALLEGRO_ERROR("FIXME: Need better visual selection.\n");
      ALLEGRO_ERROR("No matching visual found.\n");
      al_free(d);
      al_free(ogl);
      _al_mutex_unlock(&system->lock);
      return NULL;
   }

   ALLEGRO_INFO("Selected X11 visual %lu.\n", d->xvinfo->visualid);

   /* Add ourself to the list of displays. */
   ALLEGRO_DISPLAY_XGLX **add;
   add = _al_vector_alloc_back(&system->system.displays);
   *add = d;

   /* Each display is an event source. */
   _al_event_source_init(&display->es);

   /* Create a colormap. */
   Colormap cmap = XCreateColormap(system->x11display,
      RootWindow(system->x11display, d->xvinfo->screen),
      d->xvinfo->visual, AllocNone);

   /* Create an X11 window */
   XSetWindowAttributes swa;
   int mask = CWBorderPixel | CWColormap | CWEventMask;
   swa.colormap = cmap;
   swa.border_pixel = 0;
   swa.event_mask =
      KeyPressMask |
      KeyReleaseMask |
      StructureNotifyMask |
      EnterWindowMask |
      LeaveWindowMask |
      FocusChangeMask |
      ExposureMask |
      PropertyChangeMask |
      ButtonPressMask |
      ButtonReleaseMask |
      PointerMotionMask;

   /* For a non-compositing window manager, a black background can look
    * less broken if the application doesn't react to expose events fast
    * enough. However in some cases like resizing, the black background
    * causes horrible flicker.
    */
   if (!(display->flags & ALLEGRO_RESIZABLE)) {
      mask |= CWBackPixel;
      swa.background_pixel = BlackPixel(system->x11display, d->xvinfo->screen);
   }

   int x_off = INT_MAX, y_off = INT_MAX;
   if(display->flags & ALLEGRO_FULLSCREEN) {
      _al_xglx_get_display_offset(system, d->xscreen, &x_off, &y_off);
   }
   else {
      al_get_new_window_position(&x_off, &y_off);
   }

   d->window = XCreateWindow(system->x11display, RootWindow(
      system->x11display, d->xvinfo->screen), x_off != INT_MAX ? x_off : 0, y_off != INT_MAX ? y_off : 0, w, h, 0, d->xvinfo->depth,
      InputOutput, d->xvinfo->visual, mask, &swa);

   // Tell WMs to respect our chosen position,
   // otherwise the x_off/y_off positions passed to
   // XCreateWindow will be ignored by most WMs.
   if (x_off != INT_MAX && y_off != INT_MAX) {
      ALLEGRO_DEBUG("Force window position to %d, %d.\n",
         x_off, y_off);

      XSizeHints sh;
      sh.flags = PPosition;
      XSetWMNormalHints(system->x11display, d->window, &sh);
   }

      // Try to set full screen mode if requested, fail if we can't
   if (display->flags & ALLEGRO_FULLSCREEN) {
      xdpy_toggle_frame(display, false);

      if (!_al_xglx_fullscreen_set_mode(system, d, w, h, 0, display->refresh_rate)) {
         ALLEGRO_DEBUG("xdpy: failed to set fullscreen mode.\n");
         xdpy_destroy_display(display);
         _al_mutex_unlock(&system->lock);
         return NULL;
      }
   }
   
   if (display->flags & ALLEGRO_NOFRAME)
      xdpy_toggle_frame(display, false);

   ALLEGRO_DEBUG("X11 window created.\n");

   set_size_hints(display);

   d->wm_delete_window_atom = XInternAtom (system->x11display,
      "WM_DELETE_WINDOW", False);
   XSetWMProtocols(system->x11display, d->window, &d->wm_delete_window_atom, 1);

   XMapWindow(system->x11display, d->window);
   ALLEGRO_DEBUG("X11 window mapped.\n");

   /* Send the pending request to the X server. */
   XSync(system->x11display, False);
   /* To avoid race conditions where some X11 functions fail before the window
    * is mapped, we wait here until it is mapped. Note that the thread is
    * locked, so the event could not possibly have been processed yet in the
    * events thread. So as long as no other map events occur, the condition
    * should only be signalled when our window gets mapped.
    */
   while (!d->is_mapped) {
      _al_cond_wait(&d->mapped, &system->lock);
   }

   if (display->flags & ALLEGRO_FULLSCREEN) {
      _al_xglx_set_above(display);
   }
      
   /* We can do this at any time, but if we already have a mapped
    * window when switching to fullscreen it will use the same
    * monitor (with the MetaCity version I'm using here right now).
    */
   if (display->flags & ALLEGRO_FULLSCREEN_WINDOW) {
      ALLEGRO_INFO("Toggling fullscreen flag for %d x %d window.\n",
         display->w, display->h);
      reset_size_hints(display);
      _al_xglx_toggle_fullscreen_window(display);
      set_size_hints(display);

      XWindowAttributes xwa;
      XGetWindowAttributes(system->x11display, d->window, &xwa);
      display->w = xwa.width;
      display->h = xwa.height;
      ALLEGRO_INFO("Using ALLEGRO_FULLSCREEN_WINDOW of %d x %d\n",
         display->w, display->h);
   }

   if (!_al_xglx_config_create_context(d)) {
      ALLEGRO_ERROR("Failed to create a context.\n");
      xdpy_destroy_display(display);
      _al_mutex_unlock(&system->lock);
      return NULL;
   }

   if (display->flags & ALLEGRO_FULLSCREEN) {
      _al_xglx_fullscreen_to_display(system, d);
   }

   /* Make our GLX context current for reading and writing in the current
    * thread.
    */
   if (d->fbc) {
      glXMakeContextCurrent(system->gfxdisplay, d->glxwindow, d->glxwindow,
         d->context);
   }
   else {
      glXMakeCurrent(system->gfxdisplay, d->glxwindow, d->context);
   }

   _al_ogl_manage_extensions(display);
   _al_ogl_set_extensions(ogl->extension_api);

   /* Print out OpenGL version info */
   ALLEGRO_INFO("OpenGL Version: %s\n", (const char*)glGetString(GL_VERSION));
   ALLEGRO_INFO("Vendor: %s\n", (const char*)glGetString(GL_VENDOR));
   ALLEGRO_INFO("Renderer: %s\n\n", (const char*)glGetString(GL_RENDERER));

   if (display->ogl_extras->ogl_info.version < 1.2) {
      ALLEGRO_EXTRA_DISPLAY_SETTINGS *eds = _al_get_new_display_settings();
      if (eds->required & (1<<ALLEGRO_COMPATIBLE_DISPLAY)) {
         ALLEGRO_ERROR("Allegro requires at least OpenGL version 1.2 to work.");
         xdpy_destroy_display(display);
         _al_mutex_unlock(&system->lock);
         return NULL;
      }
      display->extra_settings.settings[ALLEGRO_COMPATIBLE_DISPLAY] = 0;
   }
#if 0
   // Apparently, you can get a OpenGL 3.0 context without specifically creating
   // it with glXCreateContextAttribsARB, and not every OpenGL 3.0 is evil, but we
   // can't tell the difference at this stage.
   else if (display->ogl_extras->ogl_info.version > 2.1) {
      /* We don't have OpenGL3 a driver. */
      display->extra_settings.settings[ALLEGRO_COMPATIBLE_DISPLAY] = 0;
   }
#endif

   if (display->extra_settings.settings[ALLEGRO_COMPATIBLE_DISPLAY])
      setup_gl(display);

   /* vsync */

   /* Fill in the user setting. */
   display->extra_settings.settings[ALLEGRO_VSYNC] =
      _al_get_new_display_settings()->settings[ALLEGRO_VSYNC];

   /* We set the swap interval to 0 if vsync is forced off, and to 1
    * if it is forced on.
    * http://www.opengl.org/registry/specs/SGI/swap_control.txt
    * If the option is set to 0, we simply use the system default. The
    * above extension specifies vsync on as default to, so in the end
    * with GLX we can't force vsync on, just off.
    */
   ALLEGRO_DEBUG("requested vsync=%d.\n",
      display->extra_settings.settings[ALLEGRO_VSYNC]);
   if (display->extra_settings.settings[ALLEGRO_VSYNC]) {
      if (display->ogl_extras->extension_list->ALLEGRO_GLX_SGI_swap_control) {
         int x = 1;
         if (display->extra_settings.settings[ALLEGRO_VSYNC] == 2)
            x = 0;
         if (glXSwapIntervalSGI(x)) {
            ALLEGRO_WARN("glXSwapIntervalSGI(%d) failed.\n", x);
         }
      }
      else {
         ALLEGRO_WARN("no vsync, GLX_SGI_swap_control missing.\n");
         display->extra_settings.settings[ALLEGRO_VSYNC] = 0;
      }
   }

   d->invisible_cursor = None; /* Will be created on demand. */
   d->current_cursor = None; /* Initially, we use the root cursor. */
   d->cursor_hidden = false;

   d->icon = None;
   d->icon_mask = None;

   _al_mutex_unlock(&system->lock);

   return display;
}



static void xdpy_destroy_display(ALLEGRO_DISPLAY *d)
{
   ALLEGRO_SYSTEM_XGLX *s = (void *)al_get_system_driver();
   ALLEGRO_DISPLAY_XGLX *glx = (void *)d;
   ALLEGRO_OGL_EXTRAS *ogl = d->ogl_extras;

   ALLEGRO_DEBUG("xdpy: destroy display.\n");

   /* If we're the last display, convert all bitmpas to display independent
    * (memory) bitmaps. */
   if (s->system.displays._size == 1) {
      while (d->bitmaps._size > 0) {
         ALLEGRO_BITMAP **bptr = _al_vector_ref_back(&d->bitmaps);
         ALLEGRO_BITMAP *b = *bptr;
         _al_convert_to_memory_bitmap(b);
      }
   }
   else {
      /* Pass all bitmaps to any other living display. (We assume all displays
       * are compatible.) */
      size_t i;
      ALLEGRO_DISPLAY **living = NULL;
      ASSERT(s->system.displays._size > 1);

      for (i = 0; i < s->system.displays._size; i++) {
         living = _al_vector_ref(&s->system.displays, i);
         if (*living != d)
            break;
      }

      for (i = 0; i < d->bitmaps._size; i++) {
         ALLEGRO_BITMAP **add = _al_vector_alloc_back(&(*living)->bitmaps);
         ALLEGRO_BITMAP **ref = _al_vector_ref(&d->bitmaps, i);
         *add = *ref;
         (*add)->display = *living;
      }
   }

   _al_ogl_unmanage_extensions(d);
   ALLEGRO_DEBUG("xdpy: unmanaged extensions.\n");

   _al_mutex_lock(&s->lock);
   _al_vector_find_and_delete(&s->system.displays, &d);
   XDestroyWindow(s->x11display, glx->window);

   ALLEGRO_DEBUG("xdpy: destroy window.\n");

   if (d->flags & ALLEGRO_FULLSCREEN) {
      ALLEGRO_DEBUG("xfullscreen: restore modes.\n");
      _al_xglx_restore_video_mode(s, glx->xscreen);
   }

   if (ogl->backbuffer) {
      _al_ogl_destroy_backbuffer(ogl->backbuffer);
      ogl->backbuffer = NULL;
      ALLEGRO_DEBUG("xdpy: destroy backbuffer.\n");
   }

   if (glx->context) {
      glXDestroyContext(s->gfxdisplay, glx->context);
      glx->context = NULL;
      ALLEGRO_DEBUG("xdpy: destroy context.\n");
   }

   /* XXX quick pre-release hack */
   /* In multi-window programs these result in a double-free bugs. */
#if 0
   if (glx->fbc) {
      al_free(glx->fbc);
      glx->fbc = NULL;
      XFree(glx->xvinfo);
      glx->xvinfo = NULL;
   }
   else if (glx->xvinfo) {
      al_free(glx->xvinfo);
      glx->xvinfo = NULL;
   }
#endif

   _al_cond_destroy(&glx->mapped);

   _al_vector_free(&d->bitmaps);
   _al_event_source_free(&d->es);

   al_free(d->ogl_extras);
   al_free(d->vertex_cache);
   al_free(d);

   _al_mutex_unlock(&s->lock);

    ALLEGRO_DEBUG("xdpy: destroy display fin.\n");
}



static bool xdpy_set_current_display(ALLEGRO_DISPLAY *d)
{
   ALLEGRO_SYSTEM_XGLX *system = (ALLEGRO_SYSTEM_XGLX *)al_get_system_driver();
   ALLEGRO_DISPLAY_XGLX *glx = (ALLEGRO_DISPLAY_XGLX *)d;
   ALLEGRO_OGL_EXTRAS *ogl = d->ogl_extras;

   /* Make our GLX context current for reading and writing in the current
    * thread.
    */
   if (glx->fbc) {
      if (!glXMakeContextCurrent(system->gfxdisplay, glx->glxwindow,
                                glx->glxwindow, glx->context))
         return false;
   }
   else {
      if (!glXMakeCurrent(system->gfxdisplay, glx->glxwindow, glx->context))
         return false;
   }

   _al_ogl_set_extensions(ogl->extension_api);

   return true;
}



static void xdpy_unset_current_display(ALLEGRO_DISPLAY *d)
{
   ALLEGRO_SYSTEM_XGLX *system = (ALLEGRO_SYSTEM_XGLX *)al_get_system_driver();
   (void)d;

   glXMakeContextCurrent(system->gfxdisplay, None, None, NULL);
}



/* Dummy implementation of flip. */
static void xdpy_flip_display(ALLEGRO_DISPLAY *d)
{
   ALLEGRO_SYSTEM_XGLX *system = (ALLEGRO_SYSTEM_XGLX *)al_get_system_driver();
   ALLEGRO_DISPLAY_XGLX *glx = (ALLEGRO_DISPLAY_XGLX *)d;
   if (d->extra_settings.settings[ALLEGRO_SINGLE_BUFFER])
      glFlush();
   else
      glXSwapBuffers(system->gfxdisplay, glx->glxwindow);
}

static void xdpy_update_display_region(ALLEGRO_DISPLAY *d, int x, int y,
   int w, int h)
{
   (void)x;
   (void)y;
   (void)w;
   (void)h;
   xdpy_flip_display(d);
}

static bool xdpy_acknowledge_resize(ALLEGRO_DISPLAY *d)
{
   ALLEGRO_SYSTEM_XGLX *system = (ALLEGRO_SYSTEM_XGLX *)al_get_system_driver();
   ALLEGRO_DISPLAY_XGLX *glx = (ALLEGRO_DISPLAY_XGLX *)d;
   XWindowAttributes xwa;
   unsigned int w, h;

   _al_mutex_lock(&system->lock);

   /* glXQueryDrawable is GLX 1.3+. */
   /*
   glXQueryDrawable(system->x11display, glx->glxwindow, GLX_WIDTH, &w);
   glXQueryDrawable(system->x11display, glx->glxwindow, GLX_HEIGHT, &h);
   */

   XGetWindowAttributes(system->x11display, glx->window, &xwa);
   w = xwa.width;
   h = xwa.height;

   d->w = w;
   d->h = h;

   ALLEGRO_DEBUG("xdpy: acknowledge_resize (%d, %d)\n", d->w, d->h);

   /* No context yet means this is a stray call happening during
    * initialization.
    */
   if (glx->context)
      setup_gl(d);

   _al_mutex_unlock(&system->lock);

   return true;
}



/* Note: The system mutex must be locked (exactly once) so when we
 * wait for the condition variable it gets auto-unlocked. For a
 * nested lock that would not be the case.
 */
void _al_display_xglx_await_resize(ALLEGRO_DISPLAY *d)
{
   ALLEGRO_SYSTEM_XGLX *system = (void *)al_get_system_driver();
   ALLEGRO_DISPLAY_XGLX *glx = (ALLEGRO_DISPLAY_XGLX *)d;
   int old_resize_count;

   XSync(system->x11display, False);

   /* Wait until we are actually resized. */
   old_resize_count = glx->resize_count;
   while (old_resize_count == glx->resize_count) {
      _al_cond_wait(&system->resized, &system->lock);
   }

   /* XXX: This hack makes the second and subsequent toggles of
    * ALLEGRO_FULLSCREEN_WINDOW work in ex_fs_window on my machine,
    * with IceWM and Fluxbox.  xfwm and kwin were okay without.
    */
   al_rest(0.05);

   /* TODO: Right now, we still generate a resize event (from the events
    * thread, in response to the Configure event) which is X11
    * specific though - if there's a simple way to prevent it we
    * should do so.
    */
   xdpy_acknowledge_resize(d);
}



static bool xdpy_resize_display(ALLEGRO_DISPLAY *d, int w, int h)
{
   ALLEGRO_SYSTEM_XGLX *system = (ALLEGRO_SYSTEM_XGLX *)al_get_system_driver();
   ALLEGRO_DISPLAY_XGLX *glx = (ALLEGRO_DISPLAY_XGLX *)d;

   /* A fullscreen-window can't be resized. */
   if (d->flags & ALLEGRO_FULLSCREEN_WINDOW)
      return false;

   _al_mutex_lock(&system->lock);

   reset_size_hints(d);
   XResizeWindow(system->x11display, glx->window, w, h);

   _al_display_xglx_await_resize(d);
   
   // XXX is this even a valid action?
   if (d->flags & ALLEGRO_FULLSCREEN) {
      _al_xglx_fullscreen_set_mode(system, glx, w, h, 0, 0);
      _al_xglx_fullscreen_to_display(system, glx);
   }
   
   set_size_hints(d);

   _al_mutex_unlock(&system->lock);
   return true;
}



/* Handle an X11 configure event. [X11 thread]
 * Only called from the event handler with the system locked.
 */
void _al_display_xglx_configure(ALLEGRO_DISPLAY *d, XEvent *xevent)
{
   ALLEGRO_DISPLAY_XGLX *glx = (ALLEGRO_DISPLAY_XGLX *)d;

   ALLEGRO_EVENT_SOURCE *es = &glx->display.es;
   _al_event_source_lock(es);

   /* Generate a resize event if the size has changed. We cannot asynchronously
    * change the display size here yet, since the user will only know about a
    * changed size after receiving the resize event. Here we merely add the
    * event to the queue.
    */
   if (d->w != xevent->xconfigure.width ||
      d->h != xevent->xconfigure.height) {
      if (_al_event_source_needs_to_generate_event(es)) {
         ALLEGRO_EVENT event;
         event.display.type = ALLEGRO_EVENT_DISPLAY_RESIZE;
         event.display.timestamp = al_current_time();
         event.display.x = xevent->xconfigure.x;
         event.display.y = xevent->xconfigure.y;
         event.display.width = xevent->xconfigure.width;
         event.display.height = xevent->xconfigure.height;
         _al_event_source_emit_event(es, &event);
      }
   }

   /* This ignores the event when changing the border (which has bogus
    * coordinates).
    */
   if (xevent->xconfigure.send_event) {
      glx->x = xevent->xconfigure.x;
      glx->y = xevent->xconfigure.y;
   }

   _al_event_source_unlock(es);
}



/* Handle an X11 close button event. [X11 thread]
 * Only called from the event handler with the system locked.
 */
void _al_display_xglx_closebutton(ALLEGRO_DISPLAY *d, XEvent *xevent)
{
   ALLEGRO_EVENT_SOURCE *es = &d->es;
   (void)xevent;

   _al_event_source_lock(es);

   if (_al_event_source_needs_to_generate_event(es)) {
      ALLEGRO_EVENT event;
      event.display.type = ALLEGRO_EVENT_DISPLAY_CLOSE;
      event.display.timestamp = al_current_time();
      _al_event_source_emit_event(es, &event);
   }
   _al_event_source_unlock(es);
}



/* Handle X11 switch event. [X11 thread]
 */
void _al_xwin_display_switch_handler(ALLEGRO_DISPLAY *display,
   XFocusChangeEvent *xevent)
{
   /* Anything but NotifyNormal seem to indicate the switch is not "real".
    * TODO: Find out details?
    */
   if (xevent->mode != NotifyNormal)
      return;

   ALLEGRO_EVENT_SOURCE *es = &display->es;
   _al_event_source_lock(es);
   if (_al_event_source_needs_to_generate_event(es)) {
      ALLEGRO_EVENT event;
      if (xevent->type == FocusOut)
         event.display.type = ALLEGRO_EVENT_DISPLAY_SWITCH_OUT;
      else
         event.display.type = ALLEGRO_EVENT_DISPLAY_SWITCH_IN;
      event.display.timestamp = al_current_time();
      _al_event_source_emit_event(es, &event);
   }
   _al_event_source_unlock(es);
}



void _al_xwin_display_expose(ALLEGRO_DISPLAY *display,
   XExposeEvent *xevent)
{
   ALLEGRO_EVENT_SOURCE *es = &display->es;
   _al_event_source_lock(es);
   if (_al_event_source_needs_to_generate_event(es)) {
      ALLEGRO_EVENT event;
      event.display.type = ALLEGRO_EVENT_DISPLAY_EXPOSE;
      event.display.timestamp = al_current_time();
      event.display.x = xevent->x;
      event.display.y = xevent->y;
      event.display.width = xevent->width;
      event.display.height = xevent->height;
      _al_event_source_emit_event(es, &event);
   }
   _al_event_source_unlock(es);
}



static bool xdpy_is_compatible_bitmap(ALLEGRO_DISPLAY *display,
   ALLEGRO_BITMAP *bitmap)
{
   /* All GLX bitmaps are compatible. */
   (void)display;
   (void)bitmap;
   return true;
}



static void xdpy_get_window_position(ALLEGRO_DISPLAY *display, int *x, int *y)
{
   ALLEGRO_DISPLAY_XGLX *glx = (ALLEGRO_DISPLAY_XGLX *)display;
   /* We could also query the X11 server, but it just would take longer, and
    * would not be synchronized to our events. The latter can be an advantage
    * or disadvantage.
    */
   *x = glx->x;
   *y = glx->y;
}



static void xdpy_set_window_title(ALLEGRO_DISPLAY *display, const char *title)
{
   ALLEGRO_SYSTEM_XGLX *system = (ALLEGRO_SYSTEM_XGLX *)al_get_system_driver();
   ALLEGRO_DISPLAY_XGLX *glx = (ALLEGRO_DISPLAY_XGLX *)display;

   _al_mutex_lock(&system->lock);
   Atom _NET_WM_NAME = XInternAtom(system->x11display, "_NET_WM_NAME", False);
   char *list[] = {(void *)title};
   XTextProperty property;
   Xutf8TextListToTextProperty(system->x11display, list, 1, XUTF8StringStyle,
      &property);
   XSetTextProperty(system->x11display, glx->window, &property, _NET_WM_NAME);
   XFree(property.value);
   _al_mutex_unlock(&system->lock);
}



static bool xdpy_wait_for_vsync(ALLEGRO_DISPLAY *display)
{
   (void) display;

   if (al_get_opengl_extension_list()->ALLEGRO_GLX_SGI_video_sync) {
      unsigned int count;
      glXGetVideoSyncSGI(&count);
      glXWaitVideoSyncSGI(2, (count+1) & 1, &count);
      return true;
   }

   return false;
}



/* Obtain a reference to this driver. */
ALLEGRO_DISPLAY_INTERFACE *_al_display_xglx_driver(void)
{
   if (xdpy_vt.create_display)
      return &xdpy_vt;

   xdpy_vt.create_display = xdpy_create_display;
   xdpy_vt.destroy_display = xdpy_destroy_display;
   xdpy_vt.set_current_display = xdpy_set_current_display;
   xdpy_vt.unset_current_display = xdpy_unset_current_display;
   xdpy_vt.flip_display = xdpy_flip_display;
   xdpy_vt.update_display_region = xdpy_update_display_region;
   xdpy_vt.acknowledge_resize = xdpy_acknowledge_resize;
   xdpy_vt.create_bitmap = _al_ogl_create_bitmap;
   xdpy_vt.create_sub_bitmap = _al_ogl_create_sub_bitmap;
   xdpy_vt.get_backbuffer = _al_ogl_get_backbuffer;
   xdpy_vt.get_frontbuffer = _al_ogl_get_backbuffer;
   xdpy_vt.set_target_bitmap = _al_ogl_set_target_bitmap;
   xdpy_vt.is_compatible_bitmap = xdpy_is_compatible_bitmap;
   xdpy_vt.resize_display = xdpy_resize_display;
   xdpy_vt.set_icon = xdpy_set_icon;
   xdpy_vt.set_window_title = xdpy_set_window_title;
   xdpy_vt.set_window_position = xdpy_set_window_position;
   xdpy_vt.get_window_position = xdpy_get_window_position;
   xdpy_vt.toggle_display_flag = xdpy_toggle_display_flag;
   xdpy_vt.wait_for_vsync = xdpy_wait_for_vsync;

   _al_xglx_add_cursor_functions(&xdpy_vt);
   _al_ogl_add_drawing_functions(&xdpy_vt);

   return &xdpy_vt;
}

/* vi: set sts=3 sw=3 et: */
