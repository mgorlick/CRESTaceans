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
 *      Windows mouse driver.
 *
 *      By Milan Mimica.
 *
 *      See readme.txt for copyright information.
 */

#if 0
/* Raw input */
#define _WIN32_WINNT 0x0501
#ifndef WINVER
#define WINVER 0x0600
#endif
#endif
#include <windows.h>

/*
 * Even the most recent MinGW at the moment of writing this is missing
 * this symbol.
 */
#ifndef SM_MOUSEHORIZONTALWHEELPRESENT
#define SM_MOUSEHORIZONTALWHEELPRESENT    91
#endif

#include "allegro5/allegro5.h"
#include "allegro5/internal/aintern.h"
#include "allegro5/internal/aintern_mouse.h"
#include "allegro5/platform/aintwin.h"
#include "allegro5/internal/aintern_display.h"

static ALLEGRO_MOUSE_STATE mouse_state;
static ALLEGRO_MOUSE the_mouse;
static bool installed = false;


static bool init_mouse(void)
{
   if (installed)
      return false;

   memset(&mouse_state, 0, sizeof(mouse_state));
   _al_event_source_init(&the_mouse.es);

#if 0
   if (al_get_new_display_flags() & ALLEGRO_FULLSCREEN) {
      RAWINPUTDEVICE rid[1];
      rid[0].usUsagePage = 0x01; 
      rid[0].usUsage = 0x02; 
      rid[0].dwFlags = RIDEV_NOLEGACY;
      rid[0].hwndTarget = 0;
      if (RegisterRawInputDevices(rid, 1, sizeof(rid[0])) == FALSE) {
         return false;
      }
   }
#endif

   installed = true;

   return true;
}


static void exit_mouse(void)
{
   if (!installed)
      return;

   memset(&mouse_state, 0, sizeof(mouse_state));
   _al_event_source_free(&the_mouse.es);
   installed = false;
}


static void generate_mouse_event(unsigned int type,
                                 int x, int y, int z,
                                 int dx, int dy, int dz,
                                 unsigned int button,
                                 ALLEGRO_DISPLAY *source)
{
   ALLEGRO_EVENT event;

   if (!_al_event_source_needs_to_generate_event(&the_mouse.es))
      return;

   _al_event_source_lock(&the_mouse.es);
   event.mouse.type = type;
   event.mouse.timestamp = al_current_time();
   event.mouse.display = source;
   event.mouse.x = x;
   event.mouse.y = y;
   event.mouse.z = z;
   event.mouse.w = 0;
   event.mouse.dx = dx;
   event.mouse.dy = dy;
   event.mouse.dz = dz;
   event.mouse.dw = 0;
   event.mouse.button = button;
   event.mouse.pressure = 0.0; /* TODO */
   _al_event_source_emit_event(&the_mouse.es, &event);
   _al_event_source_unlock(&the_mouse.es);
}


static ALLEGRO_MOUSE* get_mouse(void)
{
   return &the_mouse;
}


static unsigned int get_num_buttons(void)
{
   return GetSystemMetrics(SM_CMOUSEBUTTONS);
}


static unsigned int get_num_axes(void)
{
   bool x = GetSystemMetrics(SM_MOUSEHORIZONTALWHEELPRESENT);
   bool z = GetSystemMetrics(SM_MOUSEWHEELPRESENT);
   if (x && z)
      return 4;
   if (x || z)
      return 3;
   return 2;
}


static bool set_mouse_xy(ALLEGRO_DISPLAY *disp, int x, int y)
{
   int dx, dy;
   int wx, wy;
   ALLEGRO_DISPLAY_WIN *win_disp = (void*)disp;

   if (!installed)
      return false;

   dx = x - mouse_state.x;
   dy = y - mouse_state.y;

   if (dx || dy) {
      mouse_state.x = x;
      mouse_state.y = y;

      generate_mouse_event(
         ALLEGRO_EVENT_MOUSE_WARPED,
         mouse_state.x, mouse_state.y, mouse_state.z,
         dx, dy, 0,
         0, (void*)win_disp);
   }

   _al_win_get_window_position(win_disp->window, &wx, &wy);

   if (!(win_disp->display.flags & ALLEGRO_FULLSCREEN)) {
      SetCursorPos(x+wx, y+wy);
   }

   return true;
}


static bool set_mouse_axis(int which, int z)
{
   if (which != 2) {
      return false;
   }

   {
      int dz = (z - mouse_state.z);

      if (dz != 0) {
         mouse_state.z = z;

         generate_mouse_event(
            ALLEGRO_EVENT_MOUSE_AXES,
            mouse_state.x, mouse_state.y, mouse_state.z,
            0, 0, dz,
            0, mouse_state.display);
      }
   }

   return true;
}


static void get_mouse_state(ALLEGRO_MOUSE_STATE *ret_state)
{
   _al_event_source_lock(&the_mouse.es);
   *ret_state = mouse_state;
   _al_event_source_unlock(&the_mouse.es);
}


/* the driver vtable */
#define MOUSE_WINAPI AL_ID('W','A','P','I')

static ALLEGRO_MOUSE_DRIVER mousedrv_winapi =
{
   MOUSE_WINAPI,
   "",
   "",
   "WinAPI mouse",
   init_mouse,
   exit_mouse,
   get_mouse,
   get_num_buttons,
   get_num_axes,
   set_mouse_xy,
   set_mouse_axis,
   get_mouse_state
};


_DRIVER_INFO _al_mouse_driver_list[] =
{
   {MOUSE_WINAPI, &mousedrv_winapi, true},
   {0, NULL, 0}
};


void _al_win_mouse_handle_leave(ALLEGRO_DISPLAY_WIN *win_disp)
{
   if (!installed)
      return;
   if (mouse_state.display == (void*)win_disp)
      mouse_state.display = NULL;
   generate_mouse_event(ALLEGRO_EVENT_MOUSE_LEAVE_DISPLAY,
      mouse_state.x, mouse_state.y, mouse_state.z,
      0, 0, 0,
      0, (void*)win_disp);
}


void _al_win_mouse_handle_enter(ALLEGRO_DISPLAY_WIN *win_disp)
{
   if (!installed)
      return;
   mouse_state.display = (void*)win_disp;
   generate_mouse_event(ALLEGRO_EVENT_MOUSE_ENTER_DISPLAY,
      mouse_state.x, mouse_state.y, mouse_state.z,
      0, 0, 0,
      0, (void*)win_disp);
}


void _al_win_mouse_handle_move(int x, int y, bool abs, ALLEGRO_DISPLAY_WIN *win_disp)
{
   int dx, dy;
   int oldx, oldy;

   oldx = mouse_state.x;
   oldy = mouse_state.y;

   if (!installed)
      return;

   if (!abs) {
      mouse_state.x += x;
      mouse_state.y += y;
      dx = x;
      dy = y;
   }
   else {
      dx = x - mouse_state.x;
      dy = y - mouse_state.y;
      mouse_state.x = x;
      mouse_state.y = y;
   }

   if (oldx != mouse_state.x || oldy != mouse_state.y) {
      generate_mouse_event(ALLEGRO_EVENT_MOUSE_AXES,
         mouse_state.x, mouse_state.y, mouse_state.z,
         dx, dy, 0,
         0, (void*)win_disp);
   }
}


void _al_win_mouse_handle_wheel(int z, bool abs, ALLEGRO_DISPLAY_WIN *win_disp)
{
   int d;

   if (!installed)
      return;

   if (!abs) {
      mouse_state.z += z;
      d = z;
   }
   else {
      d = z - mouse_state.z;
      mouse_state.z = z;
   }

   generate_mouse_event(ALLEGRO_EVENT_MOUSE_AXES,
      mouse_state.x, mouse_state.y, mouse_state.z,
      0, 0, d,
      0, (void*)win_disp);
}


void _al_win_mouse_handle_button(int button, bool down, int x, int y, bool abs,
                                 ALLEGRO_DISPLAY_WIN *win_disp)
{
   int type = down ? ALLEGRO_EVENT_MOUSE_BUTTON_DOWN
                   : ALLEGRO_EVENT_MOUSE_BUTTON_UP;

   if (!installed)
      return;

   if (!abs) {
      mouse_state.x += x;
      mouse_state.y += y;
   }
   else {
      mouse_state.x = x;
      mouse_state.y = y;
   }

   if (down)
      mouse_state.buttons |= (1 << (button-1));
   else
      mouse_state.buttons &= ~(1 << (button-1));

   generate_mouse_event(type,
   mouse_state.x, mouse_state.y, mouse_state.z,
   0, 0, 0,
   button, (void*)win_disp);
}
