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
 *      New mouse API.
 *
 *      By Peter Wang.
 *
 *      See readme.txt for copyright information.
 */

/* Title: Mouse routines
 */


#define ALLEGRO_NO_COMPATIBILITY

#include "allegro5/allegro5.h"
#include "allegro5/internal/aintern.h"
#include "allegro5/internal/aintern_mouse.h"
#include "allegro5/internal/aintern_bitmap.h"
#include "allegro5/internal/aintern_system.h"



/* the active keyboard driver */
static ALLEGRO_MOUSE_DRIVER *new_mouse_driver = NULL;



/* Function: al_is_mouse_installed
 */
bool al_is_mouse_installed(void)
{
   return (new_mouse_driver ? true : false);
}



/* Function: al_install_mouse
 */
bool al_install_mouse(void)
{
   if (new_mouse_driver)
      return true;
   
   //FIXME: seems A4/A5 driver list stuff doesn't quite agree right now
   if (al_get_system_driver()->vt->get_mouse_driver) {
       new_mouse_driver = al_get_system_driver()->vt->get_mouse_driver();
       if (!new_mouse_driver->init_mouse()) {
          new_mouse_driver = NULL;
          return false;
       }
       _al_add_exit_func(al_uninstall_mouse, "al_uninstall_mouse");
       return true;
   }


   return false;
#if 0

   if (system_driver && system_driver->mouse_drivers)
      driver_list = system_driver->mouse_drivers();
   else
      driver_list = _al_mouse_driver_list;

   ASSERT(driver_list);

   for (i=0; driver_list[i].driver; i++) {
      new_mouse_driver = driver_list[i].driver;
      //name = get_config_text(new_mouse_driver->msedrv_ascii_name);
	  name = new_mouse_driver->msedrv_ascii_name;
      new_mouse_driver->msedrv_name = name;
      new_mouse_driver->msedrv_desc = name;
      if (new_mouse_driver->init_mouse()) {
	 break;
      }
   }

   if (!driver_list[i].driver) {
      new_mouse_driver = NULL;
      return false;
   }
   _al_add_exit_func(al_uninstall_mouse, "al_uninstall_mouse");


   return true;
#endif

}




/* Function: al_uninstall_mouse
 */
void al_uninstall_mouse(void)
{
   if (!new_mouse_driver)
      return;

   new_mouse_driver->exit_mouse();
   new_mouse_driver = NULL;
}



/* This was in the public API but its only purpose is now served by
 * al_get_mouse_event_source().
 */
static ALLEGRO_MOUSE *al_get_mouse(void)
{
   ALLEGRO_MOUSE *mse;

   ASSERT(new_mouse_driver);

   mse = new_mouse_driver->get_mouse();
   ASSERT(mse);

   return mse;
}



/* Function: al_get_mouse_num_buttons
 */
unsigned int al_get_mouse_num_buttons(void)
{
   ASSERT(new_mouse_driver);

   return new_mouse_driver->get_mouse_num_buttons();
}



/* Function: al_get_mouse_num_axes
 */
unsigned int al_get_mouse_num_axes(void)
{
   ASSERT(new_mouse_driver);

   return new_mouse_driver->get_mouse_num_axes();
}



/* Function: al_set_mouse_xy
 */
bool al_set_mouse_xy(ALLEGRO_DISPLAY *display, int x, int y)
{
   ASSERT(new_mouse_driver);
   ASSERT(new_mouse_driver->set_mouse_xy);

   return new_mouse_driver->set_mouse_xy(display, x, y);
}



/* Function: al_set_mouse_z
 */
bool al_set_mouse_z(int z)
{
   ASSERT(new_mouse_driver);
   ASSERT(new_mouse_driver->set_mouse_axis);

   return new_mouse_driver->set_mouse_axis(2, z);
}



/* Function: al_set_mouse_w
 */
bool al_set_mouse_w(int w)
{
   ASSERT(new_mouse_driver);
   ASSERT(new_mouse_driver->set_mouse_axis);

   return new_mouse_driver->set_mouse_axis(3, w);
}



/* Function: al_set_mouse_axis
 */
bool al_set_mouse_axis(int which, int value)
{
   ASSERT(new_mouse_driver);
   ASSERT(new_mouse_driver->set_mouse_axis);
   ASSERT(which >= 2);
   ASSERT(which < 4 + ALLEGRO_MOUSE_MAX_EXTRA_AXES);

   if (which >= 2 && which < 4 + ALLEGRO_MOUSE_MAX_EXTRA_AXES)
      return new_mouse_driver->set_mouse_axis(which, value);
   else
      return false;
}



/* Function: al_get_mouse_state
 */
void al_get_mouse_state(ALLEGRO_MOUSE_STATE *ret_state)
{
   ASSERT(new_mouse_driver);
   ASSERT(ret_state);

   new_mouse_driver->get_mouse_state(ret_state);
}



/* Function: al_get_mouse_state_axis
 */
int al_get_mouse_state_axis(ALLEGRO_MOUSE_STATE *ret_state, int axis)
{
   ASSERT(ret_state);
   ASSERT(axis >= 0);
   ASSERT(axis < (4 + ALLEGRO_MOUSE_MAX_EXTRA_AXES));

   switch (axis) {
      case 0:
         return ret_state->x;
      case 1:
         return ret_state->y;
      case 2:
         return ret_state->z;
      case 3:
         return ret_state->w;
      default:
         return ret_state->more_axes[axis - 4];
   }
}



/* Function: al_mouse_button_down
 */
bool al_mouse_button_down(ALLEGRO_MOUSE_STATE *state, int button)
{
   ASSERT(state);
   ASSERT(button > 0);

   return (state->buttons & (1 << (button-1)));
}



/*****************************************************************************
 *      Cursor API
 *****************************************************************************/


/* Function: al_create_mouse_cursor
 */
ALLEGRO_MOUSE_CURSOR *al_create_mouse_cursor(ALLEGRO_BITMAP *bmp,
   int x_focus, int y_focus)
{
   ALLEGRO_SYSTEM *sysdrv = al_get_system_driver();
   ASSERT(bmp);

   ASSERT(sysdrv->vt->create_mouse_cursor);
   return sysdrv->vt->create_mouse_cursor(bmp, x_focus, y_focus);
}


/* Function: al_destroy_mouse_cursor
 */
void al_destroy_mouse_cursor(ALLEGRO_MOUSE_CURSOR *cursor)
{
   ALLEGRO_SYSTEM *sysdrv;

   if (!cursor) {
      return;
   }

   sysdrv = al_get_system_driver();

   ASSERT(sysdrv->vt->destroy_mouse_cursor);
   sysdrv->vt->destroy_mouse_cursor(cursor);
}



/* Function: al_set_mouse_cursor
 */
bool al_set_mouse_cursor(ALLEGRO_DISPLAY *display, ALLEGRO_MOUSE_CURSOR *cursor)
{
   if (!cursor) {
      return false;
   }

   if (display) {
      ASSERT(display->vt->set_mouse_cursor);
      return display->vt->set_mouse_cursor(display, cursor);
   }

   return false;
}



/* Function: al_set_system_mouse_cursor
 */
bool al_set_system_mouse_cursor(ALLEGRO_DISPLAY *display,
   ALLEGRO_SYSTEM_MOUSE_CURSOR cursor_id)
{
   /* XXX should you be able to set ALLEGRO_SYSTEM_MOUSE_CURSOR_NONE? */
   ASSERT(cursor_id > ALLEGRO_SYSTEM_MOUSE_CURSOR_NONE);
   ASSERT(cursor_id < ALLEGRO_NUM_SYSTEM_MOUSE_CURSORS);
   ASSERT(display);

   if (cursor_id <= ALLEGRO_SYSTEM_MOUSE_CURSOR_NONE) {
      return false;
   }
   if (cursor_id > ALLEGRO_NUM_SYSTEM_MOUSE_CURSORS) {
      return false;
   }
   if (!display) {
      return false;
   }

   ASSERT(display->vt->set_system_mouse_cursor);
   return display->vt->set_system_mouse_cursor(display, cursor_id);
}



/* Function: al_show_mouse_cursor
 */
bool al_show_mouse_cursor(ALLEGRO_DISPLAY *display)
{
   if (display) {
      ASSERT(display->vt->show_mouse_cursor);
      return display->vt->show_mouse_cursor(display);
   }

   return false;
}



/* Function: al_hide_mouse_cursor
 */
bool al_hide_mouse_cursor(ALLEGRO_DISPLAY *display)
{
   if (display) {
      ASSERT(display->vt->hide_mouse_cursor);
      return display->vt->hide_mouse_cursor(display);
   }

   return false;
}



/* Function: al_get_mouse_cursor_position
 */
bool al_get_mouse_cursor_position(int *ret_x, int *ret_y)
{
   ALLEGRO_SYSTEM *alsys = al_get_system_driver();
   ASSERT(ret_x);
   ASSERT(ret_y);

   if (alsys->vt->get_cursor_position) {
      return alsys->vt->get_cursor_position(ret_x, ret_y);
   }
   else {
      *ret_x = 0;
      *ret_y = 0;
      return false;
   }
}



/* Function: al_get_mouse_event_source
 */
ALLEGRO_EVENT_SOURCE *al_get_mouse_event_source(void)
{
   ALLEGRO_MOUSE *mouse = al_get_mouse();

   return (mouse) ? &mouse->es : NULL;
}


/* vim: set sts=3 sw=3 et: */
