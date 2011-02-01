/*
 *    Example program for the Allegro library, by Peter Wang.
 *
 *    This program tests joystick events.
 */

#include <allegro5/allegro5.h>
#include <allegro5/allegro_primitives.h>

#include "common.c"

#define MAX_STICKS   8
#define MAX_BUTTONS  32


/* globals */
ALLEGRO_EVENT_QUEUE  *event_queue;
ALLEGRO_COLOR        black;
ALLEGRO_COLOR        grey;
ALLEGRO_COLOR        white;

int num_sticks;
float joys_x[MAX_STICKS];
float joys_y[MAX_STICKS];
int num_buttons;
bool joys_buttons[MAX_BUTTONS];



static void draw_joystick_axes(int cx, int cy, int stick)
{
   const int size = 30;
   const int osize = 35;
   int x = cx + joys_x[stick] * size;
   int y = cy + joys_y[stick] * size;

   al_draw_filled_rectangle(cx-osize, cy-osize, cx+osize, cy+osize, grey);
   al_draw_rectangle(cx-osize, cy-osize, cx+osize, cy+osize, black, 0);
   al_draw_filled_rectangle(x-5, y-5, x+5.5, y+5.5, black);
}



static void draw_joystick_button(int button, bool down)
{
   ALLEGRO_BITMAP *bmp = al_get_target_bitmap();
   int x = al_get_bitmap_width(bmp)/2 + (button % 5) * 30;
   int y = al_get_bitmap_height(bmp)-60 + (button / 5) * 30;

   al_draw_filled_rectangle(x, y, x + 25.5, y + 25.5, grey);
   al_draw_rectangle(x, y, x + 25.5, y + 25.5, black, 0);
   if (down) {
      al_draw_filled_rectangle(x + 2, y + 2, x + 24.5, y + 24.5, black);
   }
}



static void draw_all(void)
{
   ALLEGRO_BITMAP *bmp = al_get_target_bitmap();
   int width = al_get_bitmap_width(bmp);
   int height = al_get_bitmap_height(bmp);
   int i;

   al_clear_to_color(al_map_rgb(0xff, 0xff, 0xc0));

   for (i = 0; i < num_sticks; i++) {
      int cx = (i + 0.5) * width / num_sticks;
      int cy = height / 2;
      draw_joystick_axes(cx, cy, i);
   }

   for (i = 0; i < num_buttons; i++) {
      draw_joystick_button(i, joys_buttons[i]);
   }

   al_flip_display();
}



static void main_loop(void)
{
   ALLEGRO_EVENT event;

   while (true) {
      if (al_event_queue_is_empty(event_queue))
         draw_all();

      al_wait_for_event(event_queue, &event);

      switch (event.type) {

         /* ALLEGRO_EVENT_JOYSTICK_AXIS - a joystick axis value changed.
          * For simplicity, in this example we only work with the first
          * 'stick' on the first joystick on the system.
          */
         case ALLEGRO_EVENT_JOYSTICK_AXIS:
            if (event.joystick.stick < MAX_STICKS) {
               int stick = event.joystick.stick;
               if (event.joystick.axis == 0)
                  joys_x[stick] = event.joystick.pos;
               else if (event.joystick.axis == 1)
                  joys_y[stick] = event.joystick.pos;
            }
            break;

         /* ALLEGRO_EVENT_JOYSTICK_BUTTON_DOWN - a joystick button was pressed.
          */
         case ALLEGRO_EVENT_JOYSTICK_BUTTON_DOWN:
            joys_buttons[event.joystick.button] = true;
            break;

         /* ALLEGRO_EVENT_JOYSTICK_BUTTON_UP - a joystick button was released.
          */
         case ALLEGRO_EVENT_JOYSTICK_BUTTON_UP:
            joys_buttons[event.joystick.button] = false;
            break;

         case ALLEGRO_EVENT_KEY_DOWN:
            if (event.keyboard.keycode == ALLEGRO_KEY_ESCAPE)
               return;
            break;

         /* ALLEGRO_EVENT_DISPLAY_CLOSE - the window close button was pressed.
          */
         case ALLEGRO_EVENT_DISPLAY_CLOSE:
            return;

         /* We received an event of some type we don't know about.
          * Just ignore it.
          */
         default:
            break;
      }
   }
}



int main(void)
{
   ALLEGRO_DISPLAY *display;
   ALLEGRO_JOYSTICK *zero_joy;
   ALLEGRO_JOYSTICK_STATE jst;
   int i;

   if (!al_init()) {
      abort_example("Could not init Allegro.\n");
      return 1;
   }
   al_init_primitives_addon();

   display = al_create_display(640, 480);
   if (!display) {
      abort_example("al_create_display failed\n");
      return 1;
   }

#ifndef ALLEGRO_GP2XWIZ
   if (!al_install_keyboard()) {
      abort_example("al_install_keyboard failed\n");
      return 1;
   }
#endif

   black = al_map_rgb(0, 0, 0);
   grey = al_map_rgb(0xe0, 0xe0, 0xe0);
   white = al_map_rgb(255, 255, 255);

   al_install_joystick();
   if (al_get_num_joysticks() == 0) {
      abort_example("No joysticks found.\n");
      return 1;
   }
   zero_joy = al_get_joystick(0);

   event_queue = al_create_event_queue();
   if (!event_queue) {
      abort_example("al_create_event_queue failed\n");
      return 1;
   }

#ifndef ALLEGRO_GP2XWIZ
   al_register_event_source(event_queue, al_get_keyboard_event_source());
#endif
   al_register_event_source(event_queue, al_get_display_event_source(display));
   al_register_event_source(event_queue, al_get_joystick_event_source(zero_joy));

   /* Initialise values. */
   al_get_joystick_state(zero_joy, &jst);

   num_sticks = al_get_joystick_num_sticks(zero_joy);
   if (num_sticks > MAX_STICKS)
      num_sticks = MAX_STICKS;
   for (i = 0; i < num_sticks; i++) {
      joys_x[i] = jst.stick[i].axis[0];
      joys_y[i] = jst.stick[i].axis[1];
   }

   num_buttons = al_get_joystick_num_buttons(zero_joy);
   if (num_buttons > MAX_BUTTONS) {
      num_buttons = MAX_BUTTONS;
   }
   for (i = 0; i < num_buttons; i++) {
      joys_buttons[i] = (jst.button[i] >= 16384);
   }

   main_loop();

   return 0;
}

/* vim: set ts=8 sts=3 sw=3 et: */
