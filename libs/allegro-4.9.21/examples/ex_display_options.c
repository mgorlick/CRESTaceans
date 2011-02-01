/* Test retrieving and settings possible modes.
 * 
 * FIXME: I can't test fullscreen support - I only get a single mode
 *        listed which does not work. Most likely the format parameter
 *        should be used somehow.
 * 
 */
#include <allegro5/allegro5.h>
#include <allegro5/allegro_image.h>
#include <allegro5/allegro_font.h>
#include <allegro5/allegro_primitives.h>
#include <stdio.h>

#include "common.c"

ALLEGRO_FONT *font;
ALLEGRO_COLOR white;
int font_h;
int modes_count;
int options_count;
char status[256];
int flags, old_flags;

int selected_column;
int selected_mode;
int selected_option;

#define X(x, m) {#x, ALLEGRO_##x, 0, m, 0}
struct {
    char const *name;
    int option;
    int value, max_value;
    int required;
} options[] = {
    X(COLOR_SIZE, 32),
    X(RED_SIZE, 8),
    X(GREEN_SIZE, 8),
    X(BLUE_SIZE, 8),
    X(ALPHA_SIZE, 8),
    X(RED_SHIFT, 32),
    X(GREEN_SHIFT, 32),
    X(BLUE_SHIFT, 32),
    X(ALPHA_SHIFT, 32),
    X(DEPTH_SIZE, 32),
    X(FLOAT_COLOR, 1),
    X(FLOAT_DEPTH, 1),
    X(STENCIL_SIZE, 32),
    X(SAMPLE_BUFFERS, 1),
    X(SAMPLES, 8),
    X(RENDER_METHOD, 2),
    X(SINGLE_BUFFER, 1),
    X(SWAP_METHOD, 1),
    X(VSYNC, 1),
    X(COMPATIBLE_DISPLAY, 1),
    X(MAX_BITMAP_SIZE, 65536),
    X(SUPPORT_NPOT_BITMAP, 1),
    X(CAN_DRAW_INTO_BITMAP, 1),
    X(SUPPORT_SEPARATE_ALPHA, 1),
};
#undef X
static char const *flag_names[32];
static void init_flags(void)
{
   int i;
   #define X(f) if (1 << i == ALLEGRO_##f) flag_names[i] = #f;
   for (i = 0; i < 32; i++) {
      X(WINDOWED)
      X(FULLSCREEN)
      X(OPENGL)
      X(RESIZABLE)
      X(NOFRAME)
      X(GENERATE_EXPOSE_EVENTS)
      X(FULLSCREEN_WINDOW)
      X(MINIMIZED)
   }
   #undef X
}

static void display_options(ALLEGRO_DISPLAY *display)
{
   int i, y = 10;
   int x = 10;
   int n = options_count;
   int dw = al_get_display_width(display);
   int dh = al_get_display_height(display);

   ALLEGRO_COLOR c;
   c = al_map_rgb_f(0.8, 0.8, 1);
   al_draw_textf(font, c, x, y, 0, "Create new display");
   y += font_h;
   for (i = 0; i < modes_count + 1; i++) {
      ALLEGRO_DISPLAY_MODE mode;
      if (i < modes_count) {
         al_get_display_mode(i, &mode);
      }
      else {
         mode.width = 800;
         mode.height = 600;
         mode.format = 0;
         mode.refresh_rate = 0;
      }
      if (selected_column == 0 && selected_mode == i) {
         c = al_map_rgb_f(1, 1, 0);
         al_set_blender(ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
         al_draw_filled_rectangle(x, y, x + 300, y + font_h, c);
      }
      c = al_map_rgb_f(0, 0, 0);
      al_set_blender(ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_INVERSE_ALPHA);
      al_draw_textf(font, c, x, y, 0, "%s %d x %d (%d, %d)",
         i < modes_count ? "Fullscreen" : "Windowed",
         mode.width,
         mode.height, mode.format, mode.refresh_rate);
      y += font_h;
   }
   
   x = dw / 2 + 10;
   y = 10;
   c = al_map_rgb_f(0.8, 0.8, 1);
   al_draw_textf(font, c, x, y, 0, "Options for new display");
   al_draw_textf(font, c, dw - 10, y, ALLEGRO_ALIGN_RIGHT, "(current display)");
   y += font_h;
   for (i = 0; i < n; i++) {
      if (selected_column == 1 && selected_option == i) {
         c = al_map_rgb_f(1, 1, 0);
         al_draw_filled_rectangle(x, y, x + 300, y + font_h, c);
      }

      switch (options[i].required) {
         case ALLEGRO_REQUIRE: c = al_map_rgb_f(0.5, 0, 0); break;
         case ALLEGRO_SUGGEST: c = al_map_rgb_f(0, 0, 0); break;
         case ALLEGRO_DONTCARE: c = al_map_rgb_f(0.5, 0.5, 0.5); break;
      }
      al_draw_textf(font, c, x, y, 0, "%s: %d (%s)", options[i].name,
         options[i].value,
            options[i].required == ALLEGRO_REQUIRE ? "required" :
            options[i].required == ALLEGRO_SUGGEST ? "suggested" :
            "ignored");
            
      c = al_map_rgb_f(0.9, 0.5, 0.3);
      al_draw_textf(font, c, dw - 10, y, ALLEGRO_ALIGN_RIGHT, "%d",
         al_get_display_option(display, options[i].option));
      y += font_h;
   }

   c = al_map_rgb_f(0, 0, 0.8);
   x = 10;
   y = dh - font_h - 10;
   y -= font_h;
   al_draw_textf(font, c, x, y, 0, "PageUp/Down: modify values");
   y -= font_h;
   al_draw_textf(font, c, x, y, 0, "Return: set mode or require option");
   y -= font_h;
   al_draw_textf(font, c, x, y, 0, "Cursor keys: change selection");
   
   y -= font_h * 2;
   for (i = 0; i < 32; i++) {
      if (flag_names[i]) {
         if (flags & (1 << i)) c = al_map_rgb_f(0.5, 0, 0);
         else if (old_flags & (1 << i)) c = al_map_rgb_f(0.5, 0.4, 0.4);
         else continue;
         al_draw_text(font, c, x, y, 0, flag_names[i]);
         x += al_get_text_width(font, flag_names[i]) + 10;
      }
   }
   
   c = al_map_rgb_f(1, 0, 0);
   al_draw_text(font, c, dw / 2, dh - font_h, ALLEGRO_ALIGN_CENTRE, status);
}

int main(void)
{
   ALLEGRO_DISPLAY *display;
   ALLEGRO_EVENT_QUEUE *queue;
   ALLEGRO_TIMER *timer;
   int n;
   bool redraw = false;

   if (!al_init()) {
      abort_example("Could not init Allegro.\n");
      return 1;
   }
   init_flags();
   al_init_primitives_addon();
   
   white = al_map_rgba_f(1, 1, 1, 1);

   al_install_keyboard();
   al_install_mouse();
   al_init_image_addon();
   al_init_font_addon();

   display = al_create_display(800, 600);

   font = al_load_font("data/fixed_font.tga", 0, 0);
   if (!font) {
      abort_example("data/fixed_font.tga not found\n");
      return 1;
   }
   
   timer = al_create_timer(1.0 / 60);

   modes_count = al_get_num_display_modes();
   options_count = sizeof(options) / sizeof(options[0]);
   n = 0;
   font_h = al_get_font_line_height(font);
   al_clear_to_color(al_map_rgb_f(1, 1, 1));
   display_options(display);
   al_flip_display();

   queue = al_create_event_queue();
   al_register_event_source(queue, al_get_keyboard_event_source());
   al_register_event_source(queue, al_get_mouse_event_source());
   al_register_event_source(queue, al_get_display_event_source(display));
   al_register_event_source(queue, al_get_timer_event_source(timer));
   
   al_start_timer(timer);

   while (1) {
      ALLEGRO_EVENT event;
      al_wait_for_event(queue, &event);
      if (event.type == ALLEGRO_EVENT_DISPLAY_CLOSE) {
         break;
      }
      if (event.type == ALLEGRO_EVENT_MOUSE_BUTTON_DOWN) {
         if (event.mouse.button == 1) {
            int dw = al_get_display_width(display);
            int y = 10;
            int row = (event.mouse.y - y) / font_h - 1;
            int column = event.mouse.x / (dw / 2);
            if (column == 0) {
               if (row >= 0 && row <= modes_count) {
                  selected_column = column;
                  selected_mode = row;
                  redraw = true;
               }
            }
            if (column == 1) {
               if (row >= 0 && row < options_count) {
                  selected_column = column;
                  selected_option = row;
                  redraw = true;
               }
            }
         }
      }
      if (event.type == ALLEGRO_EVENT_TIMER) {
          int f = al_get_display_flags(display);
          if (f != flags) {
              redraw = true;
              flags = f;
              old_flags |= f;
          }
      }
      if (event.type == ALLEGRO_EVENT_KEY_DOWN ||
         event.type == ALLEGRO_EVENT_KEY_REPEAT) {
         int change;
         if (event.keyboard.keycode == ALLEGRO_KEY_ESCAPE)
            break;
         if (event.keyboard.keycode == ALLEGRO_KEY_LEFT) {
            selected_column = 0;
            redraw = true;
         }
         if (event.keyboard.keycode == ALLEGRO_KEY_RIGHT) {
            selected_column = 1;
            redraw = true;
         }
         if (event.keyboard.keycode == ALLEGRO_KEY_UP) {
            if (selected_column == 0) selected_mode -= 1;
            if (selected_column == 1) selected_option -= 1;
            redraw = true;
         }
         if (event.keyboard.keycode == ALLEGRO_KEY_DOWN) {
            if (selected_column == 0) selected_mode += 1;
            if (selected_column == 1) selected_option += 1;
            redraw = true;
         }
         if (event.keyboard.keycode == ALLEGRO_KEY_ENTER) {
             if (selected_column == 0) {
                ALLEGRO_DISPLAY_MODE mode;
                ALLEGRO_DISPLAY *new_display;
                if (selected_mode < modes_count)
                    al_get_display_mode(selected_mode, &mode);
                else {
                    mode.width = 800;
                    mode.height = 600;
                }
                printf("al_create_display\n");
                new_display = al_create_display(
                   mode.width, mode.height);
                if (new_display) {
                   al_destroy_display(display);
                   display = new_display;
                   al_register_event_source(queue, al_get_display_event_source(display));
                   sprintf(status, "Display creation succeeded.");
                }
                else {
                   sprintf(status, "Display creation failed.");
                }
             }
             if (selected_column == 1) {
                 options[selected_option].required += 1;
                 options[selected_option].required %= 3;
                 al_set_new_display_option(options[selected_option].option,
                    options[selected_option].value,
                    options[selected_option].required);
             }
             redraw = true;
         }
         change = 0;
         if (event.keyboard.keycode == ALLEGRO_KEY_PGUP) change = 1;
         if (event.keyboard.keycode == ALLEGRO_KEY_PGDN) change = -1;
         if (change && selected_column == 1) {
            options[selected_option].value += change;
            if (options[selected_option].value < 0)
               options[selected_option].value = 0;
            if (options[selected_option].value > options[selected_option].max_value)
               options[selected_option].value = options[selected_option].max_value;
            al_set_new_display_option(options[selected_option].option,
               options[selected_option].value,
               options[selected_option].required);
            redraw = true;
         }
      }
      
      if (selected_mode < 0) selected_mode = 0;
      if (selected_mode > modes_count) selected_mode = modes_count;
      if (selected_option < 0) selected_option = 0;
      if (selected_option >= options_count) selected_option = options_count - 1;

      if (redraw && al_event_queue_is_empty(queue)) {
         redraw = false;
         al_clear_to_color(al_map_rgb_f(1, 1, 1));
         display_options(display);
         al_flip_display();
      }
   }

   al_destroy_font(font);

   return 0;
}
