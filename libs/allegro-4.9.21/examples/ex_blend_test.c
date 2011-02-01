#include <allegro5/allegro5.h>
#include <allegro5/allegro_primitives.h>
#include <stdio.h>
#include <math.h>

#include "common.c"

int test_only_index = 0;
int test_index = 0;
bool test_display = false;
ALLEGRO_DISPLAY *display;

static void print_color(ALLEGRO_COLOR c)
{
   float r, g, b, a;
   al_unmap_rgba_f(c, &r, &g, &b, &a);
   printf("%.2f, %.2f, %.2f, %.2f", r, g, b, a);
}

static ALLEGRO_COLOR test(ALLEGRO_COLOR src_col, ALLEGRO_COLOR dst_col,
   int src_format, int dst_format,
   int src, int dst, int src_a, int dst_a,
   int operation, bool verbose)
{
   ALLEGRO_COLOR result;
   ALLEGRO_BITMAP *dst_bmp;

   al_set_new_bitmap_format(dst_format);
   al_set_new_bitmap_flags(ALLEGRO_MEMORY_BITMAP);
   al_set_blender(ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
   dst_bmp = al_create_bitmap(1, 1);
   al_set_target_bitmap(dst_bmp);
   al_clear_to_color(dst_col);
   if (operation == 0) {
      ALLEGRO_BITMAP *src_bmp;
      al_set_new_bitmap_format(src_format);
      src_bmp = al_create_bitmap(1, 1);
      al_set_target_bitmap(src_bmp);
      al_clear_to_color(src_col);
      al_set_target_bitmap(dst_bmp);
      al_set_separate_blender(ALLEGRO_ADD, src, dst, ALLEGRO_ADD, src_a, dst_a);
      al_draw_bitmap(src_bmp, 0, 0, 0);
      al_destroy_bitmap(src_bmp);
   }
   else  if (operation == 1) {
      al_set_separate_blender(ALLEGRO_ADD, src, dst, ALLEGRO_ADD, src_a, dst_a);
      al_draw_pixel(0, 0, src_col);
   }
   else  if (operation == 2) {
      al_set_separate_blender(ALLEGRO_ADD, src, dst, ALLEGRO_ADD, src_a, dst_a);
      al_draw_line(0, 0, 1, 1, src_col, 0);
   }

   result = al_get_pixel(dst_bmp, 0, 0);

   if (test_display) {
      al_set_target_backbuffer(display);
      al_set_blender(ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_ZERO);
      al_draw_bitmap(dst_bmp, 0, 0, 0);
   }

   al_destroy_bitmap(dst_bmp);
   
   if (!verbose)
      return result;
   
   printf("---\n");
   printf("test id: %d\n", test_index);

   printf("source     : ");
   print_color(src_col);
   printf(" %s format=%d mode=%d alpha=%d\n",
      operation == 0 ? "bitmap" : operation == 1 ? "pixel" : "prim",
      src_format, src, src_a);

   printf("destination: ");
   print_color(dst_col);
   printf(" format=%d mode=%d alpha=%d\n",
      dst_format, dst, dst_a);
   
   printf("result     : ");
   print_color(result);
   printf("\n");
   
   return result;
}

static bool same_color(ALLEGRO_COLOR c1, ALLEGRO_COLOR c2)
{
   float r1, g1, b1, a1;
   float r2, g2, b2, a2;
   float dr, dg, db, da;
   float d;
   al_unmap_rgba_f(c1, &r1, &g1, &b1, &a1);
   al_unmap_rgba_f(c2, &r2, &g2, &b2, &a2);
   dr = r1 - r2;
   dg = g1 - g2;
   db = b1 - b2;
   da = a1 - a2;
   d = sqrt(dr * dr + dg * dg + db * db + da * da);
   if (d < 0.01)
      return true;
   else
      return false;
}

static float get_factor(int operation, float alpha)
{
   switch(operation) {
       case ALLEGRO_ZERO: return 0;
       case ALLEGRO_ONE: return 1;
       case ALLEGRO_ALPHA: return alpha;
       case ALLEGRO_INVERSE_ALPHA: return 1 - alpha;
   }
   return 0;
}

static bool has_alpha(int format)
{
   if (format == ALLEGRO_PIXEL_FORMAT_RGB_888)
      return false;
   if (format == ALLEGRO_PIXEL_FORMAT_BGR_888)
      return false;
   return true;
}

#define CLAMP(x) (x > 1 ? 1 : x)

static ALLEGRO_COLOR reference_implementation(
   ALLEGRO_COLOR src_col, ALLEGRO_COLOR dst_col,
   int src_format, int dst_format,
   int src_mode, int dst_mode, int src_alpha, int dst_alpha,
   int operation)
{
   float sr, sg, sb, sa;
   float dr, dg, db, da;
   float r, g, b, a;
   float src, dst, asrc, adst;

   al_unmap_rgba_f(src_col, &sr, &sg, &sb, &sa);
   al_unmap_rgba_f(dst_col, &dr, &dg, &db, &da);

   /* Do we even have source alpha? */
   if (operation == 0) {
      if (!has_alpha(src_format)) {
         sa = 1;
      }
   }

   r = sr;
   g = sg;
   b = sb;
   a = sa;

   src = get_factor(src_mode, a);
   dst = get_factor(dst_mode, a);
   asrc = get_factor(src_alpha, a);
   adst = get_factor(dst_alpha, a);

   r = r * src + dr * dst;
   g = g * src + dg * dst;
   b = b * src + db * dst;
   a = a * asrc + da * adst;
   
   r = CLAMP(r);
   g = CLAMP(g);
   b = CLAMP(b);
   a = CLAMP(a);

   /* Do we even have destination alpha? */
   if (!has_alpha(dst_format)) {
      a = 1;
   }

   return al_map_rgba_f(r, g, b, a);
}

static void do_test2(ALLEGRO_COLOR src_col, ALLEGRO_COLOR dst_col,
   int src_format, int dst_format,
   int src_mode, int dst_mode, int src_alpha, int dst_alpha,
   int operation)
{
   ALLEGRO_COLOR reference, result, from_display;
   test_index++;

   if (test_only_index && test_index != test_only_index)
      return;

   reference = reference_implementation(
      src_col, dst_col, src_format, dst_format,
      src_mode, dst_mode, src_alpha, dst_alpha, operation);

   result = test(src_col, dst_col, src_format,
      dst_format, src_mode, dst_mode, src_alpha, dst_alpha,
      operation, false);

   if (!same_color(reference, result)) {
      test(src_col, dst_col, src_format,
      dst_format, src_mode, dst_mode, src_alpha, dst_alpha,
      operation, true);
      printf("expected   : ");
      print_color(reference);
      printf("\n");
      printf("FAILED\n");
   }
   else {
      printf(" OK");
      fflush(stdout);
   }

   if (test_display) {
      dst_format = al_get_display_format(display);
      from_display = al_get_pixel(al_get_backbuffer(display), 0, 0);
      reference = reference_implementation(
         src_col, dst_col, src_format, dst_format,
         src_mode, dst_mode, src_alpha, dst_alpha, operation);
      
      if (!same_color(reference, from_display)) {
         test(src_col, dst_col, src_format,
         dst_format, src_mode, dst_mode, src_alpha, dst_alpha,
         operation, true);
         printf("displayed  : ");
         print_color(from_display);
         printf("\n");
         printf("expected   : ");
         print_color(reference);
         printf("\n");
         printf("(FAILED on display)\n");
      }
   }
}

static void do_test1(ALLEGRO_COLOR src_col, ALLEGRO_COLOR dst_col,
   int src_format, int dst_format)
{
   int i, j, k, l, m;
   int smodes[4] = {ALLEGRO_ALPHA, ALLEGRO_ZERO, ALLEGRO_ONE,
      ALLEGRO_INVERSE_ALPHA};
   int dmodes[4] = {ALLEGRO_INVERSE_ALPHA, ALLEGRO_ZERO, ALLEGRO_ONE,
      ALLEGRO_ALPHA};
   for (i = 0; i < 4; i++) {
      for (j = 0; j < 4; j++) {
         for (k = 0; k < 4; k++) {
            for (l = 0; l < 4; l++) {
               for (m = 0; m < 3; m++) {
                  do_test2(src_col, dst_col,
                     src_format, dst_format,
                     smodes[i], dmodes[j], smodes[k], dmodes[l],
                     m);
               }
            }
         }
      }
   }
}

#define C al_map_rgba_f

int main(int argc, char **argv)
{
   int i, j, l, m;
   ALLEGRO_COLOR src_colors[2];
   ALLEGRO_COLOR dst_colors[2];
   int src_formats[2] = {
      ALLEGRO_PIXEL_FORMAT_ABGR_8888,
      ALLEGRO_PIXEL_FORMAT_BGR_888
      };
   int dst_formats[2] = {
      ALLEGRO_PIXEL_FORMAT_ABGR_8888,
      ALLEGRO_PIXEL_FORMAT_BGR_888
      };
   src_colors[0] = C(0, 0, 0, 1);
   src_colors[1] = C(1, 1, 1, 1);
   dst_colors[0] = C(1, 1, 1, 1);
   dst_colors[1] = C(0, 0, 0, 0);

   for (i = 1; i < argc; i++) {
      if (!strcmp(argv[i], "-d"))
         test_display = 1;
      else
         test_only_index = strtol(argv[i], NULL, 10);
   }

   al_init();
   al_init_primitives_addon();
   if (test_display)
      display = al_create_display(100, 100);

   for (i = 0; i < 2; i++) {
      for (j = 0; j < 2; j++) {
         for (l = 0; l < 2; l++) {
            for (m = 0; m < 2; m++) {
               do_test1(
                  src_colors[i],
                  dst_colors[j],
                  src_formats[l],
                  dst_formats[m]);
            }
         }
      }
   }
   printf("\n");
   
   if (test_only_index && test_display) {
      ALLEGRO_EVENT_QUEUE *queue;
      ALLEGRO_EVENT event;
      al_flip_display();
      al_install_keyboard();
      queue = al_create_event_queue();
      al_register_event_source(queue, al_get_keyboard_event_source());
      al_wait_for_event(queue, &event);
   }

   return 0;
}

