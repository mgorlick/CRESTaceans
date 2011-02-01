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
 *      Some high level routines, provided for user's convinience.
 *
 *
 *      By Pavel Sountsov.
 *
 *
 *      Bezier spline plotter By Seymour Shlien.
 *
 *      Optimised version by Sven Sandberg.
 *
 *      I'm not sure wether or not we still use the Castelau Algorithm
 *      described in the book :o)
 *
 *      Interactive Computer Graphics
 *      by Peter Burger and Duncan Gillies
 *      Addison-Wesley Publishing Co 1989
 *      ISBN 0-201-17439-1
 *
 *      The 4 th order Bezier curve is a cubic curve passing
 *      through the first and fourth point. The curve does
 *      not pass through the middle two points. They are merely
 *      guide points which control the shape of the curve. The
 *      curve is tangent to the lines joining points 1 and 2
 *      and points 3 and 4.
 *
 *      See readme.txt for copyright information.
 */


#include "allegro5/allegro_primitives.h"
#ifdef ALLEGRO_CFG_OPENGL
#include "allegro5/allegro_opengl.h"
#endif
#include "allegro5/internal/aintern_bitmap.h"
#include <math.h>

#ifdef ALLEGRO_MSVC
   #define hypotf(x, y) _hypotf((x), (y))
#endif

#define LOCAL_VERTEX_CACHE  ALLEGRO_VERTEX vertex_cache[ALLEGRO_VERTEX_CACHE_SIZE]

/*
 * Make an estimate of the scale of the current transformation. 
 */
static float get_scale(void)
{
   const ALLEGRO_TRANSFORM* t = al_get_current_transform();
   return (hypotf(t->m[0][0], t->m[0][1]) + hypotf(t->m[1][0], t->m[1][1])) / 2;
}

/* Function: al_draw_line
 */
void al_draw_line(float x1, float y1, float x2, float y2,
   ALLEGRO_COLOR color, float thickness)
{
   if (thickness > 0) {
      int ii;
      float tx, ty;
      float len = (float)hypot(x2 - x1, y2 - y1);
      
      ALLEGRO_VERTEX vtx[4];

      if (len == 0)
         return;

      tx = 0.5f * thickness * (y2 - y1) / len;
      ty = 0.5f * thickness * -(x2 - x1) / len;
            
      vtx[0].x = x1 + tx; vtx[0].y = y1 + ty;
      vtx[1].x = x1 - tx; vtx[1].y =  y1 - ty;
      vtx[2].x = x2 - tx; vtx[2].y = y2 - ty;
      vtx[3].x = x2 + tx; vtx[3].y = y2 + ty;
      
      for (ii = 0; ii < 4; ii++) {
         vtx[ii].color = color;
         vtx[ii].z = 0;
      }
      
      al_draw_prim(vtx, 0, 0, 0, 4, ALLEGRO_PRIM_TRIANGLE_FAN);
      
   } else {
      ALLEGRO_VERTEX vtx[2];
         
      vtx[0].x = x1; vtx[0].y = y1;
      vtx[1].x = x2; vtx[1].y = y2;
      
      vtx[0].color = color;
      vtx[1].color = color;
      vtx[0].z = 0;
      vtx[1].z = 0;
      
      al_draw_prim(vtx, 0, 0, 0, 2, ALLEGRO_PRIM_LINE_LIST);
   }
}

/* Function: al_draw_triangle
 */
void al_draw_triangle(float x1, float y1, float x2, float y2,
   float x3, float y3, ALLEGRO_COLOR color, float thickness)
{
   if (thickness > 0) {
      int ii;
      float side1, side2, side3;      
      float p, s;
      float outer_frac, inner_frac;
      float incenter_x, incenter_y;
      float vert_x1, vert_y1, vert_x2, vert_y2, vert_x3, vert_y3;
      float incircle_rad;
      ALLEGRO_VERTEX vtx[8];
  
      side1 = (float)hypot(x2 - x1, y2 - y1);
      side2 = (float)hypot(x3 - x1, y3 - y1);
      side3 = (float)hypot(x3 - x2, y3 - y2);

      p = side1 + side2 + side3;
      s = p / 2.0f;
      if (s < 0.00001f)
         return;

      incircle_rad = sqrtf((s - side1) * (s - side2) * (s - side3) / s);

      if (incircle_rad < 0.00001f)
         return;

      outer_frac = (incircle_rad + thickness / 2) / incircle_rad;
      inner_frac = (incircle_rad - thickness / 2) / incircle_rad;

      incenter_x = (side1 * x3 + side2 * x2 + side3 * x1) / p;
      incenter_y = (side1 * y3 + side2 * y2 + side3 * y1) / p;
      
      vert_x1 = x1 - incenter_x;
      vert_y1 = y1 - incenter_y;
      vert_x2 = x2 - incenter_x;
      vert_y2 = y2 - incenter_y;
      vert_x3 = x3 - incenter_x;
      vert_y3 = y3 - incenter_y;

      vtx[1].x = incenter_x + vert_x1 * outer_frac; vtx[1].y = incenter_y + vert_y1 * outer_frac;
      vtx[0].x = incenter_x + vert_x1 * inner_frac; vtx[0].y = incenter_y + vert_y1 * inner_frac;
      
      vtx[3].x = incenter_x + vert_x2 * outer_frac; vtx[3].y = incenter_y + vert_y2 * outer_frac;
      vtx[2].x = incenter_x + vert_x2 * inner_frac; vtx[2].y = incenter_y + vert_y2 * inner_frac;
      
      vtx[5].x = incenter_x + vert_x3 * outer_frac; vtx[5].y = incenter_y + vert_y3 * outer_frac;
      vtx[4].x = incenter_x + vert_x3 * inner_frac; vtx[4].y = incenter_y + vert_y3 * inner_frac;
      
      vtx[7].x = incenter_x + vert_x1 * outer_frac; vtx[7].y = incenter_y + vert_y1 * outer_frac;
      vtx[6].x = incenter_x + vert_x1 * inner_frac; vtx[6].y = incenter_y + vert_y1 * inner_frac;
      
      for (ii = 0; ii < 8; ii++) {
         vtx[ii].color = color;
         vtx[ii].z = 0;
      }
      
      al_draw_prim(vtx, 0, 0, 0, 8, ALLEGRO_PRIM_TRIANGLE_STRIP);
      
   } else {
      ALLEGRO_VERTEX vtx[3];
         
      vtx[0].x = x1; vtx[0].y = y1;
      vtx[1].x = x2; vtx[1].y = y2;
      vtx[2].x = x3; vtx[2].y = y3;
      
      vtx[0].color = color;
      vtx[1].color = color;
      vtx[2].color = color;
      
      vtx[0].z = 0;
      vtx[1].z = 0;
      vtx[2].z = 0;
      
      al_draw_prim(vtx, 0, 0, 0, 3, ALLEGRO_PRIM_LINE_LOOP);
   }
}

/* Function: al_draw_filled_triangle
 */
void al_draw_filled_triangle(float x1, float y1, float x2, float y2,
   float x3, float y3, ALLEGRO_COLOR color)
{
   ALLEGRO_VERTEX vtx[3];

   vtx[0].x = x1; vtx[0].y = y1;
   vtx[1].x = x2; vtx[1].y = y2;
   vtx[2].x = x3; vtx[2].y = y3;
  
   vtx[0].color = color;
   vtx[1].color = color;
   vtx[2].color = color;
   
   vtx[0].z = 0;
   vtx[1].z = 0;
   vtx[2].z = 0;
   
   al_draw_prim(vtx, 0, 0, 0, 3, ALLEGRO_PRIM_TRIANGLE_LIST);
}

/* Function: al_draw_rectangle
 */
void al_draw_rectangle(float x1, float y1, float x2, float y2,
   ALLEGRO_COLOR color, float thickness)
{
   int ii;

   if (thickness > 0) {
      float t = thickness / 2;
      ALLEGRO_VERTEX vtx[10];
               
      vtx[0].x = x1 - t; vtx[0].y = y1 - t;
      vtx[1].x = x1 + t; vtx[1].y = y1 + t;
      vtx[2].x = x2 + t; vtx[2].y = y1 - t;
      vtx[3].x = x2 - t; vtx[3].y = y1 + t;
      vtx[4].x = x2 + t; vtx[4].y = y2 + t;
      vtx[5].x = x2 - t; vtx[5].y = y2 - t;
      vtx[6].x = x1 - t; vtx[6].y = y2 + t;
      vtx[7].x = x1 + t; vtx[7].y = y2 - t;
      vtx[8].x = x1 - t; vtx[8].y = y1 - t;
      vtx[9].x = x1 + t; vtx[9].y = y1 + t;
      
      for (ii = 0; ii < 10; ii++) {
         vtx[ii].color = color;
         vtx[ii].z = 0;
      }
      
      al_draw_prim(vtx, 0, 0, 0, 10, ALLEGRO_PRIM_TRIANGLE_STRIP);
   } else {
      ALLEGRO_VERTEX vtx[4];
         
      vtx[0].x = x1; vtx[0].y = y1;
      vtx[1].x = x2; vtx[1].y = y1;
      vtx[2].x = x2; vtx[2].y = y2;
      vtx[3].x = x1; vtx[3].y = y2;
      
      for (ii = 0; ii < 4; ii++) {
         vtx[ii].color = color;
         vtx[ii].z = 0;
      }
      
      al_draw_prim(vtx, 0, 0, 0, 4, ALLEGRO_PRIM_LINE_LOOP);
   }
}

/* Function: al_draw_filled_rectangle
 */
void al_draw_filled_rectangle(float x1, float y1, float x2, float y2,
   ALLEGRO_COLOR color)
{
   ALLEGRO_VERTEX vtx[4];
   int ii;

   vtx[0].x = x1; vtx[0].y = y1;
   vtx[1].x = x1; vtx[1].y = y2;
   vtx[2].x = x2; vtx[2].y = y2;
   vtx[3].x = x2; vtx[3].y = y1;
   
   for (ii = 0; ii < 4; ii++) {
      vtx[ii].color = color;
      vtx[ii].z = 0;
   }
   
   al_draw_prim(vtx, 0, 0, 0, 4, ALLEGRO_PRIM_TRIANGLE_FAN);
}

/* Function: al_calculate_arc
 */
void al_calculate_arc(float* dest, int stride, float cx, float cy,
   float rx, float ry, float start_theta, float delta_theta, float thickness,
   int num_segments)
{   
   float theta;
   float c;
   float s;
   float x, y, t;
   int ii;
 
   ASSERT(dest);
   ASSERT(num_segments > 1);
   ASSERT(rx >= 0);
   ASSERT(ry >= 0);

   if (thickness > 0.0f) {
      theta = delta_theta / ((float)(num_segments) - 1);
      c = cosf(theta);
      s = sinf(theta);
      x = cosf(start_theta);
      y = sinf(start_theta);
 
      if (rx == ry) {
         /*
         The circle case is particularly simple
         */
         float r1 = rx - thickness / 2.0f;
         float r2 = rx + thickness / 2.0f;
         for (ii = 0; ii < num_segments; ii ++) {
            *dest =       r2 * x + cx;
            *(dest + 1) = r2 * y + cy;
            dest = (float*)(((char*)dest) + stride);
            *dest =        r1 * x + cx;
            *(dest + 1) =  r1 * y + cy;
            dest = (float*)(((char*)dest) + stride);
            
            t = x;
            x = c * x - s * y;
            y = s * t + c * y;
         }
      } else {
         if (rx != 0 && !ry == 0) {
            for (ii = 0; ii < num_segments; ii++) {
               float denom = (float)hypot(ry * x, rx * y);
               float nx = thickness / 2 * ry * x / denom;
               float ny = thickness / 2 * rx * y / denom;

               *dest =       rx * x + cx + nx;
               *(dest + 1) = ry * y + cy + ny;
               dest = (float*)(((char*)dest) + stride);
               *dest =       rx * x + cx - nx;
               *(dest + 1) = ry * y + cy - ny;
               dest = (float*)(((char*)dest) + stride);

               t = x;
               x = c * x - s * y;
               y = s * t + c * y;
            }
         }
      }
   } else {
      theta = delta_theta / ((float)num_segments - 1);
      c = cosf(theta);
      s = sinf(theta);
      x = cosf(start_theta);
      y = sinf(start_theta);
      
      for (ii = 0; ii < num_segments; ii++) {
         *dest =       rx * x + cx;
         *(dest + 1) = ry * y + cy;
         dest = (float*)(((char*)dest) + stride);

         t = x;
         x = c * x - s * y;
         y = s * t + c * y;
      }
   }
}

/* Function: al_draw_ellipse
 */
void al_draw_ellipse(float cx, float cy, float rx, float ry,
   ALLEGRO_COLOR color, float thickness)
{
   LOCAL_VERTEX_CACHE;
   float scale = get_scale();

   ASSERT(rx >= 0);
   ASSERT(ry >= 0);

   if (thickness > 0) {
      int num_segments = ALLEGRO_PRIM_QUALITY * scale * sqrtf((rx + ry) / 2.0f);
      int ii;

      /* In case rx and ry are both 0. */
      if (!num_segments)
         return;

      if (2 * num_segments >= ALLEGRO_VERTEX_CACHE_SIZE) {
         num_segments = (ALLEGRO_VERTEX_CACHE_SIZE - 1) / 2;
      }
      
      al_calculate_arc(&(vertex_cache[0].x), sizeof(ALLEGRO_VERTEX), cx, cy, rx, ry, 0, ALLEGRO_PI * 2, thickness, num_segments);
      for (ii = 0; ii < 2 * num_segments; ii++) {
         vertex_cache[ii].color = color;
         vertex_cache[ii].z = 0;
      }
         
      al_draw_prim(vertex_cache, 0, 0, 0, 2 * num_segments, ALLEGRO_PRIM_TRIANGLE_STRIP);
   } else {
      int num_segments = ALLEGRO_PRIM_QUALITY * scale * sqrtf((rx + ry) / 2.0f);
      int ii;
      
      /* In case rx and ry are both 0. */
      if (!num_segments)
         return;

      if (num_segments >= ALLEGRO_VERTEX_CACHE_SIZE) {
         num_segments = ALLEGRO_VERTEX_CACHE_SIZE - 1;
      }
      
      al_calculate_arc(&(vertex_cache[0].x), sizeof(ALLEGRO_VERTEX), cx, cy, rx, ry, 0, ALLEGRO_PI * 2, 0, num_segments);
      for (ii = 0; ii < num_segments; ii++) {
         vertex_cache[ii].color = color;
         vertex_cache[ii].z = 0;
      }
         
      al_draw_prim(vertex_cache, 0, 0, 0, num_segments - 1, ALLEGRO_PRIM_LINE_LOOP);
   }
}

/* Function: al_draw_filled_ellipse
 */
void al_draw_filled_ellipse(float cx, float cy, float rx, float ry,
   ALLEGRO_COLOR color)
{
   LOCAL_VERTEX_CACHE;
   int num_segments, ii;
   float scale = get_scale();

   ASSERT(rx >= 0);
   ASSERT(ry >= 0);
   
   num_segments = ALLEGRO_PRIM_QUALITY * scale * sqrtf((rx + ry) / 2.0f);

   /* In case rx and ry are both close to 0. If al_calculate_arc is passed
    * 0 or 1 it will assert.
    */
   if (num_segments < 2)
      return;
   
   if (num_segments >= ALLEGRO_VERTEX_CACHE_SIZE) {
      num_segments = ALLEGRO_VERTEX_CACHE_SIZE - 1;
   }
      
   al_calculate_arc(&(vertex_cache[1].x), sizeof(ALLEGRO_VERTEX), cx, cy, rx, ry, 0, ALLEGRO_PI * 2, 0, num_segments);
   vertex_cache[0].x = cx; vertex_cache[0].y = cy;
   
   for (ii = 0; ii < num_segments + 1; ii++) {
      vertex_cache[ii].color = color;
      vertex_cache[ii].z = 0;
   }
   
   al_draw_prim(vertex_cache, 0, 0, 0, num_segments + 1, ALLEGRO_PRIM_TRIANGLE_FAN);
}

/* Function: al_draw_circle
 */
void al_draw_circle(float cx, float cy, float r, ALLEGRO_COLOR color,
   float thickness)
{
   al_draw_ellipse(cx, cy, r, r, color, thickness);
}

/* Function: al_draw_filled_circle
 */
void al_draw_filled_circle(float cx, float cy, float r, ALLEGRO_COLOR color)
{
   al_draw_filled_ellipse(cx, cy, r, r, color);
}

/* Function: al_draw_arc
 */
void al_draw_arc(float cx, float cy, float r, float start_theta,
   float delta_theta, ALLEGRO_COLOR color, float thickness)
{
   LOCAL_VERTEX_CACHE;
   float scale = get_scale();

   ASSERT(r >= 0);
   if (thickness > 0) {
      int num_segments = fabs(delta_theta / (2 * ALLEGRO_PI) * ALLEGRO_PRIM_QUALITY * scale * sqrtf(r));
      int ii;
      
      if (2 * num_segments >= ALLEGRO_VERTEX_CACHE_SIZE) {
         num_segments = (ALLEGRO_VERTEX_CACHE_SIZE - 1) / 2;
      }
      
      al_calculate_arc(&(vertex_cache[0].x), sizeof(ALLEGRO_VERTEX), cx, cy, r, r, start_theta, delta_theta, thickness, num_segments);
      
      for (ii = 0; ii < 2 * num_segments; ii++) {
         vertex_cache[ii].color = color;
         vertex_cache[ii].z = 0;
      }
      
      al_draw_prim(vertex_cache, 0, 0, 0, 2 * num_segments, ALLEGRO_PRIM_TRIANGLE_STRIP);
   } else {
      int num_segments = fabs(delta_theta / (2 * ALLEGRO_PI) * ALLEGRO_PRIM_QUALITY * scale * sqrtf(r));
      int ii;
      
      if (num_segments >= ALLEGRO_VERTEX_CACHE_SIZE) {
         num_segments = ALLEGRO_VERTEX_CACHE_SIZE - 1;
      }
      
      al_calculate_arc(&(vertex_cache[0].x), sizeof(ALLEGRO_VERTEX), cx, cy, r, r, start_theta, delta_theta, 0, num_segments);
      
      for (ii = 0; ii < num_segments; ii++) {
         vertex_cache[ii].color = color;
         vertex_cache[ii].z = 0;
      }
      
      al_draw_prim(vertex_cache, 0, 0, 0, num_segments, ALLEGRO_PRIM_LINE_STRIP);
   }
}

/* Function: al_draw_rounded_rectangle
 */
void al_draw_rounded_rectangle(float x1, float y1, float x2, float y2,
   float rx, float ry, ALLEGRO_COLOR color, float thickness)
{
   LOCAL_VERTEX_CACHE;
   float scale = get_scale();

   ASSERT(rx >= 0);
   ASSERT(ry >= 0);

   if (thickness > 0) {
      int num_segments = ALLEGRO_PRIM_QUALITY * scale * sqrtf((rx + ry) / 2.0f) / 4;
      int ii;

      /* In case rx and ry are both 0. */
      if (!num_segments)
         al_draw_rectangle(x1, y1, x2, y2, color, thickness);

      if (8 * num_segments + 2 >= ALLEGRO_VERTEX_CACHE_SIZE) {
         num_segments = (ALLEGRO_VERTEX_CACHE_SIZE - 3) / 8;
      }
      
      al_calculate_arc(&(vertex_cache[0].x), sizeof(ALLEGRO_VERTEX), 0, 0, rx, ry, 0, ALLEGRO_PI / 2, thickness, num_segments);
      
      for (ii = 0; ii < 2 * num_segments; ii += 2) {
         vertex_cache[ii + 2 * num_segments + 1].x = x1 + rx - vertex_cache[2 * num_segments - 1 - ii].x;
         vertex_cache[ii + 2 * num_segments + 1].y = y1 + ry - vertex_cache[2 * num_segments - 1 - ii].y;
         vertex_cache[ii + 2 * num_segments].x = x1 + rx - vertex_cache[2 * num_segments - 1 - ii - 1].x;
         vertex_cache[ii + 2 * num_segments].y = y1 + ry - vertex_cache[2 * num_segments - 1 - ii - 1].y;

         vertex_cache[ii + 4 * num_segments].x = x1 + rx - vertex_cache[ii].x;
         vertex_cache[ii + 4 * num_segments].y = y2 - ry + vertex_cache[ii].y;
         vertex_cache[ii + 4 * num_segments + 1].x = x1 + rx - vertex_cache[ii + 1].x;
         vertex_cache[ii + 4 * num_segments + 1].y = y2 - ry + vertex_cache[ii + 1].y;

         vertex_cache[ii + 6 * num_segments + 1].x = x2 - rx + vertex_cache[2 * num_segments - 1 - ii].x;
         vertex_cache[ii + 6 * num_segments + 1].y = y2 - ry + vertex_cache[2 * num_segments - 1 - ii].y;
         vertex_cache[ii + 6 * num_segments].x = x2 - rx + vertex_cache[2 * num_segments - 1 - ii - 1].x;
         vertex_cache[ii + 6 * num_segments].y = y2 - ry + vertex_cache[2 * num_segments - 1 - ii - 1].y;
      }
      for (ii = 0; ii < 2 * num_segments; ii += 2) {
         vertex_cache[ii].x = x2 - rx + vertex_cache[ii].x;
         vertex_cache[ii].y = y1 + ry - vertex_cache[ii].y;
         vertex_cache[ii + 1].x = x2 - rx + vertex_cache[ii + 1].x;
         vertex_cache[ii + 1].y = y1 + ry - vertex_cache[ii + 1].y;
      }
      vertex_cache[8 * num_segments] = vertex_cache[0];
      vertex_cache[8 * num_segments + 1] = vertex_cache[1];

      for (ii = 0; ii < 8 * num_segments + 2; ii++) {
         vertex_cache[ii].color = color;
         vertex_cache[ii].z = 0;
      }
         
      al_draw_prim(vertex_cache, 0, 0, 0, 8 * num_segments + 2, ALLEGRO_PRIM_TRIANGLE_STRIP);
   } else {
      int num_segments = ALLEGRO_PRIM_QUALITY * scale * sqrtf((rx + ry) / 2.0f) / 4;
      int ii;
      
      /* In case rx and ry are both 0. */
      if (!num_segments)
         al_draw_rectangle(x1, y1, x2, y2, color, thickness);

      if (num_segments * 4 >= ALLEGRO_VERTEX_CACHE_SIZE) {
         num_segments = (ALLEGRO_VERTEX_CACHE_SIZE - 1) / 4;
      }
      
      al_calculate_arc(&(vertex_cache[0].x), sizeof(ALLEGRO_VERTEX), 0, 0, rx, ry, 0, ALLEGRO_PI / 2, 0, num_segments + 1);

      for (ii = 0; ii < num_segments; ii++) {
         vertex_cache[ii + 1 * num_segments].x = x1 + rx - vertex_cache[num_segments - 1 - ii].x;
         vertex_cache[ii + 1 * num_segments].y = y1 + ry - vertex_cache[num_segments - 1 - ii].y;

         vertex_cache[ii + 2 * num_segments].x = x1 + rx - vertex_cache[ii].x;
         vertex_cache[ii + 2 * num_segments].y = y2 - ry + vertex_cache[ii].y;

         vertex_cache[ii + 3 * num_segments].x = x2 - rx + vertex_cache[num_segments - 1 - ii].x;
         vertex_cache[ii + 3 * num_segments].y = y2 - ry + vertex_cache[num_segments - 1 - ii].y;
      }
      for (ii = 0; ii < num_segments; ii++) {
         vertex_cache[ii].x = x2 - rx + vertex_cache[ii].x;
         vertex_cache[ii].y = y1 + ry - vertex_cache[ii].y;
      }

      for (ii = 0; ii < 4 * num_segments; ii++) {
         vertex_cache[ii].color = color;
         vertex_cache[ii].z = 0;
      }
         
      al_draw_prim(vertex_cache, 0, 0, 0, 4 * num_segments, ALLEGRO_PRIM_LINE_LOOP);
   }
}

/* Function: al_draw_filled_rounded_rectangle
 */
void al_draw_filled_rounded_rectangle(float x1, float y1, float x2, float y2,
   float rx, float ry, ALLEGRO_COLOR color)
{
   LOCAL_VERTEX_CACHE;
   int ii;
   float scale = get_scale();
   int num_segments = ALLEGRO_PRIM_QUALITY * scale * sqrtf((rx + ry) / 2.0f) / 4;

   ASSERT(rx >= 0);
   ASSERT(ry >= 0);
   
   /* In case rx and ry are both 0. */
   if (!num_segments)
      al_draw_filled_rectangle(x1, y1, x2, y2, color);

   if (num_segments * 4 >= ALLEGRO_VERTEX_CACHE_SIZE) {
      num_segments = (ALLEGRO_VERTEX_CACHE_SIZE - 1) / 4;
   }
   
   al_calculate_arc(&(vertex_cache[0].x), sizeof(ALLEGRO_VERTEX), 0, 0, rx, ry, 0, ALLEGRO_PI / 2, 0, num_segments + 1);

   for (ii = 0; ii < num_segments; ii++) {
      vertex_cache[ii + 1 * num_segments].x = x1 + rx - vertex_cache[num_segments - 1 - ii].x;
      vertex_cache[ii + 1 * num_segments].y = y1 + ry - vertex_cache[num_segments - 1 - ii].y;

      vertex_cache[ii + 2 * num_segments].x = x1 + rx - vertex_cache[ii].x;
      vertex_cache[ii + 2 * num_segments].y = y2 - ry + vertex_cache[ii].y;

      vertex_cache[ii + 3 * num_segments].x = x2 - rx + vertex_cache[num_segments - 1 - ii].x;
      vertex_cache[ii + 3 * num_segments].y = y2 - ry + vertex_cache[num_segments - 1 - ii].y;
   }
   for (ii = 0; ii < num_segments; ii++) {
      vertex_cache[ii].x = x2 - rx + vertex_cache[ii].x;
      vertex_cache[ii].y = y1 + ry - vertex_cache[ii].y;
   }

   for (ii = 0; ii < 4 * num_segments; ii++) {
      vertex_cache[ii].color = color;
      vertex_cache[ii].z = 0;
   }

   /*
   TODO: Doing this as a triangle fan just doesn't sound all that great, perhaps shuffle the vertices somehow to at least make it a strip
   */
   al_draw_prim(vertex_cache, 0, 0, 0, 4 * num_segments, ALLEGRO_PRIM_TRIANGLE_FAN);
}

/* Function: al_calculate_spline
 */
void al_calculate_spline(float* dest, int stride, float points[8],
   float thickness, int num_segments)
{
   /* Derivatives of x(t) and y(t). */
   float x, dx, ddx, dddx;
   float y, dy, ddy, dddy;
   int ii = 0;
   
   /* Temp variables used in the setup. */
   float dt, dt2, dt3;
   float xdt2_term, xdt3_term;
   float ydt2_term, ydt3_term;

   /* This is enough to avoid malloc in ex_prim, which I take as a reasonable
    * guess to what a common number of segments might be.  To be honest, it
    * probably makes no difference.
    */
   float cache_point_buffer_storage[150];
   float* cache_point_buffer = cache_point_buffer_storage;

   ASSERT(num_segments > 1);
   ASSERT(points);

   if (num_segments > (int)(sizeof(cache_point_buffer_storage) / sizeof(float) / 2)) {
      cache_point_buffer = al_malloc(2 * sizeof(float) * num_segments);
   }

   dt = 1.0 / (num_segments - 1);
   dt2 = (dt * dt);
   dt3 = (dt2 * dt);
   
   /* x coordinates. */
   xdt2_term = 3 * (points[4] - 2 * points[2] + points[0]);
   xdt3_term = points[6] + 3 * (-points[4] + points[2]) - points[0];
   
   xdt2_term = dt2 * xdt2_term;
   xdt3_term = dt3 * xdt3_term;
   
   dddx = 6 * xdt3_term;
   ddx = -6 * xdt3_term + 2 * xdt2_term;
   dx = xdt3_term - xdt2_term + 3 * dt * (points[2] - points[0]);
   x = points[0];
   
   /* y coordinates. */
   ydt2_term = 3 * (points[5] - 2 * points[3] + points[1]);
   ydt3_term = points[7] + 3 * (-points[5] + points[3]) - points[1];
   
   ydt2_term = dt2 * ydt2_term;
   ydt3_term = dt3 * ydt3_term;
   
   dddy = 6 * ydt3_term;
   ddy = -6 * ydt3_term + 2 * ydt2_term;
   dy = ydt3_term - ydt2_term + dt * 3 * (points[3] - points[1]);
   y = points[1];
   
   cache_point_buffer[2 * ii] = x;
   cache_point_buffer[2 * ii + 1] = y;
   
   for (ii = 1; ii < num_segments; ii++) {
      ddx += dddx;
      dx += ddx;
      x += dx;
      
      ddy += dddy;
      dy += ddy;
      y += dy;
      
      cache_point_buffer[2 * ii] = x;
      cache_point_buffer[2 * ii + 1] = y;
   }
   al_calculate_ribbon(dest, stride, cache_point_buffer, 2 * sizeof(float), thickness, num_segments);

   if (cache_point_buffer != cache_point_buffer_storage) {
      al_free(cache_point_buffer);
   }
}

/* Function: al_draw_spline
 */
void al_draw_spline(float points[8], ALLEGRO_COLOR color, float thickness)
{
   int ii;
   float scale = get_scale();
   int num_segments = (int)(sqrtf((float)hypot(points[2] - points[0], points[3] - points[1]) +
                                  (float)hypot(points[4] - points[2], points[5] - points[3]) +
                                  (float)hypot(points[6] - points[4], points[7] - points[5])) *
                            1.2 * ALLEGRO_PRIM_QUALITY * scale / 10);
   LOCAL_VERTEX_CACHE;
   
   if(num_segments < 2)
      num_segments = 2;

   if (thickness > 0) {
      if (2 * num_segments >= ALLEGRO_VERTEX_CACHE_SIZE) {
         num_segments = (ALLEGRO_VERTEX_CACHE_SIZE - 1) / 2;
      }
         
      al_calculate_spline(&(vertex_cache[0].x), sizeof(ALLEGRO_VERTEX), points, thickness, num_segments);
      
      for (ii = 0; ii < 2 * num_segments; ii++) {
         vertex_cache[ii].color = color;
         vertex_cache[ii].z = 0;
      }
      
      al_draw_prim(vertex_cache, 0, 0, 0, 2 * num_segments, ALLEGRO_PRIM_TRIANGLE_STRIP);
   } else {
      if (num_segments >= ALLEGRO_VERTEX_CACHE_SIZE) {
         num_segments = ALLEGRO_VERTEX_CACHE_SIZE - 1;
      }
         
      al_calculate_spline(&(vertex_cache[0].x), sizeof(ALLEGRO_VERTEX), points, thickness, num_segments);
      
      for (ii = 0; ii < num_segments; ii++) {
         vertex_cache[ii].color = color;
         vertex_cache[ii].z = 0;
      }
      
      al_draw_prim(vertex_cache, 0, 0, 0, num_segments, ALLEGRO_PRIM_LINE_STRIP);
   }
}

/* Function: al_calculate_ribbon
 */
void al_calculate_ribbon(float* dest, int dest_stride, const float *points,
   int points_stride, float thickness, int num_segments)
{
   ASSERT(points);
   ASSERT(num_segments >= 2);

   if (thickness > 0) {
      int ii = 0;
      float x, y;
      
      float cur_dir_x;
      float cur_dir_y;
      float prev_dir_x = 0;
      float prev_dir_y = 0;
      float t = thickness / 2;
      float tx, ty;
      float nx, ny;
      float sign = 1;
      
      for (ii = 0; ii < 2 * num_segments - 2; ii += 2) {
         float dir_len;
         x = *points;
         y = *(points + 1);

         points = (float*)(((char*)points) + points_stride);
         
         cur_dir_x = *(points)     - x;
         cur_dir_y = *(points + 1) - y;
         
         dir_len = (float)hypot(cur_dir_x, cur_dir_y);
         
         if(dir_len > 0.000001f) {
            cur_dir_x /= dir_len;
            cur_dir_y /= dir_len;
         } else if (ii == 0){
            cur_dir_x = 1;
            cur_dir_y = 0;
         } else {
            cur_dir_x = prev_dir_x;
            cur_dir_y = prev_dir_y;
         }
         
         if (ii == 0) {
            tx = -t * cur_dir_y;
            ty = t * cur_dir_x;
            nx = 0;
            ny = 0;
         } else {
            float dot = cur_dir_x * prev_dir_x + cur_dir_y * prev_dir_y;
            float norm_len, cosine;
            if(dot < 0) {
               /*
                * This is by no means exact, but seems to produce acceptable results
                */
               float tx_;
               tx = cur_dir_x - prev_dir_x;
               ty = cur_dir_y - prev_dir_y;
               norm_len = (float)hypot(tx, ty);
               
               tx /= norm_len;
               ty /= norm_len;
               
               cosine = tx * cur_dir_x + ty * cur_dir_y;
               
               nx = -t * tx / cosine;
               ny = -t * ty / cosine;
               tx_ = tx;
               tx =  -t * ty * cosine;
               ty =  t * tx_ * cosine;
               sign = -sign;
            } else {
               float new_norm_len;
               tx = cur_dir_y + prev_dir_y;
               ty = -(cur_dir_x + prev_dir_x);
               norm_len = (float)hypot(tx, ty);
  
               tx /= norm_len;
               ty /= norm_len;
               cosine = tx * (-cur_dir_y) + ty * (cur_dir_x);
               new_norm_len = t / cosine;
               
               tx *= new_norm_len;
               ty *= new_norm_len;
               nx = 0;
               ny = 0;
            }
         }
         
         *dest =       x - sign * tx + nx;
         *(dest + 1) = y - sign * ty + ny;
         dest = (float*)(((char*)dest) + dest_stride);
         *dest =       x + sign * tx + nx;
         *(dest + 1) = y + sign * ty + ny;
         dest = (float*)(((char*)dest) + dest_stride);
         
         prev_dir_x = cur_dir_x;
         prev_dir_y = cur_dir_y;
      }
      tx = -t * prev_dir_y;
      ty = t * prev_dir_x;
      
      x = *points;
      y = *(points + 1);
      
      *dest =       x - sign * tx;
      *(dest + 1) = y - sign * ty;
      dest = (float*)(((char*)dest) + dest_stride);
      *dest =       x + sign * tx;
      *(dest + 1) = y + sign * ty;
   } else {
      int ii;
      for (ii = 0; ii < num_segments; ii++) {
         *dest =       *points;
         *(dest + 1) = *(points + 1);
         dest = (float*)(((char*)dest) + dest_stride);
         points = (float*)(((char*)points) + points_stride);
      }
   }
}

/* Function: al_draw_ribbon
 */
void al_draw_ribbon(const float *points, int points_stride, ALLEGRO_COLOR color,
   float thickness, int num_segments)
{
   LOCAL_VERTEX_CACHE;
   int ii;

   al_calculate_ribbon(&(vertex_cache[0].x), sizeof(ALLEGRO_VERTEX), points, points_stride, thickness, num_segments);
   
   if (thickness > 0) {
      for (ii = 0; ii < 2 * num_segments; ii++) {
         vertex_cache[ii].color = color;
         vertex_cache[ii].z = 0;
      }
      
      al_draw_prim(vertex_cache, 0, 0, 0, 2 * num_segments, ALLEGRO_PRIM_TRIANGLE_STRIP);
   } else {
      for (ii = 0; ii < num_segments; ii++) {
         vertex_cache[ii].color = color;
         vertex_cache[ii].z = 0;
      }
      
      al_draw_prim(vertex_cache, 0, 0, 0, num_segments, ALLEGRO_PRIM_LINE_STRIP);
   }
}
