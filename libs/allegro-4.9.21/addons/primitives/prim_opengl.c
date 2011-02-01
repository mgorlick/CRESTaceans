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
 *      OpenGL implementation of some of the primitive routines.
 *
 *
 *      By Pavel Sountsov.
 *
 *      See readme.txt for copyright information.
 */

#include "allegro5/allegro5.h"
#include "allegro5/allegro_primitives.h"
#include "allegro5/allegro_opengl.h"
#include "allegro5/internal/aintern_prim_opengl.h"
#include "allegro5/internal/aintern_prim_soft.h"
#include "allegro5/platform/alplatf.h"
#include "allegro5/internal/aintern_prim.h"

#ifdef ALLEGRO_CFG_OPENGL

#include "allegro5/allegro_opengl.h"
#include "allegro5/internal/aintern_opengl.h"

static void setup_blending(ALLEGRO_DISPLAY *ogl_disp)
{
   int op, src_color, dst_color, op_alpha, src_alpha, dst_alpha;
   const int blend_modes[4] = {
      GL_ZERO, GL_ONE, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA
   };
   const int blend_equations[3] = {
      GL_FUNC_ADD, GL_FUNC_SUBTRACT, GL_FUNC_REVERSE_SUBTRACT
   };

   al_get_separate_blender(&op, &src_color, &dst_color,
                     &op_alpha, &src_alpha, &dst_alpha);
   /* glBlendFuncSeparate was only included with OpenGL 1.4 */
#if !defined ALLEGRO_GP2XWIZ
   {
#ifndef ALLEGRO_IPHONE
      if (ogl_disp->ogl_extras->ogl_info.version >= 1.4) {
#else
      if (ogl_disp->ogl_extras->ogl_info.version >= 2.0) {
#endif
         glEnable(GL_BLEND);
         glBlendFuncSeparate(blend_modes[src_color], blend_modes[dst_color],
                        blend_modes[src_alpha], blend_modes[dst_alpha]);
         if (ogl_disp->ogl_extras->ogl_info.version >= 2.0) {
            glBlendEquationSeparate(
                              blend_equations[op],
                              blend_equations[op_alpha]);
         }
         else
            glBlendEquation(blend_equations[op]);
      }
      else {
         if (src_color == src_alpha && dst_color == dst_alpha) {
            glEnable(GL_BLEND);
            glBlendFunc(blend_modes[src_color], blend_modes[dst_color]);
         }
         else {
            return;
         }
      }
   }
#else
   (void)ogl_disp;
   glEnable(GL_BLEND);
   glBlendFunc(blend_modes[src_color], blend_modes[dst_color]);
#endif
}

static void setup_state(const char* vtxs, const ALLEGRO_VERTEX_DECL* decl, ALLEGRO_BITMAP* texture)
{
   if(decl) {
      ALLEGRO_VERTEX_ELEMENT* e;
      e = &decl->elements[ALLEGRO_PRIM_POSITION];
      if(e->attribute) {
         int ncoord = 0;
         GLenum type = 0;

         glEnableClientState(GL_VERTEX_ARRAY);

         switch(e->storage) {
            case ALLEGRO_PRIM_FLOAT_2:
               ncoord = 2;
               type = GL_FLOAT;
            break;
            case ALLEGRO_PRIM_FLOAT_3:
               ncoord = 3;
               type = GL_FLOAT;
            break;
            case ALLEGRO_PRIM_SHORT_2:
               ncoord = 2;
               type = GL_SHORT;
            break;
         }
         glVertexPointer(ncoord, type, decl->stride, vtxs + e->offset);
      } else {
         glDisableClientState(GL_VERTEX_ARRAY);
      }

      e = &decl->elements[ALLEGRO_PRIM_TEX_COORD];
      if(!e->attribute)
         e = &decl->elements[ALLEGRO_PRIM_TEX_COORD_PIXEL];
      if(texture && e->attribute) {
         GLenum type = 0;

         glEnableClientState(GL_TEXTURE_COORD_ARRAY);

         switch(e->storage) {
            case ALLEGRO_PRIM_FLOAT_2:
            case ALLEGRO_PRIM_FLOAT_3:
               type = GL_FLOAT;
            break;
            case ALLEGRO_PRIM_SHORT_2:
               type = GL_SHORT;
            break;
         }
         glTexCoordPointer(2, type, decl->stride, vtxs + e->offset);
      } else {
         glDisableClientState(GL_TEXTURE_COORD_ARRAY);
      }

      e = &decl->elements[ALLEGRO_PRIM_COLOR_ATTR];
      if(e->attribute) {
         glEnableClientState(GL_COLOR_ARRAY);

         glColorPointer(4, GL_FLOAT, decl->stride, vtxs + e->offset);
      } else {
         glDisableClientState(GL_COLOR_ARRAY);
         glColor4f(1, 1, 1, 1);
      }
   } else {
      const ALLEGRO_VERTEX* vtx = (const ALLEGRO_VERTEX*)vtxs;
   
      glEnableClientState(GL_COLOR_ARRAY);
      glEnableClientState(GL_VERTEX_ARRAY);
      glEnableClientState(GL_TEXTURE_COORD_ARRAY);

      glVertexPointer(3, GL_FLOAT, sizeof(ALLEGRO_VERTEX), &vtx[0].x);
      glColorPointer(4, GL_FLOAT, sizeof(ALLEGRO_VERTEX), &vtx[0].color.r);
      glTexCoordPointer(2, GL_FLOAT, sizeof(ALLEGRO_VERTEX), &vtx[0].u);
   }

   if (texture) {
      GLuint gl_texture = al_get_opengl_texture(texture);
      int true_w, true_h;
      int tex_x, tex_y;
      GLuint current_texture;
      float mat[4][4] = {
         {1,  0,  0, 0},
         {0, -1,  0, 0},
         {0,  0,  1, 0},
         {0,  0,  0, 1}
      };
      int height;

      if (texture->parent)
         height = texture->parent->h;
      else
         height = texture->h;
      
      al_get_opengl_texture_size(texture, &true_w, &true_h);
      al_get_opengl_texture_position(texture, &tex_x, &tex_y);
      
      mat[3][0] = (float)tex_x / true_w;
      mat[3][1] = (float)(height - tex_y) / true_h;
         
      if(decl) {
         if(decl->elements[ALLEGRO_PRIM_TEX_COORD_PIXEL].attribute) {
            mat[0][0] = 1.0f / true_w;
            mat[1][1] = -1.0f / true_h;
         } else {
            mat[0][0] = (float)al_get_bitmap_width(texture) / true_w;
            mat[1][1] = -(float)al_get_bitmap_height(texture) / true_h;
         }
      } else {
         mat[0][0] = 1.0f / true_w;
         mat[1][1] = -1.0f / true_h;
      }

      glGetIntegerv(GL_TEXTURE_BINDING_2D, (GLint*)&current_texture);
      if (current_texture != gl_texture) {
         glBindTexture(GL_TEXTURE_2D, gl_texture);
      }

      glMatrixMode(GL_TEXTURE);
      glLoadMatrixf(mat[0]);
      glMatrixMode(GL_MODELVIEW);
   } else {
      glBindTexture(GL_TEXTURE_2D, 0);
   }
}

int _al_draw_prim_opengl(ALLEGRO_BITMAP* target, ALLEGRO_BITMAP* texture, const void* vtxs, const ALLEGRO_VERTEX_DECL* decl, int start, int end, int type)
{   
#ifdef ALLEGRO_CFG_OPENGL

   int num_primitives = 0;
   ALLEGRO_DISPLAY *ogl_disp = target->display;
   ALLEGRO_BITMAP_OGL *ogl_target = (ALLEGRO_BITMAP_OGL *)target;
   const void* vtx;
   int stride = decl ? decl->stride : (int)sizeof(ALLEGRO_VERTEX);
   int num_vtx;
   
   if (target->parent) {
       ogl_target = (ALLEGRO_BITMAP_OGL *)target->parent;
   }
  
   if ((!ogl_target->is_backbuffer && ogl_disp->ogl_extras->opengl_target != ogl_target) || al_is_bitmap_locked(target)) {
      return _al_draw_prim_soft(texture, vtxs, decl, start, end, type);
   }
   
   /* For sub bitmaps. */
   if(al_is_sub_bitmap(target)) {
      int xofs, yofs;
      al_get_opengl_texture_position(target, &xofs, &yofs);
      glMatrixMode(GL_MODELVIEW);
      glPushMatrix();
      glLoadIdentity();
      glTranslatef(target->xofs, target->yofs, 0);
      glMultMatrixf((float*)al_get_current_transform()->m);
   }
   
   vtx = (const char*)vtxs + start * stride;
   num_vtx = end - start;

   setup_blending(ogl_disp);
   setup_state(vtx, decl, texture);
   
   if(texture) {
      glEnable(GL_TEXTURE_2D);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
   }

   switch (type) {
      case ALLEGRO_PRIM_LINE_LIST: {
         glDrawArrays(GL_LINES, 0, num_vtx);
         num_primitives = num_vtx / 2;
         break;
      };
      case ALLEGRO_PRIM_LINE_STRIP: {
         glDrawArrays(GL_LINE_STRIP, 0, num_vtx);
         num_primitives = num_vtx - 1;
         break;
      };
      case ALLEGRO_PRIM_LINE_LOOP: {
         glDrawArrays(GL_LINE_LOOP, 0, num_vtx);
         num_primitives = num_vtx;
         break;
      };
      case ALLEGRO_PRIM_TRIANGLE_LIST: {
         glDrawArrays(GL_TRIANGLES, 0, num_vtx);
         num_primitives = num_vtx / 3;
         break;
      };
      case ALLEGRO_PRIM_TRIANGLE_STRIP: {
         glDrawArrays(GL_TRIANGLE_STRIP, 0, num_vtx);
         num_primitives = num_vtx - 2;
         break;
      };
      case ALLEGRO_PRIM_TRIANGLE_FAN: {
         glDrawArrays(GL_TRIANGLE_FAN, 0, num_vtx);
         num_primitives = num_vtx - 2;
         break;
      };
      case ALLEGRO_PRIM_POINT_LIST: {
         glDrawArrays(GL_POINTS, 0, num_vtx);
         num_primitives = num_vtx;
         break;
      };
   }

   if(texture) {
      glDisable(GL_TEXTURE_2D);
      glMatrixMode(GL_TEXTURE);
      glLoadIdentity();
      glMatrixMode(GL_MODELVIEW);
   }
   
   glDisableClientState(GL_COLOR_ARRAY);
   glDisableClientState(GL_VERTEX_ARRAY);
   glDisableClientState(GL_TEXTURE_COORD_ARRAY);
   
   if(al_is_sub_bitmap(target)) {
      glMatrixMode(GL_MODELVIEW);
      glPopMatrix();
   }

   return num_primitives;
#else
   (void)texture;
   (void)vtxs;
   (void)start;
   (void)end;
   (void)type;
   (void)decl;

   return 0;
#endif
}

int _al_draw_prim_indexed_opengl(ALLEGRO_BITMAP *target, ALLEGRO_BITMAP* texture, const void* vtxs, const ALLEGRO_VERTEX_DECL* decl, const int* indices, int num_vtx, int type)
{   
#ifdef ALLEGRO_CFG_OPENGL

   int num_primitives = 0;
   ALLEGRO_DISPLAY *ogl_disp = target->display;
   ALLEGRO_BITMAP_OGL *ogl_target = (ALLEGRO_BITMAP_OGL *)target;
   const void* vtx;
   const void* idx = indices;
   GLenum idx_size;

#if defined ALLEGRO_GP2XWIZ || defined ALLEGRO_IPHONE
   GLushort ind[num_vtx];
   int ii;
#endif

   if (target->parent) {
       ogl_target = (ALLEGRO_BITMAP_OGL *)target->parent;
   }

   if ((!ogl_target->is_backbuffer && ogl_disp->ogl_extras->opengl_target != ogl_target) || al_is_bitmap_locked(target)) {
      return _al_draw_prim_indexed_soft(texture, decl, vtxs, indices, num_vtx, type);
   }
   
   /* For sub bitmaps. */
   if(al_is_sub_bitmap(target)) {
      int xofs, yofs;
      al_get_opengl_texture_position(target, &xofs, &yofs);
      glMatrixMode(GL_MODELVIEW);
      glPushMatrix();
      glLoadIdentity();
      glTranslatef(xofs, yofs, 0);
      glMultMatrixf((float*)al_get_current_transform()->m);
   }
   
   vtx = vtxs;

   setup_blending(ogl_disp);
   setup_state(vtx, decl, texture);
   
   if(texture) {
      glEnable(GL_TEXTURE_2D);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
   }
  
#if defined ALLEGRO_GP2XWIZ || defined ALLEGRO_IPHONE
   for (ii = 0; ii < num_vtx; ii++) {
      ind[ii] = (GLushort)indices[ii];
   }
   idx = ind;
   idx_size = GL_UNSIGNED_SHORT;
#else
   idx_size = GL_UNSIGNED_INT;
#endif

   switch (type) {
      case ALLEGRO_PRIM_LINE_LIST: {
         glDrawElements(GL_LINES, num_vtx, idx_size, idx);
         num_primitives = num_vtx / 2;
         break;
      };
      case ALLEGRO_PRIM_LINE_STRIP: {
         glDrawElements(GL_LINE_STRIP, num_vtx, idx_size, idx);
         num_primitives = num_vtx - 1;
         break;
      };
      case ALLEGRO_PRIM_LINE_LOOP: {
         glDrawElements(GL_LINE_LOOP, num_vtx, idx_size, idx);
         num_primitives = num_vtx;
         break;
      };
      case ALLEGRO_PRIM_TRIANGLE_LIST: {
         glDrawElements(GL_TRIANGLES, num_vtx, idx_size, idx);
         num_primitives = num_vtx / 3;
         break;
      };
      case ALLEGRO_PRIM_TRIANGLE_STRIP: {
         glDrawElements(GL_TRIANGLE_STRIP, num_vtx, idx_size, idx);
         num_primitives = num_vtx - 2;
         break;
      };
      case ALLEGRO_PRIM_TRIANGLE_FAN: {
         glDrawElements(GL_TRIANGLE_FAN, num_vtx, idx_size, idx);
         num_primitives = num_vtx - 2;
         break;
      };
      case ALLEGRO_PRIM_POINT_LIST: {
         glDrawElements(GL_POINTS, num_vtx, idx_size, idx);
         num_primitives = num_vtx;
         break;
      };
   }

   if(texture) {
      glDisable(GL_TEXTURE_2D);
      glMatrixMode(GL_TEXTURE);
      glLoadIdentity();
      glMatrixMode(GL_MODELVIEW);
   }
   
   glDisableClientState(GL_COLOR_ARRAY);
   glDisableClientState(GL_VERTEX_ARRAY);
   glDisableClientState(GL_TEXTURE_COORD_ARRAY);
   
   if(al_is_sub_bitmap(target)) {
      glMatrixMode(GL_MODELVIEW);
      glPopMatrix();
   }

   return num_primitives;
#else
   (void)texture;
   (void)vtxs;
   (void)start;
   (void)end;
   (void)type;
   (void)decl;

   return 0;
#endif
}

#endif
