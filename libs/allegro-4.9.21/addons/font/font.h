#ifndef INT_FONT_H
#define INT_FONT_H

extern ALLEGRO_FONT_VTABLE *al_font_vtable_color;

typedef struct ALLEGRO_FONT_COLOR_DATA
{
   int begin, end;                   /* first char and one-past-the-end char */
   ALLEGRO_BITMAP *glyphs;           /* our glyphs */
   ALLEGRO_BITMAP **bitmaps;         /* sub bitmaps pointing to our glyphs */
   struct ALLEGRO_FONT_COLOR_DATA *next;  /* linked list structure */
} ALLEGRO_FONT_COLOR_DATA;

ALLEGRO_FONT *_al_load_bitmap_font(const char *filename,
   int size, int flags);


#endif /* INT_FONT_H */
