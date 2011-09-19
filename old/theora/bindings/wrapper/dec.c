#include <stdlib.h>
#include <stdio.h>
#include <SDL/SDL.h>
#include <theora/theoradec.h>

void doc (int r) {
  switch (r) {
    case 0:
      printf ("Success.");
      break;
    case TH_DUPFRAME:
      printf ("Dropped (0-byte) frame.");
      break;
    case TH_EFAULT:
      printf ("Argument was NULL.");
      break;
    case TH_EBADPACKET:
      printf ("Does not contain encoded video data.");
      break;
    case TH_EIMPL:
      printf ("Bitstream contains unsupported features.");
      break;
    case TH_EBADHEADER:
      printf ("Bad header.");
      break;
    case TH_EVERSION:
      printf ("Header was for incompatible bitstream version.");
      break;
    case TH_ENOTFORMAT:
      printf ("Packet is not a Theora header.");
      break;
    default:
      printf ("Unknown return value.");
  }
  printf ("\n");
}



typedef struct TheoraDec {
  /* track just to switch on packet type */
  int have_comment;
  int have_id;
  int have_type;
  
  th_info* info;
  th_comment* comment;
  th_setup_info* setup;
  th_dec_ctx* ctx;

  /* video output coupled here: evil but easy */
  SDL_Surface *screen;
  SDL_Overlay *yuv_overlay;
  SDL_Rect rect;
  
} TheoraDec;

int video_init (TheoraDec *dec);
void video_display (TheoraDec *dec, th_ycbcr_buffer yuv);

int theoradec_ready_for_data (TheoraDec *dec) {
  return dec->have_comment && dec->have_type && dec->have_id;
}

void theoradec_delete (TheoraDec *dec) {

  if (!dec) return;
  
  if (dec->info != NULL) {
    th_info_clear (dec->info);
    free (dec->info);
  }
  
  if (dec->comment != NULL) {
    th_comment_clear (dec->comment);
    free (dec->comment);
  }
  
  if (dec->setup != NULL) th_setup_free (dec->setup);
  if (dec->ctx != NULL) th_decode_free (dec->ctx);

  SDL_Quit ();
  
  free (dec);
}

TheoraDec* theoradec_new (void) {
  
  TheoraDec *dec = malloc (sizeof (TheoraDec));
  
  if (dec) {
    dec->have_comment = 0;
    dec->have_id = 0;
    dec->have_type = 0;
    
    dec->info = malloc (sizeof (th_info));
    dec->comment = malloc (sizeof (th_comment));
    dec->setup = NULL;
    dec->ctx = NULL;

    if (dec->info && dec->comment) {
      th_info_init (dec->info);
      th_comment_init (dec->comment);
    } else goto init_err;
    
    if (0 > SDL_Init (SDL_INIT_VIDEO)) {
      printf ("Unable to init SDL: %s\n", SDL_GetError ());
      goto init_err;
    }
    
    return dec;
  }
  
init_err:
  theoradec_delete (dec);
  return NULL;
}

int theoradec_header_in (TheoraDec *dec,
                         unsigned char *buffer, long buffer_length) {

  int r;
  ogg_packet p;

  if (!dec || buffer_length < 1) return 0;
  
  p.packet = buffer;
  p.bytes = buffer_length;
  p.b_o_s = (buffer[0] == 0x80) ? 1 : 0;
  p.e_o_s = 0;
  p.granulepos = 0;
  p.packetno = 0;
  
  switch (buffer[0]) {
    case 0x80:
      if (dec->have_id) goto already_have;
      if (0 > (r = th_decode_headerin (dec->info, dec->comment, &(dec->setup), &p)))
        goto header_err;
      dec->have_id = 1;
      break;

    case 0x81:
      if (dec->have_comment) goto already_have;
      if (0 > (r = th_decode_headerin (dec->info, dec->comment, &(dec->setup), &p)))
        goto header_err;
      dec->have_comment = 1;
      break;

    case 0x82:
      if (dec->have_type) goto already_have;
      if (0 > (r = th_decode_headerin (dec->info, dec->comment, &(dec->setup), &p)))
        goto header_err;
      dec->have_type = 1;
      break;
      
    default:
      printf ("unknown header packet found, returning early.\n");
      return 1;
  }

  if (theoradec_ready_for_data (dec)) {
    dec->ctx = th_decode_alloc (dec->info, dec->setup);
    if (dec->ctx == NULL) goto setup_ctx_err;
    if (!(video_init (dec))) goto setup_SDL_err;
  }

  return 1;

header_err:
  doc (r);
  return 0;

already_have:
  printf ("header_in called with a packet whose type was already seen. Taking no action.\n");
  return 1;

setup_ctx_err:
  printf ("Error setting up decoding context.\n");
  return 0;

setup_SDL_err:
  printf ("Error setting up SDL components.\n");
  return 0;
}

int theoradec_data_in (TheoraDec *dec,
                       unsigned char *buffer, long buffer_length) {
  ogg_packet p;
  int r;
  ogg_int64_t gp;
  th_ycbcr_buffer yuv;

  if (!dec || !(dec->info) || !(dec->ctx)) return 0;
  
  p.packet = buffer;
  p.bytes = buffer_length;
  p.b_o_s = 0;
  p.e_o_s = 0;
  p.granulepos = -1;
  p.packetno = 0;

  if (0 == (r = th_decode_packetin (dec->ctx, &p, &gp))) {
    th_decode_ycbcr_out (dec->ctx, yuv);
    video_display (dec, yuv);
  } else {
    doc (r);
  }
  
  return r == 0 ? 1 : 0;
}

int video_init (TheoraDec *dec) {
  /* this function taken from example in libtheora-1.1 distribution */
  int w, h;

  w = (dec->info->pic_x + dec->info->frame_width + 1 & ~1) - (dec->info->pic_x & ~1);
  h = (dec->info->pic_y + dec->info->frame_height + 1 & ~1) - (dec->info->pic_y & ~1);

  dec->screen = SDL_SetVideoMode (w, h, 0, SDL_SWSURFACE);

  if (dec->screen == NULL) goto screen_init_err;

  if (dec->info->pixel_fmt == TH_PF_422)
    dec->yuv_overlay = SDL_CreateYUVOverlay (w, h, SDL_YUY2_OVERLAY, dec->screen);
  else
    dec->yuv_overlay = SDL_CreateYUVOverlay (w, h, SDL_YV12_OVERLAY, dec->screen);

  if (dec->yuv_overlay == NULL) goto overlay_init_err;

  dec->rect.x = dec->info->pic_x;
  dec->rect.y = dec->info->pic_y;
  dec->rect.w = w;
  dec->rect.h = h;

  /*SDL_DisplayYUVOverlay (dec->yuv_overlay, &(dec->rect));*/
  return 1;
  
screen_init_err:
  printf ("Error initializing SDL screen: %s\n", SDL_GetError());
  
overlay_init_err:
  printf ("Error initializing SDL overlay: %s\n", SDL_GetError());
  return 0;
}


void video_display (TheoraDec *dec, th_ycbcr_buffer yuv) {
  /* this function taken from example in libtheora-1.1 distribution */
  int i;
  int y_offset, uv_offset;
  
  /* Lock SDL_yuv_overlay */
  if (SDL_MUSTLOCK(dec->screen)) {
    if (0 > SDL_LockSurface(dec->screen)) return;
  }
  if (0 > SDL_LockYUVOverlay(dec->yuv_overlay)) return;
  
  /* let's draw the data on a SDL screen (*screen) */
  /* deal with border stride */
  /* reverse u and v for SDL */
  /* and crop input properly, respecting the encoded frame rect */
  /* problems may exist for odd frame rect for some encodings */

  printf ("buffer wxh: %dx%d\n", yuv[0].width, yuv[0].height);
  
  y_offset = (dec->info->pic_x & ~1) + yuv[0].stride * (dec->info->pic_y & ~1);

  if (dec->info->pixel_fmt == TH_PF_422) {
    uv_offset = (dec->info->pic_x/2) + (yuv[1].stride) * (dec->info->pic_y);
    /* SDL doesn't have a planar 4:2:2 */ 
    for (i = 0; i < dec->yuv_overlay->h; i++) {
      int j;
      char *in_y = (char *) yuv[0].data + y_offset + yuv[0].stride * i;
      char *out = (char *) (dec->yuv_overlay->pixels[0] + dec->yuv_overlay->pitches[0] * i);
      for (j = 0; j < dec->yuv_overlay->w; j++)
        out[j*2] = in_y[j];
      char *in_u = (char *) yuv[1].data + uv_offset + yuv[1].stride*i;
      char *in_v = (char *) yuv[2].data + uv_offset + yuv[2].stride*i;
      for (j = 0; j < dec->yuv_overlay->w >> 1; j++) {
        out[j*4+1] = in_u[j];
        out[j*4+3] = in_v[j];
      }
    }
  } else {
    uv_offset = (dec->info->pic_x/2) + (yuv[1].stride) * (dec->info->pic_y/2);
    printf ("executing I420 display, y off = %d, uv off = %d\n", y_offset, uv_offset);
    for (i = 0; i < dec->yuv_overlay->h; i++) {
      memcpy (dec->yuv_overlay->pixels[0] + dec->yuv_overlay->pitches[0] * i,
              yuv[0].data + y_offset+yuv[0].stride * i,
              dec->yuv_overlay->w);
    }
    for (i = 0; i < dec->yuv_overlay->h/2; i++) {
      memcpy (dec->yuv_overlay->pixels[1] + dec->yuv_overlay->pitches[1] * i,
              yuv[2].data + uv_offset + yuv[2].stride * i,
              dec->yuv_overlay->w/2);
      memcpy (dec->yuv_overlay->pixels[2] + dec->yuv_overlay->pitches[2] * i,
              yuv[1].data + uv_offset+yuv[1].stride * i,
              dec->yuv_overlay->w/2);
    }

    printf ("strides: %d, %d, %d\n", yuv[0].stride, yuv[1].stride, yuv[2].stride);
  }
  
  if (SDL_MUSTLOCK(dec->screen)) SDL_UnlockSurface(dec->screen);
  SDL_UnlockYUVOverlay(dec->yuv_overlay);
  SDL_DisplayYUVOverlay(dec->yuv_overlay, &(dec->rect));
}
