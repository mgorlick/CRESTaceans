/**
 * Originally digi.c from allegro wiki
 * Original authors: KC/Milan
 *
 * Converted to allegro5 by Ryan Dickie
 */

/* Title: Mixer functions
 */

#include <math.h>
#include <stdio.h>

#include "allegro5/allegro_audio.h"
#include "allegro5/internal/aintern_audio.h"
#include "allegro5/internal/aintern_audio_cfg.h"

ALLEGRO_DEBUG_CHANNEL("audio")


/* globals */
static union {
   float f32[ALLEGRO_MAX_CHANNELS]; /* max: 7.1 */
   int16_t s16[ALLEGRO_MAX_CHANNELS];
   void *ptr;
} _samp_buf;



static void maybe_lock_mutex(ALLEGRO_MUTEX *mutex)
{
   if (mutex) {
      al_lock_mutex(mutex);
   }
}


static void maybe_unlock_mutex(ALLEGRO_MUTEX *mutex)
{
   if (mutex) {
      al_unlock_mutex(mutex);
   }
}


/* _al_rechannel_matrix:
 *  This function provides a (temporary!) matrix that can be used to convert
 *  one channel configuration into another.
 *
 *  Returns a pointer to a statically allocated array.
 */
static float *_al_rechannel_matrix(ALLEGRO_CHANNEL_CONF orig,
   ALLEGRO_CHANNEL_CONF target, float gain, float pan)
{
   /* Max 7.1 (8 channels) for input and output */
   static float mat[ALLEGRO_MAX_CHANNELS][ALLEGRO_MAX_CHANNELS];

   size_t dst_chans = al_get_channel_count(target);
   size_t src_chans = al_get_channel_count(orig);
   size_t i, j;

   /* Start with a simple identity matrix */
   memset(mat, 0, sizeof(mat));
   for (i = 0; i < src_chans && i < dst_chans; i++) {
      mat[i][i] = 1.0;
   }

   /* Multi-channel -> mono conversion (cuts rear/side channels) */
   if (dst_chans == 1 && (orig>>4) > 1) {
      for (i = 0; i < 2; i++) {
         mat[0][i] = 1.0 / sqrt(2.0);
      }

      /* If the source has a center channel, make sure that's copied 1:1
       * (perhaps it should scale the overall output?)
       */
      if ((orig >> 4) & 1) {
         mat[0][(orig >> 4) - 1] = 1.0;
      }
   }
   /* Center (or mono) -> front l/r conversion */
   else if (((orig >> 4) & 1) && !((target >> 4) & 1)) {
      mat[0][(orig >> 4) - 1] = 1.0 / sqrt(2.0);
      mat[1][(orig >> 4) - 1] = 1.0 / sqrt(2.0);
   }

   /* Copy LFE */
   if ((orig >> 4) != (target >> 4) &&
      (orig & 0xF) && (target & 0xF))
   {
      mat[dst_chans-1][src_chans-1] = 1.0;
   }

   /* Apply panning, which is supposed to maintain a constant power level.
    * I took that to mean we want:
    *    sqrt(rgain^2 + lgain^2) = 1.0
    */
   if (pan != ALLEGRO_AUDIO_PAN_NONE) {
      float rgain = gain * sqrt(( pan + 1.0f) / 2.0f);
      float lgain = gain * sqrt((-pan + 1.0f) / 2.0f);

      /* I dunno what to do about >2 channels, so don't even try for now. */
      for (j = 0; j < src_chans; j++) {
         mat[0][j] *= lgain;
         mat[1][j] *= rgain;
      }
   }

   /* Apply gain */
   if (gain != 1.0f) {
      for (i = 0; i < dst_chans; i++) {
         for (j = 0; j < src_chans; j++) {
            mat[i][j] *= gain;
         }
      }
   }

#ifdef DEBUGMODE
   {
      char debug[1024];
      ALLEGRO_DEBUG("sample matrix:\n");
      for (i = 0; i < dst_chans; i++) {
         strcpy(debug, "");
         for (j = 0; j < src_chans; j++) {
            sprintf(debug + strlen(debug), " %f", mat[i][j]);
         }
         ALLEGRO_DEBUG("%s\n", debug);
      }
   }
#endif

   return &mat[0][0];
}


/* _al_kcm_mixer_rejig_sample_matrix:
 *  Recompute the mixing matrix for a sample attached to a mixer.
 *  The caller must be holding the mixer mutex.
 */
void _al_kcm_mixer_rejig_sample_matrix(ALLEGRO_MIXER *mixer,
   ALLEGRO_SAMPLE_INSTANCE *spl)
{
   float *mat;
   size_t dst_chans;
   size_t src_chans;
   size_t i, j;

   if (spl->matrix) {
      al_free(spl->matrix);
   }

   mat = _al_rechannel_matrix(spl->spl_data.chan_conf,
      mixer->ss.spl_data.chan_conf, spl->gain, spl->pan);

   dst_chans = al_get_channel_count(mixer->ss.spl_data.chan_conf);
   src_chans = al_get_channel_count(spl->spl_data.chan_conf);

   spl->matrix = al_calloc(1, src_chans * dst_chans * sizeof(float));

   for (i = 0; i < dst_chans; i++) {
      for (j = 0; j < src_chans; j++) {
         spl->matrix[i*src_chans + j] = mat[i*ALLEGRO_MAX_CHANNELS + j];
      }
   }
}


/* fix_looped_position:
 *  When a stream loops, this will fix up the position and anything else to
 *  allow it to safely continue playing as expected. Returns false if it
 *  should stop being mixed.
 */
static bool fix_looped_position(ALLEGRO_SAMPLE_INSTANCE *spl)
{
   bool is_empty;
   ALLEGRO_AUDIO_STREAM *stream;

   /* Looping! Should be mostly self-explanatory */
   switch (spl->loop) {
      case ALLEGRO_PLAYMODE_LOOP:
         if (spl->step > 0) {
            while (spl->pos >= spl->loop_end) {
               spl->pos -= (spl->loop_end - spl->loop_start);
            }
         }
         else if (spl->step < 0) {
            while (spl->pos < spl->loop_start) {
               spl->pos += (spl->loop_end - spl->loop_start);
            }
         }
         return true;

      case ALLEGRO_PLAYMODE_BIDIR:
         /* When doing bi-directional looping, you need to do a follow-up
          * check for the opposite direction if a loop occurred, otherwise
          * you could end up misplaced on small, high-step loops.
          */
         if (spl->step >= 0) {
         check_forward:
            if (spl->pos >= spl->loop_end) {
               spl->step = -spl->step;
               spl->pos = spl->loop_end - (spl->pos - spl->loop_end) - 1;
               goto check_backward;
            }
         }
         else {
         check_backward:
            if (spl->pos < spl->loop_start || spl->pos >= spl->loop_end) {
               spl->step = -spl->step;
               spl->pos = spl->loop_start + (spl->loop_start - spl->pos);
               goto check_forward;
            }
         }
         return true;

      case ALLEGRO_PLAYMODE_ONCE:
         if (spl->pos < spl->spl_data.len) {
            return true;
         }
         spl->pos = 0;
         spl->is_playing = false;
         return false;

      case _ALLEGRO_PLAYMODE_STREAM_ONCE:
      case _ALLEGRO_PLAYMODE_STREAM_ONEDIR:
         if (spl->pos < spl->spl_data.len) {
            return true;
         }
         stream = (ALLEGRO_AUDIO_STREAM *)spl;
         is_empty = !_al_kcm_refill_stream(stream);
         if (is_empty && stream->is_draining) {
            stream->spl.is_playing = false;
         }

         _al_kcm_emit_stream_events(stream);

         return !(is_empty);
   }

   ASSERT(false);
   return false;
}


/* Interpolate the next sample value of spl into the global _samp_buf,
 * for all channels.
 */
static INLINE const void *point_spl16(const ALLEGRO_SAMPLE_INSTANCE *spl,
   unsigned int maxc)
{
   any_buffer_t *buf = (any_buffer_t *) &spl->spl_data.buffer;
   int16_t *s = _samp_buf.s16;
   unsigned int i;

   for (i = 0; i < maxc; i++) {
      switch (spl->spl_data.depth) {
         case ALLEGRO_AUDIO_DEPTH_INT24:
            s[i] = buf->s24[(spl->pos>>MIXER_FRAC_SHIFT)*maxc + i]
	    	>> 9;
            break;
         case ALLEGRO_AUDIO_DEPTH_INT16:
            s[i] = buf->s16[(spl->pos>>MIXER_FRAC_SHIFT)*maxc + i];
            break;
         case ALLEGRO_AUDIO_DEPTH_INT8:
            s[i] = buf->s8[(spl->pos>>MIXER_FRAC_SHIFT)*maxc + i]
	    	<< 7;
            break;
         case ALLEGRO_AUDIO_DEPTH_FLOAT32:
            s[i] = buf->f32[(spl->pos>>MIXER_FRAC_SHIFT)*maxc + i]
	    	* 0x7FFF;
            break;

         case ALLEGRO_AUDIO_DEPTH_UINT8:
         case ALLEGRO_AUDIO_DEPTH_UINT16:
         case ALLEGRO_AUDIO_DEPTH_UINT24:
            ASSERT(false);
            break;
      }
   }
   return s;
}


/* Interpolate the next sample value of spl into the global _samp_buf,
 * for all channels.
 */
static INLINE const void *point_spl32(const ALLEGRO_SAMPLE_INSTANCE *spl,
   unsigned int maxc)
{
   any_buffer_t *buf = (any_buffer_t *) &spl->spl_data.buffer;
   float *s = _samp_buf.f32;
   unsigned int i;

   for (i = 0; i < maxc; i++) {
      switch (spl->spl_data.depth) {
         case ALLEGRO_AUDIO_DEPTH_INT24:
            s[i] = (float) buf->s24[(spl->pos>>MIXER_FRAC_SHIFT)*maxc + i]
               / ((float)0x7FFFFF + 0.5f);
            break;
         case ALLEGRO_AUDIO_DEPTH_INT16:
            s[i] = (float) buf->s16[(spl->pos>>MIXER_FRAC_SHIFT)*maxc + i]
               / ((float)0x7FFF + 0.5f);
            break;
         case ALLEGRO_AUDIO_DEPTH_INT8:
            s[i] = (float) buf->s8[(spl->pos>>MIXER_FRAC_SHIFT)*maxc + i]
               / ((float)0x7F + 0.5f);
            break;
         case ALLEGRO_AUDIO_DEPTH_FLOAT32:
            s[i] = buf->f32[(spl->pos>>MIXER_FRAC_SHIFT)*maxc + i];
            break;

         case ALLEGRO_AUDIO_DEPTH_UINT8:
         case ALLEGRO_AUDIO_DEPTH_UINT16:
         case ALLEGRO_AUDIO_DEPTH_UINT24:
            ASSERT(false);
            break;
      }
   }
   return s;
}


/* Interpolate the next sample value of spl into the global _samp_buf,
 * for all channels.
 */
static INLINE const void *point_spl32u(const ALLEGRO_SAMPLE_INSTANCE *spl,
   unsigned int maxc)
{
   any_buffer_t *buf = (any_buffer_t *) &spl->spl_data.buffer;
   float *s = _samp_buf.f32;
   unsigned int i;

   for (i = 0; i < maxc; i++) {
      switch (spl->spl_data.depth) {
         case ALLEGRO_AUDIO_DEPTH_UINT24:
            s[i] = (float)buf->u24[(spl->pos>>MIXER_FRAC_SHIFT)*maxc + i]
               / ((float)0x7FFFFF+0.5f) - 1.0;
            break;
         case ALLEGRO_AUDIO_DEPTH_UINT16:
            s[i] = (float)buf->u16[(spl->pos>>MIXER_FRAC_SHIFT)*maxc + i]
               / ((float)0x7FFF+0.5f) - 1.0;
            break;
         case ALLEGRO_AUDIO_DEPTH_UINT8:
            s[i] = (float)buf->u8[(spl->pos>>MIXER_FRAC_SHIFT)*maxc + i]
               / ((float)0x7F+0.5f) - 1.0;
            break;
         case ALLEGRO_AUDIO_DEPTH_FLOAT32:
            s[i] = buf->f32[(spl->pos>>MIXER_FRAC_SHIFT)*maxc + i] - 1.0;
            break;

         case ALLEGRO_AUDIO_DEPTH_INT8:
         case ALLEGRO_AUDIO_DEPTH_INT16:
         case ALLEGRO_AUDIO_DEPTH_INT24:
            ASSERT(false);
            break;
      }
   }
   return s;
}


/* Interpolate the next sample value of spl into the global _samp_buf,
 * for all channels.
 */
static INLINE const void *linear_spl32(const ALLEGRO_SAMPLE_INSTANCE *spl,
   unsigned int maxc)
{
   unsigned long p1, p2;
   unsigned int i;
   float frac, *s = _samp_buf.f32;

   p1 = (spl->pos>>MIXER_FRAC_SHIFT)*maxc;
   p2 = p1 + maxc;

   switch (spl->loop) {
      case ALLEGRO_PLAYMODE_ONCE:
      case _ALLEGRO_PLAYMODE_STREAM_ONCE:
      case _ALLEGRO_PLAYMODE_STREAM_ONEDIR:
         if (spl->pos+MIXER_FRAC_ONE >= spl->spl_data.len)
            p2 = p1;
         break;
      case ALLEGRO_PLAYMODE_LOOP:
         if (spl->pos+MIXER_FRAC_ONE >= spl->loop_end)
            p2 = (spl->loop_start>>MIXER_FRAC_SHIFT)*maxc;
         break;
      case ALLEGRO_PLAYMODE_BIDIR:
         if (spl->pos+MIXER_FRAC_ONE >= spl->loop_end)
            p2 = ((spl->loop_end>>MIXER_FRAC_SHIFT)-1)*maxc;
         break;
   }

   frac = (float)(spl->pos & MIXER_FRAC_MASK) / (float)MIXER_FRAC_ONE;

   for (i = 0; i < maxc; i++) {
      float s1 = 0, s2 = 0;

      switch (spl->spl_data.depth) {
         case ALLEGRO_AUDIO_DEPTH_INT24:
            s1 = (float)spl->spl_data.buffer.s24[p1 + i] / ((float)0x7FFFFF + 0.5f);
            s2 = (float)spl->spl_data.buffer.s24[p2 + i] / ((float)0x7FFFFF + 0.5f);
            break;
         case ALLEGRO_AUDIO_DEPTH_INT16:
            s1 = (float)spl->spl_data.buffer.s16[p1 + i] / ((float)0x7FFF + 0.5f);
            s2 = (float)spl->spl_data.buffer.s16[p2 + i] / ((float)0x7FFF + 0.5f);
            break;
         case ALLEGRO_AUDIO_DEPTH_INT8:
            s1 = (float)spl->spl_data.buffer.s8[p1 + i] / ((float)0x7F + 0.5f);
            s2 = (float)spl->spl_data.buffer.s8[p2 + i] / ((float)0x7F + 0.5f);
            break;
         case ALLEGRO_AUDIO_DEPTH_FLOAT32:
            s1 = (float)spl->spl_data.buffer.f32[p1 + i];
            s2 = (float)spl->spl_data.buffer.f32[p2 + i];
            break;

         case ALLEGRO_AUDIO_DEPTH_UINT8:
         case ALLEGRO_AUDIO_DEPTH_UINT16:
         case ALLEGRO_AUDIO_DEPTH_UINT24:
            ASSERT(false);
            break;
      }

      s[i] = (s1*(1.0f-frac)) + (s2*frac);
   }
   return s;
}


/* Interpolate the next sample value of spl into the global _samp_buf,
 * for all channels.
 */
static INLINE const void *linear_spl32u(const ALLEGRO_SAMPLE_INSTANCE *spl,
   unsigned int maxc)
{
   unsigned long p1, p2;
   unsigned int i;
   float frac, *s = _samp_buf.f32;

   p1 = (spl->pos>>MIXER_FRAC_SHIFT)*maxc;
   p2 = p1 + maxc;

   switch (spl->loop) {
      case ALLEGRO_PLAYMODE_ONCE:
      case _ALLEGRO_PLAYMODE_STREAM_ONCE:
      case _ALLEGRO_PLAYMODE_STREAM_ONEDIR:
         if (spl->pos+MIXER_FRAC_ONE >= spl->spl_data.len)
            p2 = p1;
         break;
      case ALLEGRO_PLAYMODE_LOOP:
         if (spl->pos+MIXER_FRAC_ONE >= spl->loop_end)
            p2 = (spl->loop_start>>MIXER_FRAC_SHIFT)*maxc;
         break;
      case ALLEGRO_PLAYMODE_BIDIR:
         if (spl->pos+MIXER_FRAC_ONE >= spl->loop_end)
            p2 = ((spl->loop_end>>MIXER_FRAC_SHIFT)-1)*maxc;
         break;
   }

   frac = (float)(spl->pos & MIXER_FRAC_MASK) / (float)MIXER_FRAC_ONE;

   for (i = 0; i < maxc; i++) {
      float s1 = 0, s2 = 0;

      switch (spl->spl_data.depth) {
         case ALLEGRO_AUDIO_DEPTH_UINT24:
            s1 = (float)spl->spl_data.buffer.u24[p1 + i]/((float)0x7FFFFF + 0.5f) - 1.0f;
            s2 = (float)spl->spl_data.buffer.u24[p2 + i]/((float)0x7FFFFF + 0.5f) - 1.0f;
            break;
         case ALLEGRO_AUDIO_DEPTH_UINT16:
            s1 = (float)spl->spl_data.buffer.u16[p1 + i]/((float)0x7FFF + 0.5f) - 1.0f;
            s2 = (float)spl->spl_data.buffer.u16[p2 + i]/((float)0x7FFF + 0.5f) - 1.0f;
            break;
         case ALLEGRO_AUDIO_DEPTH_UINT8:
            s1 = (float)spl->spl_data.buffer.u8[p1 + i]/((float)0x7F + 0.5f) - 1.0f;
            s2 = (float)spl->spl_data.buffer.u8[p2 + i]/((float)0x7F + 0.5f) - 1.0f;
            break;
         case ALLEGRO_AUDIO_DEPTH_FLOAT32:
            s1 = (float)spl->spl_data.buffer.f32[p1 + i] - 1.0f;
            s2 = (float)spl->spl_data.buffer.f32[p2 + i] - 1.0f;
            break;

         case ALLEGRO_AUDIO_DEPTH_INT8:
         case ALLEGRO_AUDIO_DEPTH_INT16:
         case ALLEGRO_AUDIO_DEPTH_INT24:
            ASSERT(false);
            break;
      }

      s[i] = (s1*(1.0f-frac)) + (s2*frac);
   }
   return s;
}


static INLINE int32_t clamp(int32_t val, int32_t min, int32_t max)
{
   /* Clamp to min */
   val -= min;
   val &= (~val) >> 31;
   val += min;

   /* Clamp to max */
   val -= max;
   val &= val >> 31;
   val += max;

   return val;
}


/* Mix as many sample values as possible from the source sample into a mixer
 * buffer.  Implements stream_reader_t.
 *
 * TYPE is the type of the sample values in the mixer buffer, and
 * NEXT_SAMPLE_VALUE must return a buffer of the same type.
 */
#define MAKE_MIXER(NAME, NEXT_SAMPLE_VALUE, TYPE)                             \
static void NAME(void *source, void **vbuf, unsigned int *samples,            \
   ALLEGRO_AUDIO_DEPTH buffer_depth, size_t dest_maxc)                        \
{                                                                             \
   ALLEGRO_SAMPLE_INSTANCE *spl = (ALLEGRO_SAMPLE_INSTANCE *)source;          \
   TYPE *buf = *vbuf;                                                         \
   size_t maxc = al_get_channel_count(spl->spl_data.chan_conf);               \
   size_t samples_l = *samples;                                               \
   size_t c;                                                                  \
                                                                              \
   if (!spl->is_playing)                                                      \
      return;                                                                 \
                                                                              \
   while (samples_l > 0) {                                                    \
      const TYPE *s;                                                          \
                                                                              \
      if (!fix_looped_position(spl))                                          \
         return;                                                              \
                                                                              \
      /* It might be worth preparing multiple sample values at once. */       \
      s = (TYPE *) NEXT_SAMPLE_VALUE(spl, maxc);                              \
                                                                              \
      for (c = 0; c < dest_maxc; c++) {                                       \
         size_t i;                                                            \
         for (i = 0; i < maxc; i++) {                                         \
            *buf += s[i] * spl->matrix[c*maxc + i];                           \
         }                                                                    \
         buf++;                                                               \
      }                                                                       \
                                                                              \
      spl->pos += spl->step;                                                  \
      samples_l--;                                                            \
   }                                                                          \
   fix_looped_position(spl);                                                  \
   (void)buffer_depth;                                                        \
}

MAKE_MIXER(read_to_mixer_point_float_32, point_spl32, float)
MAKE_MIXER(read_to_mixer_point_float_32u, point_spl32u, float)
MAKE_MIXER(read_to_mixer_linear_float_32, linear_spl32, float)
MAKE_MIXER(read_to_mixer_linear_float_32u, linear_spl32u, float)
MAKE_MIXER(read_to_mixer_point_int16_t_16, point_spl16, int16_t)

#undef MAKE_MIXER


/* _al_kcm_mixer_read:
 *  Mixes the streams attached to the mixer and writes additively to the
 *  specified buffer (or if *buf is NULL, indicating a voice, convert it and
 *  set it to the buffer pointer).
 */
void _al_kcm_mixer_read(void *source, void **buf, unsigned int *samples,
   ALLEGRO_AUDIO_DEPTH buffer_depth, size_t dest_maxc)
{
   const ALLEGRO_MIXER *mixer;
   ALLEGRO_MIXER *m = (ALLEGRO_MIXER *)source;
   int maxc = al_get_channel_count(m->ss.spl_data.chan_conf);
   unsigned long samples_l = *samples;
   int i;

   if (!m->ss.is_playing)
      return;

   /* Make sure the mixer buffer is big enough. */
   if (m->ss.spl_data.len*maxc < samples_l*maxc) {
      al_free(m->ss.spl_data.buffer.ptr);
      m->ss.spl_data.buffer.ptr = al_malloc(samples_l*maxc*al_get_audio_depth_size(m->ss.spl_data.depth));
      if (!m->ss.spl_data.buffer.ptr) {
         _al_set_error(ALLEGRO_GENERIC_ERROR,
            "Out of memory allocating mixer buffer");
         m->ss.spl_data.len = 0;
         return;
      }
      m->ss.spl_data.len = samples_l;
   }

   mixer = m;

   /* Clear the buffer to silence. */
   memset(mixer->ss.spl_data.buffer.ptr, 0, samples_l * maxc * al_get_audio_depth_size(mixer->ss.spl_data.depth));

   /* Mix the streams into the mixer buffer. */
   for (i = _al_vector_size(&mixer->streams) - 1; i >= 0; i--) {
      ALLEGRO_SAMPLE_INSTANCE **slot = _al_vector_ref(&mixer->streams, i);
      ALLEGRO_SAMPLE_INSTANCE *spl = *slot;
      spl->spl_read(spl, (void **) &mixer->ss.spl_data.buffer.ptr, samples,
         m->ss.spl_data.depth, maxc);
   }

   /* Call the post-processing callback. */
   if (mixer->postprocess_callback) {
      mixer->postprocess_callback(mixer->ss.spl_data.buffer.ptr,
         *samples, mixer->pp_callback_userdata);
   }

   samples_l *= maxc;

   /* Feeding to a non-voice.
    * Currently we only support mixers of the same audio depth doing this.
    */
   if (*buf) {
      switch (m->ss.spl_data.depth) {
         case ALLEGRO_AUDIO_DEPTH_FLOAT32: {
            /* We don't need to clamp in the mixer yet. */
            float *lbuf = *buf;
            float *src = mixer->ss.spl_data.buffer.f32;
            while (samples_l-- > 0) {
               *lbuf += *src;
               lbuf++;
               src++;
            }
            break;

         case ALLEGRO_AUDIO_DEPTH_INT16: {
            int16_t *lbuf = *buf;
            int16_t *src = mixer->ss.spl_data.buffer.s16;
            while (samples_l-- > 0) {
               int32_t x = *lbuf + *src;
               if (x < -32768)
                  x = -32768;
               else if (x > 32767)
                  x = 32767;
               *lbuf = (int16_t)x;
               lbuf++;
               src++;
            }
            break;
         }

         case ALLEGRO_AUDIO_DEPTH_INT8:
         case ALLEGRO_AUDIO_DEPTH_INT24:
         case ALLEGRO_AUDIO_DEPTH_UINT8:
         case ALLEGRO_AUDIO_DEPTH_UINT16:
         case ALLEGRO_AUDIO_DEPTH_UINT24:
            /* Unsupported mixer depths. */
            ASSERT(false);
            break;
         }
      }
      return;
   }

   /* We're feeding to a voice, so we pass it back the mixed data (make sure
    * to clamp and convert it).
    */
   *buf = mixer->ss.spl_data.buffer.ptr;
   switch (buffer_depth & ~ALLEGRO_AUDIO_DEPTH_UNSIGNED) {

      case ALLEGRO_AUDIO_DEPTH_FLOAT32:
         /* Do we need to clamp? */
         break;

      case ALLEGRO_AUDIO_DEPTH_INT24:
         switch (mixer->ss.spl_data.depth) {
            case ALLEGRO_AUDIO_DEPTH_FLOAT32: {
               int32_t off = ((buffer_depth & ALLEGRO_AUDIO_DEPTH_UNSIGNED)
                              ? 0x800000 : 0);
               int32_t *lbuf = mixer->ss.spl_data.buffer.s24;
               float *src = mixer->ss.spl_data.buffer.f32;

               while (samples_l > 0) {
                  *lbuf = clamp(*(src++) * ((float)0x7FFFFF + 0.5f),
                     ~0x7FFFFF, 0x7FFFFF);
                  *lbuf += off;
                  lbuf++;
                  samples_l--;
               }
               break;
            }

            case ALLEGRO_AUDIO_DEPTH_INT16:
               /* XXX not yet implemented */
               ASSERT(false);
               break;

            case ALLEGRO_AUDIO_DEPTH_INT8:
            case ALLEGRO_AUDIO_DEPTH_INT24:
            case ALLEGRO_AUDIO_DEPTH_UINT8:
            case ALLEGRO_AUDIO_DEPTH_UINT16:
            case ALLEGRO_AUDIO_DEPTH_UINT24:
               /* Unsupported mixer depths. */
               ASSERT(false);
               break;
         }
         break;

      case ALLEGRO_AUDIO_DEPTH_INT16:
         switch (mixer->ss.spl_data.depth) {
            case ALLEGRO_AUDIO_DEPTH_FLOAT32: {
               int16_t off = ((buffer_depth & ALLEGRO_AUDIO_DEPTH_UNSIGNED)
                              ? 0x8000 : 0);
               int16_t *lbuf = mixer->ss.spl_data.buffer.s16;
               float *src = mixer->ss.spl_data.buffer.f32;

               while (samples_l > 0) {
                  *lbuf = clamp(*(src++) * ((float)0x7FFF + 0.5f), ~0x7FFF, 0x7FFF);
                  *lbuf += off;
                  lbuf++;
                  samples_l--;
               }
               break;
            }

            case ALLEGRO_AUDIO_DEPTH_INT16:
               /* Handle signedness differences. */
               if (buffer_depth != ALLEGRO_AUDIO_DEPTH_INT16) {
                  int16_t *lbuf = mixer->ss.spl_data.buffer.s16;
                  while (samples_l > 0) {
                     *lbuf++ ^= 0x8000;
                     samples_l--;
                  }
               }
               break;

            case ALLEGRO_AUDIO_DEPTH_INT8:
            case ALLEGRO_AUDIO_DEPTH_INT24:
            case ALLEGRO_AUDIO_DEPTH_UINT8:
            case ALLEGRO_AUDIO_DEPTH_UINT16:
            case ALLEGRO_AUDIO_DEPTH_UINT24:
               /* Unsupported mixer depths. */
               ASSERT(false);
               break;
         }
         break;

      /* Ugh, do we really want to support 8-bit output? */
      case ALLEGRO_AUDIO_DEPTH_INT8:
         switch (mixer->ss.spl_data.depth) {
            case ALLEGRO_AUDIO_DEPTH_FLOAT32: {
               int8_t off = ((buffer_depth & ALLEGRO_AUDIO_DEPTH_UNSIGNED)
                              ? 0x80 : 0);
               int8_t *lbuf = mixer->ss.spl_data.buffer.s8;
               float *src = mixer->ss.spl_data.buffer.f32;

               while (samples_l > 0) {
                  *lbuf = clamp(*(src++) * ((float)0x7F + 0.5f), ~0x7F, 0x7F);
                  *lbuf += off;
                  lbuf++;
                  samples_l--;
               }
               break;
            }

            case ALLEGRO_AUDIO_DEPTH_INT16:
               /* XXX not yet implemented */
               ASSERT(false);
               break;

            case ALLEGRO_AUDIO_DEPTH_INT8:
            case ALLEGRO_AUDIO_DEPTH_INT24:
            case ALLEGRO_AUDIO_DEPTH_UINT8:
            case ALLEGRO_AUDIO_DEPTH_UINT16:
            case ALLEGRO_AUDIO_DEPTH_UINT24:
               /* Unsupported mixer depths. */
               ASSERT(false);
               break;
         }
         break;

      case ALLEGRO_AUDIO_DEPTH_UINT8:
      case ALLEGRO_AUDIO_DEPTH_UINT16:
      case ALLEGRO_AUDIO_DEPTH_UINT24:
         /* Impossible. */
         ASSERT(false);
         break;
   }

   (void)dest_maxc;
}


/* Function: al_create_mixer
 */
ALLEGRO_MIXER *al_create_mixer(unsigned int freq,
   ALLEGRO_AUDIO_DEPTH depth, ALLEGRO_CHANNEL_CONF chan_conf)
{
   ALLEGRO_MIXER *mixer;
   ALLEGRO_CONFIG *config;
   int default_mixer_quality = ALLEGRO_MIXER_QUALITY_LINEAR;

   /* XXX this is in the wrong place */
   config = al_get_system_config();
   if (config) {
      const char *p;
      p = al_get_config_value(config, "audio", "default_mixer_quality");
      if (p && p[0] != '\0') {
         if (!stricmp(p, "point"))
            default_mixer_quality = ALLEGRO_MIXER_QUALITY_POINT;
         else if (!stricmp(p, "linear"))
            default_mixer_quality = ALLEGRO_MIXER_QUALITY_LINEAR;
      }
   }

   if (!freq) {
      _al_set_error(ALLEGRO_INVALID_PARAM,
         "Attempted to create mixer with no frequency");
      return NULL;
   }

   if (depth != ALLEGRO_AUDIO_DEPTH_FLOAT32 &&
         depth != ALLEGRO_AUDIO_DEPTH_INT16) {
      _al_set_error(ALLEGRO_INVALID_PARAM, "Unsupported mixer depth");
      return NULL;
   }

   mixer = al_calloc(1, sizeof(ALLEGRO_MIXER));
   if (!mixer) {
      _al_set_error(ALLEGRO_GENERIC_ERROR,
         "Out of memory allocating mixer object");
      return NULL;
   }

   mixer->ss.is_playing = true;
   mixer->ss.spl_data.free_buf = true;

   mixer->ss.loop = ALLEGRO_PLAYMODE_ONCE;
   /* XXX should we have a specific loop mode? */
   mixer->ss.gain = 1.0f;
   mixer->ss.spl_data.depth     = depth;
   mixer->ss.spl_data.chan_conf = chan_conf;
   mixer->ss.spl_data.frequency = freq;

   mixer->ss.spl_read = _al_kcm_mixer_read;

   mixer->quality = default_mixer_quality;

   _al_vector_init(&mixer->streams, sizeof(ALLEGRO_SAMPLE_INSTANCE *));

   _al_kcm_register_destructor(mixer, (void (*)(void *)) al_destroy_mixer);

   return mixer;
}


/* Function: al_destroy_mixer
 */
void al_destroy_mixer(ALLEGRO_MIXER *mixer)
{
   if (mixer) {
      _al_kcm_unregister_destructor(mixer);
      _al_kcm_destroy_sample(&mixer->ss, false);
   }
}


/* This function is ALLEGRO_MIXER aware */
/* Function: al_attach_sample_instance_to_mixer
 */
bool al_attach_sample_instance_to_mixer(ALLEGRO_SAMPLE_INSTANCE *spl,
   ALLEGRO_MIXER *mixer)
{
   ALLEGRO_SAMPLE_INSTANCE **slot;

   ASSERT(mixer);
   ASSERT(spl);

   /* Already referenced, do not attach. */
   if (spl->parent.u.ptr) {
      _al_set_error(ALLEGRO_INVALID_OBJECT,
         "Attempted to attach a sample that's already attached");
      return false;
   }

   maybe_lock_mutex(mixer->ss.mutex);
   
   _al_kcm_stream_set_mutex(spl, mixer->ss.mutex);

   slot = _al_vector_alloc_back(&mixer->streams);
   if (!slot) {
      if (mixer->ss.mutex) {
         al_unlock_mutex(mixer->ss.mutex);
      }
      _al_set_error(ALLEGRO_GENERIC_ERROR,
         "Out of memory allocating attachment pointers");
      return false;
   }
   (*slot) = spl;

   spl->step = (spl->spl_data.frequency<<MIXER_FRAC_SHIFT) * spl->speed /
               mixer->ss.spl_data.frequency;
   /* Don't want to be trapped with a step value of 0. */
   if (spl->step == 0) {
      if (spl->speed > 0.0f)
         spl->step = 1;
      else
         spl->step = -1;
   }

   /* If this isn't a mixer, set the proper sample stream reader */
   if (spl->spl_read == NULL) {
      switch (mixer->ss.spl_data.depth) {

         case ALLEGRO_AUDIO_DEPTH_FLOAT32:
            switch (mixer->quality) {
               case ALLEGRO_MIXER_QUALITY_LINEAR:
                  if ((spl->spl_data.depth & ALLEGRO_AUDIO_DEPTH_UNSIGNED))
                     spl->spl_read = read_to_mixer_linear_float_32u;
                  else
                     spl->spl_read = read_to_mixer_linear_float_32;
                  break;

               case ALLEGRO_MIXER_QUALITY_POINT:
                  if (mixer->ss.spl_data.depth == ALLEGRO_AUDIO_DEPTH_FLOAT32) {
                     if ((spl->spl_data.depth & ALLEGRO_AUDIO_DEPTH_UNSIGNED))
                        spl->spl_read = read_to_mixer_point_float_32u;
                     else
                        spl->spl_read = read_to_mixer_point_float_32;
                  }
                  break;
            }
            break;

         case ALLEGRO_AUDIO_DEPTH_INT16:
            /* Linear interpolation not supported yet. */
            spl->spl_read = read_to_mixer_point_int16_t_16;
            break;

         case ALLEGRO_AUDIO_DEPTH_INT8:
         case ALLEGRO_AUDIO_DEPTH_INT24:
         case ALLEGRO_AUDIO_DEPTH_UINT8:
         case ALLEGRO_AUDIO_DEPTH_UINT16:
         case ALLEGRO_AUDIO_DEPTH_UINT24:
            /* Unsupported mixer depths. */
            ASSERT(false);
            break;
      }

      _al_kcm_mixer_rejig_sample_matrix(mixer, spl);
   }

   spl->parent.u.mixer = mixer;
   spl->parent.is_voice = false;

   maybe_unlock_mutex(mixer->ss.mutex);

   return true;
}


/* Function: al_attach_audio_stream_to_mixer
 */
bool al_attach_audio_stream_to_mixer(ALLEGRO_AUDIO_STREAM *stream, ALLEGRO_MIXER *mixer)
{
   ASSERT(mixer);
   ASSERT(stream);

   return al_attach_sample_instance_to_mixer(&stream->spl, mixer);
}


/* Function: al_attach_mixer_to_mixer
 */
bool al_attach_mixer_to_mixer(ALLEGRO_MIXER *stream, ALLEGRO_MIXER *mixer)
{
   ASSERT(mixer);
   ASSERT(stream);

   if (mixer->ss.spl_data.frequency != stream->ss.spl_data.frequency) {
      _al_set_error(ALLEGRO_INVALID_OBJECT,
         "Attempted to attach a mixer with different frequencies");
      return false;
   }

   if (mixer->ss.spl_data.depth != stream->ss.spl_data.depth) {
      _al_set_error(ALLEGRO_INVALID_OBJECT,
         "Mixers of different audio depths cannot be attached to one another");
      return false;
   }

   return al_attach_sample_instance_to_mixer(&stream->ss, mixer);
}


/* Function: al_set_mixer_postprocess_callback
 */
bool al_set_mixer_postprocess_callback(ALLEGRO_MIXER *mixer,
   void (*pp_callback)(void *buf, unsigned int samples, void *data),
   void *pp_callback_userdata)
{
   ASSERT(mixer);

   maybe_lock_mutex(mixer->ss.mutex);

   mixer->postprocess_callback = pp_callback;
   mixer->pp_callback_userdata = pp_callback_userdata;

   maybe_unlock_mutex(mixer->ss.mutex);

   return true;
}


/* Function: al_get_mixer_frequency
 */
unsigned int al_get_mixer_frequency(const ALLEGRO_MIXER *mixer)
{
   ASSERT(mixer);

   return mixer->ss.spl_data.frequency;
}


/* Function: al_get_mixer_channels
 */
ALLEGRO_CHANNEL_CONF al_get_mixer_channels(const ALLEGRO_MIXER *mixer)
{
   ASSERT(mixer);

   return mixer->ss.spl_data.chan_conf;
}


/* Function: al_get_mixer_depth
 */
ALLEGRO_AUDIO_DEPTH al_get_mixer_depth(const ALLEGRO_MIXER *mixer)
{
   ASSERT(mixer);

   return mixer->ss.spl_data.depth;
}


/* Function: al_get_mixer_quality
 */
ALLEGRO_MIXER_QUALITY al_get_mixer_quality(const ALLEGRO_MIXER *mixer)
{
   ASSERT(mixer);

   return mixer->quality;
}


/* Function: al_get_mixer_playing
 */
bool al_get_mixer_playing(const ALLEGRO_MIXER *mixer)
{
   ASSERT(mixer);

   return mixer->ss.is_playing;
}


/* Function: al_get_mixer_attached
 */
bool al_get_mixer_attached(const ALLEGRO_MIXER *mixer)
{
   ASSERT(mixer);

   return _al_vector_is_nonempty(&mixer->streams);
}


/* Function: al_set_mixer_frequency
 */
bool al_set_mixer_frequency(ALLEGRO_MIXER *mixer, unsigned int val)
{
   ASSERT(mixer);

   /* You can change the frequency of a mixer as long as it's not attached
    * to anything.
    */
   if (mixer->ss.parent.u.ptr) {
      _al_set_error(ALLEGRO_INVALID_OBJECT,
            "Attempted to change the frequency of an attached mixer");
      return false;
   }

   mixer->ss.spl_data.frequency = val;
   return true;
}


/* Function: al_set_mixer_quality
 */
bool al_set_mixer_quality(ALLEGRO_MIXER *mixer, ALLEGRO_MIXER_QUALITY new_quality)
{
   bool ret;
   ASSERT(mixer);

   maybe_lock_mutex(mixer->ss.mutex);

   if (mixer->quality == new_quality) {
      ret = true;
   }
   else if (_al_vector_size(&mixer->streams) == 0) {
      mixer->quality = new_quality;
      ret = true;
   }
   else {
      _al_set_error(ALLEGRO_INVALID_OBJECT,
         "Attempted to change the quality of a mixer with attachments");
      ret = false;
   }

   maybe_unlock_mutex(mixer->ss.mutex);

   return ret;
}


/* Function: al_set_mixer_playing
 */
bool al_set_mixer_playing(ALLEGRO_MIXER *mixer, bool val)
{
   ASSERT(mixer);

   mixer->ss.is_playing = val;
   return true;
}


/* Function: al_detach_mixer
 */
bool al_detach_mixer(ALLEGRO_MIXER *mixer)
{
   ASSERT(mixer);

   _al_kcm_detach_from_parent(&mixer->ss);
   return true;
}


/* vim: set sts=3 sw=3 et: */
