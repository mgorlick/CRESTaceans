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
 *      Assorted globals and setup/cleanup routines.
 *
 *      By Shawn Hargreaves.
 *
 *      See readme.txt for copyright information.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allegro5/allegro5.h"
#include "allegro5/internal/aintern.h"
#include "allegro5/internal/aintern_dtor.h"
#include "allegro5/internal/aintern_system.h"
#include "allegro5/platform/alplatf.h"
#include ALLEGRO_INTERNAL_HEADER


/* debugging stuff */
static int debug_trace_virgin = true;
static FILE *trace_file = NULL;
static _AL_MUTEX trace_mutex = _AL_MUTEX_UNINITED;

int (*_al_trace_handler)(const char *msg) = NULL;


typedef struct DEBUG_INFO
{
   /* 0: debug, 1: info, 2: warn, 3: error */
   int level;
   /* 1: line number, 2: function name, 4: timestamp */
   int flags;
   /* List of channels to log. NULL to log all channels. */
   _AL_VECTOR channels;
   _AL_VECTOR excluded;
   /* Whether settings have been read from allegro5.cfg or not. */
   bool configured;
} DEBUG_INFO;

DEBUG_INFO _al_debug_info =
{
   0,
   7,
   _AL_VECTOR_INITIALIZER(ALLEGRO_USTR *),
   _AL_VECTOR_INITIALIZER(ALLEGRO_USTR *),
   false
};


/* dynamic registration system for cleanup code */
struct al_exit_func {
   void (*funcptr)(void);
   const char *desc;
   struct al_exit_func *next;
};

static struct al_exit_func *exit_func_list = NULL;



/* _al_add_exit_func:
 *  Adds a function to the list that need to be called on Allegro shutdown.
 *  `desc' should point to a statically allocated string to help with
 *  debugging.
 */
void _al_add_exit_func(void (*func)(void), const char *desc)
{
   struct al_exit_func *n;

   for (n = exit_func_list; n; n = n->next)
      if (n->funcptr == func)
	 return;

   n = al_malloc(sizeof(struct al_exit_func));
   if (!n)
      return;

   n->next = exit_func_list;
   n->funcptr = func;
   n->desc = desc;
   exit_func_list = n;
}



/* _al_remove_exit_func:
 *  Removes a function from the list that need to be called on Allegro
 *  shutdown.
 */
void _al_remove_exit_func(void (*func)(void))
{
   struct al_exit_func *iter = exit_func_list, *prev = NULL;

   while (iter) {
      if (iter->funcptr == func) {
	 if (prev)
	    prev->next = iter->next;
	 else
	    exit_func_list = iter->next;
	 al_free(iter);
	 return;
      }
      prev = iter;
      iter = iter->next;
   }
}



/* _al_run_exit_funcs:
 *  Run all the functions registered with _al_add_exit_func, in reverse order of
 *  registration.
 */
void _al_run_exit_funcs(void)
{
   while (exit_func_list) {
      void (*func)(void) = exit_func_list->funcptr;
      _al_remove_exit_func(func);
      (*func)();
   }
}



static void delete_string_list(_AL_VECTOR *v)
{
   while (_al_vector_is_nonempty(v)) {
      int i = _al_vector_size(v) - 1;
      ALLEGRO_USTR **iter = _al_vector_ref(v, i);
      al_ustr_free(*iter);
      _al_vector_delete_at(v, i);
   }
   _al_vector_free(v);
}



static void configure_logging(void)
{
   ALLEGRO_CONFIG *config;
   char const *v;
   bool got_all = false;

   /* Messages logged before the system driver and allegro5.cfg are
    * up will always use defaults - but usually nothing is logged
    * before al_init.
    */
   config = al_get_system_config();
   if (!config)
      return;

   v = al_get_config_value(config, "trace", "channels");
   if (v) {
      ALLEGRO_USTR_INFO uinfo;
      ALLEGRO_USTR *u = al_ref_cstr(&uinfo, v);
      int pos = 0;

      while (pos >= 0) {
         int comma = al_ustr_find_chr(u, pos, ',');
         int first;
         ALLEGRO_USTR *u2, **iter;
         if (comma == -1)
            u2 = al_ustr_dup_substr(u, pos, al_ustr_length(u));
         else
            u2 = al_ustr_dup_substr(u, pos, comma);
         al_ustr_trim_ws(u2);
         first = al_ustr_get(u2, 0);

         if (first == '-') {
            al_ustr_remove_chr(u2, 0);
            iter = _al_vector_alloc_back(&_al_debug_info.excluded);
            *iter = u2;
         }
         else {
            if (first == '+')
               al_ustr_remove_chr(u2, 0);
            iter = _al_vector_alloc_back(&_al_debug_info.channels);
            *iter = u2;
            if (!strcmp(al_cstr(u2), "all"))
               got_all = true;
         }
         pos = comma;
         al_ustr_get_next(u, &pos);
      }

      if (got_all)
         delete_string_list(&_al_debug_info.channels);
   }

   v = al_get_config_value(config, "trace", "level");
   if (v) {
      if (!strcmp(v, "error")) _al_debug_info.level = 3;
      else if (!strcmp(v, "warn")) _al_debug_info.level = 2;
      else if (!strcmp(v, "info")) _al_debug_info.level = 1;
   }

   v = al_get_config_value(config, "trace", "timestamps");
   if (!v || strcmp(v, "0"))
      _al_debug_info.flags |= 4;
   else
      _al_debug_info.flags &= ~4;

   v = al_get_config_value(config, "trace", "functions");
   if (!v || strcmp(v, "0"))
      _al_debug_info.flags |= 2;
   else
      _al_debug_info.flags &= ~2;

   v = al_get_config_value(config, "trace", "lines");
   if (!v || strcmp(v, "0"))
      _al_debug_info.flags |= 1;
   else
      _al_debug_info.flags &= ~1;

   _al_mutex_init(&trace_mutex);

   _al_debug_info.configured = true;
}



bool _al_trace_prefix(char const *channel, int level,
   char const *file, int line, char const *function)
{
   size_t i;
   char *name;
   _AL_VECTOR const *v;

   /* XXX logging should be reconfigured if the system driver is reinstalled */
   if (!_al_debug_info.configured) {
      configure_logging();
   }

   if (level < _al_debug_info.level)
      return false;

   v = &_al_debug_info.channels;
   if (_al_vector_is_empty(v))
      goto channel_included;

   for (i = 0; i < _al_vector_size(v); i++) {
      ALLEGRO_USTR **iter = _al_vector_ref(v, i);
      if (!strcmp(al_cstr(*iter), channel))
         goto channel_included;
   }

   return false;

channel_included:

   v = &_al_debug_info.excluded;
   if (_al_vector_is_nonempty(v)) {
      for (i = 0; i < _al_vector_size(v); i++) {
         ALLEGRO_USTR **iter = _al_vector_ref(v, i);
         if (!strcmp(al_cstr(*iter), channel))
            return false;
      }
   }

   /* Avoid interleaved output from different threads. */
   _al_mutex_lock(&trace_mutex);

   al_trace("%-8s ", channel);
   if (level == 0) al_trace("D ");
   if (level == 1) al_trace("I ");
   if (level == 2) al_trace("W ");
   if (level == 3) al_trace("E ");

   name = strrchr(file, '/');
   if (_al_debug_info.flags & 1) {
      al_trace("%20s:%-4d ", name ? name + 1 : file, line);
   }
   if (_al_debug_info.flags & 2) {
      al_trace("%-32s ", function);
   }
   if (_al_debug_info.flags & 4) {
      double t = al_current_time();
      /* Kludge:
       * Very high timers (more than a year?) likely mean the timer
       * subsystem isn't initialized yet, so print 0.
       */
      if (t > 3600 * 24 * 365)
         t = 0;
      al_trace("[%10.5f] ", t);
   }

   _al_mutex_unlock(&trace_mutex);

   return true;
}



/* al_trace:
 *  Outputs a trace message (uses ASCII strings).
 */
void al_trace(const char *msg, ...)
{
   int olderr = errno;
   char buf[512];
   char *s;

   /* todo, some day: use vsnprintf (C99) */
   va_list ap;
   va_start(ap, msg);
   vsprintf(buf, msg, ap);
   va_end(ap);

   if (_al_trace_handler) {
      if (_al_trace_handler(buf))
	 return;
   }

   if (debug_trace_virgin) {
      s = getenv("ALLEGRO_TRACE");

      if (s)
         trace_file = fopen(s, "w");
      else
#ifdef ALLEGRO_IPHONE
         // Remember, we have no (accessible) filesystem on (not jailbroken)
         // iphone.
         // stderr will be redirected to xcode's debug console though, so
         // it's as good to use as the NSLog stuff.
         trace_file = stderr;
#else
         trace_file = fopen("allegro.log", "w");
#endif

      debug_trace_virgin = false;
   }

   if (trace_file) {
      fwrite(buf, sizeof(char), strlen(buf), trace_file);
      fflush(trace_file);
   }

   errno = olderr;
}



/* al_register_trace_handler:
 *  Installs a user handler for trace output.
 */
void al_register_trace_handler(int (*handler)(const char *msg))
{
   _al_trace_handler = handler;
}



/* Function: al_get_allegro_version
 */
uint32_t al_get_allegro_version(void)
{
   return ALLEGRO_VERSION_INT;
}



/* Function: al_run_main
 */
int al_run_main(int argc, char **argv, int (*user_main)(int, char **))
{
#ifdef ALLEGRO_MACOSX
    return _al_osx_run_main(argc, argv, user_main);
#else
    return user_main(argc, argv);
#endif
}


/* vim: set sts=3 sw=3 et: */
