#error This file is no longer used.

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
 *      Standard driver helpers for Linux Allegro.
 *
 *      By Marek Habersack, mangled by George Foot.
 *
 *      See readme.txt for copyright information.
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "allegro5/allegro5.h"
#include "allegro5/internal/aintern.h"
#include "allegro5/platform/aintunix.h"
#include "allegro5/linalleg.h"

#include <unistd.h>


/* List of standard drivers */
STD_DRIVER *__al_linux_std_drivers[N_STD_DRIVERS];

static int std_drivers_suspended = false;


/* __al_linux_add_standard_driver:
 *  Adds a standard driver; returns 0 on success, non-zero if the sanity 
 *  checks fail.
 */
int __al_linux_add_standard_driver (STD_DRIVER *spec)
{
   if (!spec) return 1;
   if (spec->type >= N_STD_DRIVERS) return 2;
   if (!spec->update) return 3;
   if (spec->fd < 0) return 4;

   __al_linux_std_drivers[spec->type] = spec;

   spec->resume();

   return 0;
}

/* __al_linux_remove_standard_driver:
 *  Removes a standard driver, returning 0 on success or non-zero on
 *  failure.
 */
int __al_linux_remove_standard_driver (STD_DRIVER *spec)
{
   if (!spec) return 1;
   if (spec->type >= N_STD_DRIVERS) return 3;
   if (!__al_linux_std_drivers[spec->type]) return 4; 
   if (__al_linux_std_drivers[spec->type] != spec) return 5;

   spec->suspend();
   
   __al_linux_std_drivers[spec->type] = NULL;

   return 0;
}


/* __al_linux_update_standard_drivers:
 *  Updates all drivers.
 */
void __al_linux_update_standard_drivers (int threaded)
{
   int i;
   if (!std_drivers_suspended) {
      for (i = 0; i < N_STD_DRIVERS; i++)
	 if (__al_linux_std_drivers[i])
	    __al_linux_std_drivers[i]->update();
   }
}


/* __al_linux_suspend_standard_drivers:
 *  Temporary disable standard drivers during a VT switch.
 */
void __al_linux_suspend_standard_drivers (void)
{
   ASSERT(!std_drivers_suspended);
   std_drivers_suspended = true;
}

/* __al_linux_resume_standard_drivers:
 *  Re-enable standard drivers after a VT switch.
 */
void __al_linux_resume_standard_drivers (void)
{
   ASSERT(std_drivers_suspended);
   std_drivers_suspended = false;
}

