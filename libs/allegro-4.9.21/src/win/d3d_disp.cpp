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
 *      Direct3D display driver
 *
 *      By Trent Gamblin.
 *
 */

#include <windows.h>

#include <string.h>
#include <stdio.h>
#include <process.h>
#include <math.h>

#include "allegro5/allegro5.h"

#include "allegro5/system.h"
#include "allegro5/internal/aintern.h"
#include "allegro5/internal/aintern_bitmap.h"
#include "allegro5/internal/aintern_display.h"
#include "allegro5/internal/aintern_system.h"
#include "allegro5/internal/aintern_thread.h"
#include "allegro5/internal/aintern_vector.h"
#include "allegro5/platform/aintwin.h"

#include "d3d.h"

ALLEGRO_DEBUG_CHANNEL("d3d")


static ALLEGRO_DISPLAY_INTERFACE *vt = 0;

static LPDIRECT3D9 _al_d3d = 0;

static D3DPRESENT_PARAMETERS d3d_pp;

static float d3d_ortho_w;
static float d3d_ortho_h;

static HWND fullscreen_focus_window;
static bool ffw_set = false;

#ifdef ALLEGRO_CFG_D3D9EX
// Stuff dynamically loaded from dlls
typedef HRESULT (WINAPI *_dyn_create_type)(UINT, IDirect3D9Ex **);
static _dyn_create_type _dyn_create;
#endif


static bool d3d_can_wait_for_vsync;

static bool render_to_texture_supported = true;
static bool is_vista = false;
static int num_faux_fullscreen_windows = 0;
static bool already_fullscreen = false; /* real fullscreen */

static DWORD d3d_min_filter = D3DTEXF_POINT;
static DWORD d3d_mag_filter = D3DTEXF_POINT;

static ALLEGRO_MUTEX *present_mutex;
ALLEGRO_MUTEX *_al_d3d_lost_device_mutex;

/*
 * These parameters cannot be gotten by the display thread because
 * they're thread local. We get them in the calling thread first.
 */
typedef struct new_display_parameters {
   ALLEGRO_DISPLAY_D3D *display;
   volatile bool init_failed;
   HANDLE AckEvent;
} new_display_parameters;


static int allegro_formats[] = {
   ALLEGRO_PIXEL_FORMAT_ANY,
   ALLEGRO_PIXEL_FORMAT_ANY_WITH_ALPHA,
   ALLEGRO_PIXEL_FORMAT_ANY_NO_ALPHA,
   ALLEGRO_PIXEL_FORMAT_ANY_15_NO_ALPHA,
   ALLEGRO_PIXEL_FORMAT_ANY_16_WITH_ALPHA,
   ALLEGRO_PIXEL_FORMAT_ANY_16_NO_ALPHA,
   ALLEGRO_PIXEL_FORMAT_ANY_24_NO_ALPHA,
   ALLEGRO_PIXEL_FORMAT_ANY_32_WITH_ALPHA,
   ALLEGRO_PIXEL_FORMAT_ANY_32_NO_ALPHA,
   ALLEGRO_PIXEL_FORMAT_XRGB_8888,
   ALLEGRO_PIXEL_FORMAT_ARGB_8888,
   //ALLEGRO_PIXEL_FORMAT_ARGB_4444,  this format seems not to be allowed
   ALLEGRO_PIXEL_FORMAT_RGB_565,
   //ALLEGRO_PIXEL_FORMAT_ARGB_1555,  this format seems not to be allowed
   ALLEGRO_PIXEL_FORMAT_ABGR_F32,
   -1
};

/* Mapping of Allegro formats to D3D formats */
static int d3d_formats[] = {
   ALLEGRO_PIXEL_FORMAT_ANY,
   ALLEGRO_PIXEL_FORMAT_ANY_WITH_ALPHA,
   ALLEGRO_PIXEL_FORMAT_ANY_NO_ALPHA,
   ALLEGRO_PIXEL_FORMAT_ANY_15_NO_ALPHA,
   ALLEGRO_PIXEL_FORMAT_ANY_16_WITH_ALPHA,
   ALLEGRO_PIXEL_FORMAT_ANY_16_NO_ALPHA,
   ALLEGRO_PIXEL_FORMAT_ANY_24_NO_ALPHA,
   ALLEGRO_PIXEL_FORMAT_ANY_32_WITH_ALPHA,
   ALLEGRO_PIXEL_FORMAT_ANY_32_NO_ALPHA,
   D3DFMT_X8R8G8B8,
   D3DFMT_A8R8G8B8,
   //D3DFMT_A4R4G4B4,
   D3DFMT_R5G6B5,
   //D3DFMT_A1R5G5B5,
   D3DFMT_A32B32G32R32F,
   -1
};

static const int NUM_DISPLAY_FORMATS = 2;
static const int _16BIT_DS = 2; /* # 16 bit depth stencil formats */
static const int _32BIT_DS = 4; /* # 32 bit depth stencil formats */
static ALLEGRO_EXTRA_DISPLAY_SETTINGS **eds_list = NULL;
static int eds_list_count = 0;

/*
 * This is a list of all supported display modes. Some other information will be
 * filled in later like multisample type and samples. This is for display scoring.
 */
// +2 for friendly mode and d3d depth/stencil format
static int d3d_fmt_desc[][ALLEGRO_DISPLAY_OPTIONS_COUNT+2] =
{
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32,  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 11, 0 },
   //{ 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 32, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 11, D3DFMT_D32 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 11, D3DFMT_D24S8 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 11, D3DFMT_D24X8 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 4, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 11, D3DFMT_D24X4S4 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32,  0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 11, 0 },
   //{ 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 32, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 11, D3DFMT_D32 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 8, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 11, D3DFMT_D24S8 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 11, D3DFMT_D24X8 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 4, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 11, D3DFMT_D24X4S4 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32,  0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 11, 0 },
   //{ 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 32, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 11, D3DFMT_D32 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 8, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 11, D3DFMT_D24S8 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 11, D3DFMT_D24X8 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 4, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 11, D3DFMT_D24X4S4 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32,  0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 11, 0 },
   //{ 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 32, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 11, D3DFMT_D32 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 8, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 11, D3DFMT_D24S8 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 11, D3DFMT_D24X8 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 4, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 11, D3DFMT_D24X4S4 },
   //
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32,  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 11, 0 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 32, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 11, D3DFMT_D32 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 11, D3DFMT_D24S8 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 11, D3DFMT_D24X8 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 4, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 11, D3DFMT_D24X4S4 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32,  0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 11, 0 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 32, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 11, D3DFMT_D32 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 8, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 11, D3DFMT_D24S8 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 11, D3DFMT_D24X8 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 4, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 11, D3DFMT_D24X4S4 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32,  0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 11, 0 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 32, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 11, D3DFMT_D32 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 8, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 11, D3DFMT_D24S8 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 11, D3DFMT_D24X8 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 4, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 11, D3DFMT_D24X4S4 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32,  0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 11, 0 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 32, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 11, D3DFMT_D32 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 8, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 11, D3DFMT_D24S8 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 11, D3DFMT_D24X8 },
   { 8, 8, 8, 0, 16, 8, 0,  0, 0, 0, 0, 0, 0, 0, 32, 24, 4, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 11, D3DFMT_D24X4S4 },
   
   
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16,  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 14, 0 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 15, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 14, D3DFMT_D15S1 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 14, D3DFMT_D16 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 32, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 14, D3DFMT_D32 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 14, D3DFMT_D24S8 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 14, D3DFMT_D24X8 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 4, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 14, D3DFMT_D24X4S4 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16,  0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 14, 0 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 15, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 14, D3DFMT_D15S1 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 16, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 14, D3DFMT_D16 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 32, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 14, D3DFMT_D32 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 8, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 14, D3DFMT_D24S8 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 14, D3DFMT_D24X8 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 4, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 14, D3DFMT_D24X4S4 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16,  0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 14, 0 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 15, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 14, D3DFMT_D15S1 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 16, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 14, D3DFMT_D16 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 32, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 14, D3DFMT_D32 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 8, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 14, D3DFMT_D24S8 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 14, D3DFMT_D24X8 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 4, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 14, D3DFMT_D24X4S4 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16,  0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 14, 0 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 15, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 14, D3DFMT_D15S1 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 16, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 14, D3DFMT_D16 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 32, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 14, D3DFMT_D32 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 8, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 14, D3DFMT_D24S8 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 14, D3DFMT_D24X8 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 4, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 14, D3DFMT_D24X4S4 },
   //
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16,  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 14, 0 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 15, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 14, D3DFMT_D15S1 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 16, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 14, D3DFMT_D16 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 32, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 14, D3DFMT_D32 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 14, D3DFMT_D24S8 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 14, D3DFMT_D24X8 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 4, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 14, D3DFMT_D24X4S4 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16,  0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 14, 0 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 15, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 14, D3DFMT_D15S1 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 16, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 14, D3DFMT_D16 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 32, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 14, D3DFMT_D32 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 8, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 14, D3DFMT_D24S8 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 14, D3DFMT_D24X8 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 4, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 14, D3DFMT_D24X4S4 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16,  0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 14, 0 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 15, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 14, D3DFMT_D15S1 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 16, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 14, D3DFMT_D16 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 32, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 14, D3DFMT_D32 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 8, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 14, D3DFMT_D24S8 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 14, D3DFMT_D24X8 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 4, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 14, D3DFMT_D24X4S4 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16,  0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 14, 0 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 15, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 14, D3DFMT_D15S1 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 16, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 14, D3DFMT_D16 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 32, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 14, D3DFMT_D32 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 8, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 14, D3DFMT_D24S8 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 14, D3DFMT_D24X8 },
   { 5, 6, 5, 0, 11, 5, 0,  0, 0, 0, 0, 0, 0, 0, 16, 24, 4, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 14, D3DFMT_D24X4S4 }
};

static const int D3D_DISPLAY_COMBINATIONS = sizeof(d3d_fmt_desc) / sizeof(*d3d_fmt_desc);


static D3DFORMAT d3d_get_depth_stencil_format(ALLEGRO_EXTRA_DISPLAY_SETTINGS *settings)
{
   struct DEPTH_STENCIL_DESC {
      int d;
      int s;
      D3DFORMAT format;
   };

   DEPTH_STENCIL_DESC formats[] = {
      {  0, 0, (D3DFORMAT)0 },
      { 32, 0, D3DFMT_D32 },
      { 15, 1, D3DFMT_D15S1 },
      { 24, 8, D3DFMT_D24S8 },
      { 24, 0, D3DFMT_D24X8 },
      { 24, 4, D3DFMT_D24X4S4 },
      { 16, 0, D3DFMT_D16 },
      { -1, -1, (D3DFORMAT)0 }
   };


   for (int i = 0; formats[i].d >= 0; i++) {
      if (settings->settings[ALLEGRO_DEPTH_SIZE] == formats[i].d &&
            settings->settings[ALLEGRO_STENCIL_SIZE] == formats[i].s)
         return formats[i].format;
   }

   return (D3DFORMAT)0;
}


bool _al_d3d_supports_separate_alpha_blend(ALLEGRO_DISPLAY *display)
{
   ALLEGRO_DISPLAY_D3D *d3d_disp = (ALLEGRO_DISPLAY_D3D *)display;
   return d3d_disp->supports_separate_alpha_blend;
}

/* Function: al_have_d3d_non_pow2_texture_support
 */
bool al_have_d3d_non_pow2_texture_support(void)
{
   D3DCAPS9 caps;
   int adapter = al_get_new_display_adapter();
   if (adapter == -1)
         adapter = 0;

   /* This might have to change for multihead */
   if (_al_d3d->GetDeviceCaps(adapter, D3DDEVTYPE_HAL, &caps) != D3D_OK) {
      if (_al_d3d->GetDeviceCaps(adapter, D3DDEVTYPE_REF, &caps) != D3D_OK) {
         return false;
      }
   }

   if ((caps.TextureCaps & D3DPTEXTURECAPS_POW2) == 0) {
      return true;
   }

   return false;
}

/* Function: al_have_d3d_non_square_texture_support
 */
bool al_have_d3d_non_square_texture_support(void)
{
   D3DCAPS9 caps;
   int adapter = al_get_new_display_adapter();
   if (adapter == -1)
      adapter = 0;

   /* This might have to change for multihead */
   if (_al_d3d->GetDeviceCaps(adapter, D3DDEVTYPE_HAL, &caps) != D3D_OK) {
      if (_al_d3d->GetDeviceCaps(adapter, D3DDEVTYPE_REF, &caps) != D3D_OK) {
         return false;
      }
   }

   if ((caps.TextureCaps & D3DPTEXTURECAPS_SQUAREONLY) == 0) {
      return true;
   }

   return false;
}


int _al_format_to_d3d(int format)
{
   int i;

   for (i = 0; allegro_formats[i] >= 0; i++) {
      if (!_al_pixel_format_is_real(allegro_formats[i]))
         continue;
      if (allegro_formats[i] == format) {
         return d3d_formats[i];
      }
   }

   //return D3DFMT_R5G6B5;
   return -1;
}

int _al_d3d_format_to_allegro(int d3d_fmt)
{
   int i;

   for (i = 0; d3d_formats[i] >= 0; i++) {
      if (!_al_pixel_format_is_real(allegro_formats[i]))
         continue;
      if (d3d_formats[i] == d3d_fmt) {
         return allegro_formats[i];
      }
   }

   return -1;
}

static int d3d_al_color_to_d3d(ALLEGRO_COLOR color)
{
   unsigned char r, g, b, a;
   int result;
   al_unmap_rgba(color, &r, &g, &b, &a);
   result = D3DCOLOR_ARGB(a, r, g, b);
   return result;
}

static DWORD d3d_get_filter(const char *s)
{
   if (!stricmp(s, "LINEAR"))
      return D3DTEXF_LINEAR;
   if (!stricmp(s, "ANISOTROPIC"))
      return D3DTEXF_ANISOTROPIC;
   return D3DTEXF_POINT;
}


static void d3d_reset_state(ALLEGRO_DISPLAY_D3D *disp)
{
   if (disp->device_lost)
      return;

   disp->device->SetRenderState(D3DRS_ZENABLE, D3DZB_TRUE);
   disp->device->SetRenderState(D3DRS_ZWRITEENABLE, true);
   disp->device->SetRenderState(D3DRS_LIGHTING, false);
   disp->device->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
   disp->device->SetRenderState(D3DRS_ALPHABLENDENABLE, true);

   /* Set texture address mode to clamp */
   if (disp->device->SetSamplerState(0, D3DSAMP_ADDRESSU, D3DTADDRESS_CLAMP) != D3D_OK)
      ALLEGRO_ERROR("SetSamplerState failed\n");
   if (disp->device->SetSamplerState(0, D3DSAMP_ADDRESSV, D3DTADDRESS_CLAMP) != D3D_OK)
      ALLEGRO_ERROR("SetSamplerState failed\n");
      
   /* Set up filtering */
   if (disp->device->SetSamplerState(0, D3DSAMP_MINFILTER, d3d_min_filter) != D3D_OK)
      ALLEGRO_ERROR("SetSamplerState failed\n");
   if (disp->device->SetSamplerState(0, D3DSAMP_MAGFILTER, d3d_mag_filter) != D3D_OK)
      ALLEGRO_ERROR("SetSamplerState failed\n");
}

void _al_d3d_get_current_ortho_projection_parameters(float *w, float *h)
{
   *w = d3d_ortho_w;
   *h = d3d_ortho_h;
}

static void d3d_get_ortho_matrix(float w, float h, D3DMATRIX *matrix)
{
   float left = 0.0f;
   float right = w;
   float top = 0.0f;
   float bottom = h;
   float neer = -1.0f;
   float farr = 1.0f;

   matrix->m[1][0] = 0.0f;
   matrix->m[2][0] = 0.0f;
   matrix->m[0][1] = 0.0f;
   matrix->m[2][1] = 0.0f;
   matrix->m[0][2] = 0.0f;
   matrix->m[1][2] = 0.0f;
   matrix->m[0][3] = 0.0f;
   matrix->m[1][3] = 0.0f;
   matrix->m[2][3] = 0.0f;

   matrix->m[0][0] = 2.0f / (right - left);
   matrix->m[1][1] = 2.0f / (top - bottom);
   matrix->m[2][2] = 2.0f / (farr - neer);

   matrix->m[3][0] = -((right+left)/(right-left));
   matrix->m[3][1] = -((top+bottom)/(top-bottom));
   matrix->m[3][2] = -((farr+neer)/(farr-neer));
   matrix->m[3][3] = 1.0f;
}

static void d3d_get_identity_matrix(D3DMATRIX *matrix)
{
   int i, j;
   int one = 0;

   for (i = 0; i < 4; i++) {
      for (j = 0; j < 4; j++) {
         if (j == one)
            matrix->m[j][i] = 1.0f;
         else
            matrix->m[j][i] = 0.0f;
      }
      one++;
   }
}

static void _al_d3d_set_ortho_projection(ALLEGRO_DISPLAY_D3D *disp, float w, float h)
{
   D3DMATRIX matOrtho;
   D3DMATRIX matIdentity;

   if (disp->device_lost)
      return;

   d3d_ortho_w = w;
   d3d_ortho_h = h;

   d3d_get_identity_matrix(&matIdentity);
   d3d_get_ortho_matrix(w, h, &matOrtho);

   disp->device->SetTransform(D3DTS_PROJECTION, &matOrtho);
   disp->device->SetTransform(D3DTS_WORLD, &matIdentity);
}

static bool d3d_display_mode_matches(D3DDISPLAYMODE *dm, int w, int h, int format, int refresh_rate)
{
   if ((dm->Width == (unsigned int)w) &&
       (dm->Height == (unsigned int)h) &&
       ((!refresh_rate) || (dm->RefreshRate == (unsigned int)refresh_rate)) &&
       ((int)dm->Format == (int)_al_format_to_d3d(format))) {
          return true;
   }
   return false;
}

static bool d3d_check_mode(int w, int h, int format, int refresh_rate, UINT adapter)
{
   UINT num_modes;
   UINT i;
   D3DDISPLAYMODE display_mode;

   num_modes = _al_d3d->GetAdapterModeCount(adapter, (D3DFORMAT)_al_format_to_d3d(format));

   for (i = 0; i < num_modes; i++) {
      if (_al_d3d->EnumAdapterModes(adapter, (D3DFORMAT)_al_format_to_d3d(format), i, &display_mode) != D3D_OK) {
         return false;
      }
      if (d3d_display_mode_matches(&display_mode, w, h, format, refresh_rate)) {
         return true;
      }
   }

   return false;
}

static int d3d_get_default_refresh_rate(UINT adapter)
{
   D3DDISPLAYMODE d3d_dm;
   _al_d3d->GetAdapterDisplayMode(adapter, &d3d_dm);
   return d3d_dm.RefreshRate;
}


static void d3d_destroy_display_format_list(void)
{
   /* Free the display format list */
   for (int j = 0; j < eds_list_count; j++) {
      al_free(eds_list[j]);
   }
   al_free(eds_list);
   eds_list = 0;
   eds_list_count = 0;
}


static void d3d_generate_display_format_list(void)
{
   static bool fullscreen = !(al_get_new_display_flags() & ALLEGRO_FULLSCREEN); /* stop warning */
   static int adapter = ~al_get_new_display_adapter(); /* stop warning */
   int i;

   if ((eds_list != NULL) && (fullscreen == (bool)(al_get_new_display_flags() & ALLEGRO_FULLSCREEN))
         && (adapter == al_get_new_display_adapter())) {
      return;
   }
   else if (eds_list != NULL) {
      d3d_destroy_display_format_list();
   }

   // Create display format list
   DWORD quality_levels[NUM_DISPLAY_FORMATS] = { 0, };
   eds_list_count = D3D_DISPLAY_COMBINATIONS;
   int count = 0;

   fullscreen = al_get_new_display_flags() & ALLEGRO_FULLSCREEN;
   adapter = al_get_new_display_adapter();
   if (adapter < 0)
      adapter = 0;

   for (i = 0; allegro_formats[i] >= 0; i++) {
      if (_al_pixel_format_is_real(allegro_formats[i]) && !_al_format_has_alpha(allegro_formats[i])) {
         if (_al_d3d->CheckDeviceMultiSampleType(adapter, D3DDEVTYPE_HAL, (D3DFORMAT)d3d_formats[i],
            !fullscreen, D3DMULTISAMPLE_NONMASKABLE, &quality_levels[count]) != D3D_OK) {
            _al_d3d->CheckDeviceMultiSampleType(adapter, D3DDEVTYPE_REF, (D3DFORMAT)d3d_formats[i],
               !fullscreen, D3DMULTISAMPLE_NONMASKABLE, &quality_levels[count]);
         }
         if (quality_levels[count] > 0) {
            if (al_get_pixel_size(allegro_formats[i]) == 4) {
               eds_list_count += (quality_levels[count]-1) * (_32BIT_DS+1) * 4; /* +1 for no DepthStencil */
            }
            else {
               eds_list_count += (quality_levels[count]-1) * (_16BIT_DS+_32BIT_DS+1) * 4;
            }
         }
         count++;
      }
   }

   eds_list = (ALLEGRO_EXTRA_DISPLAY_SETTINGS **)al_malloc(
      eds_list_count * sizeof(*eds_list)
   );
   memset(eds_list, 0, eds_list_count * sizeof(*eds_list));
   for (i = 0; i < eds_list_count; i++) {
      eds_list[i] = (ALLEGRO_EXTRA_DISPLAY_SETTINGS *)al_malloc(sizeof(ALLEGRO_EXTRA_DISPLAY_SETTINGS));
      memset(eds_list[i], 0, sizeof(ALLEGRO_EXTRA_DISPLAY_SETTINGS));
   }

   count = 0;

   int fmt_num = 0;
   int curr_fmt = d3d_fmt_desc[0][ALLEGRO_DISPLAY_OPTIONS_COUNT];

   for (i = 0; i < D3D_DISPLAY_COMBINATIONS; i++) {
      if (d3d_fmt_desc[i][ALLEGRO_SAMPLE_BUFFERS]) {
         for (int k = 0; k < (int)quality_levels[fmt_num]; k++) {
            memcpy(eds_list[count]->settings, &d3d_fmt_desc[i], sizeof(int)*ALLEGRO_DISPLAY_OPTIONS_COUNT);
            eds_list[count]->settings[ALLEGRO_SAMPLES] = k;
            count++;
         }
      }
      else {
         memcpy(eds_list[count]->settings, &d3d_fmt_desc[i], sizeof(int)*ALLEGRO_DISPLAY_OPTIONS_COUNT);
         count++;
      }
      if (d3d_fmt_desc[i][ALLEGRO_DISPLAY_OPTIONS_COUNT] != curr_fmt) {
         curr_fmt = d3d_fmt_desc[i][ALLEGRO_DISPLAY_OPTIONS_COUNT];
         fmt_num++;
      }
   }
}


static bool d3d_create_fullscreen_device(ALLEGRO_DISPLAY_D3D *d,
   int format, int refresh_rate, int flags)
{
   int ret;
   bool reset_all = false;
   ALLEGRO_DISPLAY_WIN *win_display = &d->win_display;
   ALLEGRO_DISPLAY *al_display = &win_display->display;

   (void)flags;

   if (!d3d_check_mode(al_display->w, al_display->h, format, refresh_rate, win_display->adapter)) {
      ALLEGRO_ERROR("d3d_create_fullscreen_device: Mode not supported.\n");
      return 0;
   }

   ZeroMemory(&d3d_pp, sizeof(d3d_pp));
   d3d_pp.BackBufferFormat = (D3DFORMAT)_al_format_to_d3d(format);
   d3d_pp.BackBufferWidth = al_display->w;
   d3d_pp.BackBufferHeight = al_display->h;
   d3d_pp.BackBufferCount = 1;
   d3d_pp.Windowed = 0;
   if (d->vsync) {
      d3d_pp.PresentationInterval = D3DPRESENT_INTERVAL_ONE;
   }
   else {
      d3d_pp.PresentationInterval = D3DPRESENT_INTERVAL_IMMEDIATE;
   }

   if (d->depth_stencil_format) {
      d3d_pp.EnableAutoDepthStencil = true;
      d3d_pp.AutoDepthStencilFormat = d->depth_stencil_format;
   }
   if (d->samples) {
      d3d_pp.MultiSampleType = D3DMULTISAMPLE_NONMASKABLE;
      d3d_pp.MultiSampleQuality = d->samples;
   }
   else
      d3d_pp.Flags = D3DPRESENTFLAG_LOCKABLE_BACKBUFFER;

   if (d->single_buffer) {
      d3d_pp.SwapEffect = D3DSWAPEFFECT_COPY;
   }
   else {
      d3d_pp.SwapEffect = D3DSWAPEFFECT_DISCARD;
   }
   d3d_pp.hDeviceWindow = win_display->window;

   if (refresh_rate) {
      d3d_pp.FullScreen_RefreshRateInHz = refresh_rate;
   }
   else {
      d3d_pp.FullScreen_RefreshRateInHz = d3d_get_default_refresh_rate(win_display->adapter);
      al_display->refresh_rate = d3d_pp.FullScreen_RefreshRateInHz;
   }

   if (ffw_set == false) {
      fullscreen_focus_window = win_display->window;
      ffw_set = true;
   }
   else {
      reset_all = true;
   }

#ifdef ALLEGRO_CFG_D3D9EX
   if (is_vista) {
      D3DDISPLAYMODEEX mode;
      IDirect3D9Ex *d3d = (IDirect3D9Ex *)_al_d3d;
      mode.Size = sizeof(D3DDISPLAYMODEEX);
      mode.Width = al_display->w;
      mode.Height = al_display->h;
      mode.RefreshRate = d3d_pp.FullScreen_RefreshRateInHz;
      mode.Format = d3d_pp.BackBufferFormat;
      mode.ScanLineOrdering = D3DSCANLINEORDERING_PROGRESSIVE;

      if ((ret = d3d->CreateDeviceEx(win_display->adapter,
               D3DDEVTYPE_HAL, fullscreen_focus_window,
               D3DCREATE_HARDWARE_VERTEXPROCESSING|D3DCREATE_FPU_PRESERVE|D3DCREATE_MULTITHREADED,
               &d3d_pp, &mode, (IDirect3DDevice9Ex **)(&d->device))) != D3D_OK) {
         if ((ret = d3d->CreateDeviceEx(win_display->adapter,
                  D3DDEVTYPE_HAL, fullscreen_focus_window,
                  D3DCREATE_SOFTWARE_VERTEXPROCESSING|D3DCREATE_FPU_PRESERVE|D3DCREATE_MULTITHREADED,
                  &d3d_pp, &mode, (IDirect3DDevice9Ex **)(&d->device))) != D3D_OK) {
            if ((ret = d3d->CreateDeviceEx(win_display->adapter,
                     D3DDEVTYPE_REF, fullscreen_focus_window,
                     D3DCREATE_HARDWARE_VERTEXPROCESSING|D3DCREATE_FPU_PRESERVE|D3DCREATE_MULTITHREADED,
                     &d3d_pp, &mode, (IDirect3DDevice9Ex **)(&d->device))) != D3D_OK) {
               if ((ret = d3d->CreateDeviceEx(win_display->adapter,
                        D3DDEVTYPE_REF, fullscreen_focus_window,
                        D3DCREATE_SOFTWARE_VERTEXPROCESSING|D3DCREATE_FPU_PRESERVE|D3DCREATE_MULTITHREADED,
                        &d3d_pp, &mode, (IDirect3DDevice9Ex **)(&d->device))) != D3D_OK) {
                  switch (ret) {
                     case D3DERR_INVALIDCALL:
                        ALLEGRO_ERROR("D3DERR_INVALIDCALL in create_device.\n");
                        break;
                     case D3DERR_NOTAVAILABLE:
                        ALLEGRO_ERROR("D3DERR_NOTAVAILABLE in create_device.\n");
                        break;
                     case D3DERR_OUTOFVIDEOMEMORY:
                        ALLEGRO_ERROR("D3DERR_OUTOFVIDEOMEMORY in create_device.\n");
                        break;
                     case D3DERR_DEVICELOST:
                        ALLEGRO_ERROR("D3DERR_DEVICELOST in create_device.\n");
                        break;
                     default:
                        ALLEGRO_ERROR("Direct3D Device creation failed.\n");
                        break;
                  }
                  return 0;
            }  }
         }
      }
   }
   else {
#endif
      if ((ret = _al_d3d->CreateDevice(win_display->adapter,
               D3DDEVTYPE_HAL, fullscreen_focus_window,
               D3DCREATE_HARDWARE_VERTEXPROCESSING|D3DCREATE_FPU_PRESERVE|D3DCREATE_MULTITHREADED,
               &d3d_pp, &d->device)) != D3D_OK) {
         if ((ret = _al_d3d->CreateDevice(win_display->adapter,
                  D3DDEVTYPE_HAL, fullscreen_focus_window,
                  D3DCREATE_SOFTWARE_VERTEXPROCESSING|D3DCREATE_FPU_PRESERVE|D3DCREATE_MULTITHREADED,
                  &d3d_pp, &d->device)) != D3D_OK) {
            if ((ret = _al_d3d->CreateDevice(win_display->adapter,
                     D3DDEVTYPE_REF, fullscreen_focus_window,
                     D3DCREATE_HARDWARE_VERTEXPROCESSING|D3DCREATE_FPU_PRESERVE|D3DCREATE_MULTITHREADED,
                     &d3d_pp, &d->device)) != D3D_OK) {
               if ((ret = _al_d3d->CreateDevice(win_display->adapter,
                        D3DDEVTYPE_REF, fullscreen_focus_window,
                        D3DCREATE_SOFTWARE_VERTEXPROCESSING|D3DCREATE_FPU_PRESERVE|D3DCREATE_MULTITHREADED,
                        &d3d_pp, &d->device)) != D3D_OK) {
                  switch (ret) {
                     case D3DERR_INVALIDCALL:
                        ALLEGRO_ERROR("D3DERR_INVALIDCALL in create_device.\n");
                        break;
                     case D3DERR_NOTAVAILABLE:
                        ALLEGRO_ERROR("D3DERR_NOTAVAILABLE in create_device.\n");
                        break;
                     case D3DERR_OUTOFVIDEOMEMORY:
                        ALLEGRO_ERROR("D3DERR_OUTOFVIDEOMEMORY in create_device.\n");
                        break;
                     case D3DERR_DEVICELOST:
                        ALLEGRO_ERROR("D3DERR_DEVICELOST in create_device.\n");
                        break;
                     default:
                        ALLEGRO_ERROR("Direct3D Device creation failed.\n");
                        break;
                  }
                  return 0;
               }
            }
         }
      }
#ifdef ALLEGRO_CFG_D3D9EX
   }
#endif

   d->device->GetBackBuffer(0, 0, D3DBACKBUFFER_TYPE_MONO, &d->render_target);

   if (d->depth_stencil_format)
      d->device->GetDepthStencilSurface(&d->depth_stencil);
   //d->device->GetRenderTarget(0, &d->render_target);

   ALLEGRO_INFO("Fullscreen Direct3D device created.\n");

   d->device->BeginScene();

   ALLEGRO_SYSTEM *system = (ALLEGRO_SYSTEM *)al_get_system_driver();

   if (reset_all) {
      int i;
      for (i = 0; i < (int)system->displays._size; i++) {
         ALLEGRO_DISPLAY_D3D **dptr = (ALLEGRO_DISPLAY_D3D **)_al_vector_ref(&system->displays, i);
         ALLEGRO_DISPLAY_D3D *disp = *dptr;
         if (disp != d) {
            if (disp != d && (disp->win_display.display.flags & ALLEGRO_FULLSCREEN)) {
               disp->do_reset = true;
               while (!disp->reset_done) {
                  al_rest(0.001);
               }
               disp->reset_done = false;
            }
         }
      }
   }

   return 1;
}

static void d3d_destroy_device(ALLEGRO_DISPLAY_D3D *disp)
{
   while (disp->device->Release() != 0) {
      ALLEGRO_WARN("d3d_destroy_device: ref count not 0\n");
   }
   disp->device = NULL;
}


bool _al_d3d_render_to_texture_supported(void)
{
   return render_to_texture_supported;
}



bool _al_d3d_init_display()
{
   D3DDISPLAYMODE d3d_dm;
   OSVERSIONINFO info;

   info.dwOSVersionInfoSize = sizeof(info);
   GetVersionEx(&info);
   is_vista = info.dwMajorVersion >= 6;

#ifdef ALLEGRO_CFG_D3D9EX
   if (is_vista) {
      _dyn_create = (_dyn_create_type)GetProcAddress(GetModuleHandle(TEXT("d3d9.dll")), "Direct3DCreate9Ex");
      if (_dyn_create != NULL) {
         if (_dyn_create(D3D_SDK_VERSION, (LPDIRECT3D9EX *)&_al_d3d) != D3D_OK) {
            ALLEGRO_ERROR("Direct3DCreate9Ex failed\n");
            return false;
         }
      }
      else {
         ALLEGRO_INFO("Direct3DCreate9Ex not in d3d9.dll\n");
         is_vista = false;
      }
   }

   if (!is_vista) {
#endif
      if ((_al_d3d = Direct3DCreate9(D3D9b_SDK_VERSION)) == NULL) {
         ALLEGRO_ERROR("Direct3DCreate9 failed.\n");
         return false;
      }
#ifdef ALLEGRO_CFG_D3D9EX
   }
#endif

   _al_d3d->GetAdapterDisplayMode(D3DADAPTER_DEFAULT, &d3d_dm);

   if (_al_d3d->CheckDeviceFormat(D3DADAPTER_DEFAULT,
         D3DDEVTYPE_HAL, d3d_dm.Format, D3DUSAGE_RENDERTARGET,
         D3DRTYPE_TEXTURE, d3d_dm.Format) != D3D_OK) {
      if (_al_d3d->CheckDeviceFormat(D3DADAPTER_DEFAULT,
            D3DDEVTYPE_REF, d3d_dm.Format, D3DUSAGE_RENDERTARGET,
            D3DRTYPE_TEXTURE, d3d_dm.Format) != D3D_OK) {
               render_to_texture_supported = false;
      }
      else
         render_to_texture_supported = true;
   }
   else
      render_to_texture_supported = true;


   ALLEGRO_INFO("Render-to-texture: %d\n", render_to_texture_supported);

   present_mutex = al_create_mutex();
   _al_d3d_lost_device_mutex = al_create_mutex();

   _al_d3d_bmp_init();

   return true;
}


static bool d3d_create_display_internals(ALLEGRO_DISPLAY_D3D *display);
static void d3d_destroy_display_internals(ALLEGRO_DISPLAY_D3D *display);


static void d3d_make_faux_fullscreen_stage_one(ALLEGRO_DISPLAY_D3D *d3d_display)
{
   ALLEGRO_SYSTEM *system = al_get_system_driver();

   if (already_fullscreen || num_faux_fullscreen_windows) {
      int i;
      for (i = 0; i < (int)system->displays._size; i++) {
      ALLEGRO_DISPLAY_D3D **dptr = (ALLEGRO_DISPLAY_D3D **)_al_vector_ref(&system->displays, i);
      ALLEGRO_DISPLAY_D3D *disp = *dptr;
         if (disp != d3d_display) {// && (disp->win_display.display.flags & ALLEGRO_FULLSCREEN)) {
            d3d_destroy_display_internals(disp);
            disp->win_display.end_thread = false;
            disp->win_display.thread_ended = false;
         }
      }
   }
}


static void d3d_make_faux_fullscreen_stage_two(ALLEGRO_DISPLAY_D3D *d3d_display)
{
   ALLEGRO_SYSTEM *system = al_get_system_driver();

   if (already_fullscreen || num_faux_fullscreen_windows) {
      int i;
      already_fullscreen = false;
      for (i = 0; i < (int)system->displays._size; i++) {
         ALLEGRO_DISPLAY_D3D **dptr = (ALLEGRO_DISPLAY_D3D **)_al_vector_ref(&system->displays, i);
         ALLEGRO_DISPLAY_D3D *disp = *dptr;
         if (disp != d3d_display) {// && (disp->win_display.display.flags & ALLEGRO_FULLSCREEN)) {
            if (disp->win_display.display.flags & ALLEGRO_FULLSCREEN)
               disp->faux_fullscreen = true;
            d3d_create_display_internals(disp);
            _al_d3d_recreate_bitmap_textures(disp);
         }
      }
   }
}

static bool d3d_create_device(ALLEGRO_DISPLAY_D3D *d,
   int format, int refresh_rate, int flags, bool convert_to_faux)
{
   HRESULT hr;
   ALLEGRO_DISPLAY_WIN *win_display = &d->win_display;
   ALLEGRO_DISPLAY *al_display = &win_display->display;
   int adapter = al_get_new_display_adapter();

   (void)refresh_rate;
   (void)flags;

   /* Ideally if you're targetting vanilla Direct3D 9 you should create
    * your windowed displays before any fullscreen ones. If you don't,
    * your fullscreen displays will be turned into "faux-fullscreen"
    * displays, basically screen-filling windows set out in front of
    * everything else.
    */
#ifdef ALLEGRO_CFG_D3D9EX
   if (convert_to_faux)
      d3d_make_faux_fullscreen_stage_one(d);
#else
   (void)convert_to_faux;
#endif

   ZeroMemory(&d3d_pp, sizeof(d3d_pp));

   d3d_pp.BackBufferFormat = (D3DFORMAT)_al_format_to_d3d(format);

   d3d_pp.BackBufferWidth = al_display->w;
   d3d_pp.BackBufferHeight = al_display->h;
   d3d_pp.BackBufferCount = 1;
   d3d_pp.Windowed = 1;
   if (d->vsync) {
      d3d_pp.PresentationInterval = D3DPRESENT_INTERVAL_ONE;
   }
   else {
      d3d_pp.PresentationInterval = D3DPRESENT_INTERVAL_IMMEDIATE;
   }

   if (d->depth_stencil_format) {
      d3d_pp.EnableAutoDepthStencil = true;
      d3d_pp.AutoDepthStencilFormat = d->depth_stencil_format;
      ALLEGRO_INFO("Chose depth stencil format %d\n", d->depth_stencil_format);
   }
   else {
      ALLEGRO_INFO("Using no depth stencil buffer\n");
   }

   if (d->samples) {
      d3d_pp.MultiSampleType = D3DMULTISAMPLE_NONMASKABLE;
      d3d_pp.MultiSampleQuality = d->samples;
   }
   else
      d3d_pp.Flags = D3DPRESENTFLAG_LOCKABLE_BACKBUFFER;

   if (d->single_buffer) {
      d3d_pp.SwapEffect = D3DSWAPEFFECT_COPY;
   }
   else {
      d3d_pp.SwapEffect = D3DSWAPEFFECT_DISCARD;
   }
   d3d_pp.hDeviceWindow = win_display->window;

   if (adapter == -1)
      adapter = 0;

   if ((hr = _al_d3d->CreateDevice(adapter,
         D3DDEVTYPE_HAL, win_display->window,
         D3DCREATE_HARDWARE_VERTEXPROCESSING|D3DCREATE_FPU_PRESERVE|D3DCREATE_MULTITHREADED,
         &d3d_pp, (LPDIRECT3DDEVICE9 *)&d->device)) != D3D_OK) {
      if ((hr = _al_d3d->CreateDevice(adapter,
            D3DDEVTYPE_HAL, win_display->window,
            D3DCREATE_SOFTWARE_VERTEXPROCESSING|D3DCREATE_FPU_PRESERVE|D3DCREATE_MULTITHREADED,
            &d3d_pp, (LPDIRECT3DDEVICE9 *)&d->device)) != D3D_OK) {
         ALLEGRO_DEBUG("Trying reference device\n");
         if ((hr = _al_d3d->CreateDevice(adapter,
               D3DDEVTYPE_REF, win_display->window,
               D3DCREATE_HARDWARE_VERTEXPROCESSING|D3DCREATE_FPU_PRESERVE|D3DCREATE_MULTITHREADED,
               &d3d_pp, (LPDIRECT3DDEVICE9 *)&d->device)) != D3D_OK) {
            if ((hr = _al_d3d->CreateDevice(adapter,
                  D3DDEVTYPE_REF, win_display->window,
                  D3DCREATE_SOFTWARE_VERTEXPROCESSING|D3DCREATE_FPU_PRESERVE|D3DCREATE_MULTITHREADED,
                  &d3d_pp, (LPDIRECT3DDEVICE9 *)&d->device)) != D3D_OK) {
               if (hr == D3DERR_NOTAVAILABLE) {
                  ALLEGRO_ERROR("CreateDevice failed: 1\n");
               }
               else if (hr == D3DERR_DEVICELOST) {
                  ALLEGRO_ERROR("CreateDevice failed: 2\n");
               }
               else if (hr == D3DERR_INVALIDCALL) {
                  ALLEGRO_ERROR("CreateDevice failed: 3\n");
               }
               else if (hr == D3DERR_OUTOFVIDEOMEMORY) {
                  ALLEGRO_ERROR("CreateDevice failed: 4\n");
               }
               else if (hr == E_OUTOFMEMORY) {
                  ALLEGRO_ERROR("CreateDevice failed: 5\n");
               }
               else {
                  ALLEGRO_ERROR("Unknown error %u\n", (unsigned)hr);
               }
               ALLEGRO_ERROR("d3d_create_device: CreateDevice failed.\n");
               return 0;
            }
         }
      }
   }

   if (d->device->GetBackBuffer(0, 0, D3DBACKBUFFER_TYPE_MONO, &d->render_target) != D3D_OK) {
   //if (d->device->GetRenderTarget(0, &d->render_target) != D3D_OK) {
      ALLEGRO_ERROR("d3d_create_device: GetBackBuffer failed.\n");
      return 0;
   }

   if (d->depth_stencil_format) {
      if (d->device->GetDepthStencilSurface(&d->depth_stencil) != D3D_OK) {
         ALLEGRO_ERROR("d3d_create_device: GetDepthStencilSurface failed.\n");
         return 0;
      }
   }

   if (d->device->BeginScene() != D3D_OK) {
      ALLEGRO_ERROR("BeginScene failed in create_device\n");
   }
   else {
      ALLEGRO_DEBUG("BeginScene succeeded in create_device\n");
   }

#ifdef ALLEGRO_CFG_D3D9EX
   if (convert_to_faux)
      d3d_make_faux_fullscreen_stage_two(d);
#endif

   ALLEGRO_DEBUG("Success\n");

   return 1;
}

/* When a display is destroyed, its bitmaps get converted
 * to memory bitmaps
 */
static void d3d_release_bitmaps(ALLEGRO_DISPLAY *display)
{
   while (display->bitmaps._size > 0) {
      ALLEGRO_BITMAP **bptr = (ALLEGRO_BITMAP **)_al_vector_ref_back(&display->bitmaps);
      ALLEGRO_BITMAP *b = *bptr;
      _al_convert_to_memory_bitmap(b);
   }
}


static void d3d_release_current_target(void)
{
   ALLEGRO_BITMAP *curr;
   ALLEGRO_BITMAP_D3D *curr_d3d;

   curr = al_get_target_bitmap();
   if (curr) {
      if (!(curr->flags & ALLEGRO_MEMORY_BITMAP)) {
         curr_d3d = (ALLEGRO_BITMAP_D3D *)curr;
         if (curr_d3d->render_target) {
            ALLEGRO_DISPLAY_D3D *dd = (ALLEGRO_DISPLAY_D3D *)curr->display;
            if (curr_d3d->is_backbuffer) {
               if (dd->render_target->Release() != 0) {
                  ALLEGRO_WARN("d3d_release_current_target: (bb) ref count not 0\n");
               }
               dd->render_target = NULL;
            }
            else if (!curr_d3d->is_backbuffer) {
               if (curr_d3d->render_target->Release() != 0) {
                  ALLEGRO_WARN("d3d_release_current_target: (bmp) ref count not 0\n");
               }
               curr_d3d->render_target = NULL;
               dd->render_target = NULL;
            }
         }
      }
   }
}


static void d3d_destroy_display_internals(ALLEGRO_DISPLAY_D3D *d3d_display)
{
   ALLEGRO_DISPLAY *al_display = (ALLEGRO_DISPLAY *)d3d_display;
   ALLEGRO_DISPLAY_WIN *win_display = &d3d_display->win_display;

   if (d3d_display->device) {
      if (al_display->display_invalidated)
         al_display->display_invalidated(al_display);
      d3d_display->device->EndScene();
   }

   d3d_release_bitmaps((ALLEGRO_DISPLAY *)d3d_display);

   d3d_release_current_target();

   if (win_display->window) {
      SendMessage(win_display->window, _al_win_msg_suicide, 0, 0);
      while (!win_display->thread_ended)
         al_rest(0.001);
   }
}

static void d3d_destroy_display(ALLEGRO_DISPLAY *display)
{
   ALLEGRO_SYSTEM_WIN *system = (ALLEGRO_SYSTEM_WIN *)al_get_system_driver();
   ALLEGRO_DISPLAY_D3D *d3d_display = (ALLEGRO_DISPLAY_D3D *)display;
   ALLEGRO_DISPLAY *old_disp = al_get_current_display();

   if (old_disp != display)
      _al_set_current_display_only(display);

   d3d_destroy_display_internals(d3d_display);

   _al_vector_find_and_delete(&system->system.displays, &display);

   if (system->system.displays._size <= 0) {
      ffw_set = false;
      already_fullscreen = false;
   }

   if (d3d_display->es_inited) {
      _al_event_source_free(&display->es);
      d3d_display->es_inited = false;
   }

   _al_vector_free(&display->bitmaps);
   
   if (old_disp != display)
      _al_set_current_display_only(old_disp);

   al_free(display->vertex_cache);
   al_free(display);
}

void _al_d3d_prepare_for_reset(ALLEGRO_DISPLAY_D3D *disp)
{
   _al_d3d_release_default_pool_textures();
   while (disp->render_target && disp->render_target->Release() != 0) {
      ALLEGRO_WARN("_al_d3d_prepare_for_reset: (bb) ref count not 0\n");
   }
   disp->render_target = NULL;
   if (disp->depth_stencil_format) {
      disp->depth_stencil->Release();
   }
}

static bool _al_d3d_reset_device(ALLEGRO_DISPLAY_D3D *d3d_display)
{
   ALLEGRO_DISPLAY_WIN *win_display = &d3d_display->win_display;
   ALLEGRO_DISPLAY *al_display = &win_display->display;

   al_lock_mutex(_al_d3d_lost_device_mutex);

   _al_d3d_prepare_for_reset(d3d_display);

   if (al_display->flags & ALLEGRO_FULLSCREEN) {
      HRESULT hr;

      ZeroMemory(&d3d_pp, sizeof(d3d_pp));
      d3d_pp.BackBufferFormat = (D3DFORMAT)_al_format_to_d3d(_al_deduce_color_format(&al_display->extra_settings));
      d3d_pp.BackBufferWidth = al_display->w;
      d3d_pp.BackBufferHeight = al_display->h;
      d3d_pp.BackBufferCount = 1;
      d3d_pp.Windowed = 0;
      d3d_pp.SwapEffect = D3DSWAPEFFECT_DISCARD;
      d3d_pp.hDeviceWindow = win_display->window;
      if (d3d_display->vsync) {
         d3d_pp.PresentationInterval = D3DPRESENT_INTERVAL_ONE;
      }
      else {
         d3d_pp.PresentationInterval = D3DPRESENT_INTERVAL_IMMEDIATE;
      }

      if (d3d_display->depth_stencil_format) {
         d3d_pp.EnableAutoDepthStencil = true;
         d3d_pp.AutoDepthStencilFormat = d3d_display->depth_stencil_format;
      }
      if (d3d_display->samples) {
         d3d_pp.MultiSampleType = D3DMULTISAMPLE_NONMASKABLE;
         d3d_pp.MultiSampleQuality = d3d_display->samples;
      }
      else
         d3d_pp.Flags |= D3DPRESENTFLAG_LOCKABLE_BACKBUFFER;

      if (d3d_display->single_buffer) {
         d3d_pp.SwapEffect = D3DSWAPEFFECT_COPY;
      }
      else {
         d3d_pp.SwapEffect = D3DSWAPEFFECT_DISCARD;
      }
      if (al_display->refresh_rate) {
         d3d_pp.FullScreen_RefreshRateInHz =
         al_display->refresh_rate;
      }
      else {
         d3d_pp.FullScreen_RefreshRateInHz = d3d_get_default_refresh_rate(win_display->adapter);
      }
      #ifdef ALLEGRO_CFG_D3D9EX
      if (is_vista) {
         D3DDISPLAYMODEEX mode;
         IDirect3DDevice9Ex *dev = (IDirect3DDevice9Ex *)d3d_display->device;
         mode.Size = sizeof(D3DDISPLAYMODEEX);
         mode.Width = d3d_pp.BackBufferWidth;
         mode.Height = d3d_pp.BackBufferHeight;
         mode.RefreshRate = d3d_pp.FullScreen_RefreshRateInHz;
         mode.Format = d3d_pp.BackBufferFormat;
         mode.ScanLineOrdering = D3DSCANLINEORDERING_PROGRESSIVE;
         hr = dev->ResetEx(&d3d_pp, &mode);
      }
      else {
      #endif
         hr = d3d_display->device->Reset(&d3d_pp);
      #ifdef ALLEGRO_CFG_D3D9EX
      }
      #endif
      if (hr != D3D_OK) {
         switch (hr) {
            case D3DERR_INVALIDCALL:
            ALLEGRO_ERROR("D3DERR_INVALIDCALL in reset.\n");
            break;
            case D3DERR_NOTAVAILABLE:
            ALLEGRO_ERROR("D3DERR_NOTAVAILABLE in reset.\n");
            break;
            case D3DERR_OUTOFVIDEOMEMORY:
            ALLEGRO_ERROR("D3DERR_OUTOFVIDEOMEMORY in reset.\n");
            break;
            case D3DERR_DEVICELOST:
            ALLEGRO_ERROR("D3DERR_DEVICELOST in reset.\n");
            break;
            default:
            ALLEGRO_ERROR("Direct3D Device reset failed (unknown reason).\n");
            break;
      }
      al_unlock_mutex(_al_d3d_lost_device_mutex);
      return 0;
      }
    }
    else {
      ZeroMemory(&d3d_pp, sizeof(d3d_pp));
      d3d_pp.BackBufferFormat = (D3DFORMAT)_al_format_to_d3d(_al_deduce_color_format(&al_display->extra_settings));
      d3d_pp.BackBufferWidth = al_display->w;
      d3d_pp.BackBufferHeight = al_display->h;
      d3d_pp.BackBufferCount = 1;
      d3d_pp.Windowed = 1;
      d3d_pp.SwapEffect = D3DSWAPEFFECT_DISCARD;
      d3d_pp.hDeviceWindow = win_display->window;
      if (d3d_display->vsync) {
         d3d_pp.PresentationInterval = D3DPRESENT_INTERVAL_ONE;
      }
      else {
         d3d_pp.PresentationInterval = D3DPRESENT_INTERVAL_IMMEDIATE;
      }

      if (d3d_display->depth_stencil_format) {
         d3d_pp.EnableAutoDepthStencil = true;
         d3d_pp.AutoDepthStencilFormat = d3d_display->depth_stencil_format;
      }
      if (d3d_display->samples) {
         d3d_pp.MultiSampleType = D3DMULTISAMPLE_NONMASKABLE;
         d3d_pp.MultiSampleQuality = d3d_display->samples;
      }
      else
         d3d_pp.Flags |= D3DPRESENTFLAG_LOCKABLE_BACKBUFFER;

      if (d3d_display->single_buffer) {
         d3d_pp.SwapEffect = D3DSWAPEFFECT_COPY;
      }
      else {
         d3d_pp.SwapEffect = D3DSWAPEFFECT_DISCARD;
      }

      /* Must be 0 for windowed modes */
      d3d_pp.FullScreen_RefreshRateInHz = 0;

      if (d3d_display->device->Reset(&d3d_pp) != D3D_OK) {
         ALLEGRO_WARN("Reset failed\n");
         al_unlock_mutex(_al_d3d_lost_device_mutex);
         return 0;
      }
   }

   d3d_display->device->GetBackBuffer(0, 0, D3DBACKBUFFER_TYPE_MONO, &d3d_display->render_target);

   if (d3d_display->depth_stencil_format) {
      d3d_display->device->GetDepthStencilSurface(&d3d_display->depth_stencil);
   }

   _al_d3d_refresh_texture_memory();

   d3d_display->device->BeginScene();

   d3d_reset_state(d3d_display);
   
   al_unlock_mutex(_al_d3d_lost_device_mutex);

   return 1;
}

static int d3d_choose_display_format(int fake)
{
   /* Pick an appropriate format if the user is vague */
   switch (fake) {
      case ALLEGRO_PIXEL_FORMAT_ANY:
      case ALLEGRO_PIXEL_FORMAT_ANY_NO_ALPHA:
      case ALLEGRO_PIXEL_FORMAT_ANY_32_NO_ALPHA:
         fake = ALLEGRO_PIXEL_FORMAT_XRGB_8888;
         break;
      case ALLEGRO_PIXEL_FORMAT_ANY_16_NO_ALPHA:
         fake = ALLEGRO_PIXEL_FORMAT_RGB_565;
         break;
      default:
         break;
   }

   return fake;
}

static BOOL IsTextureFormatOk(D3DFORMAT TextureFormat, D3DFORMAT AdapterFormat)
{
   HRESULT hr = _al_d3d->CheckDeviceFormat(D3DADAPTER_DEFAULT,
      D3DDEVTYPE_HAL,
      AdapterFormat,
      0,
      D3DRTYPE_TEXTURE,
      TextureFormat);

   if (hr != D3D_OK) {
      hr = _al_d3d->CheckDeviceFormat(D3DADAPTER_DEFAULT,
         D3DDEVTYPE_REF,
         AdapterFormat,
         0,
         D3DRTYPE_TEXTURE,
         TextureFormat);
   }

   return SUCCEEDED(hr);
}

static int real_choose_bitmap_format(ALLEGRO_DISPLAY_D3D *d3d_display,
   int bits, bool alpha)
{
   int i;

   for (i = 0; allegro_formats[i] >= 0; i++) {
      int aformat = allegro_formats[i];
      D3DFORMAT dformat;
      D3DFORMAT adapter_format;
      int adapter_format_allegro;
      if (!_al_pixel_format_is_real(aformat)) {
         ALLEGRO_DEBUG("Fake format\n");
         continue;
      }
      if (bits && al_get_pixel_format_bits(aformat) != bits) {
         ALLEGRO_DEBUG("#Bits don't match\n");
         continue;
      }
      if (alpha && !_al_format_has_alpha(aformat)) {
         ALLEGRO_DEBUG("Alpha doesn't match\n");
         continue;
      }
      dformat = (D3DFORMAT)d3d_formats[i];
      adapter_format_allegro = d3d_display->format;
      if (!_al_pixel_format_is_real(adapter_format_allegro))
         adapter_format_allegro = d3d_choose_display_format(adapter_format_allegro);
      ALLEGRO_DEBUG("Adapter format is %d\n", adapter_format_allegro);
      adapter_format = (D3DFORMAT)_al_format_to_d3d(adapter_format_allegro);
      if (IsTextureFormatOk(dformat, adapter_format)) {
         ALLEGRO_DEBUG("Found a format\n");
         return aformat;
      }
      ALLEGRO_DEBUG("Texture format not OK\n");
   }

   ALLEGRO_WARN("Failed to find format\n");

   return -1;
}

static int d3d_choose_bitmap_format(ALLEGRO_DISPLAY_D3D *d3d_display, int fake)
{
   switch (fake) {
      case ALLEGRO_PIXEL_FORMAT_ANY_NO_ALPHA:
         fake = real_choose_bitmap_format(d3d_display, 0, false);
         break;
      case ALLEGRO_PIXEL_FORMAT_ANY:
      case ALLEGRO_PIXEL_FORMAT_ANY_WITH_ALPHA:
         fake = real_choose_bitmap_format(d3d_display, 0, true);
         break;
      case ALLEGRO_PIXEL_FORMAT_ANY_32_NO_ALPHA:
         fake = real_choose_bitmap_format(d3d_display, 32, false);
         break;
      case ALLEGRO_PIXEL_FORMAT_ANY_32_WITH_ALPHA:
         fake = real_choose_bitmap_format(d3d_display, 32, true);
         break;
      case ALLEGRO_PIXEL_FORMAT_ANY_24_NO_ALPHA:
         fake = real_choose_bitmap_format(d3d_display, 24, false);
         break;
      case ALLEGRO_PIXEL_FORMAT_ANY_16_NO_ALPHA:
         fake = real_choose_bitmap_format(d3d_display, 16, false);
         break;
      case ALLEGRO_PIXEL_FORMAT_ANY_16_WITH_ALPHA:
         fake = real_choose_bitmap_format(d3d_display, 16, true);
         break;
      case ALLEGRO_PIXEL_FORMAT_ANY_15_NO_ALPHA:
         fake = real_choose_bitmap_format(d3d_display, 15, false);
         break;
      default:
         fake = -1;
   }

   return fake;
}

struct CREATE_WINDOW_INFO {
   ALLEGRO_DISPLAY *display;
   DISPLAY_DEVICE dd;
   ALLEGRO_MONITOR_INFO mi;
   int w;
   int h;
   int refresh_rate;
   int flags;
   bool inited;
   bool quit;
};

static void d3d_generic_window_message_loop(CREATE_WINDOW_INFO *info)
{
   ALLEGRO_DISPLAY_WIN *win_display = (ALLEGRO_DISPLAY_WIN *)(info->display);
   MSG msg;

   info->inited = true;

   while (!win_display->end_thread) {
      if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
         DispatchMessage(&msg);
      }
      al_rest(0.001);
   }

   info->quit = true;
}

static void *d3d_create_window_proc(void *arg)
{
   CREATE_WINDOW_INFO *info = (CREATE_WINDOW_INFO *)arg;
   ALLEGRO_DISPLAY_WIN *win_display = (ALLEGRO_DISPLAY_WIN *)info->display;
   
   win_display->window = _al_win_create_window(
      info->display,
      info->w,
      info->h,
      info->flags
   );
   
   d3d_generic_window_message_loop(info);

   return NULL;
}

static void *d3d_create_faux_fullscreen_window_proc(void *arg)
{
   CREATE_WINDOW_INFO *info = (CREATE_WINDOW_INFO *)arg;
   ALLEGRO_DISPLAY_WIN *win_display = (ALLEGRO_DISPLAY_WIN *)info->display;
   
   win_display->window =
      _al_win_create_faux_fullscreen_window(
         info->dd.DeviceName, 
         info->display,
         info->mi.x1,
         info->mi.y1,
         info->w,
         info->h,
         info->refresh_rate,
         info->flags
      );

   d3d_generic_window_message_loop(info);
   
   return NULL;
}

/*
 * Display must be created in same thread that resets it
 */
static void *d3d_display_thread_proc(void *arg)
{
   ALLEGRO_DISPLAY_D3D *d3d_display;
   ALLEGRO_DISPLAY_WIN *win_display;
   ALLEGRO_DISPLAY *al_display;
   HRESULT hr;
   bool lost_event_generated = false;
   new_display_parameters *params = (new_display_parameters *)arg;
   D3DCAPS9 caps;
   int new_format;
   bool convert_to_faux;
   CREATE_WINDOW_INFO *info = (CREATE_WINDOW_INFO *)al_calloc(1, sizeof(*info));

   d3d_display = params->display;
   win_display = &d3d_display->win_display;
   al_display = &win_display->display;

   if (al_display->flags & ALLEGRO_FULLSCREEN) {
      convert_to_faux = true;
   }
   else {
      convert_to_faux = false;
   }

   /* So that we can call the functions using TLS from this thread. */
   al_set_new_display_flags(al_display->flags);

   new_format = _al_deduce_color_format(&al_display->extra_settings);

   /* This should never happen, I think */
   if (!_al_pixel_format_is_real(_al_deduce_color_format(&al_display->extra_settings))) {
      int f = d3d_choose_display_format(_al_deduce_color_format(&al_display->extra_settings));
      if (f < 0) {
         d3d_destroy_display(al_display);
         params->init_failed = true;
         SetEvent(params->AckEvent);
         return NULL;
      }
      new_format = f;
      _al_set_color_components(new_format, &al_display->extra_settings, ALLEGRO_REQUIRE);
   }

   ALLEGRO_INFO("Chose a display format: %d\n", new_format);
   d3d_display->format = new_format;

   if (d3d_display->faux_fullscreen) {
      DEVMODE dm;
      bool found = true;
      int refresh_rate;

      num_faux_fullscreen_windows++;

      d3d_make_faux_fullscreen_stage_one(d3d_display);

      al_get_monitor_info(win_display->adapter, &info->mi);
      /* Yes this is an "infinite" loop (suggested by MS on msdn) */
      for (int i = 0; ; i++) {
         info->dd.cb = sizeof(info->dd);
         if (!EnumDisplayDevices(NULL, i, &info->dd, 0)) {
            found = false;
            break;
         }
         if (!EnumDisplaySettings(info->dd.DeviceName, ENUM_CURRENT_SETTINGS, &dm)) {
            continue;
         }
         if (info->mi.x1 == dm.dmPosition.x && info->mi.y1 == dm.dmPosition.y) {
            break;
         }

      }
      if (!found) {
         ALLEGRO_ERROR("d3d_display_thread_proc: Error setting faux fullscreen mode.\n");
         num_faux_fullscreen_windows--;
         d3d_destroy_display(al_display);
         params->init_failed = true;
         SetEvent(params->AckEvent);
         return NULL;
      }
      if (al_display->refresh_rate) {
         refresh_rate = al_display->refresh_rate;
      }
      else {
         refresh_rate = d3d_get_default_refresh_rate(win_display->adapter);
      }
      d3d_display->device_name = (TCHAR *)al_malloc(sizeof(TCHAR)*32);
      strcpy(d3d_display->device_name, info->dd.DeviceName);
      ALLEGRO_DEBUG("going to call _al_win_create_faux_fullscreen_window\n");

      info->display = al_display;
      info->w = al_display->w;
      info->h = al_display->h;
      info->refresh_rate = refresh_rate;
      info->flags = al_display->flags;

      al_run_detached_thread(d3d_create_faux_fullscreen_window_proc, info);

      while (!info->inited) {
         al_rest(0.001);
      }

      if (!win_display->window) {
         ALLEGRO_DEBUG("Failed to create window (faux)fullscreen.\n");
         d3d_destroy_display(al_display);
         params->init_failed = true;
         SetEvent(params->AckEvent);
         return NULL;
      }

      ALLEGRO_DEBUG("Called _al_win_create_faux_fullscreen_window.\n");

      d3d_make_faux_fullscreen_stage_two(d3d_display);

      convert_to_faux = false;
   }
   else {
      ALLEGRO_INFO("Normal window.\n");

      info->display = al_display;
      info->w = al_display->w;
      info->h = al_display->h;
      info->flags = al_display->flags;

      al_run_detached_thread(d3d_create_window_proc, info);
      
      while (!info->inited) {
         al_rest(0.001);
      }

      if (!win_display->window) {
         ALLEGRO_DEBUG("Failed to create regular window.\n");
         d3d_destroy_display(al_display);
         params->init_failed = true;
         SetEvent(params->AckEvent);
         return NULL;
      }
   }

   if (!(al_display->flags & ALLEGRO_FULLSCREEN) || d3d_display->faux_fullscreen) {
      if (!d3d_create_device(d3d_display, _al_deduce_color_format(&al_display->extra_settings),
            al_display->refresh_rate, al_display->flags, convert_to_faux)) {
         d3d_destroy_display(al_display);
         params->init_failed = true;
         SetEvent(params->AckEvent);
         return NULL;
      }
   }
   else {
      ALLEGRO_DEBUG("Creating real fullscreen device\n");
      if (!d3d_create_fullscreen_device(d3d_display, _al_deduce_color_format(&al_display->extra_settings),
            al_display->refresh_rate, al_display->flags)) {
         d3d_destroy_display(al_display);
         params->init_failed = true;
         SetEvent(params->AckEvent);
         return NULL;
      }
      ALLEGRO_INFO("Real fullscreen device created\n");
   }

   al_display->backbuffer_format = _al_deduce_color_format(&al_display->extra_settings);


   d3d_display->device->GetDeviceCaps(&caps);
   d3d_can_wait_for_vsync = ((caps.Caps & D3DCAPS_READ_SCANLINE) != 0);

   params->init_failed = false;
   win_display->thread_ended = false;
   win_display->end_thread = false;
   SetEvent(params->AckEvent);

   while (!info->quit) {
      al_rest(0.001);
      if (!d3d_display->device) {
         continue;
      }

      hr = d3d_display->device->TestCooperativeLevel();

      if (hr == D3D_OK) {
         d3d_display->device_lost = false;
      }
      else if (hr == D3DERR_DEVICELOST) {
         /* device remains lost */
         if (!lost_event_generated) {
            _al_event_source_lock(&al_display->es);
            if (_al_event_source_needs_to_generate_event(&al_display->es)) {
               ALLEGRO_EVENT event;
               event.display.type = ALLEGRO_EVENT_DISPLAY_LOST;
               event.display.timestamp = al_current_time();
               _al_event_source_emit_event(&al_display->es, &event);
            }
            _al_event_source_unlock(&al_display->es);
            lost_event_generated = true;
            al_rest(0.5); // give user time to respond
         }
      }
      else if (hr == D3DERR_DEVICENOTRESET) {
         if (_al_d3d_reset_device(d3d_display)) {
            d3d_display->device_lost = false;
            d3d_reset_state(d3d_display);
            _al_d3d_set_ortho_projection(d3d_display,
               al_display->w, al_display->h);
            _al_event_source_lock(&al_display->es);
            if (_al_event_source_needs_to_generate_event(&al_display->es)) {
               ALLEGRO_EVENT event;
               event.display.type = ALLEGRO_EVENT_DISPLAY_FOUND;
               event.display.timestamp = al_current_time();
               _al_event_source_emit_event(&al_display->es, &event);
            }
            _al_event_source_unlock(&al_display->es);
            lost_event_generated = false;
         }
      }
      if (d3d_display->do_reset) {
         d3d_display->reset_success = _al_d3d_reset_device(d3d_display);
         d3d_display->do_reset = false;
         d3d_display->reset_done = true;
      }
   }

   d3d_destroy_device(d3d_display);

   if (d3d_display->faux_fullscreen) {
      ChangeDisplaySettingsEx(d3d_display->device_name, NULL, NULL, 0, NULL);
      al_free(d3d_display->device_name);
      num_faux_fullscreen_windows--;
   }

   win_display->thread_ended = true;

//   info->quit = true;
   al_free(info);

   ALLEGRO_INFO("d3d display thread exits\n");

   return NULL;
}


/* Helper function for sorting pixel formats by index */
static int d3d_display_list_resorter(const void *p0, const void *p1)
{
   const ALLEGRO_EXTRA_DISPLAY_SETTINGS *f0 = *((ALLEGRO_EXTRA_DISPLAY_SETTINGS **)p0);
   const ALLEGRO_EXTRA_DISPLAY_SETTINGS *f1 = *((ALLEGRO_EXTRA_DISPLAY_SETTINGS **)p1);

   if (!f0)
      return 1;
   if (!f1)
      return -1;
   if (f0->index == f1->index) {
      return 0;
   }
   else if (f0->index < f1->index) {
      return -1;
   }
   else {
      return 1;
   }
}


static ALLEGRO_DISPLAY_D3D *d3d_create_display_helper(int w, int h)
{
   ALLEGRO_SYSTEM_WIN *system = (ALLEGRO_SYSTEM_WIN *)al_get_system_driver();
   ALLEGRO_DISPLAY_D3D *d3d_display = (ALLEGRO_DISPLAY_D3D *)al_malloc(sizeof(ALLEGRO_DISPLAY_D3D));
   ALLEGRO_DISPLAY_WIN *win_display = &d3d_display->win_display;
   ALLEGRO_DISPLAY *al_display = &win_display->display;
   int adapter = al_get_new_display_adapter();
   if (adapter == -1)
      adapter = 0;

   memset(d3d_display, 0, sizeof *d3d_display);

   win_display->adapter = adapter;
   /* w/h may be reset below if ALLEGRO_FULLSCREEN_WINDOW is set */
   al_display->w = w;
   al_display->h = h;
   al_display->refresh_rate = al_get_new_display_refresh_rate();
   al_display->flags = al_get_new_display_flags();
   al_display->vt = vt;

#ifdef ALLEGRO_CFG_D3D9EX
   if (!is_vista) {
#endif
      if (al_display->flags & ALLEGRO_FULLSCREEN) {
         if (already_fullscreen || system->system.displays._size != 0) {
            d3d_display->faux_fullscreen = true;
         }
         else {
            already_fullscreen = true;
            d3d_display->faux_fullscreen = false;
         }
      }
      else {
         if (al_display->flags & ALLEGRO_FULLSCREEN_WINDOW) {
            ALLEGRO_MONITOR_INFO mi;
            int adapter = al_get_new_display_adapter();
            if (adapter == -1)
                  adapter = 0;
            al_get_monitor_info(adapter, &mi);
            al_display->w = mi.x2 - mi.x1;
            al_display->h = mi.y2 - mi.y1;
            win_display->toggle_w = w;
            win_display->toggle_h = h;
            d3d_display->faux_fullscreen = true;
         }
         else {
            d3d_display->faux_fullscreen = false;
         }
      }
#ifdef ALLEGRO_CFG_D3D9EX
   }
   else {
      d3d_display->faux_fullscreen = false;
   }
#endif

   return d3d_display;
}

static bool d3d_create_display_internals(ALLEGRO_DISPLAY_D3D *d3d_display)
{
   new_display_parameters params;
   ALLEGRO_DISPLAY_WIN *win_display = &d3d_display->win_display;
   ALLEGRO_DISPLAY *al_display = &win_display->display;
   static bool cfg_read = false;
   ALLEGRO_SYSTEM *sys;
   const char *s;
   ALLEGRO_EXTRA_DISPLAY_SETTINGS *ref =  _al_get_new_display_settings();
   int num_modes;
   int i;
   
   params.display = d3d_display;
   
   d3d_generate_display_format_list();

   for (int i = 0; i < eds_list_count; i++) {
      eds_list[i]->score = _al_score_display_settings(eds_list[i], ref);
      eds_list[i]->index = i;
   }

   qsort(eds_list, eds_list_count, sizeof(void*), _al_display_settings_sorter);

   /* Checking each mode is slow, so do a resolution check first */
   if (al_display->flags & ALLEGRO_FULLSCREEN) {
      num_modes = al_get_num_display_modes();
      while (num_modes >= 0) {
         ALLEGRO_DISPLAY_MODE mode;
         al_get_display_mode(num_modes, &mode);
         if (mode.width == al_display->w && mode.height == al_display->h) {
            break;
         }
         num_modes--;
      }
      if (num_modes < 0) {
         // Failing resolution test is like failing to create a window
         // This helps determining if the window message thread needs
         // to be destroyed.
         win_display->window = NULL;
         return false;
      }
   }

   for (i = 0; i < eds_list_count; i++) {
      d3d_display->depth_stencil_format = d3d_get_depth_stencil_format(eds_list[i]);
      d3d_display->samples = eds_list[i]->settings[ALLEGRO_SAMPLES];
      d3d_display->single_buffer = eds_list[i]->settings[ALLEGRO_SINGLE_BUFFER] ? true : false;
      d3d_display->vsync = eds_list[i]->settings[ALLEGRO_VSYNC] == 1;

      memcpy(&al_display->extra_settings, eds_list[i], sizeof al_display->extra_settings);

      params.init_failed = true;
      win_display->thread_ended = true;
      params.AckEvent = CreateEvent(NULL, false, false, NULL);

      al_run_detached_thread(d3d_display_thread_proc, &params);
      /* Wait some _finite_ time (10 secs or so) for display thread to init, and
       * give up if something horrible happened to it, unless we're in debug mode
       * and we may have intentionally stopped the execution to analyze the code.
       */
#ifdef DEBUGMODE
      WaitForSingleObject(params.AckEvent, INFINITE);
#else
      WaitForSingleObject(params.AckEvent, 10*1000);
#endif

      CloseHandle(params.AckEvent);

      if (!params.init_failed) {
         break;
      }
      // Display has been destroyed in d3d_display_thread_proc, create empty template again
      d3d_display = d3d_create_display_helper(al_display->w, al_display->h);
      win_display = &d3d_display->win_display;
      al_display = &win_display->display;
      params.display = d3d_display;
   }

   // Re-sort the display format list for use later
   qsort(eds_list, eds_list_count, sizeof(void*), d3d_display_list_resorter);

   if (i == eds_list_count) {
      ALLEGRO_WARN("All %d formats failed.\n", eds_list_count);
      return false;
   }

   if (!cfg_read) {
      cfg_read = true;

      sys = al_get_system_driver();

      if (sys->config) {
         s = al_get_config_value(sys->config, "graphics", "min_filter");
         if (s)
            d3d_min_filter = d3d_get_filter(s);
         s = al_get_config_value(sys->config, "graphics", "mag_filter");
         if (s)
            d3d_mag_filter = d3d_get_filter(s);
      }
   }

   d3d_reset_state(d3d_display);

   //d3d_display->backbuffer_bmp.render_target = d3d_display->render_target;
   d3d_display->backbuffer_bmp.is_backbuffer = true;
   d3d_display->backbuffer_bmp.bitmap.display = al_display;
   d3d_display->backbuffer_bmp.bitmap.format = _al_deduce_color_format(&al_display->extra_settings);
   d3d_display->backbuffer_bmp.bitmap.flags = 0;
   d3d_display->backbuffer_bmp.bitmap.w = al_display->w;
   d3d_display->backbuffer_bmp.bitmap.h = al_display->h;
   d3d_display->backbuffer_bmp.bitmap.cl = 0;
   d3d_display->backbuffer_bmp.bitmap.ct = 0;
   d3d_display->backbuffer_bmp.bitmap.cr_excl = al_display->w;
   d3d_display->backbuffer_bmp.bitmap.cb_excl = al_display->h;
   d3d_display->backbuffer_bmp.bitmap.vt = (ALLEGRO_BITMAP_INTERFACE *)_al_bitmap_d3d_driver();
   d3d_display->backbuffer_bmp.display = d3d_display;

   /* Alpha blending is the default */
   d3d_display->device->SetRenderState(D3DRS_ALPHABLENDENABLE, true);
   d3d_display->device->SetRenderState(D3DRS_SRCBLEND, D3DBLEND_SRCALPHA);
   d3d_display->device->SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);

   return true;
}

static ALLEGRO_DISPLAY *d3d_create_display(int w, int h)
{
   ALLEGRO_SYSTEM_WIN *system = (ALLEGRO_SYSTEM_WIN *)al_get_system_driver();
   ALLEGRO_DISPLAY_D3D *d3d_display = d3d_create_display_helper(w, h);
   ALLEGRO_DISPLAY_WIN *win_display = &d3d_display->win_display;
   ALLEGRO_DISPLAY *al_display = &win_display->display;
   ALLEGRO_DISPLAY_D3D **add;
   D3DCAPS9 caps;

   ALLEGRO_INFO("faux_fullscreen=%d\n", d3d_display->faux_fullscreen);

   if (!d3d_create_display_internals(d3d_display)) {
      ALLEGRO_ERROR("d3d_create_display failed.\n");
      al_free(d3d_display);
      return NULL;
   }

   /* Add ourself to the list of displays. */
   add = (ALLEGRO_DISPLAY_D3D **)_al_vector_alloc_back(&system->system.displays);
   *add = d3d_display;

   /* Each display is an event source. */
   _al_event_source_init(&al_display->es);
   d3d_display->es_inited = true;

#if 0
   /* Setup the mouse */
   if (al_display->flags & ALLEGRO_FULLSCREEN && al_is_mouse_installed()) {
      RAWINPUTDEVICE rid[1];
      rid[0].usUsagePage = 0x01; 
      rid[0].usUsage = 0x02; 
      rid[0].dwFlags = RIDEV_NOLEGACY;
      rid[0].hwndTarget = 0;
      if (RegisterRawInputDevices(rid, 1, sizeof(rid[0])) == FALSE) {
          ALLEGRO_WARN("Failed to init mouse.\n");
      }
   }
#endif

   win_display->mouse_selected_hcursor = 0;
   win_display->mouse_cursor_shown = false;
   win_display->can_acknowledge = false;

   SetForegroundWindow(win_display->window);
   _al_win_grab_input(win_display);

   _al_win_show_mouse_cursor(al_display);

   if (_al_d3d->GetDeviceCaps(win_display->adapter, D3DDEVTYPE_HAL, &caps) != D3D_OK
         && _al_d3d->GetDeviceCaps(win_display->adapter, D3DDEVTYPE_REF, &caps) != D3D_OK) {
      d3d_display->supports_separate_alpha_blend = false;
   }
   else {
      d3d_display->supports_separate_alpha_blend =
         ((caps.PrimitiveMiscCaps & D3DPMISCCAPS_SEPARATEALPHABLEND) != 0);
   }

   return al_display;
}

static bool d3d_set_current_display(ALLEGRO_DISPLAY *d)
{
   ALLEGRO_DISPLAY_D3D *d3d_display = (ALLEGRO_DISPLAY_D3D *)d;

   if (d3d_display->do_reset)
      return false;

   return true;
}


static int d3d_al_blender_to_d3d(int al_mode)
{
   int num_modes = 4;

   int allegro_modes[] = {
      ALLEGRO_ZERO,
      ALLEGRO_ONE,
      ALLEGRO_ALPHA,
      ALLEGRO_INVERSE_ALPHA
   };

   int d3d_modes[] = {
      D3DBLEND_ZERO,
      D3DBLEND_ONE,
      D3DBLEND_SRCALPHA,
      D3DBLEND_INVSRCALPHA
   };

   int i;

   for (i = 0; i < num_modes; i++) {
      if (al_mode == allegro_modes[i]) {
         return d3d_modes[i];
      }
   }

   ALLEGRO_WARN("Unknown blend mode.\n");

   return D3DBLEND_ONE;
}

void _al_d3d_set_blender(ALLEGRO_DISPLAY_D3D *d3d_display)
{
   int op, src, dst, alpha_op, alpha_src, alpha_dst;
   DWORD d3d_op, d3d_alpha_op;
   DWORD allegro_to_d3d_blendop[ALLEGRO_NUM_BLEND_OPERATIONS] = {
      D3DBLENDOP_ADD,
      D3DBLENDOP_SUBTRACT,
      D3DBLENDOP_REVSUBTRACT
   };

   al_get_separate_blender(&op, &src, &dst,
      &alpha_op, &alpha_src, &alpha_dst);

   src = d3d_al_blender_to_d3d(src);
   dst = d3d_al_blender_to_d3d(dst);
   alpha_src = d3d_al_blender_to_d3d(alpha_src);
   alpha_dst = d3d_al_blender_to_d3d(alpha_dst);
   d3d_op = allegro_to_d3d_blendop[op];
   d3d_alpha_op = allegro_to_d3d_blendop[alpha_op];

   /* These may not be supported but they will always fall back to ADD
    * in that case.
    */
   d3d_display->device->SetRenderState(D3DRS_BLENDOP, d3d_op);
   d3d_display->device->SetRenderState(D3DRS_BLENDOPALPHA, d3d_alpha_op);

   if (d3d_display->device->SetRenderState(D3DRS_SRCBLEND, src) != D3D_OK)
      ALLEGRO_ERROR("Failed to set source blender\n");
   if (d3d_display->device->SetRenderState(D3DRS_DESTBLEND, dst) != D3D_OK)
      ALLEGRO_ERROR("Failed to set dest blender\n");

   if (d3d_display->device->SetRenderState(D3DRS_SEPARATEALPHABLENDENABLE, true) != D3D_OK)
      ALLEGRO_ERROR("D3DRS_SEPARATEALPHABLENDENABLE failed\n");
   if (d3d_display->device->SetRenderState(D3DRS_SRCBLENDALPHA, alpha_src) != D3D_OK)
      ALLEGRO_ERROR("Failed to set source alpha blender\n");
   if (d3d_display->device->SetRenderState(D3DRS_DESTBLENDALPHA, alpha_dst) != D3D_OK)
      ALLEGRO_ERROR("Failed to set dest alpha blender\n");

   d3d_display->device->SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
}



static void d3d_clear(ALLEGRO_DISPLAY *al_display, ALLEGRO_COLOR *color)
{
   ALLEGRO_DISPLAY_D3D* d3d_display = (ALLEGRO_DISPLAY_D3D*)al_display;
   if (d3d_display->device_lost)
      return;
   if (d3d_display->device->Clear(0, NULL, D3DCLEAR_TARGET,
      D3DCOLOR_ARGB((int)(color->a*255), (int)(color->r*255), (int)(color->g*255), (int)(color->b*255)),
      0, 0) != D3D_OK) {
         ALLEGRO_ERROR("Clear failed\n");
   }
}



static void d3d_draw_pixel(ALLEGRO_DISPLAY *al_display, float x, float y, ALLEGRO_COLOR *color)
{
   ALLEGRO_DISPLAY_D3D *disp = (ALLEGRO_DISPLAY_D3D *)al_display;
   ALLEGRO_COLOR c;

   D3D_TL_VERTEX vertices[1];

   vertices[0].x = x;
   vertices[0].y = y;
   vertices[0].z = 0;

   c.r = color->r;
   c.g = color->g;
   c.b = color->b;
   c.a = color->a;

   vertices[0].diffuse = d3d_al_color_to_d3d(c);

   _al_d3d_set_blender(disp);

   disp->device->SetTexture(0, NULL);

   disp->device->SetFVF(D3DFVF_TL_VERTEX);

   if (disp->device->DrawPrimitiveUP(D3DPT_POINTLIST, 1,
      vertices, sizeof(D3D_TL_VERTEX)) != D3D_OK) {
      ALLEGRO_ERROR("d3d_draw_pixel: DrawPrimitive failed.\n");
      return;
   }
}



static void d3d_flip_display(ALLEGRO_DISPLAY *al_display)
{
   ALLEGRO_DISPLAY_D3D* d3d_display = (ALLEGRO_DISPLAY_D3D*)al_display;
   ALLEGRO_DISPLAY_WIN *win_display = &d3d_display->win_display;
   HRESULT hr;

   if (d3d_display->device_lost)
      return;

   al_lock_mutex(present_mutex);

   d3d_display->device->EndScene();

   hr = d3d_display->device->Present(NULL, NULL, win_display->window, NULL);

   d3d_display->device->BeginScene();

   al_unlock_mutex(present_mutex);

   if (hr == D3DERR_DEVICELOST) {
      d3d_display->device_lost = true;
      return;
   }
   else {
      _al_d3d_prepare_bitmaps_for_reset(d3d_display);
   }
}

static void d3d_update_display_region(ALLEGRO_DISPLAY *al_display,
   int x, int y,
   int width, int height)
{
   ALLEGRO_DISPLAY_D3D* d3d_display = (ALLEGRO_DISPLAY_D3D*)al_display;
   ALLEGRO_DISPLAY_WIN *win_display = &d3d_display->win_display;
   HRESULT hr;
   RGNDATA *rgndata;

   if (d3d_display->device_lost)
      return;

   if (d3d_display->single_buffer) {
      RECT rect;

      rect.left = x;
      rect.right = x+width;
      rect.top = y;
      rect.bottom = y+height;

      rgndata = (RGNDATA *)al_malloc(sizeof(RGNDATA)+sizeof(RECT)-1);
      rgndata->rdh.dwSize = sizeof(RGNDATAHEADER);
      rgndata->rdh.iType = RDH_RECTANGLES;
      rgndata->rdh.nCount = 1;
      rgndata->rdh.nRgnSize = sizeof(RECT);
      memcpy(&rgndata->rdh.rcBound, &rect, sizeof(RECT));
      memcpy(rgndata->Buffer, &rect, sizeof(RECT));

      d3d_display->device->EndScene();

      hr = d3d_display->device->Present(&rect, &rect, win_display->window, rgndata);

      d3d_display->device->BeginScene();

      al_free(rgndata);

      if (hr == D3DERR_DEVICELOST) {
         d3d_display->device_lost = true;
         return;
      }

   }
   else {
      d3d_flip_display(al_display);
   }
}

/*
 * Sets a clipping rectangle
 */
void _al_d3d_set_bitmap_clip(ALLEGRO_BITMAP *bitmap)
{
   ALLEGRO_DISPLAY_D3D *disp = ((ALLEGRO_BITMAP_D3D *)bitmap)->display;
   RECT rect;

   if (!disp)
      return;

   if (bitmap->parent) {
      rect.left = bitmap->xofs + bitmap->cl;
      rect.right = bitmap->xofs + bitmap->cr_excl;
      rect.top = bitmap->yofs + bitmap->ct;
      rect.bottom = bitmap->yofs + bitmap->cb_excl;
   }
   else {
      rect.left = bitmap->cl;
      rect.right = bitmap->cr_excl;
      rect.top = bitmap->ct;
      rect.bottom = bitmap->cb_excl;
   }

   if (rect.left == 0 && rect.top == 0 && rect.right == disp->win_display.display.w && rect.left == disp->win_display.display.h) {
      disp->device->SetRenderState(D3DRS_SCISSORTESTENABLE, false);
      return;
   }   

   disp->device->SetRenderState(D3DRS_SCISSORTESTENABLE, true);
   disp->device->SetScissorRect(&rect);
}

static bool d3d_resize_display(ALLEGRO_DISPLAY *d, int width, int height)
{
   ALLEGRO_DISPLAY_D3D *disp = (ALLEGRO_DISPLAY_D3D *)d;
   ALLEGRO_DISPLAY_WIN *win_display = &disp->win_display;
   bool ret;
   int full_w, full_h;
   ALLEGRO_MONITOR_INFO mi;
   int adapter = al_get_new_display_adapter();
   if (adapter == -1)
         adapter = 0;
   al_get_monitor_info(adapter, &mi);
   full_w = mi.x2 - mi.x1;
   full_h = mi.y2 - mi.y1;

   if ((d->flags & ALLEGRO_FULLSCREEN_WINDOW) &&
         (full_w != width || full_h != height)) {
      win_display->toggle_w = width;
      win_display->toggle_h = height;
      return true;
   }

   win_display->can_acknowledge = false;

   if (d->flags & ALLEGRO_FULLSCREEN) {
      d3d_destroy_display_internals(disp);
      d->w = width;
      d->h = height;
      win_display->end_thread = false;
      win_display->thread_ended = false;
      /* What's this? */
      ALLEGRO_SYSTEM *system = al_get_system_driver();
      if (system->displays._size <= 1) {
         ffw_set = false;
      }
      if (!d3d_create_display_internals(disp)) {
         //al_free(disp);
         return false;
      }
      al_set_target_bitmap(al_get_backbuffer(d));
      _al_d3d_recreate_bitmap_textures(disp);

      ret = true;
   }
   else {
      RECT win_size;
      WINDOWINFO wi;

      win_size.left = 0;
      win_size.top = 0;
      win_size.right = width;
      win_size.bottom = height;

      wi.cbSize = sizeof(WINDOWINFO);
      GetWindowInfo(win_display->window, &wi);

      AdjustWindowRectEx(&win_size, wi.dwStyle, false, wi.dwExStyle);

      ret = (SetWindowPos(win_display->window, HWND_TOP,
         0, 0,
         win_size.right-win_size.left,
         win_size.bottom-win_size.top,
         SWP_NOMOVE|SWP_NOZORDER)) != 0;

      PostMessage(win_display->window, WM_USER+0, 0, 0);

      if (!(d->flags & ALLEGRO_FULLSCREEN_WINDOW)) {
         win_display->toggle_w = width;
         win_display->toggle_h = height;
      }

      /*
       * The clipping rectangle and bitmap size must be
       * changed to match the new size.
       */
       /*
      al_store_state(&backup, ALLEGRO_STATE_TARGET_BITMAP);
      al_set_target_bitmap(&disp->backbuffer_bmp.bitmap);
      disp->backbuffer_bmp.bitmap.w = width;
      disp->backbuffer_bmp.bitmap.h = height;
      al_set_clipping_rectangle(0, 0, width, height);
      _al_d3d_set_bitmap_clip(&disp->backbuffer_bmp.bitmap);
      al_restore_state(&backup);
      */

      ret = true;
   }
      
   disp->backbuffer_bmp.bitmap.w = width;
   disp->backbuffer_bmp.bitmap.h = height;

   return ret;
}

static bool d3d_acknowledge_resize(ALLEGRO_DISPLAY *d)
{
   WINDOWINFO wi;
   ALLEGRO_DISPLAY_D3D *disp = (ALLEGRO_DISPLAY_D3D *)d;
   ALLEGRO_DISPLAY_WIN *win_display = &disp->win_display;
   int w, h;
   ALLEGRO_STATE state;

   wi.cbSize = sizeof(WINDOWINFO);
   GetWindowInfo(win_display->window, &wi);
   w = wi.rcClient.right - wi.rcClient.left;
   h = wi.rcClient.bottom - wi.rcClient.top;

   if (w > 0 && h > 0) {
      d->w = w;
      d->h = h;
   }

   disp->backbuffer_bmp.bitmap.w = d->w;
   disp->backbuffer_bmp.bitmap.h = d->h;
   disp->backbuffer_bmp.bitmap.cl = 0;
   disp->backbuffer_bmp.bitmap.ct = 0;
   disp->backbuffer_bmp.bitmap.cr_excl = w;
   disp->backbuffer_bmp.bitmap.cb_excl = h;

   disp->do_reset = true;
   while (!disp->reset_done) {
      al_rest(0.001);
   }
   disp->reset_done = false;

//   if (!(d->flags  & ALLEGRO_FULLSCREEN)) {
      al_store_state(&state, ALLEGRO_STATE_DISPLAY | ALLEGRO_STATE_TARGET_BITMAP);
      al_set_target_bitmap(al_get_backbuffer(d));
      al_set_clipping_rectangle(0, 0, d->w, d->h);
      al_restore_state(&state);
//   }

   return disp->reset_success;
}

ALLEGRO_BITMAP *_al_d3d_create_bitmap(ALLEGRO_DISPLAY *d,
   int w, int h)
{
   ALLEGRO_BITMAP_D3D *bitmap = (ALLEGRO_BITMAP_D3D*)al_malloc(sizeof *bitmap);
   int format;
   int flags;

   ASSERT(bitmap);
   (void)h;

   bitmap->bitmap.size = sizeof *bitmap;

   format = al_get_new_bitmap_format();
   flags = al_get_new_bitmap_flags();

   if (!_al_pixel_format_is_real(format)) {
      format = d3d_choose_bitmap_format((ALLEGRO_DISPLAY_D3D *)d, format);
      if (format < 0) {
         return NULL;
      }
   }

   if (_al_format_to_d3d(format) < 0) {
      ALLEGRO_ERROR("Requested bitmap format not supported (%d).\n", format);
      return NULL;
   }

   ALLEGRO_INFO("Chose bitmap format %d\n", format);

   bitmap->bitmap.vt = _al_bitmap_d3d_driver();
   bitmap->bitmap.memory = NULL;
   bitmap->bitmap.format = format;
   bitmap->bitmap.flags = flags;
   bitmap->bitmap.pitch = w * al_get_pixel_size(format);
   al_identity_transform(&bitmap->bitmap.transform);

   bitmap->video_texture = 0;
   bitmap->system_texture = 0;
   bitmap->initialized = false;
   bitmap->is_backbuffer = false;
   bitmap->render_target = NULL;
   bitmap->modified = true;

   bitmap->display = (ALLEGRO_DISPLAY_D3D *)d;

   return &bitmap->bitmap;
}

static ALLEGRO_BITMAP *d3d_create_sub_bitmap(ALLEGRO_DISPLAY *display,
   ALLEGRO_BITMAP *parent, int x, int y, int width, int height)
{
   ALLEGRO_BITMAP_D3D *bitmap = (ALLEGRO_BITMAP_D3D*)al_malloc(sizeof *bitmap);

   (void)x;
   (void)y;
   (void)width;
   (void)height;

   bitmap->texture_w = 0;
   bitmap->texture_h = 0;
   bitmap->video_texture = ((ALLEGRO_BITMAP_D3D *)parent)->video_texture;
   bitmap->system_texture = ((ALLEGRO_BITMAP_D3D *)parent)->system_texture;
   bitmap->initialized = false;
   bitmap->is_backbuffer = ((ALLEGRO_BITMAP_D3D *)parent)->is_backbuffer;
   bitmap->display = (ALLEGRO_DISPLAY_D3D *)display;
   bitmap->render_target = ((ALLEGRO_BITMAP_D3D *)parent)->render_target;
   bitmap->modified = true;

   bitmap->bitmap.vt = parent->vt;
   return (ALLEGRO_BITMAP *)bitmap;
}

static void d3d_set_target_bitmap(ALLEGRO_DISPLAY *display, ALLEGRO_BITMAP *bitmap)
{
   ALLEGRO_BITMAP *target;
   ALLEGRO_BITMAP_D3D *d3d_target;
   ALLEGRO_DISPLAY_D3D *d3d_display = (ALLEGRO_DISPLAY_D3D *)display;

   if (d3d_display->device_lost)
      return;

   if (bitmap->parent) {
      target = bitmap->parent;
   }
   else {
      target = bitmap;
   }
   d3d_target = (ALLEGRO_BITMAP_D3D *)target;

   /* Release the previous target bitmap if it was not the backbuffer */

   ALLEGRO_BITMAP_D3D *currtarget = (ALLEGRO_BITMAP_D3D *)al_get_target_bitmap();
   if (currtarget && !currtarget->is_backbuffer && currtarget->render_target) {
      currtarget->render_target->Release();
      currtarget->render_target = NULL;
   }

   /* Set the render target */
   if (d3d_target->is_backbuffer) {
      d3d_display = d3d_target->display;
      if (d3d_display->device->SetRenderTarget(0, d3d_display->render_target) != D3D_OK) {
         ALLEGRO_ERROR("d3d_set_target_bitmap: Unable to set render target to texture surface.\n");
         return;
      }
      d3d_target->render_target = d3d_display->render_target;
      d3d_display->device->SetDepthStencilSurface(d3d_display->depth_stencil);
      _al_d3d_set_ortho_projection(d3d_display, display->w, display->h);
   }
   else {
      d3d_display = (ALLEGRO_DISPLAY_D3D *)display;
      if (_al_d3d_render_to_texture_supported()) {
         if (d3d_target->video_texture->GetSurfaceLevel(0, &d3d_target->render_target) != D3D_OK) {
            ALLEGRO_ERROR("d3d_set_target_bitmap: Unable to get texture surface level.\n");
            return;
         }
         if (d3d_display->device->SetRenderTarget(0, d3d_target->render_target) != D3D_OK) {
            ALLEGRO_ERROR("d3d_set_target_bitmap: Unable to set render target to texture surface.\n");
            d3d_target->render_target->Release();
            return;
         }
         _al_d3d_set_ortho_projection(d3d_display, d3d_target->texture_w, d3d_target->texture_h);
      }
      if (d3d_display->samples) {
         d3d_display->device->SetDepthStencilSurface(NULL);
      }
   }

   d3d_reset_state(d3d_display);

   _al_d3d_set_bitmap_clip(bitmap);
}

static ALLEGRO_BITMAP *d3d_get_backbuffer(ALLEGRO_DISPLAY *display)
{
   return (ALLEGRO_BITMAP *)&(((ALLEGRO_DISPLAY_D3D *)display)->backbuffer_bmp);
}

static ALLEGRO_BITMAP *d3d_get_frontbuffer(ALLEGRO_DISPLAY *display)
{
   (void)display;
   return NULL;
}

static bool d3d_is_compatible_bitmap(ALLEGRO_DISPLAY *display, ALLEGRO_BITMAP *bitmap)
{
   return display == bitmap->display;
}

static void d3d_switch_out(ALLEGRO_DISPLAY *display)
{
   (void)display;
}

static void d3d_switch_in(ALLEGRO_DISPLAY *display)
{
   (void)display;
}

static bool d3d_wait_for_vsync(ALLEGRO_DISPLAY *display)
{
   ALLEGRO_DISPLAY_D3D *d3d_display;
   D3DRASTER_STATUS status;

   if (!d3d_can_wait_for_vsync)
      return false;

   d3d_display = (ALLEGRO_DISPLAY_D3D *)display;

   do {
      d3d_display->device->GetRasterStatus(0, &status);
   } while (!status.InVBlank);

   return true;
}


/* Exposed stuff */

/* Function: al_get_d3d_device
 */
LPDIRECT3DDEVICE9 al_get_d3d_device(ALLEGRO_DISPLAY *display)
{
   ALLEGRO_DISPLAY_D3D *d3d_display = (ALLEGRO_DISPLAY_D3D *)display;
   return d3d_display->device;
}

/* Function: al_get_d3d_system_texture
 */
LPDIRECT3DTEXTURE9 al_get_d3d_system_texture(ALLEGRO_BITMAP *bitmap)
{
   return ((ALLEGRO_BITMAP_D3D *)bitmap)->system_texture;
}

/* Function: al_get_d3d_video_texture
 */
LPDIRECT3DTEXTURE9 al_get_d3d_video_texture(ALLEGRO_BITMAP *bitmap)
{
   return ((ALLEGRO_BITMAP_D3D *)bitmap)->video_texture;
}

/* Function: al_get_d3d_texture_position
 */
void al_get_d3d_texture_position(ALLEGRO_BITMAP *bitmap, int *u, int *v)
{
   *u = bitmap->xofs;
   *v = bitmap->yofs;
}

static void d3d_set_window_position(ALLEGRO_DISPLAY *display, int x, int y)
{
   _al_win_set_window_position(((ALLEGRO_DISPLAY_WIN *)display)->window, x, y);
}

static void d3d_get_window_position(ALLEGRO_DISPLAY *display, int *x, int *y)
{
   if (display->flags & ALLEGRO_FULLSCREEN) {
      ALLEGRO_MONITOR_INFO info;
      ALLEGRO_DISPLAY_WIN *win_display = (ALLEGRO_DISPLAY_WIN *)display;
      al_get_monitor_info(win_display->adapter, &info);
      *x = info.x1;
      *y = info.y1;
   }
   else {
      _al_win_get_window_position(((ALLEGRO_DISPLAY_WIN *)display)->window, x, y);
   }
}


static void d3d_shutdown(void)
{
   if (eds_list) {
      d3d_destroy_display_format_list();
      eds_list = NULL;
   }
   _al_d3d->Release();
   al_destroy_mutex(present_mutex);
   al_destroy_mutex(_al_d3d_lost_device_mutex);

   _al_d3d_bmp_destroy();

   al_free(vt);
   vt = NULL;
}

static void* d3d_prepare_vertex_cache(ALLEGRO_DISPLAY* disp, 
                                      int num_new_vertices)
{
   disp->num_cache_vertices += num_new_vertices;
   if(!disp->vertex_cache) {  
      disp->vertex_cache = al_malloc(num_new_vertices * sizeof(D3D_TL_VERTEX));
      
      disp->vertex_cache_size = num_new_vertices;
   } else if (disp->num_cache_vertices > disp->vertex_cache_size) {
      disp->vertex_cache = al_realloc(disp->vertex_cache, 
                              2 * disp->num_cache_vertices * sizeof(D3D_TL_VERTEX));
                              
      disp->vertex_cache_size = 2 * disp->num_cache_vertices;
   }
   return (D3D_TL_VERTEX*)disp->vertex_cache + 
         (disp->num_cache_vertices - num_new_vertices);
}

static void d3d_flush_vertex_cache(ALLEGRO_DISPLAY* disp)
{
   if(!disp->vertex_cache)
      return;
   if(disp->num_cache_vertices == 0)
      return;
      
   ALLEGRO_DISPLAY_D3D* d3d_disp = (ALLEGRO_DISPLAY_D3D*)disp;
   
   if (d3d_disp->device->SetTexture(0,
         (IDirect3DBaseTexture9 *)((ALLEGRO_BITMAP_D3D*)disp->cache_texture)->video_texture) != D3D_OK) {
      ALLEGRO_ERROR("d3d_flush_vertex_cache: SetTexture failed.\n");
      return;
   }

   d3d_disp->device->SetFVF(D3DFVF_TL_VERTEX);
   
   if (d3d_disp->device->DrawPrimitiveUP(D3DPT_TRIANGLELIST, disp->num_cache_vertices / 3,
      (D3D_TL_VERTEX*)disp->vertex_cache, sizeof(D3D_TL_VERTEX)) != D3D_OK) {
      ALLEGRO_ERROR("d3d_flush_vertex_cache: DrawPrimitive failed.\n");
      return;
   }
   
   disp->num_cache_vertices = 0;
   d3d_disp->device->SetTexture(0, NULL);
}

static void d3d_update_transformation(ALLEGRO_DISPLAY* disp, ALLEGRO_BITMAP *target)
{
   ALLEGRO_DISPLAY_D3D* d3d_disp = (ALLEGRO_DISPLAY_D3D*)disp;
   D3DMATRIX matrix;
   ALLEGRO_TRANSFORM tmp_transform;

   al_copy_transform(&tmp_transform, &target->transform);

   if (target->parent) {
      al_translate_transform(&tmp_transform, target->xofs, target->yofs);
   }

   memcpy(matrix.m[0], tmp_transform.m[0], 16 * sizeof(float));
   matrix.m[3][0] -= 0.5;
   matrix.m[3][1] -= 0.5;
   
   d3d_disp->device->SetTransform(D3DTS_VIEW, &matrix);
}

/* Obtain a reference to this driver. */
ALLEGRO_DISPLAY_INTERFACE *_al_display_d3d_driver(void)
{
   if (vt)
      return vt;

   vt = (ALLEGRO_DISPLAY_INTERFACE *)al_malloc(sizeof *vt);
   memset(vt, 0, sizeof *vt);

   vt->create_display = d3d_create_display;
   vt->destroy_display = d3d_destroy_display;
   vt->set_current_display = d3d_set_current_display;
   vt->clear = d3d_clear;
   vt->draw_pixel = d3d_draw_pixel;
   vt->flip_display = d3d_flip_display;
   vt->update_display_region = d3d_update_display_region;
   vt->acknowledge_resize = d3d_acknowledge_resize;
   vt->resize_display = d3d_resize_display;
   vt->create_bitmap = _al_d3d_create_bitmap;
   vt->set_target_bitmap = d3d_set_target_bitmap;
   vt->get_backbuffer = d3d_get_backbuffer;
   vt->get_frontbuffer = d3d_get_frontbuffer;
   vt->is_compatible_bitmap = d3d_is_compatible_bitmap;
   vt->switch_out = d3d_switch_out;
   vt->switch_in = d3d_switch_in;
   vt->draw_memory_bitmap_region = NULL;
   vt->create_sub_bitmap = d3d_create_sub_bitmap;
   vt->wait_for_vsync = d3d_wait_for_vsync;

   vt->set_mouse_cursor = _al_win_set_mouse_cursor;
   vt->set_system_mouse_cursor = _al_win_set_system_mouse_cursor;
   vt->show_mouse_cursor = _al_win_show_mouse_cursor;
   vt->hide_mouse_cursor = _al_win_hide_mouse_cursor;

   vt->set_icon = _al_win_set_display_icon;
   vt->set_window_position = d3d_set_window_position;
   vt->get_window_position = d3d_get_window_position;
   vt->toggle_display_flag = _al_win_toggle_display_flag;
   vt->set_window_title = _al_win_set_window_title;
   vt->shutdown = d3d_shutdown;
   
   vt->flush_vertex_cache = d3d_flush_vertex_cache;
   vt->prepare_vertex_cache = d3d_prepare_vertex_cache;
   
   vt->update_transformation = d3d_update_transformation;

   return vt;
}

int _al_d3d_get_num_display_modes(int format, int refresh_rate, int flags)
{
   UINT num_modes;
   UINT i, j;
   D3DDISPLAYMODE display_mode;
   int matches = 0;

   (void)flags;

   /* If any, go through all formats */
   if (!_al_pixel_format_is_real(format)) {
      j = 0;
   }
   /* Else find the matching format */
   else {
      for (j = 0; allegro_formats[j] != -1; j++) {
         if (allegro_formats[j] == format)
            break;
      }
      if (allegro_formats[j] == -1)
         return 0;
   }
   
   for (; allegro_formats[j] != -1; j++) {
      int adapter = al_get_new_display_adapter();
      if (adapter == -1)
         adapter = 0;

      if (!_al_pixel_format_is_real(allegro_formats[j]) || _al_format_has_alpha(allegro_formats[j]))
         continue;

      num_modes = _al_d3d->GetAdapterModeCount(adapter, (D3DFORMAT)d3d_formats[j]);

      for (i = 0; i < num_modes; i++) {
         if (_al_d3d->EnumAdapterModes(adapter, (D3DFORMAT)d3d_formats[j], i, &display_mode) != D3D_OK) {
            return matches;
         }
         if (refresh_rate && display_mode.RefreshRate != (unsigned)refresh_rate)
            continue;
         matches++;
      }

      if (_al_pixel_format_is_real(format))
         break;
   }

   return matches;
}

ALLEGRO_DISPLAY_MODE *_al_d3d_get_display_mode(int index, int format,
   int refresh_rate, int flags, ALLEGRO_DISPLAY_MODE *mode)
{
   UINT num_modes;
   UINT i, j;
   D3DDISPLAYMODE display_mode;
   int matches = 0;

   (void)flags;

   /* If any, go through all formats */
   if (!_al_pixel_format_is_real(format)) {
      j = 0;
   }
   /* Else find the matching format */
   else {
      for (j = 0; allegro_formats[j] != -1; j++) {
         if (allegro_formats[j] == format)
            break;
      }
      if (allegro_formats[j] == -1)
         return NULL;
   }

   for (; allegro_formats[j] != -1; j++) {
      int adapter = al_get_new_display_adapter();
      if (adapter == -1)
         adapter = 0;

      if (!_al_pixel_format_is_real(allegro_formats[j]) || _al_format_has_alpha(allegro_formats[j]))
         continue;

      num_modes = _al_d3d->GetAdapterModeCount(adapter, (D3DFORMAT)d3d_formats[j]);

      for (i = 0; i < num_modes; i++) {
         if (_al_d3d->EnumAdapterModes(adapter, (D3DFORMAT)d3d_formats[j], i, &display_mode) != D3D_OK) {
            return NULL;
         }
         if (refresh_rate && display_mode.RefreshRate != (unsigned)refresh_rate)
            continue;
         if (matches == index) {
            mode->width = display_mode.Width;
            mode->height = display_mode.Height;
            mode->format = allegro_formats[j];
            mode->refresh_rate = display_mode.RefreshRate;
            return mode;
         }
         matches++;
      }

      if (_al_pixel_format_is_real(format))
         break;
   }

   return mode;
}


int _al_d3d_get_num_video_adapters(void)
{
   return _al_d3d->GetAdapterCount();
   //return num_video_adapters;
}

void _al_d3d_get_monitor_info(int adapter, ALLEGRO_MONITOR_INFO *info)
{
   HMONITOR mon = _al_d3d->GetAdapterMonitor(adapter);
   MONITORINFO mi;

   if (!mon) {
      info->x1 =
         info->y1 =
         info->x2 =
         info->y2 = -1;
   }
   else {
      mi.cbSize = sizeof(mi);
      GetMonitorInfo(mon, &mi);
      info->x1 = mi.rcMonitor.left;
      info->y1 = mi.rcMonitor.top;
      info->x2 = mi.rcMonitor.right;
      info->y2 = mi.rcMonitor.bottom;
   }
}

/* vim: set sts=3 sw=3 et: */
