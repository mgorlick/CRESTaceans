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
 *      OpenGL extensions management subsystem
 *
 *      By Elias Pschernig and Milan Mimica.
 *      Based on AllegroGL extensions management.
 */

/* Title: OpenGL extensions
 */


#include "allegro5/allegro5.h"
#include "allegro5/allegro_opengl.h"
#include "allegro5/display.h"
#include "allegro5/opengl/gl_ext.h"
#include "allegro5/internal/aintern_opengl.h"
#include "allegro5/internal/aintern_display.h"
#include "allegro5/internal/aintern_system.h"
#include <stdio.h>
#include <math.h>

/* We need some driver specific details not worth of a vtable entry. */
#if defined ALLEGRO_WINDOWS
   #include "../win/wgl.h"
#elif defined ALLEGRO_UNIX && !defined ALLEGRO_EXCLUDE_GLX
   #include "allegro5/internal/aintern_xglx.h"
#endif

#include <string.h>
#if defined ALLEGRO_MACOSX
#include <OpenGL/glu.h>
#elif !defined ALLEGRO_GP2XWIZ && !defined ALLEGRO_IPHONE
#include <GL/glu.h>
#endif

ALLEGRO_DEBUG_CHANNEL("opengl")


#ifdef ALLEGRO_HAVE_DYNAMIC_LINK
   #include <dlfcn.h>
   /* Handle for dynamic library libGL.so */
   static void *__libgl_handle = NULL;
   /* Pointer to glXGetProcAddressARB */
   typedef void *(*GLXGETPROCADDRESSARBPROC) (const GLubyte *);
   static GLXGETPROCADDRESSARBPROC alXGetProcAddress;
#else /* #ifdef ALLEGRO_HAVE_DYNAMIC_LINK */
   /* Tries static linking */
   /* FIXME: set ALLEGRO_GLXGETPROCADDRESSARB on configure time, if
    * glXGetProcAddressARB must be used!
    */
   #if defined ALLEGRO_GLXGETPROCADDRESSARB
      #define alXGetProcAddress glXGetProcAddressARB
   #elif defined ALLEGRO_GP2XWIZ
      #define alXGetProcAddress eglGetProcAddress
   #else
      #define alXGetProcAddress glXGetProcAddress
   #endif
#endif /* #ifdef ALLEGRO_HAVE_DYNAMIC_LINK */


#ifdef ALLEGRO_MACOSX
   #include <CoreFoundation/CoreFoundation.h>
   static CFBundleRef opengl_bundle_ref;
#endif


/* Define the GL API pointers.
 * Example:
 * _ALLEGRO_glBlendEquation_t _al_glBlendEquation = NULL;
 */
#define AGL_API(type, name, args) _ALLEGRO_gl##name##_t _al_gl##name = NULL;
#	include "allegro5/opengl/GLext/gl_ext_api.h"
#undef AGL_API
#ifdef ALLEGRO_WINDOWS
#define AGL_API(type, name, args) _ALLEGRO_wgl##name##_t _al_wgl##name = NULL;
#	include "allegro5/opengl/GLext/wgl_ext_api.h"
#undef AGL_API
#elif defined ALLEGRO_UNIX
#define AGL_API(type, name, args) _ALLEGRO_glX##name##_t _al_glX##name = NULL;
#	include "allegro5/opengl/GLext/glx_ext_api.h"
#undef AGL_API
#endif



/* Reads version info out of glGetString(GL_VERSION) */
static float _al_ogl_version(void)
{
#ifndef ALLEGRO_IPHONE
   ALLEGRO_CONFIG *cfg;
   const char *str;
   int major, minor1, minor2;

   cfg = al_get_system_config();
   if (cfg) {
      char const *value = al_get_config_value(cfg,
	 "opengl", "force_opengl_version");
      if (value) {
         float v = strtod(value, NULL);
         ALLEGRO_WARN("OpenGL version forced to %.1f.\n", v);
         return v;
      }
   }

   str = (const char *)glGetString(GL_VERSION);
   if (str) {
      float v;
      major = minor1 = minor2 = 0;
      sscanf(str, "%d.%d.%d", &major, &minor1, &minor2);
      v = major;
      v += minor1 / 10.f;
      if (minor2 > 0)
        v += minor2 / pow(10, 2 + floor(log10(minor2)));
      return v;
   }
   else {
      /* The OpenGL driver does not return a version
       * number. However it probably supports at least OpenGL 1.0
       */
      return 1.0;
   }
#else
   const char *s = glGetString(GL_VERSION);
   if (strstr(s, "2.0"))
      return 2.0;
   else if (strstr(s, "1.1"))
      return 1.5;
   else
      return 1.3;
#endif
}



/* print_extensions:
 * Given a string containing extensions (i.e. a NULL terminated string where
 * each extension are separated by a space and which names do not contain any
 * space)
 */
static void print_extensions(char const *extension)
{
   char buf[80];
   char *start;
   ASSERT(extension);

   while (*extension != '\0') {
      start = buf;
      strncpy(buf, extension, 80);
      while ((*start != ' ') && (*start != '\0')) {
         extension++;
         start++;
      }
      *start = '\0';
      extension++;
      ALLEGRO_DEBUG("%s\n", buf);
   }
}



/* Function: al_get_opengl_version
 */
float al_get_opengl_version(void)
{
   ALLEGRO_DISPLAY *ogl_disp;

   ogl_disp = (ALLEGRO_DISPLAY*)al_get_current_display();
   if (!ogl_disp || !ogl_disp->ogl_extras)
      return 0.0f;

   return ogl_disp->ogl_extras->ogl_info.version;
}



/* Create the extension list */
static ALLEGRO_OGL_EXT_LIST *create_extension_list(void)
{
   ALLEGRO_OGL_EXT_LIST *ret = al_malloc(sizeof(ALLEGRO_OGL_EXT_LIST));

   if (!ret) {
      return NULL;
   }

   memset(ret, 0, sizeof(ALLEGRO_OGL_EXT_LIST));

   return ret;
}



/* Create the extension API table */
static ALLEGRO_OGL_EXT_API *create_extension_api_table(void)
{
   ALLEGRO_OGL_EXT_API *ret = al_malloc(sizeof(ALLEGRO_OGL_EXT_API));

   if (!ret) {
      return NULL;
   }

   memset(ret, 0, sizeof(ALLEGRO_OGL_EXT_API));

   return ret;
}



/* Load the extension API addresses into the table.
 * Should only be done on context creation.
 */
static void load_extensions(ALLEGRO_OGL_EXT_API *ext)
{
   if (!ext) {
      return;
   }
#ifdef ALLEGRO_UNIX
#ifdef ALLEGRO_HAVE_DYNAMIC_LINK
   if (!alXGetProcAddress) {
      return;
   }
#endif
#endif

#ifdef ALLEGRO_WINDOWS

   #define AGL_API(type, name, args)                                 \
      ext->name = (_ALLEGRO_gl##name##_t)wglGetProcAddress("gl" #name); \
      if (ext->name) { ALLEGRO_DEBUG("gl" #name " successfully loaded\n"); }

      #include "allegro5/opengl/GLext/gl_ext_api.h"

   #undef AGL_API

   #define AGL_API(type, name, args)                                  \
      ext->name = (_ALLEGRO_wgl##name##_t)wglGetProcAddress("wgl" #name); \
      if (ext->name) { ALLEGRO_DEBUG("wgl" #name " successfully loaded\n"); }

      #include "allegro5/opengl/GLext/wgl_ext_api.h"

   #undef AGL_API

#elif defined ALLEGRO_UNIX

   #define AGL_API(type, name, args)                                               \
      ext->name = (_ALLEGRO_gl##name##_t)alXGetProcAddress((const GLubyte*)"gl" #name);  \
      if (ext->name) { ALLEGRO_DEBUG("gl" #name " successfully loaded\n"); }

      #include "allegro5/opengl/GLext/gl_ext_api.h"

   #undef AGL_API

   #define AGL_API(type, name, args)                                               \
      ext->name = (_ALLEGRO_glX##name##_t)alXGetProcAddress((const GLubyte*)"glX" #name); \
      if (ext->name) { ALLEGRO_DEBUG("glX" #name " successfully loaded\n"); }

      #include "allegro5/opengl/GLext/glx_ext_api.h"

   #undef AGL_API

#elif defined ALLEGRO_MACOSX

#define AGL_API(type, name, args)                                                                 \
      ext->name = (_ALLEGRO_gl##name##_t)CFBundleGetFunctionPointerForName(opengl_bundle_ref, CFSTR("gl" # name)); \
      if (ext->name) { ALLEGRO_DEBUG("gl" #name " successfully loaded\n"); }

      #include "allegro5/opengl/GLext/gl_ext_api.h"

   #undef AGL_API

#endif
}



/* Set the GL API pointers to the current table 
 * Should only be called on context switches.
 */
void _al_ogl_set_extensions(ALLEGRO_OGL_EXT_API *ext)
{
   if (!ext) {
      return;
   }

#define AGL_API(type, name, args) _al_gl##name = ext->name;
#	include "allegro5/opengl/GLext/gl_ext_api.h"
#undef AGL_API

#ifdef ALLEGRO_WINDOWS
#define AGL_API(type, name, args) _al_wgl##name = ext->name;
#	include "allegro5/opengl/GLext/wgl_ext_api.h"
#undef AGL_API

#elif defined ALLEGRO_UNIX
#define AGL_API(type, name, args) _al_glX##name = ext->name;
#	include "allegro5/opengl/GLext/glx_ext_api.h"
#undef AGL_API
#endif
}



/* Destroys the extension API table */
static void destroy_extension_api_table(ALLEGRO_OGL_EXT_API *ext)
{
   if (ext) {
      al_free(ext);
   }
}



/* Destroys the extension list */
static void destroy_extension_list(ALLEGRO_OGL_EXT_LIST *list)
{
   if (list) {
      al_free(list);
   }
}



/* _al_ogl_look_for_an_extension:
 * This function has been written by Mark J. Kilgard in one of his
 * tutorials about OpenGL extensions
 */
int _al_ogl_look_for_an_extension(const char *name, const GLubyte *extensions)
{
   const GLubyte *start;
   GLubyte *where, *terminator;
   ASSERT(extensions);

   /* Extension names should not have spaces. */
   where = (GLubyte *) strchr(name, ' ');
   if (where || *name == '\0')
      return false;
   /* It takes a bit of care to be fool-proof about parsing the
    * OpenGL extensions string. Don't be fooled by sub-strings, etc.
    */
   start = extensions;
   for (;;) {
      where = (GLubyte *) strstr((const char *)start, name);
      if (!where)
         break;
      terminator = where + strlen(name);
      if (where == start || *(where - 1) == ' ')
         if (*terminator == ' ' || *terminator == '\0')
            return true;
      start = terminator;
   }
   return false;
}



static int _ogl_is_extension_supported(const char *extension,
                                       ALLEGRO_DISPLAY *disp)
{
   int ret = 0;
   GLubyte const *ext_str;
   (void)disp;

#if defined ALLEGRO_GP2XWIZ
   return false;
#endif

   if (al_get_opengl_version() >= 3) {
      int i;
      GLint ext_cnt;
      glGetIntegerv(GL_NUM_EXTENSIONS, &ext_cnt);
      for (i = 0; i < ext_cnt; i++) {
         ext_str = glGetStringi(GL_EXTENSIONS, i);
         if (ext_str && !strcmp(extension, (char*)ext_str)) {
            ret = 1;
            break;
         }
      }
   }
   else {
      ext_str = glGetString(GL_EXTENSIONS);
      if (!ext_str)
         return false;
      ret = _al_ogl_look_for_an_extension(extension, ext_str);
   }

#ifdef ALLEGRO_WINDOWS
   if (!ret && strncmp(extension, "WGL", 3) == 0) {
      ALLEGRO_DISPLAY_WGL *wgl_disp = (void*)disp;
      _ALLEGRO_wglGetExtensionsStringARB_t _wglGetExtensionsStringARB;

      if (!wgl_disp->dc)
         return false;

      _wglGetExtensionsStringARB = (void *)
         wglGetProcAddress("wglGetExtensionsStringARB");
      if (_wglGetExtensionsStringARB) {
         ret = _al_ogl_look_for_an_extension(extension, (const GLubyte *)
                                     _wglGetExtensionsStringARB(wgl_disp->dc));
      }
   }

#elif defined ALLEGRO_UNIX && !defined ALLEGRO_EXCLUDE_GLX
   if (!ret && strncmp(extension, "GLX", 3) == 0) {
      ALLEGRO_SYSTEM_XGLX *sys = (void*)al_get_system_driver();
      ALLEGRO_DISPLAY_XGLX *glx_disp = (void*)disp;
      char const *ext;

      if (!sys->gfxdisplay)
         return false;

      ext = glXQueryExtensionsString(sys->gfxdisplay, glx_disp->xscreen);
      if (!ext) {
         /* work around driver bugs? */
         ext = "";
      }
      ret = _al_ogl_look_for_an_extension(extension, (const GLubyte *)ext);
   }
#endif

   return ret;
}



static bool _ogl_is_extension_with_version_supported(
   const char *extension, ALLEGRO_DISPLAY *disp, float ver)
{
   ALLEGRO_CONFIG *cfg;
   char const *value;

  /* For testing purposes, any OpenGL extension can be disable in
    * the config by using something like:
    * 
    * [opengl_disabled_extensions]
    * GL_ARB_texture_non_power_of_two=0
    * GL_EXT_framebuffer_object=0
    * 
    */
   cfg = al_get_system_config();
   if (cfg) {
      value = al_get_config_value(cfg,
         "opengl_disabled_extensions", extension);
      if (value) {
         ALLEGRO_WARN("%s found in [opengl_disabled_extensions].\n",
            extension);
         return false;
      }
   }

   /* If the extension is included in the OpenGL version, there is no
    * need to check the extensions list.
    */
   if (disp->ogl_extras->ogl_info.version >= ver && ver > 0)
      return true;
      
   return _ogl_is_extension_supported(extension, disp);
}



/* Function: al_is_opengl_extension_supported
 */
int al_is_opengl_extension_supported(const char *extension)
{
   ALLEGRO_DISPLAY *disp;
   
   disp = al_get_current_display();
   if (!disp)
      return false;

   if (!(disp->flags & ALLEGRO_OPENGL))
      return false;

   return _ogl_is_extension_supported(extension, (ALLEGRO_DISPLAY*)disp);
}



/* Function: al_get_opengl_proc_address
 */
void *al_get_opengl_proc_address(const char *name)
{
   void *symbol = NULL;
#ifdef ALLEGRO_MACOSX
   CFStringRef function;
#endif
   ALLEGRO_DISPLAY *disp;
   
   disp = al_get_current_display();
   if (!disp)
      return false;

   if (!(disp->flags & ALLEGRO_OPENGL))
      return false;

#if defined ALLEGRO_WINDOWS
   /* For once Windows is the easiest platform to use :)
    * It provides a standardized way to get a function address
    * But of course there is a drawback : the symbol is only valid
    * under the current context :P
    */
   {
      ALLEGRO_DISPLAY_WGL *wgl_disp = (void*)disp;

      if (!wgl_disp->dc)
         return false;

      symbol = wglGetProcAddress(name);
   }
#elif defined ALLEGRO_UNIX
#if defined ALLEGRO_HAVE_DYNAMIC_LINK
   if (alXGetProcAddress)
#endif
   {
      /* This is definitely the *good* way on Unix to get a GL proc
       * address. Unfortunately glXGetProcAddress is an extension
       * and may not be available on all platforms
       */
#ifdef ALLEGRO_GP2XWIZ
      symbol = alXGetProcAddress(name);
#else
      symbol = alXGetProcAddress((const GLubyte *)name);
#endif
   }
#elif defined ALLEGRO_HAVE_DYNAMIC_LINK
   else {
      /* Hack if glXGetProcAddress is not available :
       * we try to find the symbol into libGL.so
       */
      if (__al_handle) {
         symbol = dlsym(__al_handle, name);
      }
   }
#elif defined ALLEGRO_MACOSX
   function = CFStringCreateWithCString(kCFAllocatorDefault, name,
                                        kCFStringEncodingASCII);
   if (function) {
      symbol =
          CFBundleGetFunctionPointerForName(opengl_bundle_ref, function);
      CFRelease(function);
   }
#endif

   if (!symbol) {

#if defined ALLEGRO_HAVE_DYNAMIC_LINK
      if (!alXGetProcAddress) {
         ALLEGRO_WARN("get_proc_address: libdl::dlsym: %s\n", dlerror());
      }
#endif

      ALLEGRO_WARN("get_proc_address : Unable to load symbol %s\n", name);
   }
   else {
      ALLEGRO_DEBUG("get_proc_address : Symbol %s successfully loaded\n", name);
   }
   return symbol;
}



/* fill_in_info_struct:
 *  Fills in the OPENGL_INFO info struct for blacklisting video cards.
 */
static void fill_in_info_struct(const GLubyte *rendereru, OPENGL_INFO *info)
{
   const char *renderer = (const char *)rendereru;
   ASSERT(renderer);

   /* Some cards are "special"... */
   if (strstr(renderer, "3Dfx/Voodoo")) {
      info->is_voodoo = 1;
   }
   else if (strstr(renderer, "Matrox G200")) {
      info->is_matrox_g200 = 1;
   }
   else if (strstr(renderer, "RagePRO")) {
      info->is_ati_rage_pro = 1;
   }
   else if (strstr(renderer, "RADEON 7000")) {
      info->is_ati_radeon_7000 = 1;
   }
   else if (strstr(renderer, "Mesa DRI R200")) {
      info->is_ati_r200_chip = 1;
   }

   if ((strncmp(renderer, "3Dfx/Voodoo3 ", 13) == 0)
       || (strncmp(renderer, "3Dfx/Voodoo2 ", 13) == 0)
       || (strncmp(renderer, "3Dfx/Voodoo ", 12) == 0)) {
      info->is_voodoo3_and_under = 1;
   }

   /* Read OpenGL properties */
   info->version = _al_ogl_version();
   ALLEGRO_INFO("Assumed OpenGL version: %f\n", info->version);

   return;
}



/* _al_ogl_manage_extensions:
 * This functions fills the extensions API table and extension list
 * structures and displays on the log file which extensions are available.
 */
void _al_ogl_manage_extensions(ALLEGRO_DISPLAY *gl_disp)
{
   //const GLubyte *buf;
#if defined ALLEGRO_MACOSX
   CFURLRef bundle_url;
#endif
   ALLEGRO_OGL_EXT_API *ext_api;
   ALLEGRO_OGL_EXT_LIST *ext_list;

   /* Print out OpenGL extensions */
   ALLEGRO_DEBUG("OpenGL Extensions:\n");
   print_extensions((char const *)glGetString(GL_EXTENSIONS));

   /* Print out GLU version */
   //buf = gluGetString(GLU_VERSION);
   //TRACE(PREFIX_I "GLU Version : %s\n", buf);

#ifdef ALLEGRO_HAVE_DYNAMIC_LINK
   /* Get glXGetProcAddress entry */
   __libgl_handle = dlopen("libGL.so", RTLD_LAZY);
   if (__libgl_handle) {
      alXGetProcAddress = (GLXGETPROCADDRESSARBPROC) dlsym(__libgl_handle,
                                                            "glXGetProcAddressARB");
      if (!alXGetProcAddress) {
         alXGetProcAddress = (GLXGETPROCADDRESSARBPROC) dlsym(__libgl_handle,
                                                             "glXGetProcAddress");
	      if (!alXGetProcAddress) {
		 alXGetProcAddress = (GLXGETPROCADDRESSARBPROC) dlsym(__libgl_handle,
								     "eglGetProcAddress");
	      }
      }
   }
   else {
      ALLEGRO_WARN("Failed to dlopen libGL.so : %s\n", dlerror());
   }
   ALLEGRO_INFO("glXGetProcAddress Extension: %s\n",
         alXGetProcAddress ? "Supported" : "Unsupported");
#elif defined ALLEGRO_UNIX
#ifdef ALLEGROGL_GLXGETPROCADDRESSARB
   ALLEGRO_INFO("glXGetProcAddressARB Extension: supported\n");
#else
   ALLEGRO_INFO("glXGetProcAddress Extension: supported\n");
#endif
#endif

#ifdef ALLEGRO_MACOSX
   bundle_url = CFURLCreateWithFileSystemPath(kCFAllocatorDefault,
                                              CFSTR
                                              ("/System/Library/Frameworks/OpenGL.framework"),
                                              kCFURLPOSIXPathStyle, true);
   opengl_bundle_ref = CFBundleCreate(kCFAllocatorDefault, bundle_url);
   CFRelease(bundle_url);
#endif

#if defined ALLEGRO_UNIX && !defined ALLEGRO_EXCLUDE_GLX
   ALLEGRO_DEBUG("GLX Extensions:\n");
   ALLEGRO_SYSTEM_XGLX *glx_sys = (void*)al_get_system_driver();
   ALLEGRO_DISPLAY_XGLX *glx_disp = (void *)gl_disp;
   char const *ext = glXQueryExtensionsString(
      glx_sys->gfxdisplay, glx_disp->xscreen);
   if (!ext) {
      /* work around driver bugs? */
      ext = "";
   }
   print_extensions(ext);
#endif

   fill_in_info_struct(glGetString(GL_RENDERER), &(gl_disp->ogl_extras->ogl_info));

   /* Create & load extension API table */
   ext_api = create_extension_api_table();
   load_extensions(ext_api);
   gl_disp->ogl_extras->extension_api = ext_api;

   /* Create the list of supported extensions. */
   ext_list = create_extension_list();
   gl_disp->ogl_extras->extension_list = ext_list;

   /* Fill the list. */
#define AGL_EXT(name, ver) { \
      ext_list->ALLEGRO_GL_##name = \
         _ogl_is_extension_with_version_supported("GL_" #name, gl_disp, ver); \
   }
   #include "allegro5/opengl/GLext/gl_ext_list.h"
#undef AGL_EXT

#ifdef ALLEGRO_UNIX
#define AGL_EXT(name, ver) { \
      ext_list->ALLEGRO_GLX_##name = \
         _ogl_is_extension_with_version_supported("GLX_" #name, gl_disp, ver); \
   }
   #include "allegro5/opengl/GLext/glx_ext_list.h"
#undef AGL_EXT
#elif defined ALLEGRO_WINDOWS
#define AGL_EXT(name, ver) { \
      ext_list->ALLEGRO_WGL_##name = \
         _ogl_is_extension_with_version_supported("WGL_" #name, gl_disp, ver); \
   }
   #include "allegro5/opengl/GLext/wgl_ext_list.h"
#undef AGL_EXT
#endif

    /* TODO: use these somewhere */
#if 0
   for (i = 0; i < 5; i++) {
      __allegro_gl_texture_read_format[i] = -1;
      __allegro_gl_texture_components[i] = GL_RGB;
   }
   __allegro_gl_texture_read_format[3] = GL_UNSIGNED_BYTE;
   __allegro_gl_texture_read_format[4] = GL_UNSIGNED_BYTE;
   __allegro_gl_texture_components[4] = GL_RGBA;
#endif /* #if 0 */

   /* Get number of texture units */
   if (ext_list->ALLEGRO_GL_ARB_multitexture) {
      glGetIntegerv(GL_MAX_TEXTURE_UNITS,
                    (GLint *) & gl_disp->ogl_extras->ogl_info.num_texture_units);
   }
   else {
      gl_disp->ogl_extras->ogl_info.num_texture_units = 1;
   }

   /* Get max texture size */
   glGetIntegerv(GL_MAX_TEXTURE_SIZE,
                 (GLint *) & gl_disp->ogl_extras->ogl_info.max_texture_size);

   /* Note: Voodoo (even V5) don't seem to correctly support
    * packed pixel formats. Disabling them for those cards.
    */
   ext_list->ALLEGRO_GL_EXT_packed_pixels &= !gl_disp->ogl_extras->ogl_info.is_voodoo;


   if (ext_list->ALLEGRO_GL_EXT_packed_pixels) {

      ALLEGRO_INFO("Packed Pixels formats available\n");

      /* XXX On NV cards, we want to use BGRA instead of RGBA for speed */
      /* Fills the __allegro_gl_texture_format array */
      /* TODO: use these somewhere */
#if 0
      __allegro_gl_texture_read_format[0] = GL_UNSIGNED_BYTE_3_3_2;
      __allegro_gl_texture_read_format[1] = GL_UNSIGNED_SHORT_5_5_5_1;
      __allegro_gl_texture_read_format[2] = GL_UNSIGNED_SHORT_5_6_5;
#endif /* #if 0 */
   }

   /* NVidia and ATI cards expose OpenGL 2.0 but often don't accelerate
    * non-power-of-2 textures. This check is how you verify that NP2
    * textures are hardware accelerated or not.
    * We should clobber the NPOT support if it's not accelerated.
    */
   {
      const char *vendor = (const char *)glGetString(GL_VENDOR);
      if (strstr(vendor, "NVIDIA Corporation")) {
         if (!ext_list->ALLEGRO_GL_NV_fragment_program2
          || !ext_list->ALLEGRO_GL_NV_vertex_program3) {
            ext_list->ALLEGRO_GL_ARB_texture_non_power_of_two = 0;
         }
      }
      else if (strstr(vendor, "ATI Technologies")) {
         if (!strstr((const char *)glGetString(GL_EXTENSIONS),
                     "GL_ARB_texture_non_power_of_two")
             && gl_disp->ogl_extras->ogl_info.version >= 2.0f) {
            ext_list->ALLEGRO_GL_ARB_texture_non_power_of_two = 0;
         }
      }
   }
   
   {
      int *s = gl_disp->extra_settings.settings;
      glGetIntegerv(GL_MAX_TEXTURE_SIZE, s + ALLEGRO_MAX_BITMAP_SIZE);
   
      if (gl_disp->ogl_extras->ogl_info.version >= 2.0)
         s[ALLEGRO_SUPPORT_SEPARATE_ALPHA] = 1;

      s[ALLEGRO_SUPPORT_NPOT_BITMAP] =
         ext_list->ALLEGRO_GL_ARB_texture_non_power_of_two;
   ALLEGRO_INFO("Use of non-power-of-two textures %s.\n",
      s[ALLEGRO_SUPPORT_NPOT_BITMAP] ? "enabled" : "disabled");
#ifdef ALLEGRO_IPHONE
   s[ALLEGRO_CAN_DRAW_INTO_BITMAP] =
      ext_list->ALLEGRO_GL_OES_framebuffer_object;
   ALLEGRO_INFO("Use of FBO to draw to textures %s.\n",
      s[ALLEGRO_CAN_DRAW_INTO_BITMAP] ? "enabled" :
      "disabled");
#else
   s[ALLEGRO_CAN_DRAW_INTO_BITMAP] =
      ext_list->ALLEGRO_GL_EXT_framebuffer_object;
   ALLEGRO_INFO("Use of FBO to draw to textures %s.\n",
      s[ALLEGRO_CAN_DRAW_INTO_BITMAP] ? "enabled" :
      "disabled");
#endif
   }
}



/* Function: al_get_opengl_extension_list
 */
ALLEGRO_OGL_EXT_LIST *al_get_opengl_extension_list(void)
{
   ALLEGRO_DISPLAY *disp;

   disp = al_get_current_display();
   ASSERT(disp);
   return ((ALLEGRO_DISPLAY*)disp)->ogl_extras->extension_list;
}



void _al_ogl_unmanage_extensions(ALLEGRO_DISPLAY *gl_disp)
{
   destroy_extension_api_table(gl_disp->ogl_extras->extension_api);
   destroy_extension_list(gl_disp->ogl_extras->extension_list);
   gl_disp->ogl_extras->extension_api = NULL;
   gl_disp->ogl_extras->extension_list = NULL;

#ifdef ALLEGRO_MACOSX
   CFRelease(opengl_bundle_ref);
#endif
#ifdef ALLEGRO_HAVE_DYNAMIC_LINK
   if (__libgl_handle) {
      dlclose(__libgl_handle);
      __libgl_handle = NULL;
   }
#endif
}
