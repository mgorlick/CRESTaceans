#ifndef _al_included_aintern_dtor_h
#define _al_included_aintern_dtor_h

#ifdef __cplusplus
   extern "C" {
#endif


typedef struct _AL_DTOR_LIST _AL_DTOR_LIST;


AL_FUNC(_AL_DTOR_LIST *, _al_init_destructors, (void));
AL_FUNC(void, _al_run_destructors, (_AL_DTOR_LIST *dtors));
AL_FUNC(void, _al_shutdown_destructors, (_AL_DTOR_LIST *dtors));
AL_FUNC(void, _al_register_destructor, (_AL_DTOR_LIST *dtors, void *object,
                                          void (*func)(void*)));
AL_FUNC(void, _al_unregister_destructor, (_AL_DTOR_LIST *dtors, void *object));


#ifdef __cplusplus
   }
#endif

#endif

/* vim: set ts=8 sts=3 sw=3 et: */
