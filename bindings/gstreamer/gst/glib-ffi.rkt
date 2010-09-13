#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         (only-in '#%foreign ffi-callback))

(provide (all-defined-out))

; FFI
(define glib-lib (ffi-lib "libgobject-2.0"))

(define-ffi-definer gldf glib-lib )

(define _gcharptr (_ptr io _gchar))
(define _gintptr (_ptr io _gint))

;;TYPES

;;typedef int gint;
(define _gint _int)

(define _gint32 _int32)

(define _gint64 _int64)

;;typedef gint   gboolean;
(define _gboolean _gint)

;;typedef char   gchar;
(define _gchar _byte)

;;typedef const void *gconstpointer;
(define _gconstpointer _pointer)

;;typedef double  gdouble;
(define _gdouble _double)

;;typedef float   gfloat;
(define _gfloat _float)

;;typedef long   glong;
(define _glong _long)

;;typedef void* gpointer;
(define _gpointer _pointer)

;;typedef unsigned int gsize;
(define _gsize _uint)

;;typedef unsigned char guchar;
(define _guchar _ubyte)

;;typedef unsigned int    guint;
(define _guint _uint)

(define _guint8 _uint8)

(define _guint16 _uint16)

(define _guint32 _uint32)

(define _guint64 _uint64)

;;typedef unsigned long   gulong;
(define _gulong _ulong)

;;typedef gsize GType; YES
(define _GType _long)

;;typedef guint32 GQuark; YES
(define _GQuark _uint32)


;; typedef enum
;;{
;;  G_PARAM_READABLE            = 1 << 0,
;;  G_PARAM_WRITABLE            = 1 << 1,
;;  G_PARAM_CONSTRUCT	      = 1 << 2,
;;  G_PARAM_CONSTRUCT_ONLY      = 1 << 3,
;;  G_PARAM_LAX_VALIDATION      = 1 << 4,
;;  G_PARAM_STATIC_NAME	      = 1 << 5,
;;#ifndef G_DISABLE_DEPRECATED
;;  G_PARAM_PRIVATE	      = G_PARAM_STATIC_NAME,   ;;I IGNORED THIS. DON"T KNOW HOW TO CHECK ifndef
;;#endif
;;  G_PARAM_STATIC_NICK	      = 1 << 6,
;;  G_PARAM_STATIC_BLURB	      = 1 << 7
;;} GParamFlags;

(define G_DISABLE_DEPRECATED 220)

(define G_PARAM_READABLE (arithmetic-shift 1 0))
(define G_PARAM_WRITABLE (arithmetic-shift 1 1))
(define G_PARAM_CONSTRUCT (arithmetic-shift 1 2))
(define G_PARAM_CONSTRUCT_ONLY (arithmetic-shift 1 3))
(define G_PARAM_LAX_VALIDATION (arithmetic-shift 1 4))
(define G_PARAM_STATIC_NAME (arithmetic-shift 1 5))
(define G_PARAM_PRIVATE G_PARAM_STATIC_NAME)
(define G_PARAM_STATIC_NICK (arithmetic-shift 1 6))
(define G_PARAM_STATIC_BLURB (arithmetic-shift 1 7))



;typedef enum
;;{
;;  G_THREAD_PRIORITY_LOW,
;;  G_THREAD_PRIORITY_NORMAL,
;;  G_THREAD_PRIORITY_HIGH,
;;  G_THREAD_PRIORITY_URGENT
;;} GThreadPriority;

(define _GThreadPriority
  (_enum '(G_THREAD_PRIORITY_LOW G_THREAD_PRIORITY_NORMAL G_THREAD_PRIORITY_HIGH G_THREAD_PRIORITY_URGENT)))


;;typedef enum    /*< skip >*/
;;{
;;  G_TYPE_FLAG_ABSTRACT		= (1 << 4),
;;  G_TYPE_FLAG_VALUE_ABSTRACT	= (1 << 5)
;;} GTypeFlags;

(define G_TYPE_FLAG_ABSTRACT (arithmetic-shift 1 4))
(define G_TYPE_FLAG_VALUE_ABSTRACT (arithmetic-shift 1 5))


;;typedef struct _GDate GDate;
;;struct _GDate
;;{
;;  guint julian_days : 32;
;;  guint julian : 1;    /* julian is valid */
;;  guint dmy    : 1;    /* dmy is valid */
;;  guint day    : 6;
;;  guint month  : 4;
;;  guint year   : 16;
;;}
;; defining it as a pointer since we cannot express size (e.g. 6)

(define-cpointer-type _GDate-pointer)


;typedef struct _GMainLoop GMainLoop;
(define-cpointer-type _GMainLoop-pointer)


;;typedef struct _GError GError;
;;struct _GError
;;{
;;  GQuark       domain;
;;  gint         code;
;;  gchar       *message;
;;};

(define-cstruct _GError
  ([domain _GQuark]
   [code _gint]
   [message _string]))


;;typedef struct _GList GList;  YES but his doesn't mathc the .h file
;;struct _GList
;;{
;;  gpointer data;
;;  GList *next;
;;  GList *prev;
;;};

(define-cstruct _GList
  ([data _gpointer]
   [next _GList-pointer]
   [prev _GList-pointer]))


;;typedef struct _GSList GSList; YES but different
;;struct _GSList
;;{
;;  gpointer data;
;;  GSList *next;
;;};

;(define _GSList (_cpointer/null 'GSList)) Matt Flatt's defnition

(define-cstruct _GSList
  ([data _gpointer]
   [next _GSList-pointer]))


;;typedef struct _GModule GModule; 
(define-cpointer-type _GModule-pointer)

;;typedef struct _GMutex GMutex;
(define-cpointer-type _GMutex-pointer)

;typedef struct {} GTypeInterface;
(define-cpointer-type _GTypeInterface-pointer)


;;typedef struct _GTypeClass GTypeClass;
;;struct _GTypeClass
;;{
;;  /*< private >*/
;;  GType g_type;
;;};

(define-cstruct _GTypeClass
  ([g_type _GType]))


;;typedef struct _GTypeInstance GTypeInstance;
;struct _GTypeInstance
;;{
;;  /*< private >*/
;;  GTypeClass *g_class;
;;};

(define-cstruct _GTypeInstance 
  ([g_class _GTypeClass-pointer]))


;;typedef struct _GData GData; 
(define-cpointer-type _GData-pointer)

;;typedef struct _GObject GObject;
;;struct  _GObject
;;{
;;  GTypeInstance  g_type_instance;
;;  /*< private >*/
;;  volatile guint ref_count; 
;;  GData         *qdata;
;;};

(define-cstruct _GObject                        
  ([g_type_instance _GTypeInstance]
   [ref_count _guint]
   [qdata _GData-pointer]))


;;typedef struct _GParamSpec      GParamSpec;
;struct _GParamSpec
;{
;  GTypeInstance  g_type_instance;
;  gchar         *name;
;  GParamFlags    flags;
;  GType		 value_type;
;  GType		 owner_type;	/* class or interface using this property */
;  /*< private >*/
;  gchar         *_nick;
;  gchar         *_blurb;
;  GData		*qdata;
;  guint          ref_count;
;  guint		 param_id;	/* sort-criteria */
;};

(define-cstruct _GParamSpec       
  ([g_type_instance _GTypeInstance]
   [name _string]
   [flags _int]
   [value_type _GType]
   [owner_type _GType]
   [_nick _string]
   [_blurb _string]
   [qdata _GData-pointer]
   [ref_count _guint]
   [param_id _guint]))


;;gboolean (*GSourceFunc) (gpointer data);
(define _GSourceFunc-pointer (_ptr io (_fun _gpointer -> _gboolean)))


;;typedef struct _GSourceCallbackFuncs	GSourceCallbackFuncs;
;;struct _GSourceCallbackFuncs
;;{
;;  void (*ref)   (gpointer     cb_data);
;;  void (*unref) (gpointer     cb_data);
;;  void (*get)   (gpointer     cb_data, GSource *source, GSourceFunc *func, gpointer *data);
;;};

(define-cstruct _GSourceCallbackFuncs
  ([ref (_ptr io (_fun _gpointer -> _void))]
   [unref (_ptr io (_fun _gpointer -> _void))]
   [get (_ptr io (_fun _gpointer _GSource-pointer _GSourceFunc-pointer _gpointer -> _void))]))


;typedef struct _GSourceFuncs GSourceFuncs;
;;struct _GSourceFuncs
;;{
;;  gboolean (*prepare)  (GSource    *source, gint       *timeout_);
;;  gboolean (*check)    (GSource    *source);
;;  gboolean (*dispatch) (GSource    *source, GSourceFunc callback, gpointer    user_data);
;;  void     (*finalize) (GSource    *source); /* Can be NULL */

(define-cstruct _GSourceFuncs
  ([prepare (_ptr io (_fun _GSource-pointer _gintptr -> _gboolean))]
   [check (_ptr io (_fun _GSource-pointer -> _gboolean))]
   [dispatch (_ptr io (_fun _GSource-pointer _GSourceFunc-pointer _gpointer -> _gboolean))]
   [finalize (_or-null (_ptr io (_fun _GSource-pointer -> _void)))]))


;;typedef struct _GMainContext GMainContext;	/* Opaque */ YES
(define-cpointer-type _GMainContext-pointer)
;(define _GMainContext (_cpointer 'GMainContext))


;;typedef struct _GSource GSource;
;;struct _GSource
;;{
;;  /*< private >*/
;;  gpointer callback_data;
;;  GSourceCallbackFuncs *callback_funcs;
;;  GSourceFuncs *source_funcs;
;;  guint ref_count;
;;  GMainContext *context;
;;  gint priority;
;;  guint flags;
;;  guint source_id;
;;  GSList *poll_fds;
;;  GSource *prev;
;;  GSource *next;
;;  gpointer reserved1;
;;  gpointer reserved2;
;;};

(define-cstruct _GSource
  ([callback_data _gpointer]
   [callback_funcs _GSourceCallbackFuncs-pointer]
   [source_funcs _GSourceFuncs-pointer]
   [ref_count _guint]
   [context _GMainContext-pointer]
   [priority _gint]
   [flags _guint]
   [source_id _guint]
   [poll_fds _GSList-pointer]
   [prev _GSource-pointer]
   [next _GSource-pointer]
   [reserved1 _gpointer]
   [reserved2 _gpointer]))


;;typedef struct _GStaticMutex GStaticMutex;
;;struct _GStaticMutex  ;;NOT ABLE TO DEFINE - FFI DOESN'T SUPPORT UNIONS
;;{
;;  struct _GMutex *runtime_mutex;
;;  union {
;;    char   pad[44];
;;    double dummy_double;
;;    void  *dummy_pointer;
;;    long   dummy_long;
;;  } static_mutex;
;;;};

;;(define-cstruct _GStaticMutex   
;;  ([runtime_mutex _GMutex-pointer]
;;  [static_mutex ]))

;;typedef struct _GStaticRecMutex GStaticRecMutex;
;;struct _GStaticRecMutex
;;{
;;  /*< private >*/
;;  GStaticMutex mutex;
;;  guint depth;
;;  GSystemThread owner;
;;};

;(define-cstruct _GStaticRecMutex
;  ([mutex _GStaticMutex]
;   [depth _guint]
;   [owner _GSystemThread]))

(define-cpointer-type _GStaticRecMutex-pointer)


;typedef struct _GString GString;
;;struct _GString
;;{
;;  gchar  *str;
;;  gsize len;    
;;  gsize allocated_len;
;;};

(define-cstruct _GString
  ([str _string]
   [len _gsize]
   [allocated_len _gsize]))


;;struct _GValue
;;{
;;  /*< private >*/
;;  GType		g_type;
;;  /* public for GTypeValueTable methods */
;;  union {
;;    gint	v_int;
;;    guint	v_uint;
;;    glong	v_long;
;;    gulong	v_ulong;
;;    gint64      v_int64;
;;    guint64     v_uint64;
;;    gfloat	v_float;
;;    gdouble	v_double;
;;    gpointer	v_pointer;
;;  } data[2];
;;};

(define-cpointer-type _GValue-pointer)


#|typedef struct {
  GParamSpec *pspec;
  GValue     *value;
} GObjectConstructParam;|#

(define-cstruct _GObjectConstructParam
  ([pspec _GParamSpec-pointer]
   [value _GValue-pointer]))


#|typedef struct {
  GTypeClass   g_type_class;
  /* seldomly overidden */
  GObject*   (*constructor)     (GType type, guint n_construct_properties, GObjectConstructParam *construct_properties);
  /* overridable methods */
  void       (*set_property)		(GObject *object, guint property_id, const GValue *value, GParamSpec *pspec);
  void       (*get_property)		(GObject *object, guint property_id, GValue *value, GParamSpec *pspec);
  void       (*dispose)			(GObject        *object);
  void       (*finalize)		(GObject        *object);
  /* seldomly overidden */
  void       (*dispatch_properties_changed) (GObject      *object, guint n_pspecs, GParamSpec  **pspecs);
  /* signals */
  void	     (*notify)			(GObject *object, GParamSpec *pspec);
  /* called when done constructing */
  void	     (*constructed)		(GObject *object);
} GObjectClass;|#


(define-cstruct _GObjectClass
  ([g_type_class _GTypeClass]
   [constructor (_ptr io (_fun _GType _guint _GObjectConstructParam-pointer -> _GObject-pointer))]
   [set_property (_ptr io (_fun _GObject-pointer _guint _GValue-pointer _GParamSpec-pointer -> _void))]
   [get_property (_ptr io (_fun _GObject-pointer _guint _GValue-pointer _GParamSpec-pointer -> _void))]
   [dispose (_ptr io (_fun _GObject-pointer -> _void))]
   [finalize (_ptr io (_fun _GObject-pointer -> _void))]
   [dispatch_properties_changed (_ptr io (_fun _GObject-pointer _guint _GParamSpec-pointer -> _void))]
   [notify (_ptr io (_fun _GObject-pointer _GParamSpec-pointer -> _void))]
   [constructed (_ptr io (_fun _GObject-pointer -> _void))]))



;;struct _GTypeValueTable
;;{
;;  void     (*value_init)         (GValue       *value)
;;  void     (*value_free)         (GValue       *value);
;;  void     (*value_copy)         (const GValue *src_value, GValue       *dest_value);
;;  /* varargs functionality (optional) */
;;  gpointer (*value_peek_pointer) (const GValue *value);
;;  gchar	    *collect_format;
;;  gchar*   (*collect_value)      (GValue *value, guint n_collect_values, GTypeCValue  *collect_values, guint collect_flags);
;;  gchar	    *lcopy_format;
;;  gchar*   (*lcopy_value)        (const GValue *value, guint n_collect_values, GTypeCValue  *collect_values, guint collect_flags)
;;};

;;(define-cstruct _GTypeValueTable
;;  ([value_init (_ptr io (_fun _GValue-pointer -> _void))]
;;   [value_free (_ptr io (_fun _GValue-pointer -> _void))]
;;   [value_copy (_ptr io (_fun _GValue-pointer _GValue-pointer -> _void))]
;;   [value_peek_pointer (_ptr io (_fun _GValue-pointer -> _gpointer))]
;;   [collect_format _gcharptr]
;;   [collect_value (_ptr io (_fun _GValue-pointer _guint _GTypeCValue-pointer _guint -> _gcharptr))]    ;;GTypeCValue is a union!
;;   [lcopy_format _gcharptr]
;;   [lcopy_value (_ptr io (_fun _GValue-pointer _guint _GTypeCValue-pointer _guint -> _gcharptr))]))

(define-cpointer-type _GTypeValueTable-pointer)

;typedef struct {
;} GThread;
(define-cpointer-type _GThread-pointer)

;;typedef struct _GOptionGroup GOptionGroup;
(define-cpointer-type _GOptionGroup-pointer)

;;typedef struct _GCond GCond;       ;;opaque data structure that represents a condition.
(define-cpointer-type _GCond-pointer)


;;typedef void   (*GBaseInitFunc) (gpointer g_class); ;;pointer to a function that takes a gpointer and returns void
(define _GBaseInitFunc-pointer (_ptr io (_fun _gpointer -> _void)))


;;typedef void   (*GBaseFinalizeFunc)          (gpointer g_class);
(define _GBaseFinalizeFunc-pointer (_ptr io (_fun _gpointer -> _void)))

;;typedef void  (*GCallback) (void);
(define _GCallback-pointer (_ptr io (_fun -> _void)))

;;typedef void   (*GClassFinalizeFunc) (gpointer g_class, gpointer class_data);
(define _GClassFinalizeFunc-pointer (_ptr io (_fun _gpointer _gpointer -> _void)))

;;typedef void (*GClassInitFunc) (gpointer g_class, gpointer class_data);
(define _GClassInitFunc-pointer (_ptr io (_fun _gpointer _gpointer -> _void)))

;;typedef gint (*GCompareDataFunc)     (gconstpointer  a, gconstpointer  b, gpointer user_data);
(define _GCompareDataFunc (_ptr io (_fun _gconstpointer _gconstpointer _gpointer -> _gint)))

;;typedef gint (*GCompareFunc) (gconstpointer  a, gconstpointer  b);
(define _GCompareFunc (_ptr io (_fun _gconstpointer _gconstpointer -> _gint)))

;;typedef void (*GDestroyNotify) (gpointer data);
(define _GDestroyNotify (_ptr io (_fun _gpointer -> _void)))

;;typedef void (*GFreeFunc) (gpointer data);
(define _GFreeFunc (_ptr io (_fun _gpointer -> _void)))

;;typedef void (*GFunc) (gpointer data, gpointer user_data);
(define _GFunc (_ptr io (_fun _gpointer _gpointer -> _void)))

;;typedef void (*GInstanceInitFunc) (GTypeInstance   *instance, gpointer g_class);
(define _GInstanceInitFunc (_ptr io (_fun _GTypeInstance _gpointer -> _void)))

;void                g_type_init                         (void);
(define g_type_init (get-ffi-obj 'g_type_init glib-lib (_fun -> _void)))

;void                g_object_set                        (gpointer object, const gchar *first_property_name, ...);
(define g_object_set
  (lambda (ptr . args)
    (let* ([l (build-list (length args) values)]
           [names (filter-map (lambda (x) (and (even? x) (list-ref args x))) l)]
           [values (filter-map (lambda (x) (and (odd? x) (list-ref args x))) l)])
      (map (lambda (name val) 
             (g_object_set_1 ptr name val))
           names values))))

(define g_object_set_1
  (get-ffi-obj 'g_object_set glib-lib
               (_fun _gpointer _string _string 
                     (_string = #f) -> _void)))

;GMainLoop *         g_main_loop_new                     (GMainContext *context, gboolean is_running);
(gldf g_main_loop_new (_fun (_or-null _GMainContext-pointer) _gboolean -> _GMainLoop-pointer))

;void                g_main_loop_run                     (GMainLoop *loop);
(gldf g_main_loop_run (_fun _GMainLoop-pointer -> _void))

(gldf g_main_loop_quit (_fun _GMainLoop-pointer -> _void))

(define _GCallback (_fun -> _void))

(gldf g_signal_connect_data (_fun _gpointer _string _GCallback _gpointer
                     (_gpointer = #f) (_int = 0) -> _gulong))

;; initialize the type system for all users
(g_type_init)


