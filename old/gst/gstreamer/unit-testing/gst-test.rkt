#lang racket

(require rackunit
         rackunit/text-ui
         "../gstreamer.rkt"
         ffi/unsafe
         srfi/13)

(provide (all-defined-out))

(define gst-initialization-test
    (test-suite
     "Tests for Gst library"

    (test-case
      "Test with-gst-init macro with no arguments"
      (with-gst-init #f 
                     (display "Hello Gstreamer\n")
                     ;(check-equal? (gst_is_initialized) #t)
                     ))
     
     (test-case 
      "Test with-gst-init macro with arguments"
      (let ((arg-list (list "music-file.mp3")))
      (with-gst-init arg-list 
                     (display "Hello Gstreamer with initial arguments\n"))))
     
     (test-case
      "Test with-gst-init-check macro with no arguments"
      (check-equal? (with-gst-init-check (display "Initializing Gstreamer with gst_init_check\n")) 1))
     
    (test-case 
      "Test with-gst-init-check-args macro with arguments"
      (let ((arg-list (list "music-file.mp3")))
        (check-equal? (with-gst-init-check arg-list
                                           (display "Initializing Gstreamer with gst_init_check with arguments\n"))
                      1)))
     
     (test-case
      "Test unreference object - deinit system clock"
      (with-gst-init #f 
                     (let ((clock (gst_system_clock_obtain)))
                       (gst_object_unref clock))))
     
      (test-case
      "tests if we can create an element from a compiled-in plugin"
      (with-gst-init #f 
                     (let ((pipline (gst_pipeline_new "pipeline")))
                       (gst_object_unref pipline))))
      
       (test-case
      "tests if we can load an element from a plugin"
      (with-gst-init #f 
                     (let ((element (gst_element_factory_make "fakesrc" #f)))
                       (gst_object_unref element))))
       
       ;;if this test case it's successful means that gst_version_string binding is working right
       (test-case
      "testsing Gstreamer version"
      (with-gst-init #f 
                     (let-values ([(major minor micro nano) (gst_version)])
                       (check-equal? major (get_Gst_Version_Major))
                       (check-equal? minor (get_Gst_Version_Minor))
                       (check-equal? micro (get_Gst_Version_Micro))
                       (check-equal? nano (get_Gst_Version_Nano))
                       (check-not-equal? (gst_version_string) "")
                       (check-not-equal? (string-contains (gst_version_string) "GStreamer") #f)
                       (display (string-append (gst_version_string) "\n")))))
       
       (test-case
        "Test segtrap. Enables or disables SIGSEGV handler to better catch and report errors to the application. This feature is enabled by default when loading plugins. "
        (with-gst-init #f
                       (begin
                         (gst_segtrap_set_enabled 0)
                         (check-equal? (gst_segtrap_is_enabled) 0)
                         (gst_segtrap_set_enabled 1)
                         (check-equal? (gst_segtrap_is_enabled) 1))))
       
       (test-case
        "Test registry fork - fork() for scanning and rebuilding the registry file"
        (with-gst-init #f
                       (begin
                         (gst_registry_fork_set_enabled 0)
                         (check-equal? (gst_registry_fork_is_enabled) 0)
                         (gst_registry_fork_set_enabled 1)
                         (check-equal? (gst_registry_fork_is_enabled) 1))))
       
       (test-case
        "Test registry updating - useful when the app needs to detect on the fly new plugins"
        (with-gst-init #f
                       (check-equal? (gst_update_registry) 1)))
  
))


               

