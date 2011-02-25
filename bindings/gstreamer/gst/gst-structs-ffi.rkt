#lang at-exp racket/base

(require ffi/unsafe)
(require "gst_base.rkt")

(provide (all-defined-out))

#|(provide/doc
 (proc-doc/names GstElement-pointer?
                 (--> any/c boolean?)
                 (v)
                 @{Returns @racket[#t] if @racket[v] is a pointer for a GstElement
                           @racket[gif-write], @racket[#f] otherwise.}))|#


(define-cpointer-type _GstBin-pointer)

(define-cpointer-type _GstBinClass-pointer)

(define-cpointer-type _GstBuffer-pointer)

(define-cpointer-type _GstBufferList-pointer)

(define-cpointer-type _GstBufferListIterator-pointer)

(define-cpointer-type _GstBus-pointer)

(define-cpointer-type _GstCaps-pointer)

(define-cpointer-type _GstStaticCaps-pointer)

(define-cpointer-type _GstChildProxy-pointer)

(define-cpointer-type _GstChildProxyInterface-pointer)

(define-cpointer-type _GstClock-pointer)

(define-cpointer-type _GstClockEntry-pointer)

(define-cpointer-type _GstClockClass-pointer)

(define-cpointer-type _GstElement-pointer)

(define-cpointer-type _GstElementClass-pointer)

(define-cpointer-type _GstElementDetails-pointer)

(define-cpointer-type _GstElementFactory-pointer)

(define-cpointer-type _GstEvent-pointer)

(define-cpointer-type _GstFormatDefinition-pointer)

(define-cpointer-type _GstGhostPad-pointer)

(define-cpointer-type _GstImplementsInterface-pointer)

(define-cpointer-type _GstIndexFactory-pointer)

(define-cpointer-type _GstIndex-pointer)

(define-cpointer-type _GstIndexEntry-pointer)

(define-cpointer-type _GstIndexGroup-pointer)

(define-cpointer-type _GstIndexAssociation-pointer)

(define-cpointer-type _GstIterator-pointer)

(define-cpointer-type _GstDebugMessage-pointer)

(define-cpointer-type _GstDebugCategory-pointer)

(define-cpointer-type _GstMessage-pointer)

(define-cpointer-type _GstMiniObject-pointer)

(define-cpointer-type _GstObject-pointer)

(define-cpointer-type _GstObjectClass-pointer)

(define-cpointer-type _GstPad-pointer)

(define-cpointer-type _GstPadTemplate-pointer)

(define-cpointer-type _GstQuery-pointer)

(define-cpointer-type _GstStaticPadTemplate-pointer)

(define-cpointer-type _GstParamSpecFraction-pointer)

(define-cpointer-type _GstParseContext-pointer)

(define-cpointer-type _GstPipeline-pointer)

(define-cpointer-type _GstPluginFeature-pointer)

(define-cpointer-type _GstTagList-pointer)

(define-cpointer-type _GstTypeNameData-pointer)

(define-cpointer-type _GstPlugin-pointer)

(define-cpointer-type _GstPluginDesc-pointer)

(define-cpointer-type _GstPoll-pointer)

(define-cpointer-type _GstPollFD-pointer)

(define-cpointer-type _GstPreset-pointer)

(define-cpointer-type _GstPresetInterface-pointer)

(define-cpointer-type _GstQueryTypeDefinition-pointer)

(define-cpointer-type _GstRegistry-pointer)

(define-cpointer-type _GstSegment-pointer)

(define-cpointer-type _GstStructure-pointer)

(define-cpointer-type _GstSystemClock-pointer)

(define-cpointer-type _GstTagSetter-pointer)

(define-cpointer-type _GstTagSetterIFace-pointer)

(define-cpointer-type _GstTask-pointer)

(define-cpointer-type _GstTaskPool-pointer)

(define-cpointer-type _GstTaskPoolClass-pointer)

(define-cpointer-type _GstTaskThreadCallbacks-pointer)

(define-cpointer-type _GstTrace-pointer)

(define-cpointer-type _GstTypeFind-pointer)

(define-cpointer-type _GstTypeFindFactory-pointer)

(define-cpointer-type _GstAllocTrace-pointer)

(define-cpointer-type _GstURIHandler-pointer)

(define-cpointer-type _GstURIHandlerInterface-pointer)

(define-cpointer-type _GstValueTable-pointer)
