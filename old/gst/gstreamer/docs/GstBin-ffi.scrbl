#lang scribble/doc
@(require scribble/manual
          scribble/extract
          (for-label file/gif))

@title[#:tag "gif"]{GstBin}

@defmodule["gstreamer.rkt"]

GstBin — Base class and element that can contain other elements

@section{Object Hierarchyk}

@section{Implemented Interfaces}


GstBin implements @racket[GstChildProxy].


@section{Properties}

"async-handling"           gboolean              : Read / Write
"message-forward"          gboolean              : Read / Write


@section{Properties}

"do-latency"                                     : Run Last
  "element-added"                                  : Run First
  "element-removed"                                : Run First


  @section{Description}
  
  GstBin is an element that can contain other GstElement, allowing them to be managed as a group. Pads from the child elements can be ghosted to the bin, see GstGhostPad. This makes the bin look like any other elements and enables creation of higher-level abstraction elements.

A new GstBin is created with gst_bin_new(). Use a GstPipeline instead if you want to create a toplevel bin because a normal bin doesn't have a bus or handle clock distribution of its own.

After the bin has been created you will typically add elements to it with gst_bin_add(). You can remove elements with gst_bin_remove().

An element can be retrieved from a bin with gst_bin_get_by_name(), using the elements name. gst_bin_get_by_name_recurse_up() is mainly used for internal purposes and will query the parent bins when the element is not found in the current bin.

An iterator of elements in a bin can be retrieved with gst_bin_iterate_elements(). Various other iterators exist to retrieve the elements in a bin.

gst_object_unref() is used to drop your reference to the bin.

The "element-added" signal is fired whenever a new element is added to the bin. Likewise the "element-removed" signal is fired whenever an element is removed from the bin.
Notes

A GstBin internally intercepts every GstMessage posted by its children and implements the following default behaviour for each of them:

GST_MESSAGE_EOS
	

This message is only posted by sinks in the PLAYING state. If all sinks posted the EOS message, this bin will post and EOS message upwards.

GST_MESSAGE_SEGMENT_START
	

just collected and never forwarded upwards. The messages are used to decide when all elements have completed playback of their segment.

GST_MESSAGE_SEGMENT_DONE
	

Is posted by GstBin when all elements that posted a SEGMENT_START have posted a SEGMENT_DONE.

GST_MESSAGE_DURATION
	

Is posted by an element that detected a change in the stream duration. The default bin behaviour is to clear any cached duration values so that the next duration query will perform a full duration recalculation. The duration change is posted to the application so that it can refetch the new duration with a duration query. Note that these messages can be posted before the bin is prerolled, in which case the duration query might fail.

GST_MESSAGE_CLOCK_LOST
	

This message is posted by an element when it can no longer provide a clock. The default bin behaviour is to check if the lost clock was the one provided by the bin. If so and the bin is currently in the PLAYING state, the message is forwarded to the bin parent. This message is also generated when a clock provider is removed from the bin. If this message is received by the application, it should PAUSE the pipeline and set it back to PLAYING to force a new clock distribution.

GST_MESSAGE_CLOCK_PROVIDE
	

This message is generated when an element can provide a clock. This mostly happens when a new clock provider is added to the bin. The default behaviour of the bin is to mark the currently selected clock as dirty, which will perform a clock recalculation the next time the bin is asked to provide a clock. This message is never sent tot the application but is forwarded to the parent of the bin.

OTHERS
	

posted upwards.

A GstBin implements the following default behaviour for answering to a GstQuery:

GST_QUERY_DURATION
	

If the query has been asked before with the same format and the bin is a toplevel bin (ie. has no parent), use the cached previous value. If no previous value was cached, the query is sent to all sink elements in the bin and the MAXIMUM of all values is returned. If the bin is a toplevel bin the value is cached. If no sinks are available in the bin, the query fails.

GST_QUERY_POSITION
	

The query is sent to all sink elements in the bin and the MAXIMUM of all values is returned. If no sinks are available in the bin, the query fails.

OTHERS
	

the query is forwarded to all sink elements, the result of the first sink that answers the query successfully is returned. If no sink is in the bin, the query fails.

A GstBin will by default forward any event sent to it to all sink elements. If all the sinks return TRUE, the bin will also return TRUE, else FALSE is returned. If no sinks are in the bin, the event handler will return TRUE.

Last reviewed on 2006-04-28 (0.10.6) 


@section{Details}

@(include-extracted "../gst/GstBin-ffi.rkt")
