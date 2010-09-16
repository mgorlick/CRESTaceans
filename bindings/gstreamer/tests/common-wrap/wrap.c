#include <gst/gst.h>
#include <gst/gstmessage.h>
#include <glib.h>

void on_pad_added (GstElement *element, GstPad *pad, gpointer data) {
  GstPad *sinkpad;
  GstElement *decoder = (GstElement *) data;
  g_print ("Dynamic pad created, linking demuxer/decoder\n");
  sinkpad = gst_element_get_static_pad (decoder, "sink");
  gst_pad_link (pad, sinkpad);
  gst_object_unref (sinkpad);
}

void signal_connect (GstElement* e1, GstElement* e2) {
  g_signal_connect (e1, "pad-added", G_CALLBACK (on_pad_added), e2);
}

int gst_message_type (GstMessage* m) {
  return GST_MESSAGE_CAST(m)->type;
}

void gst_message_unref_w (GstMessage* m) {
  gst_message_unref(m);
}

void print_gst_time_format (gint64 pos, gint64 len) {
    g_print ("Time: %" GST_TIME_FORMAT " / %" GST_TIME_FORMAT "\r",
	     GST_TIME_ARGS (pos), GST_TIME_ARGS (len));
}
