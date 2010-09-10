#include <gst/gst.h>
#include <glib.h>

void
on_pad_added (GstElement *element,
              GstPad     *pad,
              gpointer    data)
{
  GstPad *sinkpad;
  GstElement *decoder = (GstElement *) data;

  /* We can now link this pad with the vorbis-decoder sink pad */
  g_print ("Dynamic pad created, linking demuxer/decoder\n");

  sinkpad = gst_element_get_static_pad (decoder, "sink");

  gst_pad_link (pad, sinkpad);

  gst_object_unref (sinkpad);
}


void signal_connect (GstElement* e1, GstElement* e2) {
  g_signal_connect (e1, "pad-added", G_CALLBACK (on_pad_added), e2);
}

int gst_message_type(GstMessage* m) {
  return GST_MESSAGE_CAST(m)->type;
}
