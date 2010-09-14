#include <glib.h>
#include <gst/gst.h>

void cb_new_pad (GstElement *element, GstPad *pad, gpointer data){
  gchar *name;
  name = gst_pad_get_name (pad);
  g_print ("A new pad %s was created\n", name);
  g_free (name);
  /* here, you would setup a new pad link for the newly created pad */
  //GstPad* sinkpad;
  //GstElement* decoder = (GstElement *) data;
  //sinkpad = gst_element_get_static_pad (decoder, "sink");
  //gst_pad_link (pad, sinkpad);
  //gst_object_unref (sinkpad);
}

gulong t4_signal_connect (GstElement* e1, GstElement* e2) {
  return g_signal_connect (e1, "pad-added", G_CALLBACK (cb_new_pad), NULL);   
}
