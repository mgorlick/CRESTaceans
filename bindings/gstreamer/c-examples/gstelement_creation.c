/* how to create an element named source from the element factory named
fakesrc. It checks if the creation succeeded. After checking, it unrefs the element.*/


#include <gst/gst.h>

int main (int argc, char *argv[])
{
GstElement *element;
/* init GStreamer */
gst_init (&argc, &argv);
/* create element */
element = gst_element_factory_make ("fakesrc", "source");
if (!element) {
g_print ("Failed to create element of type ’fakesrc’\n");
return -1;
}
g_print ("Element of type ’fakesrc’ created \n");
gst_object_unref (GST_OBJECT (element));
return 0;
}
   
