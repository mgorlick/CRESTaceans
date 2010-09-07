/*The following code fragment is used to get a factory that can be used to create the fakesrc element, a fake
data source. The function gst_element_factory_create () will use the element factory to create an element with the given name.*/


#include <gst/gst.h>

int main (int argc, char *argv[])
{
GstElementFactory *factory;
GstElement * element;
/* init GStreamer */
gst_init (&argc, &argv);
/* create element, method #2 */
factory = gst_element_factory_find ("fakesrc");
if (!factory) {
g_print ("Failed to find factory of type ’fakesrc’\n");
return -1;
}
element = gst_element_factory_create (factory, "source");
if (!element) {
g_print ("Failed to create element, even though its factory exists!\n");
return -1;
}
gst_object_unref (GST_OBJECT (element));
return 0;
}

