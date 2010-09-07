/* Every GstElement inherits at least one property from its parent GstObject: the "name" property. This
is the name you provide to the functions gst_element_factory_make () or gst_element_factory_create (). 
You can get and set this property using the functions gst_object_set_name and gst_object_get_name or use the GObject property mechanism.*/


#include <gst/gst.h>

int
main (int argc, char *argv[])
{
GstElement *element;
gchar *name;
/* init GStreamer */
gst_init (&argc, &argv);
/* create element */
element = gst_element_factory_make ("fakesrc", "source");
/* get name */
g_object_get (G_OBJECT (element), "name", &name, NULL);
g_print ("The name of the element is ’%s’.\n", name);
g_free (name);
gst_object_unref (GST_OBJECT (element));
return 0;
}
