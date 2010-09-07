/* my first audio player */

#include <gst/gst.h>
#include <glib.h>
#include<stdio.h>
#include<string.h>



int
main (int argc, char *argv[])
{
	GstElementFactory  *factory;
	GstElement *created_element;
	
	/* Initialisation */
	gst_init (&argc, &argv);
	factory = gst_element_factory_find("alsasink");

	created_element = gst_element_factory_create(factory, "alsasink_element");
	
	printf ("%s", gst_element_get_name(created_element));
	printf (" created \n");
	
	return 0;
}





