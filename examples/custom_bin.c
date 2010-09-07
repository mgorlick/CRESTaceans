/*The application programmer can create custom bins packed with elements to perform a specific task.
This allows you, for example, to write an Ogg/Vorbis decoder with just the following lines of code:*/

int
main (int
argc,
char *argv[])
{
GstElement *player;
/* init */
gst_init (&argc, &argv);
/* create player */
player = gst_element_factory_make ("oggvorbisplayer", "player");
/* set the source audio file */
g_object_set (player, "location", "helloworld.ogg", NULL);
/* start playback */
gst_element_set_state (GST_ELEMENT (player), GST_STATE_PLAYING);
return 0;
}


