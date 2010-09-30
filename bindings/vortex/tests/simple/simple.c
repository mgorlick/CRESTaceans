#include <vortex.h>

#define PLAIN_PROFILE "http://fact.aspl.es/profiles/plain_profile"

int      on_accepted (VortexConnection * connection, axlPointer data)
{
	printf ("New connection accepted from: %s:%s\n", 
		 vortex_connection_get_host (connection),
		 vortex_connection_get_port (connection));

	/* return axl_true to accept the connection to be created */
	return axl_true;
}

void frame_received (VortexChannel    * channel,
		     VortexConnection * connection,
		     VortexFrame      * frame,
		     axlPointer           user_data)
{
	printf ("A frame received on channel: %d\n",     vortex_channel_get_number (channel));
	printf ("Data received: '%s'\n",                (char*) vortex_frame_get_payload (frame));

	/* reply the peer client with the same content */
	vortex_channel_send_rpyv (channel,
				  vortex_frame_get_msgno (frame),
				  "Received Ok: %s",
				  vortex_frame_get_payload (frame));
				
	printf ("VORTEX_LISTENER: end task (pid: %d)\n", getpid ());


	return;
}

int      start_channel (int                channel_num, 
			VortexConnection * connection, 
			axlPointer           user_data)
{
  printf ("in start_channel\n");
	/* implement profile requirement for allowing starting a new
	 * channel */

	/* to return axl_false denies channel creation to return axl_true
	 * allows create the channel */
	return axl_true;
}

int      close_channel (int                channel_num, 
			VortexConnection * connection, 
			axlPointer           user_data)
{
	/* implement profile requirement for allowing to closeing a
	 * the channel to return axl_false denies channel closing to
	 * return axl_true allows to close the channel */
	return axl_true;
}

int register_plain_profile (VortexCtx* ctx) {
   return vortex_profiles_register (ctx, PLAIN_PROFILE, 
				  start_channel, NULL, 
				  close_channel, NULL,
				  frame_received, NULL);
}

void set_on_accepted (VortexCtx* ctx) {
  vortex_listener_set_on_connection_accepted (ctx, on_accepted, NULL);
}
