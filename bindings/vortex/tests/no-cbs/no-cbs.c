#include <vortex.h>

#define PROFILE_URI "http://vortex.aspl.es/profiles/example"

int frame_pipe[2];
int close_pipe[2];

void set_pipes(int f1, int f2, int c1, int c2) {
  frame_pipe[0] = f1;
  frame_pipe[1] = f2;
  close_pipe[0] = c1;
  close_pipe[1] = c2;
}

int make_frame_pipe (void) {
  return pipe (frame_pipe);
}

int make_close_pipe (void) {
  return pipe (close_pipe);
}

void  queue_reply                    (VortexChannel    * channel,
				      VortexConnection * connection,
				      VortexFrame      * frame,
				      axlPointer user_data)
{
  printf ("In queue_reply (C)\n");
	VortexAsyncQueue * queue      = user_data;
	VortexFrame      * frame_copy = NULL;
	int                result;

	/* copy the frame because the first level invocation handler
	 * will deallocate the frame once terminated this handler */
	frame_copy = vortex_frame_copy (frame);

	/* queue the channel and the frame */
	vortex_async_queue_push (queue, frame_copy);

	/* write to the frame pipe */
	result = write (frame_pipe[1], "f", 1);

	/* nothing more */
	return;
}

void client_register_nocbs_profile (VortexCtx* ctx, VortexAsyncQueue* queue) {
  
	vortex_profiles_register (ctx, PROFILE_URI,
				  /* no start handler, accept all channels */
				  NULL, NULL,
				  /* no close channel, accept all
				   * channels to be closed */
				  NULL, NULL,
				  queue_reply, queue);

}

void server_register_nocbs_profile (VortexCtx* ctx, VortexAsyncQueue* queue) {
  
	vortex_profiles_register (ctx, PROFILE_URI,
				  /* no start handler, accept all channels */
				  NULL, NULL,
				  /* no close channel, accept all
				   * channels to be closed */
				  NULL, NULL,
				  vortex_channel_queue_reply, queue);

}


void close_request_received (VortexChannel * channel, 
			     int             msg_no,
			     axlPointer      user_data)
{
  printf ("In close_request_received (C)\n");
	/* get the queue */
	VortexAsyncQueue * queue      = user_data;
	int                result;

	/* queue the frame reference */
	vortex_async_queue_push (queue, channel);
	
	/* queue the msgno value */
	vortex_async_queue_push (queue, INT_TO_PTR (msg_no));

	/* notify the main loop */
	result = write (close_pipe[1], "c", 1);
	
	printf ("Received a close notify!\n");

	/* nothing more */
	return;
}

void set_close_notify (VortexChannel* channel, VortexAsyncQueue* queue) {
  vortex_channel_set_close_notify_handler (channel, close_request_received, queue);
}

void set_frame_received_handler (VortexChannel* channel, axlPointer user_data) {
  vortex_channel_set_received_handler (channel, queue_reply, user_data);
}

int* frameaddr (void) {
  return &frame_pipe[0];
}

int* closeaddr (void) {
  return &close_pipe[0];
}

int get_frame (int x) {
  return frame_pipe[x];
}

int get_close (int x) {
  return close_pipe[x];
}
