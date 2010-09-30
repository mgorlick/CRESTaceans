#include <vortex.h>

#define PROFILE_URI "http://vortex.aspl.es/profiles/example"

void register_nocbs_profile (VortexCtx* ctx, VortexAsyncQueue* queue) {
  
	vortex_profiles_register (ctx, PROFILE_URI,
				  /* no start handler, accept all channels */
				  NULL, NULL,
				  /* no close channel, accept all
				   * channels to be closed */
				  NULL, NULL,
				  vortex_channel_queue_reply, queue);

}
