/* 
 *  LibVortex:  A BEEP (RFC3080/RFC3081) implementation.
 *  Copyright (C) 2010 Advanced Software Production Line, S.L.
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2.1
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this program; if not, write to the Free
 *  Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 *  02111-1307 USA
 *  
 *  You may find a copy of the license under this software is released
 *  at COPYING file. This is LGPL software: you are welcome to develop
 *  proprietary applications using this library without any royalty or
 *  fee but returning back any change, improvement or addition in the
 *  form of source code, project image, documentation patches, etc.
 *
 *  For commercial support on build BEEP enabled solutions contact us:
 *          
 *      Postal address:
 *         Advanced Software Production Line, S.L.
 *         C/ Antonio Suarez Nº 10, 
 *         Edificio Alius A, Despacho 102
 *         Alcalá de Henares 28802 (Madrid)
 *         Spain
 *
 *      Email address:
 *         info@aspl.es - http://www.aspl.es/vortex
 */
#include <vortex.h>
#include <sys/time.h>
#include <time.h>

#define LOG_DOMAIN "vortex-thread"

/** 
 * \defgroup vortex_thread Vortex Thread: Portable threading API for vortex
 */

/** 
 * \addtogroup vortex_thread
 * @{
 */

/** 
 * @internal Creates a new thread, executing the function provided,
 * passing the referece received to the function (user_data).
 *
 * For complete examples on how to create threads, see \ref  VortexThreadConf documentation.
 *
 * @param thread_def A reference to the thread identifier created by
 * the function. This parameter is not optional.
 * 
 * @param func The function to execute.
 *
 * @param user_data User defined data to be passed to the function to
 * be executed by the newly created thread.
 * 
 * @return The function returns axl_true if the thread was created
 * properly and the variable thread_def is defined with the particular
 * thread reference created.
 *
 * @see vortex_thread_destroy
 */
axl_bool vortex_thread_create_internal (VortexThread      * thread_def,
                                        VortexThreadFunc    func, 
					axlPointer          user_data)
{
  printf ("You didn't call vortex_thread_set_create() with a replacement!\n");
  return axl_false;
}

/** 
 * @internal Wait for the provided thread to finish, destroy its
 * resources and optionally release its pointer.
 * 
 * @param thread_def A reference to the thread that must be destroyed.
 *
 * @param free_data Boolean that set whether the thread pointer should
 * be released or not.
 * 
 * @return axl_true if the destroy operation was ok, otherwise axl_false is
 * returned.
 */
axl_bool vortex_thread_destroy_internal (VortexThread * thread_def, axl_bool  free_data)
{
  scheme_kill_thread (thread_def);
  return axl_true;
}

/** 
 * @internal Variables to hold the active thread management function
 * pointers.
 *
 * They are initialised to use the default Vortex functions. If the
 * user are not interested in using external threading functions he
 * doesn't need to do anything, or even know about this functionality.
 */
VortexThreadCreateFunc  __vortex_thread_create  = vortex_thread_create_internal;
VortexThreadDestroyFunc __vortex_thread_destroy = vortex_thread_destroy_internal;

/** 
 * @brief Creates a new thread, executing the function provided,
 * passing the referece received to the function (user_data).
 *
 * For complete examples on how to create threads, see \ref  VortexThreadConf documentation.
 *
 * @param thread_def A reference to the thread identifier created by
 * the function. This parameter is not optional.
 *
 * @param func The function to execute.
 *
 * @param user_data User defined data to be passed to the function to
 * be executed by the newly created thread.
 *
 * @return The function returns axl_true if the thread was created
 * properly and the variable thread_def is defined with the particular
 * thread reference created.
 *
 * @see vortex_thread_destroy
 */
axl_bool  vortex_thread_create (VortexThread      * thread_def,
                                VortexThreadFunc    func,
                                axlPointer          user_data)
{
  FUEL_WITH_PROGRESS ("thread_create");
	 return __vortex_thread_create (thread_def, func, user_data);
}

/** 
 * @brief Wait for the provided thread to finish, destroy its
 * resources and optionally release its pointer.
 *
 * @param thread_def A reference to the thread that must be destroyed.
 *
 * @param free_data Boolean that set whether the thread pointer should
 * be released or not.
 *
 * @return axl_true if the destroy operation was ok, otherwise axl_false is
 * returned.
 */
axl_bool  vortex_thread_destroy (VortexThread * thread_def, axl_bool  free_data)
{
  FUEL_WITH_PROGRESS ("thread_destroy");
	return __vortex_thread_destroy (thread_def, free_data);
}

/** 
 * @brief Allows to specify the function Vortex library will call to create a
 * new thread.
 *
 * If the user does not have reason to change the default thread
 * creation mechanism this function can be ignored.
 *
 * NOTE: The thread mechanism functions (\ref vortex_thread_set_create
 * and \ref vortex_thread_set_destroy) must be set before any other
 * Vortex API calls are made. Changing the thread mechanism functions
 * while Vortex is running will most likely lead to memory corruption
 * or program crashes.
 *
 * @param create_fn The function to be executed to create a new
 * thread. Passing a NULL value restores to the default create
 * handler.
 *
 * @see vortex_thread_set_destroy
 */
void vortex_thread_set_create (VortexThreadCreateFunc create_fn)
{
	if (NULL != create_fn) 
		__vortex_thread_create = create_fn;
	else
		__vortex_thread_create = vortex_thread_create_internal;
}

/** 
 * @brief Allows to specify the function Vortex library will call to
 * destroy a thread's resources.
 *
 * If the user does not have reason to change the default thread
 * cleanup mechanism this function can be ignored.
 *
 * NOTE: The thread mechanism functions (\ref vortex_thread_set_create
 * and \ref vortex_thread_set_destroy) must be set before any other
 * Vortex API calls are made. Changing the thread mechanism functions
 * while Vortex is running will most likely lead to memory corruption
 * or program crashes.
 *
 * @param destroy_fn The function to be executed to clean up a
 * thread. Passing a NULL value restores to the default destroy
 * handler.
 *
 * @see vortex_thread_set_create
 */
void vortex_thread_set_destroy (VortexThreadDestroyFunc destroy_fn)
{
	if (NULL != destroy_fn) 
		__vortex_thread_destroy = destroy_fn;
	else 
		__vortex_thread_destroy = vortex_thread_destroy_internal;
}

axl_bool  vortex_mutex_create (VortexMutex** mutex_def)
{
  *mutex_def = scheme_make_sema (1);
  return axl_true;
}

axl_bool  vortex_mutex_destroy (VortexMutex* mutex_def)
{
	v_return_val_if_fail (mutex_def, axl_false);

        /* skip destroying the actual mutex;
           libracket has no semaphore destructor. */
        /* FIXME */
        return axl_true;	
}

void vortex_mutex_lock (VortexMutex* mutex_def)
{
  if (mutex_def == NULL) return;

  while (scheme_wait_sema (mutex_def, 1) == 0) {
    /* calling scheme_wait_sema (mutex_def, 1) [as opposed to
       (mutex_def, 0)] means that the thread only checks
       the wait on the sema instead of blocking on the call.
       take this opportunity to switch out to another thread,
       since waiting on mutexes is a critical bottleneck. */
    FUEL_WITH_PROGRESS ("in vortex_mutex_lock");
  }
  return;
}

void vortex_mutex_unlock (VortexMutex* mutex_def)
{
	if (mutex_def == NULL) return;
        scheme_post_sema (mutex_def);
      	return;
}

axl_bool  vortex_cond_create    (VortexCond ** cond)
{
  *cond = (VortexCond*) malloc(sizeof(VortexCond));
  if(cond == NULL) {
    return axl_false;
  }
  (*cond)->s = scheme_make_sema (0); /* initial 0; limit 999999 */
  (*cond)->x = scheme_make_sema (1); /* initial 1; limit 1 */
  (*cond)->h = scheme_make_sema (0); /* initial 0; limit 999999 */
  (*cond)->waiters = 0;
  return axl_true;
}

void semaP (VortexMutex* sema) {
  while (scheme_wait_sema (sema, 1) == 0) {
    FUEL_WITH_PROGRESS ("sema.P()");
  }
  return;
}

void semaV (VortexMutex* sema) {
  scheme_post_sema (sema);
}


/* Signal(), Birrell 2003
   precondition: none
   x.P(); {
       if (waiters > 0) { waiters--; S.V(); h.P(); }
   } x.V();
*/

void vortex_cond_signal    (VortexCond        * cond)
{
  vortex_mutex_lock (cond->x);
  if (cond->waiters > 0) {
    cond->waiters--;
    semaV (cond->s);
    semaP (cond->h); 
  }
  vortex_mutex_unlock (cond->x);
  return;
}

/* Broadcast(), Birrell 2003
   precondition: none
   x.P() {
       for (int i = 0; i < waiters; i++) s.V();
       while (waiters > 0) { waiters--; h.P(); }
   } x.V()
*/

void vortex_cond_broadcast (VortexCond        * cond)
{
  vortex_mutex_lock (cond->x);
  int i;
  for (i = 0; i < cond->waiters; i++) {
    semaV (cond->s);
  }
  while (cond->waiters > 0) {
    cond->waiters--;
    semaP (cond->h);
  }
  printf ("**********cond_broadcast %i waiters on %p after broadcast\n", cond->waiters, cond);
  vortex_mutex_unlock (cond->x);
  return;
}
/* Wait(), Birrell 2003
   precondition: this thread holds mutex m
   x.P(); {
       waiters++;
   } x.V();
   m.Release();
   s.P();
   h.V();
   m.Acquire();
*/
   
axl_bool  vortex_cond_wait      (VortexCond        * cond, 
				 VortexMutex* mutex)
{
  
  vortex_mutex_lock (cond->x);
  cond->waiters++;
  vortex_mutex_unlock (cond->x);
  vortex_mutex_unlock (mutex);
  printf ("**************cond_wait, cond=%p, m=%p, x=%p, h=%p, s=%p\n",
          cond, mutex, cond->x, cond->h, cond->s);
  printf ("**************%i waiters on cond %p now\n", cond->waiters, cond);
  printf ("**************cond_wait s.P(), cond %p\n", cond);
  semaP (cond->s);
  
  printf ("**************cond_wait h.V(), cond %p\n", cond);
  semaV (cond->h);
  
  printf ("**************cond_wait m.Acquire(), cond %p\n", cond);
  vortex_mutex_lock (mutex);
  
  return axl_true;
}

/* Subtract the `struct timeval' values X and Y,
   storing the result in RESULT.
   Return 1 if the difference is negative, otherwise 0.  */

int timeval_subtract (struct timeval* result, struct timeval* x, struct timeval* y)
{
  /* Perform the carry for the later subtraction by updating y. */
  if (x->tv_usec < y->tv_usec) {
    int nsec = (y->tv_usec - x->tv_usec) / 1000000 + 1;
    y->tv_usec -= 1000000 * nsec;
    y->tv_sec += nsec;
  }
  
  if (x->tv_usec - y->tv_usec > 1000000) {
    int nsec = (x->tv_usec - y->tv_usec) / 1000000;
    y->tv_usec += 1000000 * nsec;
    y->tv_sec -= nsec;
  }

  /* Compute the time remaining to wait.
     tv_usec is certainly positive. */
  result->tv_sec = x->tv_sec - y->tv_sec;
  result->tv_usec = x->tv_usec - y->tv_usec;

  /* Return 1 if result is negative. */
  if (x->tv_sec < y->tv_sec) {
    return 1;
  } else {
    return 0;
  }
}

long timeval_subt (struct timeval *r) {
  return (r->tv_usec + (r->tv_usec * 1000000));
}

long timeval_difference_in_usec (struct timeval *x, struct timeval *y) {
  struct timeval result;
  timeval_subtract (&result, x, y);
  return timeval_subt (&result);
}

axl_bool  vortex_cond_timedwait (VortexCond * cond,
				 VortexMutex * mutex,
				 long  microseconds)
{
  struct timeval t, n;
  int i, w;
  i = gettimeofday (&t, NULL);
  if (i == -1) return axl_false;
  
  i = gettimeofday (&n, NULL);
  if (i == -1) return axl_false;
  
  vortex_mutex_lock (cond->x); /* x.P() */
  cond->waiters++;
  vortex_mutex_unlock (cond->x); /* x.V() */
  vortex_mutex_unlock (mutex); /* m.Release() */

  w = scheme_wait_sema (cond->s, 1); /* try s.P() */
  
  while ((w == 0) && (timeval_difference_in_usec(&t, &n) < microseconds)) {
    FUEL_WITH_PROGRESS ("timedwait while()");
    gettimeofday (&n, NULL);
    w = scheme_wait_sema (cond->s, 1); /* try s.P() again */
  }

  if(timeval_difference_in_usec (&t, &n) >= microseconds) {
    /* waiting took too long, remove self from waiter count
       and reacquire mutex m */
    vortex_mutex_lock (cond->x);
    cond->waiters--;
    vortex_mutex_unlock (cond->x);
    vortex_mutex_lock (mutex);
    return axl_false;
  }

  /* If we hit this point, we waited less than the
     time limit on semaphore s */
  semaV(cond->h); /* h.V() */
  vortex_mutex_lock (mutex); /* m.Acquire() */
  printf ("**************cond_timedwait SUCCESS, cond=%p, m=%p, x=%p, h=%p, s=%p\n",
          cond, mutex, cond->x, cond->h, cond->s);
  return axl_true;
}

void vortex_cond_destroy   (VortexCond        * cond)
{
  /* DANGER, WILL ROBINSON!
     How do we free the cond's semas? */
  if (cond == NULL) return;
  free (cond);
}

/** 
 * @internal Definition for the async queue.
 */
struct _VortexAsyncQueue {
	/** 
	 * @internal Mutex used to synchronize the implemetnation.
	 */
	VortexMutex* mutex;
	/** 
	 * @internal Conditional variable used to hang threads inside
	 * the mutex condition when no data is available.
	 */
	VortexCond*    cond;
	/** 
	 * @internal The list of items stored in the list.
	 */
	axlList     * data;
	/** 
	 * @internal The number of waiting threads.
	 */
	int           waiters;
	/** 
	 * @internal Reference counting support.
	 */
	int           reference;
};

/** 
 * @brief Creates a new async message queue, a inter thread
 * communication that allows to communicate and synchronize data
 * between threads inside the same process.
 * 
 * Once created, you can use the following function to push and
 * retrieve data from the queue:
 * 
 *  - \ref vortex_async_queue_push
 *  - \ref vortex_async_queue_pop
 *  - \ref vortex_async_queue_timedpop
 *
 * You can increase the reference counting by using \ref
 * vortex_async_queue_ref. If the reference count reaches zero value,
 * the queue is deallocated.
 *
 * A particular useful function is \ref vortex_async_queue_length
 * which returns the number of queue items minus waiting threads. 
 * 
 * @return A newly created async queue, with a reference count equal
 * to 1. To dealloc it when no longer needed, use \ref
 * vortex_async_queue_unref. Note reference returned must be checked
 * to be not NULL (caused by memory allocation error).
 */
VortexAsyncQueue * vortex_async_queue_new       (void)
{
	VortexAsyncQueue * result;

	/* create the node */
	result            = axl_new (VortexAsyncQueue, 1);
	VORTEX_CHECK_REF (result, NULL);

	/* init list of stored items */
	result->data      = axl_list_new (axl_list_always_return_1, NULL);
	VORTEX_CHECK_REF2 (result->data, NULL, result, axl_free);

	/* init mutex and conditional variable */
	vortex_mutex_create (&result->mutex);
	vortex_cond_create  (&result->cond);
	
	/* reference counting support initialized to 1 */
	result->reference = 1;

	return result;
}

/** 
 * @brief Allows to push data into the queue.
 * 
 * @param queue The queue where data will be pushed.
 *
 * @param data A reference to the data to be pushed. It is not allowed
 * to push null references.
 */
void               vortex_async_queue_push      (VortexAsyncQueue * queue,
						 axlPointer         data)
{
	v_return_if_fail (queue);
	v_return_if_fail (data);
	
	/* get the mutex */
	vortex_mutex_lock(queue->mutex);

	/* push the data */
	axl_list_prepend (queue->data, data);
        FUEL_WITH_PROGRESS ("async_queue_push inside lock");
	/* signal if waiters are available */
        if (queue->waiters > 0)
		vortex_cond_signal (queue->cond);

	/* unlock the mutex */
	vortex_mutex_unlock(queue->mutex);
	
	return;
}

/** 
 * @brief Allows to push data into the queue withtout acquiring the
 * internal lock. This function must be used in conjuntion with \ref
 * vortex_async_queue_lock and \ref vortex_async_queue_unlock.
 * 
 * @param queue The queue where data will be pushed.
 *
 * @param data A reference to the data to be pushed. It is not allowed
 * to push null references.
 */
void               vortex_async_queue_unlocked_push  (VortexAsyncQueue * queue,
						      axlPointer         data)
{

	v_return_if_fail (queue);
	v_return_if_fail (data);

	/* push the data */
	axl_list_prepend (queue->data, data);
	
	return;
}

/** 
 * @brief Allows to push data into the queue but moving the reference
 * provided into the queue head (causing next call to
 * vortex_async_queue_pop to receive this reference). This function
 * performs the same as \ref vortex_async_queue_push but skiping all
 * items already pushed.
 * 
 * @param queue The queue where data will be pushed.
 *
 * @param data A reference to the data to be pushed. It is not allowed
 * to push null references.
 */
void               vortex_async_queue_priority_push  (VortexAsyncQueue * queue,
						      axlPointer         data)
{
	v_return_if_fail (queue);
	v_return_if_fail (data);
	
	/* get the mutex */
	vortex_mutex_lock(queue->mutex);
        
	/* push the data at the head */
	axl_list_append (queue->data, data);
        FUEL_WITH_PROGRESS ("async_queue_priority_push inside lock");
	/* signal if waiters are available */
        if (queue->waiters > 0)
		vortex_cond_signal (queue->cond);

	/* unlock the mutex */
	vortex_mutex_unlock(queue->mutex);
	
	return;
}

/** 
 * @brief Pop the first data available in the queue, locking
 * the calling if no data is available.
 *
 * The function is ensured to return with a reference to some data. 
 * 
 * @param queue The queue where data will be required.
 * 
 * @return A reference to the next data available.
 */
axlPointer         vortex_async_queue_pop       (VortexAsyncQueue * queue)
{
	axlPointer _result;

	v_return_val_if_fail (queue, NULL);

        FUEL_WITH_PROGRESS ("vortex_async_queue_pop [before grabbing mutex]");
	/* get the mutex */
	vortex_mutex_lock(queue->mutex);
	/* update the number of waiters */
	queue->waiters++;
	/* check if data is available */

	while (axl_list_length (queue->data) == 0) {
          FUEL_WITH_PROGRESS ("vortex_async_queue_pop [in while loop, waiting on cond]");
          printf ("cond_wait on %p->cond\n", queue);
          VORTEX_COND_WAIT (queue->cond, queue->mutex);
        }

        FUEL_WITH_PROGRESS ("vortex_async_queue_pop [getting last]");
	/* get data from the queue */
	_result = axl_list_get_last (queue->data);

        FUEL_WITH_PROGRESS ("in vortex_async_queue_pop [removing last]");
	/* remove the data from the queue */
	axl_list_remove_last (queue->data);

	/* decrease the number of waiters */
	queue->waiters--;

        FUEL_WITH_PROGRESS ("vortex_async_queue_pop [unlocking mutex]");
	/* unlock the mutex */
	vortex_mutex_unlock(queue->mutex);
	return _result;
}

/** 
 * @brief Pop the first data available in the queue, locking the
 * calling if no data is available, but bounding the waiting to the
 * value provided.
 *
 * The function is ensured to return with a reference to some data.
 * 
 * @param queue The queue where data will be required.
 * 
 * @param microseconds The period to wait.
 * 
 * @return A reference to the next data available.
 * 
 * @param queue 
 * @param microseconds 
 * 
 * @return A reference to the data queue, or NULL if the timeout is
 * reached.
 */
axlPointer         vortex_async_queue_timedpop  (VortexAsyncQueue * queue,
						 long               microseconds)
{
	axlPointer _result;
	axl_bool   r;

	v_return_val_if_fail (queue, NULL);
	v_return_val_if_fail (microseconds > 0, NULL);

	/* get the mutex */
	vortex_mutex_lock(queue->mutex);
        FUEL_WITH_PROGRESS ("async_queue_timedpop inside lock");
	/* update the number of waiters */
	queue->waiters++;

	/* check timed wait */
	if (axl_list_length (queue->data) == 0) {

		/* check if data is available */
          FUEL_WITH_PROGRESS ("before timedwait");
		VORTEX_COND_TIMEDWAIT (r, queue->cond, queue->mutex, microseconds);
		/* check again the queue */
		if (axl_list_length (queue->data) == 0) {

			
			/* decrease the number of waiters */
			queue->waiters--;
			
			vortex_mutex_unlock(queue->mutex);
			return NULL;
		} /* end if */
	} /* end if */

	/* get data from the queue */
	_result = axl_list_get_last (queue->data);
	
	/* remove the data from the queue */
	axl_list_remove_last (queue->data);

	/* decrease the number of waiters */
	queue->waiters--;

	/* unlock the mutex */
	vortex_mutex_unlock(queue->mutex);
	
	return _result;
}

/** 
 * @brief Allows to get current queue status.
 * 
 * @param queue The queue to oper.
 * 
 * @return The number of items stored minus the number of thread
 * waiting. The function returns 0 if the reference received is null.
 */
int                vortex_async_queue_length    (VortexAsyncQueue * queue)
{
	int result;

	v_return_val_if_fail (queue, 0);

	/* get the mutex */
	vortex_mutex_lock(queue->mutex);
        FUEL_WITH_PROGRESS ("async_queue_length inside lock");
	/* check status */
	result = axl_list_length (queue->data) - queue->waiters;

	/* unlock the mutex */
	vortex_mutex_unlock(queue->mutex);

	return result;
}

/** 
 * @brief Allows to get current waiting threads on the provided queue.
 * 
 * @param queue The queue that is being used to request the number of
 * waiting threads.
 * 
 * @return The number of waiting threads or -1 if it fails. The only
 * way to make the function to fail is to provide a null queue
 * reference.
 */
int                vortex_async_queue_waiters   (VortexAsyncQueue * queue)
{
	int result;

	v_return_val_if_fail (queue, -1);

	/* get the mutex */
	vortex_mutex_lock(queue->mutex);

	/* check status */
	result = queue->waiters;

	/* unlock the mutex */
	vortex_mutex_unlock(queue->mutex);

	return result;
}

/** 
 * @brief Allows to get current items installed on the queue, pending
 * to be readed.
 * 
 * @param queue A reference to the queue that will be checked for its
 * pending data.
 * 
 * @return 0 or the number of data pending. -1 is returned if a null
 * reference is received.
 */
int                vortex_async_queue_items     (VortexAsyncQueue * queue)
{

	int result;

	v_return_val_if_fail (queue, -1);

	/* get the mutex */
	vortex_mutex_lock(queue->mutex);

	/* check status */
	result = axl_list_length (queue->data);

	/* unlock the mutex */
	vortex_mutex_unlock(queue->mutex);

	return result;
	
}

/** 
 * @brief Allows to update the reference counting for the provided queue.
 * 
 * @param queue The async queue to increase its reference.
 */
void               vortex_async_queue_ref       (VortexAsyncQueue * queue)
{
	v_return_if_fail (queue);

	/* get the mutex */
	vortex_mutex_lock(queue->mutex);

	/* update reference */
	queue->reference++;

	/* unlock the mutex */
	vortex_mutex_unlock(queue->mutex);

	return;
}

/** 
 * @brief Decrease the reference counting deallocating all resources
 * associated with the queue if such counting reach zero.
 * 
 * @param queue The queue to decrease its reference counting.
 */
void               vortex_async_queue_unref     (VortexAsyncQueue * queue)
{
	v_return_if_fail (queue);

	/* get the mutex */
	vortex_mutex_lock(queue->mutex);

	/* update reference */
	queue->reference--;

	/* check reference couting */
	if (queue->reference == 0) {

		/* free the list */
		axl_list_free (queue->data);
		queue->data = NULL;

		/* free the conditional var */
		vortex_cond_destroy (queue->cond);
		
		/* unlock the mutex */
		vortex_mutex_unlock(queue->mutex);

		/* destroy the mutex */
		vortex_mutex_destroy(queue->mutex);

		/* free the node itself */
		axl_free (queue);

		return;
	} /* end if */

	/* unlock the mutex */
	vortex_mutex_unlock(queue->mutex);
	
	return;
}

/** 
 * @internal Release memory used by queue without acquiring mutexes or
 * checking queue references. This is currently used by vortex
 * reinitialization after fork operations.
 */
void             vortex_async_queue_release (VortexAsyncQueue * queue)
{
	if (queue == NULL)
		return;
	axl_list_free (queue->data);
	queue->data = NULL;
	axl_free (queue);
	return;
}

/** 
 * @brief Allows to perform a safe unref operation (nullifying the
 * caller's queue reference).
 *
 * @param queue The queue where to perform the safe unref operation.
 */
void               vortex_async_queue_safe_unref (VortexAsyncQueue ** queue)
{
	VortexAsyncQueue * _queue = (*queue);

	v_return_if_fail (_queue);

	/* get the mutex */
	vortex_mutex_lock(_queue->mutex);

	/* update reference */
	_queue->reference--;

	/* check reference couting */
	if (_queue->reference == 0) {

		/* nullify queue */
		(*queue) = NULL;

		/* free the list */
		axl_list_free (_queue->data);
		_queue->data = NULL;

		/* free the conditional var */
		vortex_cond_destroy (_queue->cond);
		
		/* unlock the mutex */
		vortex_mutex_unlock(_queue->mutex);

		/* destroy the mutex */
		vortex_mutex_destroy(_queue->mutex);

		/* free the node itself */
		axl_free (_queue);

		return;
	} /* end if */

	/* unlock the mutex */
	vortex_mutex_unlock(_queue->mutex);
	
	return;
}


/** 
 * @brief Allows to perform a foreach operation on the provided queue,
 * applying the provided function over all items stored.
 * 
 * @param queue The queue that will receive the foreach operation.
 * @param foreach_func The function to call for each item found.
 * @param user_data User defined pointer to be passed to the function.
 */
void               vortex_async_queue_foreach   (VortexAsyncQueue         * queue,
						 VortexAsyncQueueForeach    foreach_func,
						 axlPointer                 user_data)
{
	axlListCursor * cursor;
	int             iterator;
	axlPointer      ref;

	v_return_if_fail (queue);
	v_return_if_fail (foreach_func);

	/* get the mutex */
	vortex_mutex_lock(queue->mutex);

	/* create a cursor */
	cursor   = axl_list_cursor_new (queue->data);
	iterator = 0;
	while (axl_list_cursor_has_item (cursor)) {
          FUEL_WITH_PROGRESS ("async_queue_foreach in while()");
		/* call to the function */
		ref = axl_list_cursor_get (cursor);
		foreach_func (queue, ref, iterator, user_data);
		
		/* next item */
		axl_list_cursor_next (cursor);
		iterator++;

	} /* end while */

	/* free cursor */
	axl_list_cursor_free (cursor);

	/* unlock the mutex */
	vortex_mutex_unlock(queue->mutex);

	return;
}

/** 
 * @brief Allows to lock the queue, making the caller the only thread
 * owning the queue. This function should be used in conjuntion with
 * vortex_async_queue_unlocked_push. Call to vortex_async_queue_push
 * will lock the caller forever until a call to
 * vortex_async_queue_unlock is done.
 *
 * @param queue The queue to lock.
 * 
 * NOTE: To produce portable code, the thread calling to this function
 * must also call to \ref vortex_async_queue_unlock. It is not
 * supported by Microsoft Windows platforms to do a call to \ref
 * vortex_async_queue_unlock from a different thread that issue the
 * call to \ref vortex_async_queue_lock.
 */
void               vortex_async_queue_lock      (VortexAsyncQueue * queue)
{
	v_return_if_fail (queue);
	vortex_mutex_lock(queue->mutex);
	return;
}

/** 
 * @brief Allows to unlock the queue. See \ref
 * vortex_async_queue_lock.
 * @param queue The queue to unlock.
 */
void               vortex_async_queue_unlock    (VortexAsyncQueue * queue)
{
	v_return_if_fail (queue);

	/* signal if waiters are available */
        if (queue->waiters > 0)
		vortex_cond_signal (queue->cond);

	vortex_mutex_unlock(queue->mutex);
	return;
}

/** 
 * @} 
 */
