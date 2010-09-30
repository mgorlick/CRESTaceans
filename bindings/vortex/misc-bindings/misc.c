#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/select.h>

int rkt_pipe (int fd[2]) {
  return pipe(fd);
}

int rkt_read (int fd, char* Buff, int NumBytes) {
  return read(fd, Buff, NumBytes);
}

int rkt_write (int fd, char* Buff, int NumBytes) {
  return write(fd, Buff, NumBytes);
}

fd_set* rkt_fd_make (void) {
  return (malloc (sizeof (fd_set)));
}

void rkt_fd_free (fd_set* set) {
  free(set);
}

void rkt_fd_zero (fd_set* set) {
  FD_ZERO(set);
}

void rkt_fd_set (int fd, fd_set* set) {
  FD_SET(fd, set);
}

void rkt_fd_clr (int fd, fd_set* set) {
  FD_CLR(fd, set);
}

int rkt_fd_isset (int fd, fd_set* set) {
  return FD_ISSET(fd, set);
}

int rkt_select (int nfds, fd_set* readfds, fd_set* writefds,
                fd_set* exceptfds, struct timeval* timeout) {
  return select(nfds, readfds, writefds, exceptfds, timeout);
}

struct timeval* rkt_timeval_make (long sec, long usec) {
  struct timeval* t = (struct timeval*) malloc(sizeof (struct timeval));
  t->tv_sec = sec;
  t->tv_usec = usec;
  return t;
}

void rkt_timeval_free (struct timeval* t) {
  free(t);
}
