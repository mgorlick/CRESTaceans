#include <errno.h>

inline void log_err (char *msg) {
  int i = errno;
  printf ("error (%s): ", msg);
    switch (i) {
      case EINVAL:
        printf ("EINVAL");
        break;
      case EAGAIN:
        printf ("EAGAIN");
        break;
      case ENOMEM:
        printf ("ENOMEM");
        break;
      case EIO:
        printf ("EIO");
        break;
      default:
        printf ("unknown error %d", i);
    }
    printf ("\n");
}
