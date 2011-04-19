#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/poll.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <linux/videodev2.h>

#include "enc_settings.h"

#define BUFFERS_REQUESTED 20

typedef struct mmap_buffer {
  void *start;
  size_t length;
} mmap_buffer;

typedef struct v4l2_reader {
  int fd;
  size_t mmap_buffer_count;
  mmap_buffer *mmap_buffers;
} v4l2_reader;

inline void prep (struct v4l2_buffer *buffer) {
  memset (buffer, 0, sizeof (struct v4l2_buffer));
  buffer->type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  buffer->memory = V4L2_MEMORY_MMAP;
}

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

void v4l2_reader_delete (v4l2_reader *v) {
  int i;
  
  if (!v) return;

  if (v->mmap_buffers) {
    for (i = 0; i < v->mmap_buffer_count; i++) {
      munmap (v->mmap_buffers[i].start, v->mmap_buffers[i].length);
    }
    free (v->mmap_buffers);
  }
  close (v->fd);
  free (v);
}

v4l2_reader* v4l2_reader_new (void) {
  v4l2_reader *v;

  v = malloc (sizeof (v4l2_reader));
  if (v) {
    if (v->fd > -1) close (v->fd);
    v->mmap_buffer_count = 0;
    v->mmap_buffers = NULL;
  }
  return v;
}

int v4l2_reader_open (v4l2_reader *v) {
  int err;
  struct v4l2_streamparm stream;
  struct v4l2_format format;
  
  v->fd = open ("/dev/video0", O_RDWR);

  /* set pixel format, width, height, fps */
  /* try setting format and size */
  memset (&format, 0x00, sizeof (struct v4l2_format));
  format.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  if (ioctl (v->fd, VIDIOC_G_FMT, &format) < 0) {
    log_err ("get video format info");
    return 0;
  }
  
  if (format.type != V4L2_BUF_TYPE_VIDEO_CAPTURE ||
      format.fmt.pix.width != enc_frame_width ||
      format.fmt.pix.height != enc_frame_height ||
      format.fmt.pix.pixelformat != V4L2_PIX_FMT_YUYV) {
    
    format.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    format.fmt.pix.width = enc_frame_width;
    format.fmt.pix.height = enc_frame_height;
    format.fmt.pix.pixelformat = V4L2_PIX_FMT_YUYV;
    format.fmt.pix.field = 1;

    if ((err = ioctl (v->fd, VIDIOC_S_FMT, &format)) < 0) {
      log_err ("setting pixel format and frame dimensions");
      return 0;
    }
  }

  printf ("colorspace: %d\n", format.fmt.pix.colorspace);
  
  /* stream params: first set type and get output params */
  memset (&stream, 0x00, sizeof (struct v4l2_streamparm));
  stream.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  ioctl (v->fd, VIDIOC_G_PARM, &stream);

  /* some cameras don't allow changing frame rates. try it... */
  if ((stream.parm.capture.capability & V4L2_CAP_TIMEPERFRAME) == 1) {
    stream.parm.capture.timeperframe.numerator = enc_fps_numerator;
    stream.parm.capture.timeperframe.denominator = enc_fps_denominator;

    if (ioctl (v->fd, VIDIOC_S_PARM, &stream) < 0) {
      log_err ("setting framerate");
      /* don't return -1 here. just accept the lower framerate */
    }
  }

  printf ("framerate set to %2.2f fps\n",
          ((float) stream.parm.capture.timeperframe.denominator /
           (float) stream.parm.capture.timeperframe.numerator));
  
  return 1;
}

void v4l2_reader_get_params (v4l2_reader *v,
                             /* output */
                             int *frame_width,
                             int *frame_height,
                             int *fps_num,
                             int *fps_denom,
                             int *buffer_count) {
  struct v4l2_streamparm stream;
  struct v4l2_format format;

  ioctl (v->fd, VIDIOC_G_PARM, &stream);
  ioctl (v->fd, VIDIOC_G_FMT, &format);
  
  *frame_width = format.fmt.pix.width;
  *frame_height = format.fmt.pix.height;
  *fps_num = stream.parm.capture.timeperframe.numerator;
  *fps_denom = stream.parm.capture.timeperframe.denominator;
  *buffer_count = v->mmap_buffer_count;
}

int v4l2_reader_make_buffers (v4l2_reader *v) {
  int i;
  struct v4l2_requestbuffers reqbuf;
  struct v4l2_buffer buffer;
      
  memset (&reqbuf, 0, sizeof (struct v4l2_requestbuffers));
  reqbuf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  reqbuf.count = BUFFERS_REQUESTED;
  reqbuf.memory = V4L2_MEMORY_MMAP;

  if (ioctl (v->fd, VIDIOC_REQBUFS, &reqbuf) < 0) {
    log_err ("requesting mmap buffers");
    return 0;
  }

  printf ("using %d buffers\n", reqbuf.count);
  v->mmap_buffers = calloc (reqbuf.count, sizeof (mmap_buffer));
  v->mmap_buffer_count = reqbuf.count;

  for (i = 0; i < reqbuf.count; i++) {  
    prep (&buffer);
    buffer.index = i;
    
    if (-1 == ioctl (v->fd, VIDIOC_QUERYBUF, &buffer)) {
      log_err ("VIDIOC_QUERYBUF");
      return 0;
    }
    
    v->mmap_buffers[i].length = buffer.length;
    v->mmap_buffers[i].start = mmap (NULL, buffer.length,
                                PROT_READ | PROT_WRITE,
                                MAP_SHARED, v->fd, buffer.m.offset);
    if (MAP_FAILED == v->mmap_buffers[i].start) {
      log_err ("starting buffer mapping");
      return 0;
    }
  }

  return 1;
}

int v4l2_reader_start_stream (v4l2_reader *v) {
  int i = V4L2_BUF_TYPE_VIDEO_CAPTURE;

  if (ioctl (v->fd, VIDIOC_STREAMON, &i) < 0) {
    log_err ("starting streaming on device");
    return 0;
  } else {
    return 1;
  }
}

int v4l2_reader_enqueue_buffer (v4l2_reader *v, int index) {
  struct v4l2_buffer buffer;

  prep (&buffer);
  buffer.index = index;
  
  if (ioctl (v->fd, VIDIOC_QBUF, &buffer) < 0) {
    log_err ("queuing buffer");
    return 0;
  } else {
    return 1;
  }
}

v4l2_reader* v4l2_reader_setup (void) {
  v4l2_reader *v;
  int i;

  v = v4l2_reader_new ();
  if (!v) return NULL;
  if (! (v4l2_reader_open (v))) {
    perror ("could not open device. reader not deleted yet!\n");
  } else {
    v4l2_reader_make_buffers (v);
    v4l2_reader_start_stream (v);
    for (i = 0; i < v->mmap_buffer_count; i++) {
      v4l2_reader_enqueue_buffer (v, i);
    }
  }
  return v;
}

int is_ready (v4l2_reader *v) {
  struct pollfd pfd;

  pfd.fd = v->fd;
  pfd.events = POLLIN;
  
  if (poll (&pfd, 1, -1) > 0) {
    return pfd.revents & POLLIN;
  } else {
    return 0;
  }
}

int v4l2_reader_dequeue_buffer (v4l2_reader *v, struct v4l2_buffer *buffer) {

  prep (buffer);
  
  if (ioctl (v->fd, VIDIOC_DQBUF, buffer) < 0) {
    log_err ("dequeuing buffer");
    return 0;
  } else {
    return 1;
  }
}

unsigned char * v4l2_reader_get_frame (v4l2_reader *v,
                                       /* output */
                                       int *size,
                                       int *framenum,
                                       int *index) {
  struct v4l2_buffer buffer;
  
  if (is_ready (v)) {
    if (v4l2_reader_dequeue_buffer (v, &buffer)) {
      *size = buffer.bytesused;
      *framenum = buffer.sequence;
      *index = buffer.index;
      return v->mmap_buffers[buffer.index].start;
    } else {
      log_err ("couldn't dequeue anything");
    }
  }

  *size = 0;
  *framenum = -1;
  *index = -1;
  return NULL;
}

int main (void) { return 0; }
