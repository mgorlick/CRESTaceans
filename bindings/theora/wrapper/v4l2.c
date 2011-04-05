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

typedef  struct mmap_buffer {
    void *start;
    size_t length;
} mmap_buffer;

typedef struct v4l2_reader {
  int fd;
  mmap_buffer *buffers;
  int mmap_buffer_count;
} v4l2_reader;

v4l2_reader* v4l2_reader_new (void) {
  v4l2_reader *v;

  v = malloc (sizeof (v4l2_reader));
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
    perror ("Couldn't get video format info\n");
    return -1;
  }

  printf ("defaults:\n");
  printf ("\twidth = %d\n", format.fmt.pix.width);
  printf ("\theight = %d\n", format.fmt.pix.height);
  printf ("\ttype = %d\n", format.type);
  printf ("\tpixelfmt = %d (YVU420 is %d; YUYV is %d; I420 is %d)\n",
          format.fmt.pix.pixelformat,
          V4L2_PIX_FMT_YVU420, V4L2_PIX_FMT_YUYV, v4l2_fourcc ('I','4','2','0'));
  printf ("\tfield = %d\n", format.fmt.pix.field);
  
  if (format.type != V4L2_BUF_TYPE_VIDEO_CAPTURE ||
      format.fmt.pix.width != enc_frame_width ||
      format.fmt.pix.height != enc_frame_height ||
      format.fmt.pix.pixelformat != V4L2_PIX_FMT_YUYV) {
    
    format.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    format.fmt.pix.width = enc_frame_width;
    format.fmt.pix.height = enc_frame_height;
    format.fmt.pix.pixelformat = V4L2_PIX_FMT_YUYV;
    format.fmt.pix.field = V4L2_FIELD_INTERLACED;
    
    if ((err = ioctl (v->fd, VIDIOC_S_FMT, &format)) < 0) {
      /* note: could add "try again with progressive video" step */
      printf ("Error setting pixel format, frame dimensions: %d\n", errno);
      return -1;
    }    
  }

  printf ("video capture set to %dx%d size\n", enc_frame_width, enc_frame_height);
  printf ("video capture set to %d format\n", format.fmt.pix.pixelformat);
  
  /* stream params: first set type and get output params */
  memset (&stream, 0x00, sizeof (struct v4l2_streamparm));
  stream.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  ioctl (v->fd, VIDIOC_G_PARM, &stream);

  /* according to gstreamer comments some cameras don't allow
     changing frame rates. try it... */
  if ((stream.parm.capture.capability & V4L2_CAP_TIMEPERFRAME) == 0) {
    /* FIXME: check framerate fraction inequality before changing */
    stream.parm.capture.timeperframe.numerator = enc_fps_numerator;
    stream.parm.capture.timeperframe.denominator = enc_fps_denominator;

    if (ioctl (v->fd, VIDIOC_S_PARM, &stream) < 0) {
      printf ("Error setting framerate\n");
    }
  }
  
  return 1;
}

void v4l2_reader_make_buffers (v4l2_reader *v) {
  int i;
  struct v4l2_requestbuffers reqbuf;

  memset (&reqbuf, 0, sizeof (struct v4l2_requestbuffers));
  reqbuf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  reqbuf.count = 2;
  reqbuf.memory = V4L2_MEMORY_MMAP;

  if (ioctl (v->fd, VIDIOC_REQBUFS, &reqbuf) < 0) {
    switch (errno) {
      case EINVAL:
        perror ("mmap not supported on this device\n");
      default:
        perror ("couldn't request any buffers with mmap\n");
    }
    return;
  }

  printf ("Using %d buffers\n", reqbuf.count);
  v->buffers = calloc (reqbuf.count, sizeof (mmap_buffer));
  v->mmap_buffer_count = reqbuf.count;
  
  for (i = 0; i < reqbuf.count; i++) {
    struct v4l2_buffer buffer;

    memset (&buffer, 0, sizeof (buffer));
    buffer.type = reqbuf.type;
    buffer.memory = V4L2_MEMORY_MMAP;
    buffer.index = i;
    
    if (-1 == ioctl (v->fd, VIDIOC_QUERYBUF, &buffer)) {
      perror ("VIDIOC_QUERYBUF");
    }
    
    v->buffers[i].length = buffer.length; /* remember for munmap() */
    v->buffers[i].start = mmap (NULL, buffer.length,
                                PROT_READ | PROT_WRITE, /* recommended */
                                MAP_SHARED,             /* recommended */
                                v->fd, buffer.m.offset);
    if (MAP_FAILED == v->buffers[i].start) {
      perror ("Error starting buffer mapping\n");
    }
  }

  for (i = 0; i < v->mmap_buffer_count; i++) {
    printf ("buffer %d length: %d\n", i, v->buffers[i].length);
  }

}

void v4l2_reader_delete (v4l2_reader *v) {
  if (!v) return;
  free (v);
}

v4l2_reader* v4l2_reader_setup (void) { /* equivalent to gst_v4l2_open() */
  v4l2_reader *v;

  v = v4l2_reader_new ();
  if (!v) return NULL;
  if (v4l2_reader_open (v) < 1) {
    printf ("Error, could not open device. Reader has not been deleted!\n");
  } else {
    printf ("Successfully made v4l2 reader\n");
    v4l2_reader_make_buffers (v);
  }
  return v;
}

void v4l2_reader_read (v4l2_reader *v) {

}

int main (void) { return 0; }
