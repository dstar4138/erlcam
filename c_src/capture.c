#include "capture.h"
#include "util.h"

#include <assert.h>
#include <fcntl.h>
#include <errno.h>
#include <linux/videodev2.h>

int start_capture(int fd) {
    unsigned int i;
    enum v4l2_buf_type type;

    for(i = 0; i < BUFFER_COUNT; ++i) {
        struct v4l2_buffer buf;
        CLEAR(buf);
        buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        buf.memory = V4L2_MEMORY_MMAP;
        buf.index = i;

        if(-1 == xioctl(fd, VIDIOC_QBUF, &buf)) {
            return errnoexit("VIDIOC_QBUF");
        }
    }

    type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    if(-1 == xioctl(fd, VIDIOC_STREAMON, &type)) {
        return errnoexit("VIDIOC_STREAMON");
    }

    return SUCCESS_LOCAL;
}

int stop_capturing(int fd) {
    enum v4l2_buf_type type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    if(-1 != fd && -1 == xioctl(fd, VIDIOC_STREAMOFF, &type)) {
        return errnoexit("VIDIOC_STREAMOFF");
    }

    return SUCCESS_LOCAL;
}

int read_frame(int fd, buffer* frame_buffers, int width, int height, 
               unsigned char* yuyvbuf) {
    struct v4l2_buffer buf;
    CLEAR(buf);
    buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory = V4L2_MEMORY_MMAP;

    if(-1 == xioctl(fd, VIDIOC_DQBUF, &buf)) {
        switch(errno) {
            case EAGAIN:
                return 0;
            case EIO:
            default:
                return errnoexit("VIDIOC_DQBUF");
        }
    }

    assert(buf.index < BUFFER_COUNT);
    memcpy( yuyvbuf, frame_buffers[buf.index].start, width*height );

    if(-1 == xioctl(fd, VIDIOC_QBUF, &buf)) {
        return errnoexit("VIDIOC_QBUF");
    }

    return 1;
}

void capture(int fd, buffer* frame_buffers, int width, int height,  
                    unsigned char *yuyvbuf) {
    if(fd == -1) {
        return;
    }

    for(;;) {
        fd_set fds;
        FD_ZERO(&fds);
        FD_SET(fd, &fds);

        //struct timeval tv;
        //tv.tv_sec = 2;
        //tv.tv_usec = 0;

        int result = select(fd + 1, &fds, NULL, NULL, NULL);//&tv);
        if(-1 == result) {
            if(EINTR == errno) {
                continue;
            }
            errnoexit("select");
        } else if(0 == result) {
            DEBUG_PRINT("select timeout");
        }

        if(read_frame(fd, frame_buffers, width, height, yuyvbuf) == 1) {
            break;
        }
    }
}

