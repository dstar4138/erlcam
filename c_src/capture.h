#ifndef __CAPTURE_H__
#define __CAPTURE_H__

#include <stdio.h>

#include "video_device.h"

int start_capture(int fd);
int stop_capturing(int fd);

void capture(int fd, buffer* frame_buffers, int width, int height, 
                    unsigned char* yuyvbuf);

#endif
