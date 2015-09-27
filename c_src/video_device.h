#ifndef __VIDEO_DEVICE_H__
#define __VIDEO_DEVICE_H__

#include <stdio.h>

typedef struct {
    void* start;
    size_t length;
} buffer;

unsigned int BUFFER_COUNT;
buffer* FRAME_BUFFERS;

int open_device(const char* dev_name, int* fd);
int init_mmap(int fd);
int init_device(int fd, int width, int height, int format, int field);
int uninit_device();
int close_device(int* fd);

#endif // __VIDEO_DEVICE_H__
