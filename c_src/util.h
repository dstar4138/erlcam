#ifndef __UTIL__H__
#define __UTIL__H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/ioctl.h>

#define ERROR_LOCAL   EXIT_FAILURE
#define SUCCESS_LOCAL EXIT_SUCCESS

#ifdef DEBUG
    #define DEBUG_PRINT( args... ) fprintf(stderr, args)
#else
    #define DEBUG_PRINT( args... )
#endif

#define CLEAR(x) memset(&(x), 0, sizeof(x))

int errnoexit(const char *s);

int xioctl(int fd, int request, void *arg);

void write_exact( unsigned char* buf, int len );

#endif // __UTIL__H__
