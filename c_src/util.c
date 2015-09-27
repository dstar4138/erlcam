#include "util.h"

#include <errno.h>
#include <string.h>

int errnoexit(const char *s) {
    DEBUG_PRINT("%s error %d, %s", s, errno, strerror(errno));
    return ERROR_LOCAL;
}

void write_exact( unsigned char *buf, int len ) {
    int i, wrote=0;
    do {
        if ((i = write(1, buf+wrote, len-wrote)) <= 0) {
            break;
        }        
        wrote += i;
    } while (wrote<len);
}

int xioctl(int fd, int request, void *arg) {
    int r;

    do {
        r = ioctl(fd, request, arg);
    } while(-1 == r && EINTR == errno);

    return r;
}
