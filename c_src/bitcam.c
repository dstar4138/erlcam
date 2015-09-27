#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <linux/videodev2.h>

#include "video_device.h"
#include "capture.h"
#include "util.h"

#define USER_BUF_SIZE 20

static int DEVICE_DESCRIPTOR = -1;
unsigned char* YUYV_BUFFER = NULL;

void initialize( );
int determine_format( char *format, int *format_type );
int determine_field( char *field, int *field_type );
void dump_frame( int widht, int height );
int startCamera( char* device_name, int width, int height, 
                    char *format, char *field );
void stopCamera( );

void print_usage( char** argv );
int input_available( fd_set *input, char *buffer, int buffer_size );
int is_quit( char *user_input );

int main( int argc, char **argv ) {

    int ret = EXIT_SUCCESS;
    fd_set input_stream;
    char input_buffer[USER_BUF_SIZE];
    char *device_name;

    // Default height/width/format/field.
    int height = 640;
    int width = 480;
    char *format = "MJPEG";
    char *field  = "NONE";

    /* Validate Args, make sure we have a device to listen to. */
    if( argc != 2 && argc != 6 ) {
        print_usage( argv );
        exit( EXIT_FAILURE );
    }
    
    device_name = argv[1];
    if( argc > 2 ) {
        height = atoi(argv[2]);
        width  = atoi(argv[3]);
        format = argv[4];
        field  = argv[5]; 
    }
    DEBUG_PRINT("Device: %s\n", device_name);
    DEBUG_PRINT("Height: %d, Width: %d\n", height, width);
    DEBUG_PRINT("Format: %s, Field: %s\n", format, field);

    // Start the camera.
    if( startCamera( device_name, width, height, format, field ) == 
            EXIT_FAILURE ) 
    {
       stopCamera( );
       exit( EXIT_FAILURE );
    }

    // Program loop. On user input (except 'q') push encoded frame to stdout.
    while (1) {
        // Hang for input.   
        ret = input_available( &input_stream, input_buffer, USER_BUF_SIZE );
       
        // If input was a request to quit, then break loop.
        if( ret || is_quit( input_buffer ) ) {
            break;
        }

        // Otherwise, foreach time around the loop, we want 
        // to dump the current frame.
        dump_frame( width, height );
    }

    // Cleanup
    stopCamera();
    exit(ret);
}

void print_usage(char** argv) {
    printf("BitCam, prints what your webcam sees. Just press Enter, or q to Quit.\n");
    printf("Usage: %s <device> [ <height> <width> <format> <field> ]\n", argv[0]);
    printf("Note that device should be the full path, i.e. /dev/video0\n");
}

int is_quit( char *user_input ) {
    switch( user_input[0] ) {
    case 'q':
        return 1;
    default:
        return 0;
    }
}

int input_available( fd_set *input_stream, char *buffer, int buffer_size ) {
    bzero( buffer, buffer_size );
    FD_ZERO( input_stream );
    FD_SET( STDIN_FILENO, input_stream );

    // No timeout, hang on user input via STDIN.
    if( select( STDIN_FILENO + 1, input_stream, NULL, NULL, NULL ) < 0 ) {
        return EXIT_FAILURE;
    }
    
    // If no failure, dump what was read, from input stream.
    fgets( buffer, buffer_size, stdin );
    return EXIT_SUCCESS;
}

void dump_frame(int width, int height) {
    if( !YUYV_BUFFER ) {
        DEBUG_PRINT("Unable to load frame, buffer not initialized");
        return;
    }

    // Dump the details from the camera. 
    capture(DEVICE_DESCRIPTOR, FRAME_BUFFERS, width, height, YUYV_BUFFER);

    // Write the bytes out exactly as we saw them. 
    write_exact( YUYV_BUFFER, width*height );
}

int startCamera( char* device_name, int width, int height, 
                    char *format, char *field ) {
    int result = open_device( device_name, &DEVICE_DESCRIPTOR );
    if(result == ERROR_LOCAL) {
        return result;
    }

    int format_type = 0;
    result = determine_format( format, &format_type );
    if( result == ERROR_LOCAL ) {
        return result;
    }

    int field_type = V4L2_FIELD_NONE;
    result = determine_field( field, &field_type );
    if( result == ERROR_LOCAL ) {
        return result;
    }

    result = init_device( DEVICE_DESCRIPTOR, width, height, 
                            format_type, field_type );
    if(result == ERROR_LOCAL) {
        return result;
    }

    result = start_capture( DEVICE_DESCRIPTOR );
    if(result != SUCCESS_LOCAL) {
        stopCamera();
        DEBUG_PRINT("Unable to start capture, resetting device");
    } else {
        int area = width * height;
        YUYV_BUFFER = (unsigned char*)malloc( sizeof(char) * area);
    }

    return result;
}

void stopCamera() {
    stop_capturing( DEVICE_DESCRIPTOR );
    uninit_device( );
    close_device( &DEVICE_DESCRIPTOR );
    
    if( YUYV_BUFFER ) {
        free(YUYV_BUFFER);
    }
}

int determine_format( char *format, int *format_type ) {
    // http://linuxtv.org/downloads/v4l-dvb-apis/pixfmt.html
    if (strcasecmp("mjpeg",format)) {
        *format_type = V4L2_PIX_FMT_MJPEG;
    } else if (strcasecmp("argb32",format)) {
        *format_type = V4L2_PIX_FMT_ARGB32;
    } else if (strcasecmp("yuyv",format)) {
        *format_type = V4L2_PIX_FMT_YUYV;
    } else if (strcasecmp("grey",format)) {
        *format_type = V4L2_PIX_FMT_GREY;
    } else {
        DEBUG_PRINT("Only supported formats are: mjpeg, argb32, yuyv, grey");
        return ERROR_LOCAL;
    }

    return SUCCESS_LOCAL;
}

int determine_field( char *field, int *field_type ) {
    //http://linuxtv.org/downloads/v4l-dvb-apis/field-order.html
    if (strcasecmp("none",field)) {
        *field_type = V4L2_FIELD_NONE;
    } else if (strcasecmp("interlaced",field)) {
        *field_type = V4L2_FIELD_INTERLACED;
    } else if (strcasecmp("top",field)) {
        *field_type = V4L2_FIELD_TOP;
    } else if (strcasecmp("bottom",field)) {
        *field_type = V4L2_FIELD_BOTTOM;
    } else {
        DEBUG_PRINT("Only supported fields are: none, interlaced, top, bottom");
        return ERROR_LOCAL;
    }

    return SUCCESS_LOCAL;
}

