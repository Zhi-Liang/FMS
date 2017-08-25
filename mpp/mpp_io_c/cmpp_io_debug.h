#ifndef SET_CMPP_IO_DEBUG_H_
#define SET_CMPP_IO_DEBUG_H_

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"

/*Throw fatal errors.*/
#define fatal(mesg,...) \
    fprintf(stderr, \
            "[Error] %s(%s,%d): " mesg "\n", \
            __func__, \
            __FILE__, \
            __LINE__, \
            __VA_ARGS__); \
    MPI_Abort(MPI_COMM_WORLD,1);

/*Print warning.*/
#define warn(mesg,...) \
    fprintf(stderr, \
            "[Warn] %s(%s,%d): " mesg "\n", \
            __func__, \
            __FILE__, \
            __LINE__, \
            __VA_ARGS__);

/*Print messages.*/
#define note(mesg,...) \
    fprintf(stdout, \
            "[Mesg] %s(%s,%d): " mesg "\n", \
            __func__, \
            __FILE__, \
            __LINE__, \
            __VA_ARGS__);

/*Debug check.*/
#ifdef NO_DEBUG
#define debug_check(val,mesg,...)
#else
#define debug_check(val,mesg,...) \
{ \
    if (!(val)) \
    { \
        fatal(mesg, \
              __VA_ARGS__); \
    } \
}
#endif

/*Error check.*/
#define error_check(val,mesg,...) \
{ \
    if (!(val)) \
    { \
        fatal(mesg,\
              __VA_ARGS__); \
    } \
}

#endif
