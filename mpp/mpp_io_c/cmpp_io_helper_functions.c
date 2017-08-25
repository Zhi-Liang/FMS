#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cmpp_io_debug.h"
#include "cmpp_io_helper_functions.h"

/*---------------------------------------------------------------------------*/
/*A portable way to clearly identify and silence intentionally unused
  variables.*/
#ifdef __GNUC__
#  define UNUSED(x) UNUSED_ ## x __attribute__((__unused__))
#else
#  define UNUSED(x) UNUSED_ ## x
#endif

#ifdef __GNUC__
#  define UNUSED_FUNCTION(x) __attribute__((__unused__)) UNUSED_ ## x
#else
#  define UNUSED_FUNCTION(x) UNUSED_ ## x
#endif

/*---------------------------------------------------------------------------*/
/*Check that an inputted string is not longer than an inputted maximum length.
  If it is, then throw a fatal error.  If not, then copy the string into the
  inputted buffer and null-terminate the buffer.  Return the number of
  bytes copied plus one for the trailing null byte.*/
size_t safe_string_copy(char const * const str,
                        size_t const max_length,
                        char * const buf)
{
    /*Check inputs.*/
    error_check(str,
                "null input string at address %p.",
                (void *) &str);
    error_check(buf,
                "null input buffer at address %p.",
                (void *) &buf);
    error_check(max_length > 0,
                "input maximum length at address %p is zero.",
                (void *) &max_length);
    error_check(strlen(str)+1 <= max_length,
                "inputted string (%s) is longer than maximum length"
                    " (%zu) characters.",
                str,
                max_length);

    /*Copy the string into the buffer.*/
    snprintf(buf,
             max_length,
             "%s",
             str);

    return (strlen(buf)+1)*sizeof(char);
}

/*--------------------------------------------------------------------------*/
/*Safe malloc function.*/
void cmpp_io_safemalloc(void **addrOfptr,
                        size_t nbytes)
{
    if (*addrOfptr == NULL)
    {
        *addrOfptr = malloc(nbytes);
        error_check(*addrOfptr,
                    "malloc of %zu bytes at address %p failed.",
                    nbytes,
                    (void *) addrOfptr);
    }
    else
    {
        fatal("inputted pointer at address %p is not null.",
              (void *) addrOfptr);
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Safe free function.*/
void cmpp_io_safefree(void **addrOfptr)
{
    if (*addrOfptr != NULL)
    {
        free(*addrOfptr);
        *addrOfptr = NULL;
    }
    else
    {
        fatal("inputted pointer at address %p is null.",
              (void *)addrOfptr);
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Safe memset function.*/
void cmpp_io_safememset(void *ptr,
                        int const value,
                        size_t const nbytes)
{
    error_check(ptr,
                "inputted pointer at address %p is null.",
                (void *) &ptr);
    memset(ptr,
           value,
           nbytes);

    return;
}

/*---------------------------------------------------------------------------*/
/*Safe memcpy function.*/
void cmpp_io_safememcpy(void *dest,
                        void const * const source,
                        size_t const num_bytes)
{
    error_check(dest,
                "inputted destination pointer at address %p is null.",
                (void *) &dest);
    error_check(source,
                "inputted source pointer at address %p is null.",
                (void *) &source);
    memcpy(dest,
           source,
           num_bytes);

    return;
}

/*---------------------------------------------------------------------------*/
