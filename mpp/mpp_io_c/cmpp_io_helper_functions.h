#ifndef SET_CMPP_IO_HELPER_FUNCTIONS_H_
#define SET_CMPP_IO_HELPER_FUNCTIONS_H_

#include <stdlib.h>
#include "netcdf.h"

/*---------------------------------------------------------------------------*/
/*Macros*/
/* clearly and explicitly silence intentionally unused things */
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
/*Functions*/

size_t safe_string_copy(char const * const str,
                        size_t const max_length,
                        char * const buf);

void cmpp_io_safemalloc(void **,
                        size_t);

void cmpp_io_safefree(void **);

void cmpp_io_safememset(void *,
                        int const,
                        size_t const);

void cmpp_io_safememcpy(void *,
                        void const * const,
                        size_t const);

#endif
