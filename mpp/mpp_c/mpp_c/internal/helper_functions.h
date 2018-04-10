/*Header file for helper functions.*/

#ifndef SET_MPP_C_HELPER_FUNCTIONS_H_
#define SET_MPP_C_HELPER_FUNCTIONS_H_

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "mpp_c_mpi.h"
#include "verbosity_t_enumdef.h"

/*Function declarations*/
void verbose_print(verbosity_t,
                   const char *,
                   ...);

void verbose_fprint(FILE *,
                    verbosity_t,
                    const char *,
                    ...);

void verbose_fprint_(FILE *,
                     verbosity_t,
                     const char*,
                     va_list);

void safemalloc(void **,
                size_t);

void safefree(void **);

uint32_t is_int_in_int_array(uint32_t const,
                             uint32_t const *,
                             size_t const);

size_t get_MPI_data_size_in_bytes(MPI_Datatype const,
                                  size_t const);

#endif
