#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include "helper_functions.h"
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"
#include "verbosity_t_enumdef.h"

/*---------------------------------------------------------------------------*/
/*Verbose print to stdout.*/
void verbose_print(verbosity_t verbosity,const char *format,...)
{
    va_list args;
    va_start(args,format);
    verbose_fprint_(stdout,verbosity,format,args);
    va_end(args);
    return;
}

/*---------------------------------------------------------------------------*/
/*Verbose print to a file.*/
void verbose_fprint(FILE *stream,verbosity_t verbosity,const char *format,...)
{
    va_list args;
    va_start(args,format);
    verbose_fprint_(stream,verbosity,format,args);
    va_end(args);
    return;
}

/*---------------------------------------------------------------------------*/
/*Verbose print workhorse.*/
void verbose_fprint_(FILE *stream,verbosity_t verbosity,const char *format,
                     va_list args)
{
    if (verbosity)
    {
        vfprintf(stream,format,args);
    }
    return;
}

/*---------------------------------------------------------------------------*/
/*Safe malloc.*/
void safemalloc(void **pointer_address,size_t num_bytes)
{
    if (*pointer_address == NULL)
    {
        *pointer_address = malloc(num_bytes);
        if (*pointer_address == NULL)
        {
            fprintf(stderr,"FATAL: SAFEMALLOC: malloc of %lu bytes failed.\n",
                    num_bytes);
            exit(EXIT_FAILURE);
        }
    }
    else
    {
        fprintf(stderr,"FATAL: SAFEMALLOC: cannot malloc a non-NULL pointer"
                " (%p) at given address %p.\n",*pointer_address,
                (void *)pointer_address);
        exit(EXIT_FAILURE);
    }
    return;
}

/*---------------------------------------------------------------------------*/
/*Safe free.*/
void safefree(void **pointer_address)
{
    if (*pointer_address != NULL)
    {
        free(*pointer_address);
        *pointer_address = NULL;
    }
    else
    {
        fprintf(stderr,"FATAL: SAFEFREE: cannot free a NULL pointer (%p)"
                " at given address %p.\n",*pointer_address,
                (void *)pointer_address);
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that determines if an unsigned integer value exists in an
  unsigned integer array.*/
uint32_t is_int_in_int_array(uint32_t const val,
                             uint32_t const *int_list,
                             size_t const int_list_size)
{
    /*Local variables*/
    int32_t match_flag = -1; /*Flag denoting a match.*/
    uint32_t i = 0;          /*Loop variable.*/

    /*Search through the array for a match.*/
    match_flag = MPP_C_FALSE;
    for (i=0;i<int_list_size;i++)
    {
        if (val == int_list[i])
        {
            match_flag = MPP_C_TRUE;
            break;
        }
    }

    return match_flag;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the total number of bytes of an MPI_Datatype array.*/
size_t get_MPI_data_size_in_bytes(MPI_Datatype const mpi_type_val,
                                  size_t const array_len)
{
    /*Local variables.*/
    size_t num_bytes = 0;

    switch (mpi_type_val)
    {
        case MPI_INT32_T:
            num_bytes = sizeof(int32_t)*array_len;
            break;
        case MPI_UINT32_T:
            num_bytes = sizeof(uint32_t)*array_len;
            break;
        case MPI_INT64_T:
            num_bytes = sizeof(int64_t)*array_len;
            break;
        case MPI_UINT64_T:
            num_bytes = sizeof(uint64_t)*array_len;
            break;
        case MPI_FLOAT:
            num_bytes = sizeof(float)*array_len;
            break;
        case MPI_DOUBLE:
            num_bytes = sizeof(double)*array_len;
            break;
        case MPI_CHAR:
            num_bytes = sizeof(char)*array_len;
            break;
        default:
            fprintf(stderr,"FATAL: GET_MPI_DATA_SIZE_IN_BYTES: unknown"
                           " MPI type.");
            exit(EXIT_FAILURE);
    }

    return num_bytes;
}

/*---------------------------------------------------------------------------*/
