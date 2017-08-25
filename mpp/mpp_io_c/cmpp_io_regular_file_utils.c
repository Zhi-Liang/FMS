#include <stdbool.h>
#include <stdlib.h>
#include "cmpp_io_context_t_api.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_regular_file_t_api.h"
#include "cmpp_io_regular_file_utils.h"

/*---------------------------------------------------------------------------*/
/*Flush a regular file.*/
void cmpp_io_flush_regular_file(cmpp_io_context_t * const context,
                                int const file_index)
{
    /*Local variables*/
    cmpp_io_regular_file_t *fptr = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_ASCII);

    /*Flush the file.*/
    cmpp_io_regular_file_flush(fptr);
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Get the name of a regular file.*/
char *cmpp_io_get_regular_file_name(cmpp_io_context_t * const context,
                                    int const file_index)
{
    /*Local variables*/
    cmpp_io_regular_file_t *fptr = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_ASCII);

    return cmpp_io_regular_file_get_name(fptr);
}

/*---------------------------------------------------------------------------*/
/*Get a flag telling if a regular file is open.*/
bool cmpp_io_get_regular_file_is_open(cmpp_io_context_t * const context,
                                      int const file_index)
{
    /*Local variables*/
    cmpp_io_regular_file_t *fptr = NULL;
    cmpp_io_file_props_t *file_props = NULL;
    bool is_open;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_ASCII);

    file_props = cmpp_io_regular_file_get_file_props(fptr);
    is_open = cmpp_io_file_props_get_is_open(file_props);
    file_props = NULL;
    fptr = NULL;

    return is_open;
}

/*---------------------------------------------------------------------------*/
/*Perform a fortran style read of a list of values from a regular file.*/
void cmpp_io_fread_regular_file(cmpp_io_context_t * const context,
                                int const file_index,
                                int const num_vals,
                                int const data_type,
                                int const val_max_width,
                                char const delimiter,
                                void **data_buf,
                                bool const last_read)
{
    /*Local variables*/
    cmpp_io_regular_file_t *fptr = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_ASCII);

    /*Read from the file.*/
    cmpp_io_regular_file_fread(fptr,
                               num_vals,
                               data_type,
                               val_max_width,
                               delimiter,
                               data_buf,
                               last_read);
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
