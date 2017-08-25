#include <stdlib.h>
#include "cmpp_io_context_t_api.h"
#include "cmpp_io_debug.h"
#include "cmpp_io_file_props_t_api.h"
#include "cmpp_io_finalize.h"
#include "cmpp_io_hdf5_file_t_api.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_netcdf_file_t_api.h"
#include "cmpp_io_regular_file_t_api.h"

/*---------------------------------------------------------------------------*/
/*Finalize the cmpp_io library.*/
void cmpp_io_finalize(cmpp_io_context_t **context)
{
    /*Local variables*/
    cmpp_io_context_t *cptr = NULL;
    int num_files;
    int i;
    void *fptr = NULL;
    cmpp_io_file_props_t *file_props = NULL;
    char *file_name = NULL;

    /*Set the local pointer to the cmpp_io_context_t object.*/
    cptr = *context;

    /*Make sure that all netcdf files have been closed properly.*/
    num_files = cmpp_io_context_get_cur_num_netcdf_files(cptr);
    for (i=0;i<num_files;i++)
    {
        fptr = (void *)cmpp_io_context_get_file_ptr(cptr,
                                                    i,
                                                    CMPP_NETCDF);
        if (fptr)
        {
            file_props = cmpp_io_netcdf_file_get_file_props((cmpp_io_netcdf_file_t *)fptr);
            file_name = cmpp_io_file_props_get_name(file_props);
            fatal("please first close netcdf file %s before finalizing the"
                      " cmpp_io library to prevent memory leaks.",
                  file_name);
        }
    }

    /*Make sure that all regular files have been closed properly.*/
    num_files = cmpp_io_context_get_cur_num_regular_files(cptr);
    for (i=0;i<num_files;i++)
    {
        fptr = (void *)cmpp_io_context_get_file_ptr(cptr,
                                                    i,
                                                    CMPP_ASCII);
        if (fptr)
        {
            file_props = cmpp_io_regular_file_get_file_props((cmpp_io_regular_file_t *)fptr);
            file_name = cmpp_io_file_props_get_name(file_props);
            fatal("please first close regular file %s before finalizing the"
                      " cmpp_io library to prevent memory leaks.",
                  file_name);
        }
    }

    /*Make sure that all hdf5 files have been closed properly.*/
    num_files = cmpp_io_context_get_cur_num_hdf5_files(cptr);
    for (i=0;i<num_files;i++)
    {
        fptr = (void *)cmpp_io_context_get_file_ptr(cptr,
                                                    i,
                                                    CMPP_HDF5);
        if (fptr)
        {
            file_props = cmpp_io_hdf5_file_get_file_props((cmpp_io_hdf5_file_t *)fptr);
            file_name = cmpp_io_file_props_get_name(file_props);
            fatal("please first close hdf5 file %s before finalizing the"
                      " cmpp_io library to prevent memory leaks.",
                  file_name);
        }
    }

    /*Destroy the inputted context.*/
    cmpp_io_context_destroy(&cptr);

    /*Point to the deallocated object.*/
    *context = cptr;
    fptr = NULL;
    file_props = NULL;
    file_name = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
