#include <stdlib.h>
#include "cmpp_io_context_t_api.h"
#include "cmpp_io_file_props_t_api.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_netcdf_file_t_api.h"
#include "cmpp_io_open.h"
#include "cmpp_io_regular_file_t_api.h"

/*---------------------------------------------------------------------------*/
/*Open a netcdf file and store necessary file properties.*/
int cmpp_io_open_netcdf_file(cmpp_io_context_t * const * const context,
                             char * const name,
                             int const action,
                             int const max_num_global_attributes,
                             int const max_num_dimensions,
                             int const max_num_variables,
                             int const max_num_attributes_per_variable,
                             int const max_num_dimensions_per_variable,
                             size_t const max_metadata_num_bytes)
{
    /*Local variables*/
    cmpp_io_context_t *cptr = NULL;
    cmpp_io_file_props_t *fptr = NULL;
    cmpp_io_netcdf_file_t *nfptr = NULL;
    int file_index;

    /*Set the local pointer to the cmpp_io_context_t object.*/
    cptr = *context;

    /*Create a netcdf file object.*/
    fptr = cmpp_io_file_props_create(name,
                                     action);
    nfptr = cmpp_io_netcdf_file_create(fptr,
                                       max_num_global_attributes,
                                       max_num_dimensions,
                                       max_num_variables,
                                       max_num_attributes_per_variable,
                                       max_num_dimensions_per_variable,
                                       max_metadata_num_bytes);
    fptr = NULL;

    /*Add a the netcdf file to the context's netcdf files array.*/
    file_index = cmpp_io_context_add_file(&cptr,
                                          nfptr,
                                          CMPP_NETCDF);
    /*Open the file.*/
    cmpp_io_netcdf_file_open(&nfptr);
    cptr = NULL;
    nfptr = NULL;

    return file_index;
}

/*---------------------------------------------------------------------------*/
/*Open a regular file and store necessary file properties.*/
int cmpp_io_open_regular_file(cmpp_io_context_t * const * const context,
                              char * const name,
                              int const action)
{
    /*Local variables*/
    cmpp_io_context_t *cptr = NULL;
    cmpp_io_file_props_t *fptr = NULL;
    cmpp_io_regular_file_t *rfptr = NULL;
    int file_index;

    /*Set the local pointer to the cmpp_io_context_t object.*/
    cptr = *context;

    /*Create a regular file object.*/
    fptr = cmpp_io_file_props_create(name,
                                     action);
    rfptr = cmpp_io_regular_file_create(fptr);
    fptr = NULL;

    /*Add the regular file to the context's regular files array.*/
    file_index = cmpp_io_context_add_file(&cptr,
                                          rfptr,
                                          CMPP_ASCII);
    /*Open the file.*/
    cmpp_io_regular_file_open(&rfptr);
    cptr = NULL;
    rfptr = NULL;

    return file_index;
}

/*---------------------------------------------------------------------------*/
