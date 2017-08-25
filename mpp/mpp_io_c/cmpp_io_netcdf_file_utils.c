#include <stdbool.h>
#include <stdlib.h>
#include "cmpp_io_context_t_api.h"
#include "cmpp_io_file_props_t_api.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_netcdf_file_t_api.h"
#include "cmpp_io_netcdf_file_utils.h"

/*---------------------------------------------------------------------------*/
/*Flush a netcdf file.*/
void cmpp_io_flush_netcdf_file(cmpp_io_context_t * const context,
                               int const file_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Flush the file.*/
    cmpp_io_netcdf_file_flush(fptr);
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Get the name of a netcdf file.*/
char *cmpp_io_get_netcdf_file_name(cmpp_io_context_t * const context,
                                   int const file_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_file_props_t *file_props = NULL;
    char *file_name = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    file_props = cmpp_io_netcdf_file_get_file_props(fptr);
    file_name = cmpp_io_file_props_get_name(file_props);
    file_props = NULL;
    fptr = NULL;

    return file_name;
}

/*---------------------------------------------------------------------------*/
/*Get a flag telling if a netcdf file is open.*/
bool cmpp_io_get_netcdf_file_is_open(cmpp_io_context_t * const context,
                                     int const file_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_file_props_t *file_props = NULL;
    bool is_open;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    file_props = cmpp_io_netcdf_file_get_file_props(fptr);
    is_open = cmpp_io_file_props_get_is_open(file_props);
    file_props = NULL;
    fptr = NULL;

    return is_open;
}

/*---------------------------------------------------------------------------*/
