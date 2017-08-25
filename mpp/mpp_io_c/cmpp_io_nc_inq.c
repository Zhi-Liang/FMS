#include <stdlib.h>
#include "cmpp_io_context_t_api.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_nc_inq.h"
#include "cmpp_io_netcdf_file_t_api.h"
#include "cmpp_io_netcdf_utils.h"
#include "netcdf.h"

/*---------------------------------------------------------------------------*/
/*Return the number of global attributes, dimensions, and variables in a
  netcdf file.*/
void cmpp_io_nc_inq(cmpp_io_context_t * const context,
                    int const file_index,
                    int * const num_global_atts,
                    int * const num_dims,
                    int * const num_vars)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    int err;
    int dummy;

    /*Get a pointer to the file object.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Read the desired file properies.*/
    err = nc_inq(cmpp_io_netcdf_file_get_ncid(fptr),
                 num_dims,
                 num_vars,
                 num_global_atts,
                 &dummy);
    check_netcdf_call(err);
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
