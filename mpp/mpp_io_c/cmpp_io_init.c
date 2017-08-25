#include <stdbool.h>
#include "cmpp_io_context_t_api.h"
#include "cmpp_io_debug.h"
#include "cmpp_io_init.h"
#include "netcdf.h"

static int major_version = 0;
static int minor_version = 0;

/*---------------------------------------------------------------------------*/
/*Initialize the cmpp_io library.*/
void cmpp_io_init(cmpp_io_context_t **context,
                  int const max_num_netcdf_files,
                  int const max_num_regular_files,
                  int const max_num_hdf5_files,
                  bool const debug_flag,
                  bool const verbose_flag,
                  int const header_buffer_val,
                  int const shuffle,
                  int const deflate,
                  int const deflate_level)
{
    /*Local variables*/
    cmpp_io_context_t *cptr = NULL;

    /*Set the local pointer to the cmpp_io_context_t object.*/
    cptr = *context;

    /*If this a null context pointer was passed in, then initialize it. If
      a non-null pointer was passed in, then return.*/
    if (cptr == NULL)
    {
        cptr = cmpp_io_context_create(max_num_netcdf_files,
                                      max_num_regular_files,
                                      max_num_hdf5_files,
                                      debug_flag,
                                      verbose_flag,
                                      header_buffer_val,
                                      shuffle,
                                      deflate,
                                      deflate_level);
    }
    else
    {
        note("the cmpp_io library (version %d.%d) has already been"
             " initialized.",
             major_version,
             minor_version);
        return;
    }

    /*Write out an initialization message.*/
    note("initializing the cmpp_io library (version %d.%d).\nUsing netCDF"
             " (version %s).",
         major_version,
         minor_version,
         nc_inq_libvers());

    /*Point to the allocated object.*/
    *context = cptr;
    cptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
