#ifndef SET_CMPP_IO_NETCDF_FILE_UTILS_H_
#define SET_CMPP_IO_NETCDF_FILE_UTILS_H_

#include <stdbool.h>
#include "cmpp_io_context_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

void cmpp_io_flush_netcdf_file(cmpp_io_context_t * const context,
                               int const file_index);

char *cmpp_io_get_netcdf_file_name(cmpp_io_context_t * const context,
                                   int const file_index);

bool cmpp_io_get_netcdf_file_is_open(cmpp_io_context_t * const context,
                                     int const file_index);

#endif
