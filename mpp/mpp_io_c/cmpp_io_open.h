#ifndef SET_CMPP_IO_OPEN_H_
#define SET_CMPP_IO_OPEN_H_

#include <stdlib.h>
#include "cmpp_io_context_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

int cmpp_io_open_netcdf_file(cmpp_io_context_t * const * const context,
                             char * const name,
                             int const action,
                             int const max_num_global_attributes,
                             int const max_num_dimensions,
                             int const max_num_variables,
                             int const max_num_attributes_per_variable,
                             int const max_num_dimensions_per_variable,
                             size_t const max_metadata_num_bytes);

int cmpp_io_open_regular_file(cmpp_io_context_t * const * const context,
                              char * const name,
                              int const action);

#endif
