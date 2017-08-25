#ifndef SET_CMPP_IO_CLOSE_H_
#define SET_CMPP_IO_CLOSE_H_

#include <stdbool.h>
#include "cmpp_io_context_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

void cmpp_io_close_netcdf_file(cmpp_io_context_t * const * const context,
                               int * const file_index,
                               int const action,
                               bool const do_warn);

void cmpp_io_close_regular_file(cmpp_io_context_t * const * const context,
                                int * const file_index,
                                int const action,
                                bool const do_warn);

#endif
