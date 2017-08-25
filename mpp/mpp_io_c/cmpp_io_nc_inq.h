#ifndef SET_CMPP_IO_NC_INQ_H_
#define SET_CMPP_IO_NC_INQ_H_

#include "cmpp_io_context_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

void cmpp_io_nc_inq(cmpp_io_context_t * const context,
                    int const file_index,
                    int * const num_global_atts,
                    int * const num_dims,
                    int * const num_vars);

#endif
