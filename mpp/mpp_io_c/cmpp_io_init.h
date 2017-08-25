#ifndef SET_CMPP_IO_INIT_H_
#define SET_CMPP_IO_INIT_H_

#include <stdbool.h>
#include "cmpp_io_context_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

void cmpp_io_init(cmpp_io_context_t **context,
                  int const max_num_netcdf_files,
                  int const max_num_regular_files,
                  int const max_num_hdf5_files,
                  bool const debug_flag,
                  bool const verbose_flag,
                  int const header_buffer_val,
                  int const shuffle,
                  int const deflate,
                  int const deflate_level);

#endif
