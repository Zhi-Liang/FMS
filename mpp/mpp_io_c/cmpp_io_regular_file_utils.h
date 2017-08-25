#ifndef SET_CMPP_IO_REGULAR_FILE_UTILS_H_
#define SET_CMPP_IO_REGULAR_FILE_UTILS_H_

#include <stdbool.h>
#include "cmpp_io_context_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

void cmpp_io_flush_regular_file(cmpp_io_context_t * const context,
                                int const file_index);

char *cmpp_io_get_regular_file_name(cmpp_io_context_t * const context,
                                    int const file_index);

bool cmpp_io_get_regular_file_is_open(cmpp_io_context_t * const context,
                                      int const file_index);

void cmpp_io_fread_regular_file(cmpp_io_context_t * const context,
                                int const file_index,
                                int const num_vals,
                                int const data_type,
                                int const val_max_width,
                                char const delimiter,
                                void **data_buf,
                                bool const last_read);

#endif
