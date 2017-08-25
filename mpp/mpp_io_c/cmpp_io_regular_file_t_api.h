#ifndef SET_CMPP_IO_REGULAR_FILE_T_API_H_
#define SET_CMPP_IO_REGULAR_FILE_T_API_H_

#include <stdio.h>
#include "cmpp_io_file_props_t_api.h"

typedef struct cmpp_io_regular_file_type cmpp_io_regular_file_t;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

cmpp_io_regular_file_t *cmpp_io_regular_file_create(cmpp_io_file_props_t * const file_props);

void cmpp_io_regular_file_destroy(cmpp_io_regular_file_t **self);

cmpp_io_file_props_t *cmpp_io_regular_file_get_file_props(cmpp_io_regular_file_t * const self);

FILE *cmpp_io_regular_file_get_file_ptr(cmpp_io_regular_file_t * const self);

char *cmpp_io_regular_file_get_name(cmpp_io_regular_file_t * const self);

void cmpp_io_regular_file_open(cmpp_io_regular_file_t * const * const self);

void cmpp_io_regular_file_flush(cmpp_io_regular_file_t const * const self);

void cmpp_io_regular_file_close(cmpp_io_regular_file_t * const * const self);

void cmpp_io_regular_file_delete(cmpp_io_regular_file_t const * const self);

void cmpp_io_regular_file_fread(cmpp_io_regular_file_t const * const self,
                                int const num_vals,
                                int const data_type,
                                int const val_max_width,
                                char const delimiter,
                                void **data_buf,
                                bool const last_read);

#endif
