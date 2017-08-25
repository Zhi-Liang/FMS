#ifndef SET_CMPP_IO_ATTRIBUTE_T_API_H_
#define SET_CMPP_IO_ATTRIBUTE_T_API_H_

#include <stdlib.h>
#include "netcdf.h"

typedef struct cmpp_io_attribute_type cmpp_io_attribute_t;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

cmpp_io_attribute_t *cmpp_io_attribute_create(char const * const name,
                                              nc_type const type_in_file,
                                              nc_type const type_in_mem,
                                              size_t const num_values,
                                              void const * const values);

void cmpp_io_attribute_destroy(cmpp_io_attribute_t **self);

char *cmpp_io_attribute_get_name(cmpp_io_attribute_t * const self);

nc_type cmpp_io_attribute_get_type_in_file(cmpp_io_attribute_t const * const self);

nc_type cmpp_io_attribute_get_type_in_mem(cmpp_io_attribute_t const * const self);

size_t cmpp_io_attribute_get_num_values(cmpp_io_attribute_t const * const self);

void *cmpp_io_attribute_get_values(cmpp_io_attribute_t * const self);

#endif
