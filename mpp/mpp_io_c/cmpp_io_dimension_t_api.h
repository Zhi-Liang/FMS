#ifndef SET_CMPP_IO_DIMENSION_T_API_H_
#define SET_CMPP_IO_DIMENSION_T_API_H_

#include <stdbool.h>
#include <stdlib.h>

typedef struct cmpp_io_dimension_type cmpp_io_dimension_t;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

cmpp_io_dimension_t *cmpp_io_dimension_create(char const * const name,
                                              size_t const length,
                                              int const netcdf_id,
                                              bool const is_unlimited,
                                              int const current_level);

void cmpp_io_dimension_destroy(cmpp_io_dimension_t **self);

char *cmpp_io_dimension_get_name(cmpp_io_dimension_t * const self);

size_t cmpp_io_dimension_get_length(cmpp_io_dimension_t const * const self);

bool cmpp_io_dimension_get_is_unlimited(cmpp_io_dimension_t const * const self);

int cmpp_io_dimension_get_current_level(cmpp_io_dimension_t const * const self);

int cmpp_io_dimension_get_netcdf_id(cmpp_io_dimension_t const * const self);

void cmpp_io_dimension_advance_level(cmpp_io_dimension_t * const * const self);

#endif
