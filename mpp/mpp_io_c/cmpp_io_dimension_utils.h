#ifndef SET_CMPP_IO_DIMENSION_UTILS_H_
#define SET_CMPP_IO_DIMENSION_UTILS_H_

#include <stdbool.h>
#include <stdlib.h>
#include "cmpp_io_context_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

int cmpp_io_read_netcdf_dimension(cmpp_io_context_t * const context,
                                  int const file_index,
                                  int const netcdf_dimension_id);

int cmpp_io_write_netcdf_dimension(cmpp_io_context_t * const context,
                                   int const file_index,
                                   char * const name,
                                   size_t const length,
                                   bool const buffer_values);

void cmpp_io_advance_netcdf_dimension_level(cmpp_io_context_t * const context,
                                            int const file_index,
                                            int const dimension_index);

void cmpp_io_free_netcdf_dimension(cmpp_io_context_t * const context,
                                   int const file_index,
                                   int * const dimension_index);

int cmpp_io_get_netcdf_dimension_index(cmpp_io_context_t * const context,
                                       int const file_index,
                                       char * const dim_name);

int cmpp_io_convert_netcdf_variable_dimension_index(cmpp_io_context_t * const context,
                                                    int const file_index,
                                                    int const variable_index,
                                                    int const dimension_index);

char *cmpp_io_get_netcdf_dimension_name(cmpp_io_context_t * const context,
                                        int const file_index,
                                        int const dimension_index);

size_t cmpp_io_get_netcdf_dimension_length(cmpp_io_context_t * const context,
                                           int const file_index,
                                           int const dimension_index);

bool cmpp_io_get_netcdf_dimension_is_unlimited(cmpp_io_context_t * const context,
                                               int const file_index,
                                               int const dimension_index);

int cmpp_io_get_netcdf_dimension_current_level(cmpp_io_context_t * const context,
                                               int const file_index,
                                               int const dimension_index);

int cmpp_io_get_netcdf_dimension_id(cmpp_io_context_t * const context,
                                    int const file_index,
                                    int const dimension_index);

#endif
