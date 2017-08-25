#ifndef SET_CMPP_IO_ATTRIBUTE_UTILS_H_
#define SET_CMPP_IO_ATTRIBUTE_UTILS_H_

#include <stdbool.h>
#include <stdlib.h>
#include "cmpp_io_context_t_api.h"
#include "netcdf.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

int cmpp_io_read_netcdf_attribute(cmpp_io_context_t * const context,
                                  int const file_index,
                                  int const variable_index,
                                  int const netcdf_attribute_id);

int cmpp_io_write_netcdf_attribute(cmpp_io_context_t * const context,
                                   int const file_index,
                                   int const variable_index,
                                   nc_type const type_in_file,
                                   size_t const num_values,
                                   char * const name,
                                   void * const values,
                                   nc_type const type_in_mem,
                                   bool const buffer_values);

void cmpp_io_free_netcdf_attribute(cmpp_io_context_t * const context,
                                   int const file_index,
                                   int const variable_index,
                                   int * const attribute_index);

int cmpp_io_get_netcdf_attribute_index(cmpp_io_context_t * const context,
                                       int const file_index,
                                       int const variable_index,
                                       char * const att_name);

char * cmpp_io_get_netcdf_attribute_name(cmpp_io_context_t * const context,
                                         int const file_index,
                                         int const variable_index,
                                         int const attribute_index);

nc_type cmpp_io_get_netcdf_attribute_type_in_file(cmpp_io_context_t * const context,
                                                  int const file_index,
                                                  int const variable_index,
                                                  int const attribute_index);

nc_type cmpp_io_get_netcdf_attribute_type_in_mem(cmpp_io_context_t * const context,
                                                 int const file_index,
                                                 int const variable_index,
                                                 int const attribute_index);

size_t cmpp_io_get_netcdf_attribute_num_values(cmpp_io_context_t * const context,
                                               int const file_index,
                                               int const variable_index,
                                               int const attribute_index);

void *cmpp_io_get_netcdf_attribute_values(cmpp_io_context_t * const context,
                                          int const file_index,
                                          int const variable_index,
                                          int const attribute_index);

#endif
