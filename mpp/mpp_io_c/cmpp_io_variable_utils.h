#ifndef SET_CMPP_IO_VARIABLE_UTILS_H_
#define SET_CMPP_IO_VARIABLE_UTILS_H_

#include <stdbool.h>
#include <stdlib.h>
#include "cmpp_io_context_t_api.h"
#include "netcdf.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

int cmpp_io_read_netcdf_variable_metadata(cmpp_io_context_t * const context,
                                          int const file_index,
                                          int const netcdf_variable_id);

int cmpp_io_write_netcdf_variable_metadata(cmpp_io_context_t * const context,
                                           int const file_index,
                                           nc_type const type_in_file,
                                           char * const name,
                                           int * const dimension_indices,
                                           int const num_dimensions,
                                           int const max_num_attributes,
                                           bool const buffer_metadata);

void cmpp_io_read_netcdf_variable_data(cmpp_io_context_t * const context,
                                       int const file_index,
                                       int const variable_index,
                                       size_t * const corner_indices,
                                       size_t * const edge_lengths,
                                       void * const * const data,
                                       nc_type const type_in_mem);

void cmpp_io_write_netcdf_variable_data(cmpp_io_context_t * const context,
                                        int const file_index,
                                        int const variable_index,
                                        size_t * const corner_indices,
                                        size_t * const edge_lengths,
                                        void * const data,
                                        nc_type const type_in_mem);

void cmpp_io_buffer_netcdf_variable_data(cmpp_io_context_t * const context,
                                         int const file_index,
                                         int const variable_index,
                                         size_t * const corner_indices,
                                         size_t * const edge_lengths,
                                         void * const data,
                                         nc_type const type_in_mem,
                                         bool const overwrite);

void cmpp_io_write_buffered_netcdf_variable_data(cmpp_io_context_t * const context,
                                                 int const file_index,
                                                 int const variable_index,
                                                 bool const free_data);

void cmpp_io_free_netcdf_variable(cmpp_io_context_t * const context,
                                  int const file_index,
                                  int * const variable_index);

int cmpp_io_get_netcdf_variable_index(cmpp_io_context_t * const context,
                                      int const file_index,
                                      char * const var_name);

bool cmpp_io_get_netcdf_variable_is_data_buffered(cmpp_io_context_t * const context,
                                                  int const file_index,
                                                  int const variable_index);

int cmpp_io_get_netcdf_variable_max_num_attributes(cmpp_io_context_t * const context,
                                                   int const file_index,
                                                   int const variable_index);

char *cmpp_io_get_netcdf_variable_name(cmpp_io_context_t * const context,
                                       int const file_index,
                                       int const variable_index);

nc_type cmpp_io_get_netcdf_variable_type_in_file(cmpp_io_context_t * const context,
                                                 int const file_index,
                                                 int const variable_index);

int cmpp_io_get_netcdf_variable_num_dimensions(cmpp_io_context_t * const context,
                                               int const file_index,
                                               int const variable_index);

int cmpp_io_get_netcdf_variable_num_attributes(cmpp_io_context_t * const context,
                                               int const file_index,
                                               int const variable_index);

int cmpp_io_get_netcdf_variable_id(cmpp_io_context_t * const context,
                                   int const file_index,
                                   int const variable_index);

void *cmpp_io_get_netcdf_variable_buffered_data(cmpp_io_context_t * const context,
                                                int const file_index,
                                                int const variable_index);

size_t *cmpp_io_get_netcdf_variable_corner_indices(cmpp_io_context_t * const context,
                                                   int const file_index,
                                                   int const variable_index);

size_t *cmpp_io_get_netcdf_variable_edge_lengths(cmpp_io_context_t * const context,
                                                 int const file_index,
                                                 int const variable_index);

nc_type cmpp_io_get_netcdf_variable_type_in_mem(cmpp_io_context_t * const context,
                                                int const file_index,
                                                int const variable_index);

#endif
