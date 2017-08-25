#ifndef SET_CMPP_IO_NETCDF_UTILS_H_
#define SET_CMPP_IO_NETCDF_UTILS_H_

#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include "netcdf.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

void check_netcdf_call(int const res);

void vcheck_netcdf_call(int const res,
                        char * const prefixf,
                        char * const suffixf,
                        ...);

void check_netcdf_attribute_types(nc_type const type_in_mem,
                                  nc_type const type_in_file);

void check_netcdf_variable_types(nc_type const type_in_mem,
                                 nc_type const type_in_file);

int cmpp_io_get_ncflags(void);

size_t cmpp_io_get_netCDF_chunksize(char const * const fname);

void read_netcdf_attribute(int const ncid,
                           int const netcdf_variable_id,
                           int const netcdf_attribute_id,
                           char * const name_buf,
                           size_t const name_buf_size,
                           nc_type * const type_in_file,
                           nc_type * const type_in_mem,
                           size_t * const num_values,
                           void **values_buf,
                           char const * const file_name);

void write_netcdf_attribute(int const ncid,
                            int const variable_id,
                            nc_type const type_in_mem,
                            char * const name,
                            nc_type const type_in_file,
                            size_t const num_values,
                            void * const values,
                            char const * const file_name);

void read_netcdf_dimension(int const ncid,
                           int const netcdf_dimension_id,
                           char * const name_buf,
                           size_t const name_buf_size,
                           size_t * const length,
                           bool * const is_unlimited,
                           char const * const file_name);

int write_netcdf_dimension(int const ncid,
                           char * const name,
                           size_t const length,
                           char const * const file_name);

void read_netcdf_variable_metadata(int const ncid,
                                   int const netcdf_variable_id,
                                   char * const name_buf,
                                   size_t const name_buf_size,
                                   nc_type * const type_in_file,
                                   int * const num_dimensions,
                                   int * const dimension_ids,
                                   int * const num_attributes);

int write_netcdf_variable_metadata(int const ncid,
                                   nc_type const type_in_file,
                                   char * const name,
                                   int * const dimension_ids,
                                   int const num_dimensions);

void read_netcdf_variable_data(int const ncid,
                               int const netcdf_variable_id,
                               size_t const * const corner_indices,
                               size_t const * const edge_lengths,
                               void * const * const data,
                               nc_type const type_in_mem);

void write_netcdf_variable_data(int const ncid,
                                int const netcdf_variable_id,
                                size_t const * const corner_indices,
                                size_t const * const edge_lengths,
                                void * const data,
                                nc_type const type_in_mem);

#endif
