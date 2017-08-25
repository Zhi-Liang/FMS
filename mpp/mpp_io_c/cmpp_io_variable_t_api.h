#ifndef SET_CMPP_IO_VARIABLE_T_API_H_
#define SET_CMPP_IO_VARIABLE_T_API_H_

#include <stdbool.h>
#include <stdlib.h>
#include "cmpp_io_attribute_t_api.h"
#include "cmpp_io_dimension_t_api.h"
#include "netcdf.h"

typedef struct cmpp_io_variable_type cmpp_io_variable_t;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

cmpp_io_variable_t *cmpp_io_variable_create(char const * const name,
                                            nc_type const type_in_file,
                                            int const netcdf_id,
                                            cmpp_io_dimension_t * const * const dimensions,
                                            int const num_dimensions,
                                            int const max_num_attributes);

void cmpp_io_variable_destroy(cmpp_io_variable_t **self);

bool cmpp_io_variable_get_is_data_buffered(cmpp_io_variable_t const * const self);

int cmpp_io_variable_get_max_num_attributes(cmpp_io_variable_t const * const self);

char *cmpp_io_variable_get_name(cmpp_io_variable_t * const self);

nc_type cmpp_io_variable_get_type_in_file(cmpp_io_variable_t const * const self);

int cmpp_io_variable_get_num_dimensions(cmpp_io_variable_t const * const self);

int cmpp_io_variable_get_num_attributes(cmpp_io_variable_t const * const self);

cmpp_io_dimension_t **cmpp_io_variable_get_dimensions(cmpp_io_variable_t * const self);

cmpp_io_attribute_t **cmpp_io_variable_get_attributes(cmpp_io_variable_t * const self);

int cmpp_io_variable_get_netcdf_id(cmpp_io_variable_t const * const self);

void *cmpp_io_variable_get_data(cmpp_io_variable_t * const self);

size_t *cmpp_io_variable_get_corner_indices(cmpp_io_variable_t * const self);

size_t *cmpp_io_variable_get_edge_lengths(cmpp_io_variable_t * const self);

nc_type cmpp_io_variable_get_type_in_mem(cmpp_io_variable_t const * const self);

int cmpp_io_variable_add_attribute(cmpp_io_variable_t * const * const self,
                                   cmpp_io_attribute_t * const attribute);

void cmpp_io_variable_remove_attribute(cmpp_io_variable_t * const * const self,
                                       int * const attribute_index);

void *cmpp_io_variable_get_ptr(cmpp_io_variable_t const * const self,
                               int const index,
                               int const ptr_type);

int cmpp_io_variable_get_ptr_index(cmpp_io_variable_t const * const self,
                                   char * const val,
                                   int const ptr_type);

void cmpp_io_variable_buffer_data(cmpp_io_variable_t * const * const self,
                                  size_t const * const corner_indices,
                                  size_t const * const edge_lengths,
                                  void const * const data,
                                  nc_type const type_in_mem,
                                  bool const overwrite);

void cmpp_io_variable_write_buffered_data(cmpp_io_variable_t * const * const self,
                                          int const ncid,
                                          bool const free_data);

#endif
