#ifndef SET_CMPP_IO_NETCDF_FILE_T_API_H_
#define SET_CMPP_IO_NETCDF_FILE_T_API_H_

#include <stdbool.h>
#include <stdlib.h>
#include "cmpp_io_attribute_t_api.h"
#include "cmpp_io_dimension_t_api.h"
#include "cmpp_io_file_props_t_api.h"
#include "cmpp_io_variable_t_api.h"

typedef struct cmpp_io_netcdf_file_type cmpp_io_netcdf_file_t;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

cmpp_io_netcdf_file_t *cmpp_io_netcdf_file_create(cmpp_io_file_props_t * const file_props,
                                                  int const max_num_global_attributes,
                                                  int const max_num_dimensions,
                                                  int const max_num_variables,
                                                  int const max_num_attributes_per_variable,
                                                  int const max_num_dimensions_per_variable,
                                                  size_t const max_metadata_num_bytes);

void cmpp_io_netcdf_file_destroy(cmpp_io_netcdf_file_t **self);

cmpp_io_file_props_t *cmpp_io_netcdf_file_get_file_props(cmpp_io_netcdf_file_t * const self);

char *cmpp_io_netcdf_file_get_name(cmpp_io_netcdf_file_t * const self);

int cmpp_io_netcdf_file_get_ncid(cmpp_io_netcdf_file_t const * const self);

bool cmpp_io_netcdf_file_get_is_in_define_mode(cmpp_io_netcdf_file_t const * const self);

int cmpp_io_netcdf_file_get_num_dimensions(cmpp_io_netcdf_file_t const * const self);

void cmpp_io_netcdf_file_open(cmpp_io_netcdf_file_t * const * const self);

void cmpp_io_netcdf_file_flush(cmpp_io_netcdf_file_t const * const self);

void cmpp_io_netcdf_file_close(cmpp_io_netcdf_file_t * const * const self);

void cmpp_io_netcdf_file_delete(cmpp_io_netcdf_file_t const * const self);

int cmpp_io_netcdf_file_add_ptr(cmpp_io_netcdf_file_t * const * const self,
                                void * const ptr,
                                int const ptr_type);

void cmpp_io_netcdf_file_remove_ptr(cmpp_io_netcdf_file_t * const * const self,
                                    int * const index,
                                    int const ptr_type);

void *cmpp_io_netcdf_file_get_ptr(cmpp_io_netcdf_file_t const * const self,
                                  int const index,
                                  int const ptr_type);

int cmpp_io_netcdf_file_get_ptr_index(cmpp_io_netcdf_file_t const * const self,
                                      char * const val,
                                      int const ptr_type);

void *cmpp_io_netcdf_file_get_ptr_by_netcdf_id(cmpp_io_netcdf_file_t const * const self,
                                               int netcdf_id,
                                               int const ptr_type);

void cmpp_io_netcdf_file_enter_data_mode(cmpp_io_netcdf_file_t * const * const self,
                                         int const header_buffer_val);

void cmpp_io_netcdf_file_enter_define_mode(cmpp_io_netcdf_file_t * const * const self);

#endif
