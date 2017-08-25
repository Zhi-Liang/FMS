#ifndef SET_CMPP_IO_H_
#define SET_CMPP_IO_H_

#include <stdbool.h>
#include <stdlib.h>
#include "cmpp_io_macros.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

size_t mpp_io_c_strlen(void *ptr);

void mpp_io_c_init(int const max_num_netcdf_files,
                   int const max_num_regular_files,
                   int const max_num_hdf5_files,
                   bool const debug_flag,
                   bool const verbose_flag,
                   int const header_buffer_val,
                   int const shuffle,
                   int const deflate,
                   int const deflate_level);

void mpp_io_c_finalize(void);

int mpp_io_c_open_regular_file(char * const fname,
                               int const f_action);

void mpp_io_c_close_regular_file(int * const file_index,
                                 int const action,
                                 bool const do_warn);

void mpp_io_c_flush_regular_file(int const file_index);

void mpp_io_c_fread_regular_file(int const file_index,
                                 int const num_vals,
                                 int const data_type,
                                 int const val_max_width,
                                 char const delimiter,
                                 void **data_buf,
                                 bool const last_read);

int mpp_io_c_open_netcdf_file(char * const fname,
                              int const f_action,
                              int const max_num_global_attributes,
                              int const max_num_dimensions,
                              int const max_num_variables,
                              int const max_num_attributes_per_variable,
                              int const max_num_dimensions_per_variable,
                              size_t const max_metadata_num_bytes);

void mpp_io_c_close_netcdf_file(int * const file_index,
                                int const action,
                                bool const do_warn);

void mpp_io_c_flush_netcdf_file(int const file_index);

int mpp_io_c_read_netcdf_attribute(int const file_index,
                                   int const variable_index,
                                   int const netcdf_attribute_id);

int mpp_io_c_write_netcdf_attribute(int const file_index,
                                    int const variable_index,
                                    int const type_in_file,
                                    size_t const num_values,
                                    char * const name,
                                    void * const values,
                                    int const type_in_mem,
                                    bool const buffer_values);

void mpp_io_c_free_netcdf_attribute(int const file_index,
                                    int const variable_index,
                                    int * const attribute_index);

int mpp_io_c_get_netcdf_attribute_index(int const file_index,
                                        int const variable_index,
                                        char * const att_name);

char *mpp_io_c_get_netcdf_attribute_name(int const file_index,
                                         int const variable_index,
                                         int const attribute_index);

int mpp_io_c_get_netcdf_attribute_type_in_file(int const file_index,
                                               int const variable_index,
                                               int const attribute_index);

int mpp_io_c_get_netcdf_attribute_type_in_mem(int const file_index,
                                              int const variable_index,
                                              int const attribute_index);

size_t mpp_io_c_get_netcdf_attribute_num_values(int const file_index,
                                                int const variable_index,
                                                int const attribute_index);

void *mpp_io_c_get_netcdf_attribute_values(int const file_index,
                                           int const variable_index,
                                           int const attribute_index);

int mpp_io_c_read_netcdf_dimension(int const file_index,
                                   int const netcdf_dimension_id);

int mpp_io_c_write_netcdf_dimension(int const file_index,
                                    char * const name,
                                    size_t const length,
                                    bool const buffer_values);

void mpp_io_c_advance_netcdf_dimension_level(int const file_index,
                                             int const dimension_index);

void mpp_io_c_free_netcdf_dimension(int const file_index,
                                    int * const dimension_index);

int mpp_io_c_get_netcdf_dimension_index_global(int const file_index,
                                               char * const dim_name);

int mpp_io_c_get_netcdf_dimension_index_variable(int const file_index,
                                                 int const variable_index,
                                                 int const dimension_index);

char *mpp_io_c_get_netcdf_dimension_name(int const file_index,
                                         int const dimension_index);

size_t mpp_io_c_get_netcdf_dimension_length(int const file_index,
                                            int const dimension_index);

bool mpp_io_c_get_netcdf_dimension_is_unlimited(int const file_index,
                                                int const dimension_index);

int mpp_io_c_get_netcdf_dimension_current_level(int const file_index,
                                                int const dimension_index);

int mpp_io_c_get_netcdf_dimension_id(int const file_index,
                                     int const dimension_index);

int mpp_io_c_read_netcdf_variable_metadata(int const file_index,
                                           int const netcdf_variable_id);

int mpp_io_c_write_netcdf_variable_metadata(int const file_index,
                                            int const type_in_file,
                                            char * const name,
                                            int * const dimension_indices,
                                            int const num_dimensions,
                                            int const max_num_attributes,
                                            bool const buffer_metadata);

void mpp_io_c_read_netcdf_variable_data(int const file_index,
                                        int const variable_index,
                                        size_t * const corner_indices,
                                        size_t * const edge_lengths,
                                        void * const * const data,
                                        int const type_in_mem);

void mpp_io_c_write_netcdf_variable_data(int const file_index,
                                         int const variable_index,
                                         size_t * const corner_indices,
                                         size_t * const edge_lengths,
                                         void * const data,
                                         int const type_in_mem);

void mpp_io_c_buffer_netcdf_variable_data(int const file_index,
                                          int const variable_index,
                                          size_t * const corner_indices,
                                          size_t * const edge_lengths,
                                          void * const data,
                                          int const type_in_mem,
                                          bool const overwrite);

void mpp_io_c_write_buffered_netcdf_variable_data(int const file_index,
                                                  int const variable_index,
                                                  bool const free_data);

void mpp_io_c_free_netcdf_variable(int const file_index,
                                   int * const variable_index);

int mpp_io_c_get_netcdf_variable_index(int const file_index,
                                       char * const var_name);

bool mpp_io_c_get_netcdf_variable_is_data_buffered(int const file_index,
                                                   int const variable_index);

int mpp_io_c_get_netcdf_variable_max_num_attributes(int const file_index,
                                                    int const variable_index);

char *mpp_io_c_get_netcdf_variable_name(int const file_index,
                                        int const variable_index);

int mpp_io_c_get_netcdf_variable_type_in_file(int const file_index,
                                              int const variable_index);

int mpp_io_c_get_netcdf_variable_num_dimensions(int const file_index,
                                                int const variable_index);

int mpp_io_c_get_netcdf_variable_num_attributes(int const file_index,
                                                int const variable_index);

int mpp_io_c_get_netcdf_variable_id(int const file_index,
                                    int const variable_index);

void *mpp_io_c_get_netcdf_variable_buffered_data(int const file_index,
                                                 int const variable_index);

size_t *mpp_io_c_get_netcdf_variable_corner_indices(int const file_index,
                                                    int const variable_index);

size_t *mpp_io_c_get_netcdf_variable_edge_lengths(int const file_index,
                                                  int const variable_index);

int mpp_io_c_get_netcdf_variable_type_in_mem(int const file_index,
                                             int const variable_index);

void mpp_io_c_nc_inq(int const file_index,
                     int * const num_global_atts,
                     int * const num_dims,
                     int * const num_vars);

char *mpp_io_c_get_netcdf_file_name(int const file_index);

char *mpp_io_c_get_regular_file_name(int const file_index);

bool mpp_io_c_get_netcdf_file_is_open_by_index(int const file_index);

bool mpp_io_c_get_netcdf_file_is_open_by_name(char * const name);

bool mpp_io_c_get_regular_file_is_open_by_index(int const file_index);

bool mpp_io_c_get_regular_file_is_open_by_name(char * const name);

int mpp_io_c_get_file_index(char * const name,
                            int const type);

#endif
