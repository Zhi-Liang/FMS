#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "cmpp_io_attribute_utils.h"
#include "cmpp_io_close.h"
#include "cmpp_io_context_t_api.h"
#include "cmpp_io_debug.h"
#include "cmpp_io_dimension_utils.h"
#include "cmpp_io_finalize.h"
#include "cmpp_io_init.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_nc_inq.h"
#include "cmpp_io_netcdf_file_utils.h"
#include "cmpp_io_open.h"
#include "cmpp_io_regular_file_utils.h"
#include "cmpp_io_variable_utils.h"
#include "netcdf.h"

#ifdef _OPENMP
#include <omp.h>
#endif

/*Library context.*/
static cmpp_io_context_t *context;

/*Helper macros.*/
#ifdef _OPENMP
#define trap_openmp_thread(void) \
    int threading_level = omp_get_level(); \
    error_check(threading_level == 0, \
                "this routine is not thread-safe, but is being called" \
                    " in an OpenMP threaded region (OpenMP level = %d).", \
                threading_level);
#else
#define trap_openmp_thread(void)
#endif

/*Static functions.*/
static nc_type get_nc_type(int const nc_type_in);
static int get_variable_param(int const variable_index);
static int get_int_from_nc_type(nc_type const nc_type_in);
static size_t get_length_param(size_t const length);

/*---------------------------------------------------------------------------*/
/*Wrapper around strlen so it may be called from fortran.*/
size_t mpp_io_c_strlen(void *ptr)
{
    return strlen((char *)ptr);
}

/*---------------------------------------------------------------------------*/
/*Initialize cmpp_io.*/
void mpp_io_c_init(int const max_num_netcdf_files,
                   int const max_num_regular_files,
                   int const max_num_hdf5_files,
                   bool const debug_flag,
                   bool const verbose_flag,
                   int const header_buffer_val,
                   int const shuffle,
                   int const deflate,
                   int const deflate_level)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    /*Initialize the cmpp_io library.*/
    cmpp_io_init(&context,
                 max_num_netcdf_files,
                 max_num_regular_files,
                 max_num_hdf5_files,
                 debug_flag,
                 verbose_flag,
                 header_buffer_val,
                 shuffle,
                 deflate,
                 deflate_level);

    return;
}

/*---------------------------------------------------------------------------*/
/*Finalize cmpp_io.*/
void mpp_io_c_finalize(void)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    /*Finalize the cmpp_io library.*/
    cmpp_io_finalize(&context);

    return;
}

/*---------------------------------------------------------------------------*/
/*Open a regular file.*/
int mpp_io_c_open_regular_file(char * const fname,
                               int const action)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_open_regular_file(&context,
                                     fname,
                                     action);
}

/*---------------------------------------------------------------------------*/
/*Close a regular file.*/
void mpp_io_c_close_regular_file(int * const file_index,
                                 int const action,
                                 bool const do_warn)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    /*Open the file.*/
    cmpp_io_close_regular_file(&context,
                               file_index,
                               action,
                               do_warn);

    return;
}

/*---------------------------------------------------------------------------*/
/*Flush a regular file.*/
void mpp_io_c_flush_regular_file(int const file_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    /*Open the file.*/
    cmpp_io_flush_regular_file(context,
                               file_index);

    return;
}

/*---------------------------------------------------------------------------*/
/*Perform a fortran style read of a list of values from a regular file.*/
void mpp_io_c_fread_regular_file(int const file_index,
                                 int const num_vals,
                                 int const data_type,
                                 int const val_max_width,
                                 char const delimiter,
                                 void **data_buf,
                                 bool const last_read)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    /*Read from the file.*/
    cmpp_io_fread_regular_file(context,
                               file_index,
                               num_vals,
                               data_type,
                               val_max_width,
                               delimiter,
                               data_buf,
                               last_read);

    return;
}

/*---------------------------------------------------------------------------*/
/*Open a netcdf file.*/
int mpp_io_c_open_netcdf_file(char * const fname,
                              int const action,
                              int const max_num_global_attributes,
                              int const max_num_dimensions,
                              int const max_num_variables,
                              int const max_num_attributes_per_variable,
                              int const max_num_dimensions_per_variable,
                              size_t const max_metadata_num_bytes)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    /*Open the file.*/
    return cmpp_io_open_netcdf_file(&context,
                                    fname,
                                    action,
                                    max_num_global_attributes,
                                    max_num_dimensions,
                                    max_num_variables,
                                    max_num_attributes_per_variable,
                                    max_num_dimensions_per_variable,
                                    max_metadata_num_bytes);
}

/*---------------------------------------------------------------------------*/
/*Close a netcdf file.*/
void mpp_io_c_close_netcdf_file(int * const file_index,
                                int const action,
                                bool const do_warn)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    /*Open the file.*/
    cmpp_io_close_netcdf_file(&context,
                              file_index,
                              action,
                              do_warn);

    return;
}

/*---------------------------------------------------------------------------*/
/*Flush a netcdf file.*/
void mpp_io_c_flush_netcdf_file(int const file_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    /*Open the file.*/
    cmpp_io_flush_netcdf_file(context,
                              file_index);

    return;
}

/*---------------------------------------------------------------------------*/
/*Read in an attribute from a netcdf file.*/
int mpp_io_c_read_netcdf_attribute(int const file_index,
                                   int const variable_index,
                                   int const netcdf_attribute_id)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_read_netcdf_attribute(context,
                                         file_index,
                                         get_variable_param(variable_index),
                                         netcdf_attribute_id);
}

/*---------------------------------------------------------------------------*/
/*Write out an attribute to a netcdf file.*/
int mpp_io_c_write_netcdf_attribute(int const file_index,
                                    int const variable_index,
                                    int const type_in_file,
                                    size_t const num_values,
                                    char * const name,
                                    void * const values,
                                    int const type_in_mem,
                                    bool const buffer_values)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_write_netcdf_attribute(context,
                                          file_index,
                                          get_variable_param(variable_index),
                                          get_nc_type(type_in_file),
                                          num_values,
                                          name,
                                          values,
                                          get_nc_type(type_in_mem),
                                          buffer_values);
}

/*---------------------------------------------------------------------------*/
/*Free memory used to hold an attribute value.*/
void mpp_io_c_free_netcdf_attribute(int const file_index,
                                    int const variable_index,
                                    int * const attribute_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    cmpp_io_free_netcdf_attribute(context,
                                  file_index,
                                  get_variable_param(variable_index),
                                  attribute_index);
    return;
}

/*---------------------------------------------------------------------------*/
/*Given the name of an attribute, return the index of the attribute in the
  corresponding file or variable attributes array.*/
int mpp_io_c_get_netcdf_attribute_index(int const file_index,
                                        int const variable_index,
                                        char * const att_name)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_attribute_index(context,
                                              file_index,
                                              get_variable_param(variable_index),
                                              att_name);
}

/*---------------------------------------------------------------------------*/
/*Return the name of an attribute.*/
char *mpp_io_c_get_netcdf_attribute_name(int const file_index,
                                         int const variable_index,
                                         int const attribute_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_attribute_name(context,
                                             file_index,
                                             get_variable_param(variable_index),
                                             attribute_index);
}

/*---------------------------------------------------------------------------*/
/*Return the type that an attribute is stored in a netcdf file as.*/
int mpp_io_c_get_netcdf_attribute_type_in_file(int const file_index,
                                               int const variable_index,
                                               int const attribute_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return get_int_from_nc_type(cmpp_io_get_netcdf_attribute_type_in_file(context,
                                                                          file_index,
                                                                          get_variable_param(variable_index),
                                                                          attribute_index));
}

/*---------------------------------------------------------------------------*/
/*Return the type that an attribute is stored in memory as.*/
int mpp_io_c_get_netcdf_attribute_type_in_mem(int const file_index,
                                              int const variable_index,
                                              int const attribute_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return get_int_from_nc_type(cmpp_io_get_netcdf_attribute_type_in_mem(context,
                                                                         file_index,
                                                                         get_variable_param(variable_index),
                                                                         attribute_index));
}

/*---------------------------------------------------------------------------*/
/*Return an attribute's number of values stored in a netcdf file.*/
size_t mpp_io_c_get_netcdf_attribute_num_values(int const file_index,
                                                int const variable_index,
                                                int const attribute_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_attribute_num_values(context,
                                                   file_index,
                                                   get_variable_param(variable_index),
                                                   attribute_index);
}

/*---------------------------------------------------------------------------*/
/*Return an attribute's values stored in a netcdf file.*/
void *mpp_io_c_get_netcdf_attribute_values(int const file_index,
                                           int const variable_index,
                                           int const attribute_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_attribute_values(context,
                                               file_index,
                                               get_variable_param(variable_index),
                                               attribute_index);
}

/*---------------------------------------------------------------------------*/
/*Read in a dimension from a netcdf file.*/
int mpp_io_c_read_netcdf_dimension(int const file_index,
                                   int const netcdf_dimension_id)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_read_netcdf_dimension(context,
                                         file_index,
                                         netcdf_dimension_id);
}

/*---------------------------------------------------------------------------*/
/*Write out a dimension to a netcdf file.*/
int mpp_io_c_write_netcdf_dimension(int const file_index,
                                    char * const name,
                                    size_t const length,
                                    bool const buffer_values)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_write_netcdf_dimension(context,
                                          file_index,
                                          name,
                                          get_length_param(length),
                                          buffer_values);
}

/*---------------------------------------------------------------------------*/
/*Advance a dimension's level by one in a netcdf file.*/
void mpp_io_c_advance_netcdf_dimension_level(int const file_index,
                                             int const dimension_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    cmpp_io_advance_netcdf_dimension_level(context,
                                           file_index,
                                           dimension_index);

    return;
}

/*---------------------------------------------------------------------------*/
/*Free memory used to store dimenison values.*/
void mpp_io_c_free_netcdf_dimension(int const file_index,
                                    int * const dimension_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    cmpp_io_free_netcdf_dimension(context,
                                  file_index,
                                  dimension_index);
    return;
}

/*---------------------------------------------------------------------------*/
/*Given the name of a dimension, return the index of the dimension in the
  file's dimensions array.*/
int mpp_io_c_get_netcdf_dimension_index_global(int const file_index,
                                               char * const dim_name)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_dimension_index(context,
                                              file_index,
                                              dim_name);
}

/*---------------------------------------------------------------------------*/
/*Given a file index, the index of variable in the file's variables array,
  and the index of a dimension in the variable's dimensions array, return
  the corresponding index of the dimension in file's dimensions array.*/
int mpp_io_c_get_netcdf_dimension_index_variable(int const file_index,
                                                 int const variable_index,
                                                 int const dimension_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_convert_netcdf_variable_dimension_index(context,
                                                           file_index,
                                                           variable_index,
                                                           dimension_index);
}

/*---------------------------------------------------------------------------*/
/*Return the name of a netcdf dimension.*/
char *mpp_io_c_get_netcdf_dimension_name(int const file_index,
                                         int const dimension_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_dimension_name(context,
                                             file_index,
                                             dimension_index);
}

/*---------------------------------------------------------------------------*/
/*Return the length of a netcdf dimension.*/
size_t mpp_io_c_get_netcdf_dimension_length(int const file_index,
                                            int const dimension_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_dimension_length(context,
                                               file_index,
                                               dimension_index);
}

/*---------------------------------------------------------------------------*/
/*Return a flag telling if a netcdf dimension is unlimited.*/
bool mpp_io_c_get_netcdf_dimension_is_unlimited(int const file_index,
                                                int const dimension_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_dimension_is_unlimited(context,
                                                     file_index,
                                                     dimension_index);
}

/*---------------------------------------------------------------------------*/
/*Return the current level of a netcdf dimension.*/
int mpp_io_c_get_netcdf_dimension_current_level(int const file_index,
                                                int const dimension_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_dimension_current_level(context,
                                                      file_index,
                                                      dimension_index);
}

/*---------------------------------------------------------------------------*/
/*Return the netcdf id of a netcdf dimension.*/
int mpp_io_c_get_netcdf_dimension_id(int const file_index,
                                     int const dimension_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_dimension_id(context,
                                           file_index,
                                           dimension_index);
}

/*---------------------------------------------------------------------------*/
/*Read in a variable's metadata from a netcdf file.*/
int mpp_io_c_read_netcdf_variable_metadata(int const file_index,
                                           int const netcdf_variable_id)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_read_netcdf_variable_metadata(context,
                                                 file_index,
                                                 netcdf_variable_id);
}

/*---------------------------------------------------------------------------*/
/*Write out metadata for a variable to a netcdf file.*/
int mpp_io_c_write_netcdf_variable_metadata(int const file_index,
                                            int const type_in_file,
                                            char * const name,
                                            int * const dimension_indices,
                                            int const num_dimensions,
                                            int const max_num_attributes,
                                            bool const buffer_metadata)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_write_netcdf_variable_metadata(context,
                                                  file_index,
                                                  get_nc_type(type_in_file),
                                                  name,
                                                  dimension_indices,
                                                  num_dimensions,
                                                  max_num_attributes,
                                                  buffer_metadata);
}

/*---------------------------------------------------------------------------*/
/*Read in a variable's data from a netcdf file.*/
void mpp_io_c_read_netcdf_variable_data(int const file_index,
                                        int const variable_index,
                                        size_t * const corner_indices,
                                        size_t * const edge_lengths,
                                        void * const * const data,
                                        int const type_in_mem)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    cmpp_io_read_netcdf_variable_data(context,
                                      file_index,
                                      variable_index,
                                      corner_indices,
                                      edge_lengths,
                                      data,
                                      get_nc_type(type_in_mem));

    return;
}

/*---------------------------------------------------------------------------*/
/*Write out data to a variable in a netcdf file.*/
void mpp_io_c_write_netcdf_variable_data(int const file_index,
                                         int const variable_index,
                                         size_t * const corner_indices,
                                         size_t * const edge_lengths,
                                         void * const data,
                                         int const type_in_mem)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    cmpp_io_write_netcdf_variable_data(context,
                                       file_index,
                                       variable_index,
                                       corner_indices,
                                       edge_lengths,
                                       data,
                                       get_nc_type(type_in_mem));

    return;
}

/*---------------------------------------------------------------------------*/
/*Store data that will be written out to a variable in a netcdf file at
  a later time.*/
void mpp_io_c_buffer_netcdf_variable_data(int const file_index,
                                          int const variable_index,
                                          size_t * const corner_indices,
                                          size_t * const edge_lengths,
                                          void * const data,
                                          int const type_in_mem,
                                          bool const overwrite)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    cmpp_io_buffer_netcdf_variable_data(context,
                                        file_index,
                                        variable_index,
                                        corner_indices,
                                        edge_lengths,
                                        data,
                                        get_nc_type(type_in_mem),
                                        overwrite);

    return;
}

/*---------------------------------------------------------------------------*/
/*Write out data that was previously stored to a variable in a netcdf file.*/
void mpp_io_c_write_buffered_netcdf_variable_data(int const file_index,
                                                  int const variable_index,
                                                  bool const free_data)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    cmpp_io_write_buffered_netcdf_variable_data(context,
                                                file_index,
                                                variable_index,
                                                free_data);

    return;
}

/*---------------------------------------------------------------------------*/
/*Free memory used to store a netcdf variable's metadata and data.*/
void mpp_io_c_free_netcdf_variable(int const file_index,
                                   int * const variable_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    cmpp_io_free_netcdf_variable(context,
                                 file_index,
                                 variable_index);

    return;
}

/*---------------------------------------------------------------------------*/
/*Given the name of a variable, return the index of the variable in the
  file's variables array.*/
int mpp_io_c_get_netcdf_variable_index(int const file_index,
                                       char * const var_name)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_variable_index(context,
                                             file_index,
                                             var_name);
}

/*---------------------------------------------------------------------------*/
/*Return a flag telling if data has been buffered for a variable in a netcdf
  file.*/
bool mpp_io_c_get_netcdf_variable_is_data_buffered(int const file_index,
                                                   int const variable_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_variable_is_data_buffered(context,
                                                        file_index,
                                                        variable_index);
}

/*---------------------------------------------------------------------------*/
/*Return the maximum number of attributes allowed for the inputted variable in
  a netcdf file.*/
int mpp_io_c_get_netcdf_variable_max_num_attributes(int const file_index,
                                                    int const variable_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_variable_max_num_attributes(context,
                                                          file_index,
                                                          variable_index);
}

/*---------------------------------------------------------------------------*/
/*Return the name of a variable in a netcdf file.*/
char *mpp_io_c_get_netcdf_variable_name(int const file_index,
                                        int const variable_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_variable_name(context,
                                            file_index,
                                            variable_index);
}

/*---------------------------------------------------------------------------*/
/*Return the type that data that was buffered for a variable in a netcdf
  file is stored in the file as.*/
int mpp_io_c_get_netcdf_variable_type_in_file(int const file_index,
                                              int const variable_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return get_int_from_nc_type(cmpp_io_get_netcdf_variable_type_in_file(context,
                                                                         file_index,
                                                                         variable_index));
}

/*---------------------------------------------------------------------------*/
/*Return the number of dimensions of a variable in a netcdf file.*/
int mpp_io_c_get_netcdf_variable_num_dimensions(int const file_index,
                                                int const variable_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_variable_num_dimensions(context,
                                                      file_index,
                                                      variable_index);
}

/*---------------------------------------------------------------------------*/
/*Return the number of attributes associated with the inputted variable in
  a netcdf file.*/
int mpp_io_c_get_netcdf_variable_num_attributes(int const file_index,
                                                int const variable_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_variable_num_attributes(context,
                                                      file_index,
                                                      variable_index);
}

/*---------------------------------------------------------------------------*/
/*Return the netcdf id of the inputted variable in a netcdf file.*/
int mpp_io_c_get_netcdf_variable_id(int const file_index,
                                    int const variable_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_variable_id(context,
                                          file_index,
                                          variable_index);
}

/*---------------------------------------------------------------------------*/
/*Return a pointer to data that was buffered for a variable in a netcdf
  file.*/
void *mpp_io_c_get_netcdf_variable_buffered_data(int const file_index,
                                                 int const variable_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_variable_buffered_data(context,
                                                     file_index,
                                                     variable_index);
}

/*---------------------------------------------------------------------------*/
/*Return a pointer to an array of corner indices that was buffered for a
  variable in a netcdf file.*/
size_t *mpp_io_c_get_netcdf_variable_corner_indices(int const file_index,
                                                    int const variable_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_variable_corner_indices(context,
                                                      file_index,
                                                      variable_index);
}

/*---------------------------------------------------------------------------*/
/*Return a pointer to an array of edge lengths that was buffered for a
  variable in a netcdf file.*/
size_t *mpp_io_c_get_netcdf_variable_edge_lengths(int const file_index,
                                                  int const variable_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_variable_edge_lengths(context,
                                                    file_index,
                                                    variable_index);
}

/*---------------------------------------------------------------------------*/
/*Return the type that data that was buffered for a variable in a netcdf
  file is stored in memory as.*/
int mpp_io_c_get_netcdf_variable_type_in_mem(int const file_index,
                                             int const variable_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return get_int_from_nc_type(cmpp_io_get_netcdf_variable_type_in_mem(context,
                                                                        file_index,
                                                                        variable_index));
}



















/*---------------------------------------------------------------------------*/
/*Return the number of global attributes, dimensions, and variables in a
  netcdf file.*/
void mpp_io_c_nc_inq(int const file_index,
                     int * const num_global_atts,
                     int * const num_dims,
                     int * const num_vars)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    cmpp_io_nc_inq(context,
                   file_index,
                   num_global_atts,
                   num_dims,
                   num_vars);

    return;
}

/*---------------------------------------------------------------------------*/
/*Get the name of a netcdf file.*/
char *mpp_io_c_get_netcdf_file_name(int const file_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_file_name(context,
                                        file_index);
}

/*---------------------------------------------------------------------------*/
/*Get the name of a regular file.*/
char *mpp_io_c_get_regular_file_name(int const file_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_regular_file_name(context,
                                         file_index);
}

/*---------------------------------------------------------------------------*/
/*Get a flag telling if a netcdf file is open.*/
bool mpp_io_c_get_netcdf_file_is_open_by_index(int const file_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_netcdf_file_is_open(context,
                                           file_index);
}

/*---------------------------------------------------------------------------*/
/*Get a flag telling if a netcdf file is open.*/
bool mpp_io_c_get_netcdf_file_is_open_by_name(char * const name)
{
    /*Local variables*/
    int file_index;
    int empty_index;

    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    file_index = cmpp_io_context_get_file_index(context,
                                                name,
                                                CMPP_NETCDF,
                                                &empty_index);

    if (file_index != CMPP_IO_INDEX_NOT_FOUND)
    {
        return cmpp_io_get_netcdf_file_is_open(context,
                                               file_index);
    }
    else
    {
        return false;
    }
}

/*---------------------------------------------------------------------------*/
/*Get a flag telling if a regular file is open.*/
bool mpp_io_c_get_regular_file_is_open_by_index(int const file_index)
{
    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_get_regular_file_is_open(context,
                                            file_index);
}

/*---------------------------------------------------------------------------*/
/*Get a flag telling if a regular file is open.*/
bool mpp_io_c_get_regular_file_is_open_by_name(char * const name)
{
    /*Local variables*/
    int file_index;
    int empty_index;

    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    file_index = cmpp_io_context_get_file_index(context,
                                                name,
                                                CMPP_ASCII,
                                                &empty_index);

    if (file_index != CMPP_IO_INDEX_NOT_FOUND)
    {
        return cmpp_io_get_regular_file_is_open(context,
                                                file_index);
    }
    else
    {
        return false;
    }
}

/*---------------------------------------------------------------------------*/
/*Get the file index from the file name.*/
int mpp_io_c_get_file_index(char * const name,
                            int const type)
{
    /*Local variables*/
    int empty_index;

    /*Catch OpenMP threads.*/
    trap_openmp_thread();

    return cmpp_io_context_get_file_index(context,
                                          name,
                                          type,
                                          &empty_index);
}

/*---------------------------------------------------------------------------*/
/*If necessary, convert from CMPP_IO_GLOBAL_ATT to NC_GLOBAL.*/
static int get_variable_param(int const variable_index)
{
    if (variable_index == (int)CMPP_IO_GLOBAL_ATT)
    {
        return (int)NC_GLOBAL;
    }
    else
    {
        return variable_index;
    }
}

/*---------------------------------------------------------------------------*/
/*Convert from CMPP_IO_NC_TYPE to nc_type.*/
static nc_type get_nc_type(int const nc_type_in)
{
    /*Local variables*/
    nc_type nc_type_out;

    switch (nc_type_in)
    {
        case CMPP_IO_NC_CHAR:
            nc_type_out = NC_CHAR;
            break;
        case CMPP_IO_NC_BYTE:
            nc_type_out = NC_BYTE;
            break;
        case CMPP_IO_NC_SHORT:
            nc_type_out = NC_SHORT;
            break;
        case CMPP_IO_NC_INT:
            nc_type_out = NC_INT;
            break;
        case CMPP_IO_NC_FLOAT:
            nc_type_out = NC_FLOAT;
            break;
        case CMPP_IO_NC_DOUBLE:
            nc_type_out = NC_DOUBLE;
            break;
        default:
            fatal("the inputted value (%d) must be one of:"
                      " CMPP_IO_NC_CHAR (%d), CMPP_IO_NC_BYTE (%d),"
                      " CMPP_IO_NC_SHORT (%d), CMPP_IO_NC_INT (%d),"
                      " CMPP_IO_NC_FLOAT (%d), or CMPP_IO_NC_DOUBLE (%d).",
                  nc_type_in,
                  (int)CMPP_IO_NC_CHAR,
                  (int)CMPP_IO_NC_BYTE,
                  (int)CMPP_IO_NC_SHORT,
                  (int)CMPP_IO_NC_INT,
                  (int)CMPP_IO_NC_FLOAT,
                  (int)CMPP_IO_NC_DOUBLE);
    }

    return nc_type_out;
}

/*---------------------------------------------------------------------------*/
/*Convert from nc_type to CMPP_IO_NC_TYPE.*/
static int get_int_from_nc_type(nc_type const nc_type_in)
{
    /*Local variables*/
    int int_type_out;

    switch (nc_type_in)
    {
        case NC_CHAR:
            int_type_out = (int)CMPP_IO_NC_CHAR;
            break;
        case NC_BYTE:
            int_type_out = (int)CMPP_IO_NC_BYTE;
            break;
        case NC_SHORT:
            int_type_out = (int)CMPP_IO_NC_SHORT;
            break;
        case NC_INT:
            int_type_out = (int)CMPP_IO_NC_INT;
            break;
        case NC_FLOAT:
            int_type_out = (int)CMPP_IO_NC_FLOAT;
            break;
        case NC_DOUBLE:
            int_type_out = (int)CMPP_IO_NC_DOUBLE;
            break;
        default:
            fatal("the inputted value (%d) must be one of:"
                      " NC_CHAR, NC_BYTE, NC_SHORT, NC_INT, NC_FLOAT,"
                      " or NC_DOUBLE.",
                  (int)nc_type_in);
    }

    return int_type_out;
}

/*---------------------------------------------------------------------------*/
/*If necessary, convert from CMPP_IO_NC_UNLIMITED to NC_UNLIMITED.*/
static size_t get_length_param(size_t const length)
{
    if (length == (size_t)CMPP_IO_GLOBAL_ATT)
    {
        return (size_t)NC_UNLIMITED;
    }
    else
    {
        return length;
    }
}

/*---------------------------------------------------------------------------*/
