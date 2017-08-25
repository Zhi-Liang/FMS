#ifndef SET_CMPP_IO_UTILS_H_
#define SET_CMPP_IO_UTILS_H_

#include <stdbool.h>
#include <stdlib.h>
#include <sys/stat.h>
#include "cmpp_io_attribute_t_api.h"
#include "cmpp_io_dimension_t_api.h"
#include "cmpp_io_variable_t_api.h"
#include "cmpp_io_netcdf_file_t_api.h"
#include "cmpp_io_regular_file_t_api.h"
#include "cmpp_io_hdf5_file_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#define ATT_CLEANUP 0
#define DIM_CLEANUP 1
#define VAR_CLEANUP 2
#define NETCDF_FILE_CLEANUP 3
#define REGULAR_FILE_CLEANUP 4
#define HDF5_FILE_CLEANUP 5

struct cleanup
{
    int type;
    union
    {
        void (*att_cleanup)(cmpp_io_attribute_t **);
        void (*dim_cleanup)(cmpp_io_dimension_t **);
        void (*var_cleanup)(cmpp_io_variable_t **);
        void (*netcdf_file_cleanup)(cmpp_io_netcdf_file_t **);
        void (*regular_file_cleanup)(cmpp_io_regular_file_t **);
        void (*hdf5_file_cleanup)(cmpp_io_hdf5_file_t **);
    } fp;
};
typedef struct cleanup cleanup_t;

#define ATT_NAME 0
#define DIM_NAME 1
#define VAR_NAME 2
#define REGULAR_FILE_NAME 3
#define NETCDF_FILE_NAME 4
#define HDF5_FILE_NAME 5
#define DIM_NC_ID 10
#define VAR_NC_ID 11

struct compare
{
    int type;
    union
    {
        char *(*att_name)(cmpp_io_attribute_t *);
        char *(*dim_name)(cmpp_io_dimension_t *);
        char *(*var_name)(cmpp_io_variable_t *);
        char *(*regular_file_name)(cmpp_io_regular_file_t *);
        char *(*netcdf_file_name)(cmpp_io_netcdf_file_t *);
        char *(*hdf5_file_name)(cmpp_io_hdf5_file_t *);
        int (*dim_id)(cmpp_io_dimension_t const * const);
        int (*var_id)(cmpp_io_variable_t const * const);
    } fp;
};
typedef struct compare compare_t;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

bool cmpp_io_inquire(char const * const fname,
                     struct stat *res_stat);

size_t cmpp_io_file_size(char const * const fsize,
                         char const * const fname);

void cmpp_io_get_stream_fmode(int const action,
                              char *mode);

void cmpp_io_check_file_status_and_action(char const * const fname,
                                          int const action);

void cmpp_io_delete_file(char const *fileName);

int add_pointer_to_array(void **ptr_array,
                         int const ptr_array_size,
                         int * const num_array_spots_filled,
                         void *ptr);

void *get_pointer_by_index(void **ptr_array,
                           int const ptr_array_size,
                           int const ptr_index);

void remove_pointer_from_array(void **ptr_array,
                               int const ptr_array_size,
                               int * const ptr_index,
                               cleanup_t *cfunc);

int find_pointer_in_array(void **ptr_array,
                          int const ptr_search_size,
                          void * const in_val,
                          compare_t *cfunc,
                          int *empty_index);

#endif
