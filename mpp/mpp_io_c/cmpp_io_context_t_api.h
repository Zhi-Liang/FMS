/** @file */

#ifndef SET_CMPP_IO_CONTEXT_T_API_H_
#define SET_CMPP_IO_CONTEXT_T_API_H_

#include <stdbool.h>
#include <stdlib.h>
#include "cmpp_io_hdf5_file_t_api.h"
#include "cmpp_io_netcdf_file_t_api.h"
#include "cmpp_io_regular_file_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Opaque object typedefs.*/

/**
    Context type.  To use this structure:
    cmpp_io_context_t *foo = cmpp_io_context_create();
    and when finished, use:
    cmpp_io_context_destroy(&foo);
*/
typedef struct cmpp_io_context_type cmpp_io_context_t;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

cmpp_io_context_t *cmpp_io_context_create(int const max_num_netcdf_files,
                                          int const max_num_regular_files,
                                          int const max_num_hdf5_files,
                                          bool const debug_flag,
                                          bool const verbose_flag,
                                          int const header_buffer_val,
                                          int const shuffle,
                                          int const deflate,
                                          int const deflate_level);

void cmpp_io_context_destroy(cmpp_io_context_t **self);

cmpp_io_netcdf_file_t **cmpp_io_context_get_netcdf_files(cmpp_io_context_t * const self);

int cmpp_io_context_get_max_netcdf_files_allowed(cmpp_io_context_t const * const self);

int cmpp_io_context_get_cur_num_netcdf_files(cmpp_io_context_t const * const self);

cmpp_io_regular_file_t **cmpp_io_context_get_regular_files(cmpp_io_context_t * const self);

int cmpp_io_context_get_max_regular_files_allowed(cmpp_io_context_t const * const self);

int cmpp_io_context_get_cur_num_regular_files(cmpp_io_context_t const * const self);

cmpp_io_hdf5_file_t **cmpp_io_context_get_hdf5_files(cmpp_io_context_t * const self);

int cmpp_io_context_get_max_hdf5_files_allowed(cmpp_io_context_t const * const self);

int cmpp_io_context_get_cur_num_hdf5_files(cmpp_io_context_t const * const self);

bool cmpp_io_context_get_debug_flag(cmpp_io_context_t const * const self);

bool cmpp_io_context_get_verbose_flag(cmpp_io_context_t const * const self);

int cmpp_io_context_get_header_buffer_val(cmpp_io_context_t const * const self);

int cmpp_io_context_get_shuffle(cmpp_io_context_t const * const self);

int cmpp_io_context_get_deflate(cmpp_io_context_t const * const self);

int cmpp_io_context_get_deflate_level(cmpp_io_context_t const * const self);

int cmpp_io_context_get_file_index(cmpp_io_context_t const * const self,
                                   char const * const fname,
                                   int const ftype,
                                   int *empty_index);

int cmpp_io_context_add_file(cmpp_io_context_t * const * const self,
                             void * const fptr,
                             int const file_type);

void *cmpp_io_context_get_file_ptr(cmpp_io_context_t * const self,
                                   int const file_index,
                                   int const file_type);

void cmpp_io_context_remove_file(cmpp_io_context_t * const * const self,
                                 int * const file_index,
                                 int const file_type);

#endif
