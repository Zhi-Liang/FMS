#ifndef SET_CMPP_IO_HDF5_FILE_T_API_H_
#define SET_CMPP_IO_HDF5_FILE_T_API_H_

#include "cmpp_io_file_props_t_api.h"

typedef struct cmpp_io_hdf5_file_type cmpp_io_hdf5_file_t;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

cmpp_io_hdf5_file_t *cmpp_io_hdf5_file_create(cmpp_io_file_props_t * const file_props);

void cmpp_io_hdf5_file_destroy(cmpp_io_hdf5_file_t **self);

cmpp_io_file_props_t *cmpp_io_hdf5_file_get_file_props(cmpp_io_hdf5_file_t * const self);

char *cmpp_io_hdf5_file_get_name(cmpp_io_hdf5_file_t * const self);

#endif
