#ifndef SET_CMPP_IO_FILE_PROPS_T_API_H_
#define SET_CMPP_IO_FILE_PROPS_T_API_H_

#include <stdbool.h>

typedef struct cmpp_io_file_props_type cmpp_io_file_props_t;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

cmpp_io_file_props_t *cmpp_io_file_props_create(char * const name,
                                                int const action);

void cmpp_io_file_props_destroy(cmpp_io_file_props_t **self);

bool cmpp_io_file_props_get_is_open(cmpp_io_file_props_t const * const self);

char *cmpp_io_file_props_get_name(cmpp_io_file_props_t * const self);

int cmpp_io_file_props_get_action(cmpp_io_file_props_t const * const self);

void cmpp_io_file_props_set_is_open(cmpp_io_file_props_t * const * const self,
                                    bool const is_open);

#endif
