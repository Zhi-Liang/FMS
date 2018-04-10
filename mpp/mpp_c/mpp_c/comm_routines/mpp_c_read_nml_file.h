
#ifndef SET_MPP_C_READ_NML_FILE_H_
#define SET_MPP_C_READ_NML_FILE_H_

#include "mpp_c_context_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

/*---------------------------------------------------------------------------*/
/**
    Function that reads a .nml namelist file, and stores the contents of the
    file in the mpp_c_context_t object's namelist buffer.

    \param [in,out] self  Context handle.
*/
void mpp_c_read_nml_file(mpp_c_context_t **context);

/*---------------------------------------------------------------------------*/

#endif
