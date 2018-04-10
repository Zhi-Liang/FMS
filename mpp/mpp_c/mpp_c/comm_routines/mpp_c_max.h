/** @file */

#ifndef SET_MPP_C_MAX_H_
#define SET_MPP_C_MAX_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

/*---------------------------------------------------------------------------*/
/**
    Get the maximum value of data across all ranks on a pelist. If a null
    pelist is inputted, then the current pelist is used.

    \param [in,out] max_data        Data whose maximum will be found.
    \param [in]     max_len         Size of max_data.
    \param [in]     mpp_type        MPP_C type value.
    \param [in]     rank_list       Array of MPI rank ids from the world
                                    pelist.
    \param [in]     rank_list_size  Size of the array of MPI rank ids.
    \param [in,out] context         Context handle.
*/
void mpp_c_max(void **max_data,
               size_t const max_len,
               mpp_c_datatype_t const mpp_type,
               int32_t const *rank_list,
               size_t const rank_list_size,
               mpp_c_context_t * const * const context);

/*---------------------------------------------------------------------------*/

#endif