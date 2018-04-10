/** @file */

#ifndef SET_MPP_C_CHKSUM_H_
#define SET_MPP_C_CHKSUM_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_context_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

/*---------------------------------------------------------------------------*/
/**
    Perform a check-sum across all ranks of a pelist.  If a null pelist is
    inputted, then the current pelist is used.

    \param [in]     chksum_data     Data to be check-summed.
    \param [in]     chksum_len      Size of the data to be check-summed.
    \param [in]     num_bytes       Total number of bytes occupied by one
                                    element of the data to be check-summed.
    \param [in]     mask_val        Value that should not be included in the
                                    check-sum
    \param [in]     rank_list       Array of MPI rank ids from the world
                                    pelist.
    \param [in]     rank_list_size  Size of the array of MPI rank ids.
    \param [in,out] context         Context handle.
    \return                         Check-sum value.

*/
int64_t mpp_c_chksum(void const *chksum_data,
                     size_t const chksum_len,
                     size_t const num_bytes,
                     void const *mask_val,
                     int32_t const *rank_list,
                     size_t const rank_list_size,
                     mpp_c_context_t * const * const context);

/*---------------------------------------------------------------------------*/

#endif
