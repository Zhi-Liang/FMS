/** @file */

#ifndef SET_MPP_C_BROADCAST_H_
#define SET_MPP_C_BROADCAST_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

/**
    Broadcast data from one rank to all others in the same pelist.  The
    inputted from_rank should be the rank id of the broadcast root in the
    current pelist.  If a null rank_list is inputted, then the current pelist
    is used in the broadcast.

    \param [in,out] bcast_data      Data that is broadcasted.
    \param [in]     bcast_len       Size of the data being broadcasted.
    \param [in]     from_rank       MPI rank id from the world pelist of the
                                    "broadcast root".
    \param [in]     mpp_type        MPP_C type value.
    \param [in]     rank_list       Array of MPI rank ids from the world
                                    pelist.
    \param [in]     rank_list_size  Size of the array of MPI rank ids.
    \param [in,out] context         Context handle.
*/
void mpp_c_broadcast(void **bcast_data,
                     size_t const bcast_len,
                     int32_t from_rank,
                     mpp_c_datatype_t const mpp_type,
                     int32_t const *rank_list,
                     size_t const rank_list_size,
                     mpp_c_context_t * const * const context);

#endif
