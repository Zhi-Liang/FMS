/** @file */

#ifndef SET_MPP_C_SYNC_H_
#define SET_MPP_C_SYNC_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"
#include "mpp_c_mpi.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

/*---------------------------------------------------------------------------*/
/**
    Sychronize ranks in a pelist using a MPI barrier.

    \param [in]     rank_list       Array of MPI rank ids from the world
                                    pelist communicator.
    \param [in]     rank_list_size  Size of the array of MPI rank ids.
    \param [in,out] context         Context handle.
*/
void mpp_c_sync(int32_t const *rank_list,
                size_t const rank_list_size,
                mpp_c_context_t * const * const context);

/*---------------------------------------------------------------------------*/
/**
    Complete outstanding MPI sends or receives inputted in the request array.

    \param [in]     comm_type     Communication type value.
    \param [in]     request       Array of MPI request ids.
    \param [in]     num_requests  Size of the request array.
    \param [in]     msg_size      Array of message sizes.
    \param [in]     msg_type      Array of MPP_C type values.
    \param [in,out] context       Context handle.
*/
void mpp_c_sync_self(int32_t const comm_type,
                     MPI_Request *request,
                     size_t const num_requests,
                     size_t const *msg_size,
                     mpp_c_datatype_t const *msg_type,
                     mpp_c_context_t * const * const context);

/*---------------------------------------------------------------------------*/

#endif
