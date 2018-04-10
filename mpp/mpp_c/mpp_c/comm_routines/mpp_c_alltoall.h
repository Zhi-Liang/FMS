/** @file */

#ifndef SET_MPP_C_ALLTOALL_H_
#define SET_MPP_C_ALLTOALL_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

/*---------------------------------------------------------------------------*/
/**
    Send data from all ranks to all other ranks on a pelist. If a null pelist
    is inputted, then the current pelist is used.

    \param [in]     send_data       Array of data that will be sent to all
                                    other ranks.
    \param [in]     send_len        Size of the send_data array.
    \param [in,out] recv_data       Array which stores all received data.
    \param [in]     recv_len        Size of the recv_data array.
    \param [in]     size_per_send   Size of the data to be sent to each of
                                    the other ranks.
    \param [in]     size_per_recv   Size of the data to be received from each
                                    of the other ranks.
    \param [in]     mpp_type        MPP_C type value.
    \param [in]     rank_list       Array of MPI rank ids from the world
                                    pelist.
    \param [in]     rank_list_size  Size of the array of MPI rank ids.
    \param [in,out] context          Context handle.
*/
void mpp_c_alltoall(void *send_data,
                    size_t const send_len,
                    void **recv_data,
                    size_t const recv_len,
                    size_t const size_per_send,
                    size_t const size_per_recv,
                    mpp_c_datatype_t const mpp_type,
                    int32_t const *rank_list,
                    size_t const rank_list_size,
                    mpp_c_context_t * const * const context);

/*---------------------------------------------------------------------------*/

#endif
