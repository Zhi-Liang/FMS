/** @file */

#ifndef SET_MPP_C_RECV_H_
#define SET_MPP_C_RECV_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"
#include "mpp_c_mpi.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

/*---------------------------------------------------------------------------*/
/**
    MPI receive from a rank on the world pelist.

    \param [in,out] recv_data  Place where the received data will be stored.
    \param [in]     recv_len   Size of recv_data.
    \param [in]     from_rank  MPI rank from the world pelist from which the
                               data was/will be sent.
    \param [in]     mpp_type   MPP_C type value.
    \param [in]     tag        MPI tag for the receive.
    \param [in,out] request    MPI request for the receive (if non-blocking)
    \param [in]     block      Flag denoting whether or not the receive is
                               blocking
    \param [in,out] context    Context handle.
*/
void mpp_c_recv(void **recv_data,
                size_t const recv_len,
                int32_t const from_rank,
                mpp_c_datatype_t const mpp_type,
                int32_t const tag,
                MPI_Request *request,
                int32_t const block,
                mpp_c_context_t * const * const context);

/*---------------------------------------------------------------------------*/

#endif
