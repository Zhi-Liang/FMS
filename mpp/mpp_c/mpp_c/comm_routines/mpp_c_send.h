/** @file */

#ifndef SET_MPP_C_SEND_H_
#define SET_MPP_C_SEND_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"
#include "mpp_c_mpi.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

/*---------------------------------------------------------------------------*/
/**
    Non-blocking MPI send to a rank on the world pelist.

    \param [in]     send_data  Data that will be sent.
    \param [in]     send_len   Size of send_data.
    \param [in]     to_rank    MPI rank from the world pelist that will
                               receive the data.
    \param [in]     mpp_type   MPP_C type value.
    \param [in]     tag        MPI tag for the send.
    \param [in]     request    MPI request for the send.
    \param [in,out] context    Context handle.
*/
void mpp_c_send(void **send_data,
                size_t const send_len,
                int32_t const to_rank,
                mpp_c_datatype_t const mpp_type,
                int32_t const tag,
                MPI_Request *request,
                mpp_c_context_t * const * const context);

/*---------------------------------------------------------------------------*/

#endif
