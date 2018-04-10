/** @file */

#ifndef SET_MPP_C_GATHERV_H_
#define SET_MPP_C_GATHERV_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

/*---------------------------------------------------------------------------*/
/**
    Gather variable length data from all ranks on a pelist to the first rank
    of the pelist. If a null pelist is inputted, then the current pelist is
    used.

    \param [in]     send_data            Array of data that will be sent to
                                         the "gather root".
    \param [in]     send_len             Size of the send_data array.
    \param [in,out] recv_data            Array which stores all received data.
    \param [in]     recv_data_size       Size of the recv_data array.
    \param [in]     recv_len_array       Array containing the size of the
                                         send_data array being sent to the
                                         "gather root" from each rank.
    \param [in]     recv_len_array_size  Size of the recv_len_array array.
    \param [in]     mpp_type             MPP_C type value.
    \param [in]     rank_list            Array of MPI rank ids from the world
                                         pelist.
    \param [in]     rank_list_size       Size of the array of MPI rank ids.
    \param [in,out] context              Context handle.
*/
void mpp_c_gatherv(void *send_data,
                   size_t const send_len,
                   void **recv_data,
                   size_t const recv_data_size,
                   size_t const *recv_len_array,
                   size_t const recv_len_array_size,
                   mpp_c_datatype_t const mpp_type,
                   int32_t *rank_list,
                   size_t const rank_list_size,
                   mpp_c_context_t * const * const context);

/*---------------------------------------------------------------------------*/

#endif
