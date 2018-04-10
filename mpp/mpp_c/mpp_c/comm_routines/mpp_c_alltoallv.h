/** @file */

#ifndef SET_MPP_C_ALLTOALLV_H_
#define SET_MPP_C_ALLTOALLV_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

/*---------------------------------------------------------------------------*/
/**>
    Send variable length data from all ranks to all other ranks on a pelist.
    If a null pelist is inputted, then the current pelist is used.

    \param [in]     send_data       Array of data to be sent to each of the
                                    ranks.
    \param [in]     send_len        Array of sizes of the data to be sent
                                    to each of the ranks.
    \param [in]     send_displace   Array of displacements for the send_data
                                    array.
    \param [in,out] recv_data       Array which stores all the received data.
    \param [in]     recv_len        Array of sizes of the data to be received
                                    from each of the ranks.
    \param [in]     recv_displace   Array of displacements for the receive
                                    data array.
    \param [in]     mpp_type        MPP_C type value.
    \param [in]     rank_list       Array of MPI rank ids from the world
                                    pelist.
    \param [in]     rank_list_size  Size of the array of MPI rank ids.
    \param [in,out] context         Context handle.

*/
void mpp_c_alltoallv(void *send_data,
                     int32_t *send_len,
                     int32_t *send_displace,
                     void **recv_data,
                     int32_t *recv_len,
                     int32_t *recv_displace,
                     mpp_c_datatype_t const mpp_type,
                     int32_t const *rank_list,
                     size_t const rank_list_size,
                     mpp_c_context_t * const * const context);

/*---------------------------------------------------------------------------*/

#endif
