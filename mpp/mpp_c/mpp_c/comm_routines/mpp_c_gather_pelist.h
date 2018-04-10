/** @file */

#ifndef SET_MPP_C_GATHER_PELIST_H_
#define SET_MPP_C_GATHER_PELIST_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

/*---------------------------------------------------------------------------*/
/**
    Gather array segments onto a rank of a pelist from all other ranks in the
    pelist and store them in the gather_data array.  Conceptually, the gather
                               can be viewed like the picture, where say rank 3
    |-----------|-----------|  fills in the remaining values of its gather_data
    | array     | array     |  array with array segments received from the other
    | segment   | segment   |  ranks in the inputted pelist (i.e. rank 0, rank 1,
    | gathered  | gathered  |  and rank 2 in this example.) The starting and
    | from      | from      |  ending indices of each spot in the gather_data
    | rank 0    | rank 1    |  array that will be filled by each rank's array
    |-----------|-----------|  segment are first sent to the root rank, then
    | array     | array     |  the array segment itself is sent and stored
    | segment   | segment   |  in the gather_data array in the spot described
    | gathered  | gathered  |  by the sent indices (plus any offsets).
    | from      | from      |
    | rank 2    | rank 3    |
    |-----------|-----------|

    \param [in,out] gather_data                Array where all the gathered
                                               data is stored.
    \param [in]     gather_data_x_size         Size of the x-dimension of the
                                               gather_data array.
    \param [in]     gather_data_y_size         Size of the y-dimension of the
                                               gather_data array.
    \param [in]     gather_data_z_size         Size of the z-dimension of the
                                               gather_data array
    \param [in]     mpp_type                   MPP_C type value.
    \param [in]     array_segment              Array segment that will be sent
                                               to the "gather root".
    \param [in]     gather_spot_x_start_index  Starting index in the x-dimension
                                               of the spot in the gather_data
                                               array where the array segment
                                               will be stored.
    \param [in]     gather_spot_x_end_index    Ending index in the x-dimension
                                               of the spot in the gather_data
                                               array where the array segment
                                               will be stored.
    \param [in]     gather_spot_y_start_index  Starting index in the y-dimension
                                               of the spot in the gather_data
                                               array where the array segment
                                               will be stored.
    \param [in]     gather_spot_y_end_index    Ending index in the y-dimension
                                               of the spot in the gather_data
                                               array where the array segment
                                               will be stored.
    \param [in]     gather_spot_z_start_index  Starting index in the z-dimension
                                               of the spot in the gather_data
                                               array where the array segment
                                               will be stored.
    \param [in]     gather_spot_z_end_index    Ending index in the z-dimension
                                               of the spot in the gather_data
                                               array where the array segment
                                               will be stored.
    \param [in]     rank_list                  Array of MPI rank ids from the
                                               world pelist.
    \param [in]     rank_list_size             Size of the array of MPI rank ids.
    \param [in]     rank_list_root_flag        Flag denoting the "gather root".
    \param [in]     root_gather_spot_x_offset  Offset in the x-dimension for
                                               the starting and ending indices
                                               of the spot in the gather_data
                                               array where the array segment
                                               will be stored.
    \param [in]     root_gather_spot_y_offset  Offset in the y-dimension for
                                               the starting and ending indices
                                               of the spot in the gather_data
                                               array where the array segment
                                               will be stored.
    \param [in]     root_gather_spot_z_offset  Offset in the z-dimension for
                                               the starting and ending indices
                                               of the spot in the gather_data
                                               array where the array segment
                                               will be stored.
    \param [in,out] context                    Context handle.
*/
void mpp_c_gather_pelist(void **gather_data,
                         size_t const gather_data_x_size,
                         size_t const gather_data_y_size,
                         size_t const gather_data_z_size,
                         mpp_c_datatype_t const mpp_type,
                         void **array_segment,
                         int32_t const gather_spot_x_start_index,
                         int32_t const gather_spot_x_end_index,
                         int32_t const gather_spot_y_start_index,
                         int32_t const gather_spot_y_end_index,
                         int32_t const gather_spot_z_start_index,
                         int32_t const gather_spot_z_end_index,
                         int32_t const *rank_list,
                         size_t const rank_list_size,
                         int32_t const rank_list_root_flag,
                         int32_t const root_gather_spot_x_offset,
                         int32_t const root_gather_spot_y_offset,
                         int32_t const root_gather_spot_z_offset,
                         mpp_c_context_t * const * const context);

/*---------------------------------------------------------------------------*/

#endif
