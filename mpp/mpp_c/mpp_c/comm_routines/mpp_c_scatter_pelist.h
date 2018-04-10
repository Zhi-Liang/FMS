/** @file */

#ifndef SET_MPP_C_SCATTER_PELIST_H_
#define SET_MPP_C_SCATTER_PELIST_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

/*---------------------------------------------------------------------------*/
/**
    Scatter a data array from a rank into array segments on all other ranks on
    a pelist. Conceptually, the scatter can be viewed like the picture, where
                               say rank 1 fills each of the array segments on
    |-----------|-----------|  ranks 0, 2, and 3 by sending sections of its
    | array     | array     |  inputted scatter_data array. The starting and
    | segment   | segment   |  ending indices of each spot in the scatter_data
    | scattered | scattered |  array that will be sent to the each other rank
    | to        | to        |  are first scattered by the scattering root rank,
    | rank 0    | rank 1    |  and then the data is scattered and stored in
    |-----------|-----------|  each ranks array_segment array.
    | array     | array     |
    | segment   | segment   |
    | scattered | scattered |
    | to        | to        |
    | rank 2    | rank 3    |
    |-----------|-----------|

    \param [in]     scatter_data                Array containing data that will
                                                be scattered to the ranks on
                                                the inputted rank_list.
    \param [in]     scatter_data_x_size         Size of the x-dimension of the
                                                scatter_data array.
    \param [in]     scatter_data_y_size         Size of the y-dimension of the
                                                scatter_data array.
    \param [in]     scatter_data_z_size         Size of the z-dimension of the
                                                scatter_data array.
    \param [in]     mpp_type                    MPP_C type value.
    \param [in,out] array_segment               Array that will hold the
                                                scattered data.
    \param [in]     scatter_spot_x_start_index  Starting index in the x-dimension
                                                of the spot in the scatter_data
                                                array that will be sent as an
                                                array segment.
    \param [in]     scatter_spot_x_end_index    Ending index in the x-dimension
                                                of the spot in the scatter_data
                                                array that will be sent as an
                                                array segment.
    \param [in]     scatter_spot_y_start_index  Starting index in the y-dimension
                                                of the spot in the scatter_data
                                                array that will be sent as an
                                                array segment.
    \param [in]     scatter_spot_y_end_index    Ending index in the y-dimension
                                                of the spot in the scatter_data
                                                array that will be sent as an
                                                array segment.
    \param [in]     scatter_spot_z_start_index  Starting index in the z-dimension
                                                of the spot in the scatter_data
                                                array that will be sent as an
                                                array segment.
    \param [in]     scatter_spot_z_end_index    Ending index in the z-diension
                                                of the spot in the scatter_data
                                                array that will be sent as an
                                                array segment.
    \param [in]     rank_list                   Array of MPI rank ids from the
                                                world pelist.
    \param [in]     rank_list_size              Size of the array of MPI rank ids.
    \param [in]     rank_list_root_flag         Flag denoting the "scatter root".
    \param [in]     root_scatter_spot_x_offset  Offset in the x-dimension for
                                                the starting and ending indices
                                                of the spot in the scatter_data
                                                array that will be sent as
                                                an array segment.
    \param [in]     root_scatter_spot_y_offset  Offset in the y-dimension for
                                                the starting and ending indices
                                                of the spot in the scatter_data
                                                array that will be sent as
                                                an array segment.
    \param [in]     root_scatter_spot_z_offset  Offset in the z-dimension for
                                                the starting and ending indices
                                                of the spot in the scatter_data
                                                array that will be sent as
                                                an array segment.
    \param [in,out] context                      Context handle.
*/
void mpp_c_scatter_pelist(void **scatter_data,
                         size_t const scatter_data_x_size,
                         size_t const scatter_data_y_size,
                         size_t const scatter_data_z_size,
                         mpp_c_datatype_t const mpp_type,
                         void **array_segment,
                         int32_t const scatter_spot_x_start_index,
                         int32_t const scatter_spot_x_end_index,
                         int32_t const scatter_spot_y_start_index,
                         int32_t const scatter_spot_y_end_index,
                         int32_t const scatter_spot_z_start_index,
                         int32_t const scatter_spot_z_end_index,
                         int32_t const *rank_list,
                         size_t const rank_list_size,
                         int32_t const rank_list_root_flag,
                         int32_t const root_scatter_spot_x_offset,
                         int32_t const root_scatter_spot_y_offset,
                         int32_t const root_scatter_spot_z_offset,
                         mpp_c_context_t * const * const context);

/*---------------------------------------------------------------------------*/

#endif
