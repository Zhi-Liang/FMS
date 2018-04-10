/** @file */

#ifndef SET_MPP_C_DATATYPE_T_API_H_
#define SET_MPP_C_DATATYPE_T_API_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_mpi.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Type definition.*/
/**
    This typedef is used for parameters that specifiy specific variable
    types.  These types are then translated to MPI.
*/
typedef int32_t mpp_c_datatype_t;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

/*---------------------------------------------------------------------------*/
/**
    Function that returns the MPI type parameter value that corresponds to the
    inputted mpp_c_datatype_t value.

    \param [in] mpp_type   MPP_C type value.
    \param [in] context    Context handle.
    \return                MPI type value.
*/
MPI_Datatype mpp_c_datatype_get_MPI_param(mpp_c_datatype_t const mpp_type);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the size in bytes of a mpp_c_datatype_t variable.

    \param [in] mpp_type   MPP_C type value.
    \param [in] context    Context handle.
    \return                Size of the MPP_C type variable in bytes.
*/
size_t mpp_c_datatype_get_num_bytes(mpp_c_datatype_t const mpp_type);

/*---------------------------------------------------------------------------*/

#endif
