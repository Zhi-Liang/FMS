#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "mpp_c_datatype_t_api.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"

/*---------------------------------------------------------------------------*/
/*Function that returns the MPI type parameter value that corresponds to the
  inputted mpp_c_datatype_t value.*/
MPI_Datatype mpp_c_datatype_get_MPI_param(mpp_c_datatype_t const mpp_type)
{
    /*Local variables*/
    MPI_Datatype mpi_type_val; /*MPI type parameter value.*/

    /*Get the corresponding MPI parameter.*/
    switch (mpp_type)
    {
        case MPP_INT32:
            mpi_type_val = MPI_INT32_T;
            break;
        case MPP_UINT32:
            mpi_type_val = MPI_UINT32_T;
            break;
        case MPP_INT64:
            mpi_type_val = MPI_INT64_T;
            break;
        case MPP_UINT64:
            mpi_type_val = MPI_UINT64_T;
            break;
        case MPP_REAL32:
            mpi_type_val = MPI_FLOAT;
            break;
        case MPP_REAL64:
            mpi_type_val = MPI_DOUBLE;
            break;
        case MPP_CHAR:
            mpi_type_val = MPI_CHAR;
            break;
        default:
            throw_internal_error("MPP_C_DATATYPE_GET_MPI_PARAM",
                                 "unsupported type.");
    }

    return mpi_type_val;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the size in bytes of a mpp_c_datatype_t variable.*/
size_t mpp_c_datatype_get_num_bytes(mpp_c_datatype_t const mpp_type)
{
    /*Local variables*/
    size_t num_bytes = 0;    /*Size in bytes of the mpp_c_datatype_t variable.*/

    /*Return the size of each type in bytes.*/
    switch (mpp_type)
    {
        case MPP_INT32:
            num_bytes = sizeof(int32_t);
            break;
        case MPP_UINT32:
            num_bytes = sizeof(uint32_t);
            break;
        case MPP_INT64:
            num_bytes = sizeof(int64_t);
            break;
        case MPP_UINT64:
            num_bytes = sizeof(uint64_t);
            break;
        case MPP_REAL32:
            num_bytes = sizeof(float);
            break;
        case MPP_REAL64:
            num_bytes = sizeof(double);
            break;
        case MPP_CHAR:
            num_bytes = sizeof(char);
            break;
        default:
            throw_internal_error("MPP_C_DATATYPE_GET_NUM_BYTES",
                                 " unsupported type.");
    }

    return num_bytes;
}

/*---------------------------------------------------------------------------*/

