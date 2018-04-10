
#ifndef SET_MPP_C_INIT_H_
#define SET_MPP_C_INIT_H_

#include "mpp_c_context_t_api.h"
#include "mpp_c_mpi.h"
#include "mpp_c_shared_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

/*---------------------------------------------------------------------------*/
/**
    Function that initializes the mpp_c library.

    \param [in,out] shared      Pointer to the data that is shared between all
                                contexts.
    \param [in,out] context     A context handle.
    \param [in]     local_comm  An MPI communicator id.
*/
void mpp_c_init(mpp_c_shared_t **shared,
                mpp_c_context_t **context,
                MPI_Comm local_comm);

/*---------------------------------------------------------------------------*/

#endif
