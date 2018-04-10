
#ifndef SET_MPP_C_EXIT_H_
#define SET_MPP_C_EXIT_H_

#include "mpp_c_context_t_api.h"
#include "mpp_c_shared_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

/*---------------------------------------------------------------------------*/
/**
    Function that finalizes the mpp_c library.

    \param [in,out] shared   Pointer to data shared between all contexts.
    \param [in,out] context  A context handle.
*/
void mpp_c_exit(mpp_c_shared_t **shared,
                mpp_c_context_t **context);

/*---------------------------------------------------------------------------*/

#endif
