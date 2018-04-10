/** @file */
/*Header file for mpp_c errorr functions.*/

#ifndef SET_MPP_C_ERROR_H_
#define SET_MPP_C_ERROR_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_context_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

/*---------------------------------------------------------------------------*/
/**
    Print out an error message and abort if necessary.

    \param [in] error_type  Type of error message (NOTE,WARNING,FATAL).
    \param [in] error_mesg  Error message string.
    \param [in] mesg_len    Length of the error message string.
    \param [in] context     Context handle.
*/
void mpp_c_error(int32_t const error_type,
                 char const *error_mesg,
                 size_t const mesg_len,
                 mpp_c_context_t const * const context);

/*---------------------------------------------------------------------------*/

#endif
