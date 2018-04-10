/** @file */
/*Header file for mpp_c_parse_nml methods.*/

#ifndef SET_MPP_C_PARSE_NML_H_
#define SET_MPP_C_PARSE_NML_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_typedefs.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

/*---------------------------------------------------------------------------*/
/**
    Using a Fortran read to parse a namelist contained in a buffer.

    \param [in]  buffer_len        The length of the inputted character buffer.
    \param [in]  buffer            A character buffer containing the namelist.
    \param [out] etc_stderr_flag   Namelist flag that determines whether or
                                   not the etcfiles are redirected to stderr.
    \param [out] request_factor    Namelist parameter used to size the
                                   internal send/recv request arrays.
    \param [out] timers_allowed    Namelist flag that determines whether or
                                   not timers are allowed.
    \param [out] sync_timers_flag  Namelist flag that denotes whether or not
                                   all timers are synchronized when started.
    \param [out] nml_parse_err     Error code returned by the Fortran internal
                                   read. 0 indicates success.  This should
                                   always be checked.
*/
void mpp_c_parse_nml(size_t buffer_len,
                     char * buffer,
                     mpp_c_flag_t *etc_stderr_flag,
                     size_t *request_factor,
                     mpp_c_flag_t *timers_allowed,
                     mpp_c_flag_t *sync_timers_flag,
                     int32_t *nml_parse_err);

/*---------------------------------------------------------------------------*/

#endif
