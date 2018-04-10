/** @file */

#ifndef SET_MPP_C_SHARED_T_API_H_
#define SET_MPP_C_SHARED_T_API_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_macros.h"
#include "mpp_c_pelist_t_api.h"
#include "mpp_c_typedefs.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Structure declarations.*/
/**
    This structure stores all the necessary data that is shared between
    all contexts. To use this structure:

    1.) Malloc space.

        > ptr = (mpp_c_shared_t *)malloc(sizeof(mpp_c_shared_t));

    2.) Initialize the object.

        > mpp_c_shared_init(&ptr);

*/
struct mpp_c_shared
{
    mpp_c_flag_t is_initialized;     /**<Initialization flag.*/
    mpp_c_flag_t is_set;             /**<Is set flag.*/
    mpp_c_pelist_t *world_pelist;    /**<"World" pelist.*/
    char *input_nml_buffer;          /**<Buffer which holds the contents of the input.nml file.*/
    char *input_json_buffer;         /**<Buffer which holds the contents of the input.json file*/
    size_t input_nml_buffer_size;    /**<Size in bytes of the input.nml file.*/
    size_t input_json_buffer_size;   /**<Size in bytes of the input.json file*/
    mpp_c_flag_t etc_unit_is_stderr; /**<Namelist parameter directing the etc files to stderr.*/
    size_t request_multiply;         /**<Namelist parameter for sizing request arrays.*/
    mpp_c_flag_t record_timing_data; /**<Namelist parameter telling whether to record timing data.*/
    mpp_c_flag_t sync_all_clocks;    /**<Namelist parameter for syncing all clocks whenever they are started.*/
};
typedef struct mpp_c_shared mpp_c_shared_t;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

/*---------------------------------------------------------------------------*/
/**
    Function that initializes an mpp_c_shared_t object.

    \param [in,out] self  An mpp_c_shared_t type object.
*/
void mpp_c_shared_init(mpp_c_shared_t * const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that resets default values for an mpp_c_shared_t object.

    \param [in,out] self  An mpp_c_shared_t type object.
*/
void mpp_c_shared_reset(mpp_c_shared_t * const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that sets the necessary data that will be shared between all
    contexts.

    \param [in,out] self        An mpp_c_shared_t type object.
    \param [in]     local_comm  An MPI communicator for the "world" pelist
                                for all contexts.
*/
void mpp_c_shared_set_shared_data(mpp_c_shared_t * const * const self,
                                  MPI_Comm const local_comm);

/*---------------------------------------------------------------------------*/
/**
    Function that tells whether or not an mpp_c_shared_t object has been
    initialized.

    \param  [in] self  An mpp_c_shared_t type object.
    \return            A flag telling whether or not the mpp_c_shared_t type
                       object has been initialized.
*/
mpp_c_flag_t mpp_c_shared_is_init(mpp_c_shared_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that throws a FATAL error if the inputted mpp_c_shared_t object
    is not initialized.

    \param [in] self          An mpp_c_shared_t type object.
    \param [in] routine_name  Name of calling routine.
*/
void mpp_c_shared_check_init_state(mpp_c_shared_t const * const self,
                                   char *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Function that tells whether or not an mpp_c_shared_t object has been
    set.

    \param  [in] self  An mpp_c_shared_t type object.
    \return            A flag telling whether or not the mpp_c_shared_t type
                       object has been set.
*/
mpp_c_flag_t mpp_c_shared_is_set(mpp_c_shared_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that throws a FATAL error if the inputted mpp_c_shared_t object
    is not set.

    \param [in] self          An mpp_c_shared_t type object.
    \param [in] routine_name  Name of calling routine.
*/
void mpp_c_shared_check_is_set(mpp_c_shared_t const * const self,
                               char *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Function that returns a pointer to the "world" pelist for an
    mpp_c_shared_t object.

    \param  [in] self  An mpp_c_shared_t type object.
    \return            Pointer to the "world" pelist.
*/
mpp_c_pelist_t *mpp_c_shared_get_world_pelist(mpp_c_shared_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns a pointer to the input_nml buffer an
    mpp_c_shared_t object.

    \param  [in] self  An mpp_c_shared_t type object.
    \return            Pointer to the input_nml buffer.
*/
char *mpp_c_shared_get_input_nml_buffer(mpp_c_shared_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the size in bytes of the input_nml buffer for an
    mpp_c_shared_t object.

    \param  [in] self  An mpp_c_shared_t type object.
    \return            Size in bytes of the input_nml buffer.
*/
size_t mpp_c_shared_get_input_nml_buffer_size(mpp_c_shared_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the etc_unit_is_stderr namelist parameter for an
    mpp_c_shared_t object.

    \param  [in] self  An mpp_c_shared_t type object.
    \return            etc_unit_is_stderr namelist parameter.
*/
mpp_c_flag_t mpp_c_shared_get_etc_unit_is_stderr(mpp_c_shared_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the request_multiply namelist parameter for an
    mpp_c_shared_t object.

    \param  [in] self  An mpp_c_shared_t type object.
    \return            request_multiply namelist parameter.
*/
size_t mpp_c_shared_get_request_multiply(mpp_c_shared_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the record_timing_data namelist parameter for an
    mpp_c_shared_t object.

    \param  [in] self  An mpp_c_shared_t type object.
    \return            record_timing_data namelist parameter.
*/
mpp_c_flag_t mpp_c_shared_get_record_timing_data(mpp_c_shared_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the sync_all_clocks namelist parameter for an
    mpp_c_shared_t object.

    \param  [in] self  An mpp_c_shared_t type object.
    \return            sync_all_clocks namelist parameter.
*/
mpp_c_flag_t mpp_c_shared_get_sync_all_clocks(mpp_c_shared_t const * const self);

/*---------------------------------------------------------------------------*/

#endif
