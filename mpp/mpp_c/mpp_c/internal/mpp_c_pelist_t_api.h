/** @file */

#ifndef MPP_C_PELIST_T_API_H_
#define MPP_C_PELIST_T_API_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"
#include "mpp_c_typedefs.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Structure declarations.*/
/**
    This structure stores all the necessary data for our "pelists".  These
    pelists are used to perform collective MPI operations like broadcasts,
    reductions, and all-to-all communications.  To use this structure:

    1.) Malloc space.

        > ptr = (mpp_c_pelist_t *)malloc(sizeof(mpp_c_pelist_t));

    2.) Initialize the object.

        > mpp_c_pelist_init(&ptr);

*/
struct mpp_c_pelist
{
    mpp_c_flag_t is_initialized;             /**<Flag telling whether or not the object has been initialized.*/
    mpp_c_flag_t is_set;                     /**<Flag telling if the pelist has been set.*/
    char name[MPP_C_MAX_PELIST_NAME_LENGTH]; /**<Pelist name.*/
    uint32_t *rank_list;                     /**<Array of "world pelist" rank ids included in the pelist.*/
    size_t rank_list_size;                   /**<Number of ranks in the pelist.*/
    uint32_t root_rank;                      /**<Root rank id for the pelist.*/
    MPI_Comm comm_id;                        /**<MPI communicator id for the pelist.*/
    MPI_Group group_id;                      /**<MPI group id for the pelist.*/
};
typedef struct mpp_c_pelist mpp_c_pelist_t;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

/*---------------------------------------------------------------------------*/
/**
    Function that initializes an mpp_c_pelist_t object.

    \param [in,out] self  An mpp_c_pelist_t type object.
*/
void mpp_c_pelist_init(mpp_c_pelist_t * const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that releases memory and resets default values for an
     mpp_c_pelist_t object.

    \param [in,out] self  An mpp_c_pelist_t type object.
*/
void mpp_c_pelist_reset(mpp_c_pelist_t * const * const self);

/*---------------------------------------------------------------------------*/
/**
    Set the name, rank list, and root rank for an mpp_c_pelist_t object.

    \param [in,out] self        An mpp_c_pelist_t type object.
    \param [in]     name        Name of the pelist.
    \param [in]     name_len    Length of the pelist name.
    \param [in]     rank_array  Array of MPI rank ids from the world pelist.
    \param [in]     num_ranks   Size of the array of MPI rank ids.
    \param [in]     comm_id     MPI communicator in which the rank list lives.
*/
void mpp_c_pelist_set_pelist(mpp_c_pelist_t * const * const self,
                             char const *name,
                             size_t const name_len,
                             uint32_t const *rank_array,
                             size_t const num_ranks,
                             MPI_Comm const comm_id);

/*---------------------------------------------------------------------------*/
/**
    Function that sets the root rank id for an mpp_c_pelist_t object.

    \param [in,out] self       An mpp_c_pelist_t type object.
    \param [in]     root_rank  MPI rank id of the desired pelist root.
*/
void mpp_c_pelist_set_root_rank(mpp_c_pelist_t * const * const self,
                                uint32_t const root_rank);

/*---------------------------------------------------------------------------*/
/**
    Function that tells whether or not an mpp_c_pelist_t object has been
    initialized.

    \param  [in] self  An mpp_c_pelist_t type object.
    \return            A flag denoting whether or not the mpp_c_pelist_t
                       object has been initialized.
*/
mpp_c_flag_t mpp_c_pelist_is_init(mpp_c_pelist_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that throws a FATAL error if the inputted mpp_c_pelist_t object
    is not initialized.

    \param [in] self          An mpp_c_pelist_t type object.
    \param [in] routine_name  Name of the calling routine.
*/
void mpp_c_pelist_check_init_state(mpp_c_pelist_t const * const self,
                                   char *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Function that tells whether or not an mpp_c_pelist_t object has been
    set.

    \param  [in] self  An mpp_c_pelist_t type object.
    \return            A flag denoting whether or not the mpp_c_pelist_t
                       object has been set.
*/
mpp_c_flag_t mpp_c_pelist_is_set(mpp_c_pelist_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that throws a FATAL error if the inputted mpp_c_pelist_t object
    has not been set.

    \param [in] self          An mpp_c_pelist_t type object.
    \param [in] routine_name  Name of the calling routine.
*/
void mpp_c_pelist_check_is_set(mpp_c_pelist_t const * const self,
                               char *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the name of a pelist for an mpp_pelist_type object.

    \param  [in] self  An mpp_c_pelist_t type object.
    \return            The name of the pelist.
*/
char *mpp_c_pelist_get_name(mpp_c_pelist_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the rank list for an mpp_c_pelist_t object.

    \param  [in] self  An mpp_c_pelist_t type object.
    \return            An array of MPI rank ids from the world pelist.
*/
uint32_t *mpp_c_pelist_get_rank_list(mpp_c_pelist_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the size of the rank list for an mpp_c_pelist_t
    object.

    \param  [in] self  An mpp_c_pelist_t type object.
    \return            The size of the array of MPI rank ids.
*/
size_t mpp_c_pelist_get_rank_list_size(mpp_c_pelist_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the root rank id for an mpp_c_pelist_t object.

    \param  [in] self  An mpp_c_pelist_t type object.
    \return            The MPI rank id of the pelist root.
*/
uint32_t mpp_c_pelist_get_root_rank(mpp_c_pelist_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that determines if the inputted rank is the root of the
    mpp_c_pelist_t object.

    \param  [in] self       An mpp_c_pelist_t type object.
    \param  [in] root_rank  MPI rank id.
    \return                 A flag denoting whether or not the current rank is
                            the root of the pelist.
*/
mpp_c_flag_t mpp_c_pelist_is_root_rank(mpp_c_pelist_t const * const self,
                                       uint32_t const root_rank);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the MPI communicator id for an mpp_c_pelist_t
    object.

    \param  [in] self  An mpp_c_pelist_t type object.
    \return            The MPI communicator id associated with the pelist.
*/
MPI_Comm mpp_c_pelist_get_comm_id(mpp_c_pelist_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the MPI group id for an mpp_c_pelist_t object.

    \param  [in] self  An mpp_c_pelist_t type object.
    \return            The MPI group id associated with the pelist.
*/
MPI_Group mpp_c_pelist_get_group_id(mpp_c_pelist_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that deterimines if a rank exists on the rank list for an
    mpp_c_pelist_t object.

    \param  [in] self  An mpp_c_pelist_t type object.
    \param  [in] rank  A MPI rank id.
    \return            A flag denoting whether or not the inputted rank
                       exists on the pelist.
*/
mpp_c_flag_t mpp_c_pelist_is_rank_on_rank_list(mpp_c_pelist_t const * const self,
                                               uint32_t const rank);

/*---------------------------------------------------------------------------*/

#endif
