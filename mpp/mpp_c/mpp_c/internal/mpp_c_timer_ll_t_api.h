/** @file */

#ifndef MPP_C_TIMER_LL_T_API_H_
#define MPP_C_TIMER_LL_T_API_H_

#include <stdlib.h>
#include "mpp_c_timer_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Structure declarations.*/
/**
    This structure defines a linked-list node, which is used to keep track
    of all currently "turned-on" timers.  Logged events are associated with
    the most recently "turned-on" timer.  To use this structure:

    1.) Initialize the object.

        > mpp_c_timer_ll_init(&ptr,context);

*/
struct mpp_c_timer_ll
{
    mpp_c_timer_t *cur_timer;          /**<Pointer to the current timer.*/
    struct mpp_c_timer_ll *prev_timer; /**<Pointer to the previous timer.*/
};
typedef struct mpp_c_timer_ll mpp_c_timer_ll_t;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

/*---------------------------------------------------------------------------*/
/**
    Function that initializes an mpp_c_timer_ll_t object.

    \param [in,out] self  An mpp_c_timer_ll_t type object.
*/
void mpp_c_timer_ll_init(mpp_c_timer_ll_t **self);

/*---------------------------------------------------------------------------*/
/**
    Function that releases memory and resets an mpp_c_timer_ll_t object.

    \param [in,out] self  An mpp_c_timer_ll_t type object.
*/
void mpp_c_timer_ll_reset(mpp_c_timer_ll_t **self);

/*------------------------------------------------------------------------- -*/
/**
    Function that adds a node to the timer linked list.

    \param [in,out] self   An mpp_c_timer_ll_t type object.
    \param [in]     timer  An mpp_c_timer_t type object.
*/
void mpp_c_timer_ll_set_cur_timer(mpp_c_timer_ll_t **self,
                                  mpp_c_timer_t *timer);

/*---------------------------------------------------------------------------*/
/**
    Function that removes a node from the timer linked list.

    \param [in,out] self  An mpp_c_timer_ll_t type object.
*/
void mpp_c_timer_ll_revert_cur_timer(mpp_c_timer_ll_t **self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns a pointer to the current timer for an
    mpp_c_timer_ll_t object.

    \param  [in] self  An mpp_c_timer_ll_t type object.
    \return            A pointer ot the current timer.
*/
mpp_c_timer_t *mpp_c_timer_ll_get_cur_timer(mpp_c_timer_ll_t const * const self);

/*---------------------------------------------------------------------------*/

#endif
