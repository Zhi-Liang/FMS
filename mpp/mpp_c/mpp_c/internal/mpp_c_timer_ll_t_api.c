#include <stdlib.h>
#include "helper_functions.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_timer_ll_t_api.h"
#include "mpp_c_timer_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Methods.*/

/*---------------------------------------------------------------------------*/
/*Initialize an mpp_c_timer_ll_t structure.*/
void mpp_c_timer_ll_init(mpp_c_timer_ll_t **self)
{
    /*Set the head of the linked list to NULL.*/
    *self = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Release memory and reset an mpp_c_timer_ll_t object.*/
void mpp_c_timer_ll_reset(mpp_c_timer_ll_t **self)
{
    /*Local variables*/
    mpp_c_timer_ll_t *tmp_ptr = NULL; /*Pointer to the mpp_c_timer_ll_t object.*/

    /*Set the local pointer to the mpp_c_timer_ll_t object.*/
    tmp_ptr = *self;

    /*Free the linked list.*/
    while (tmp_ptr != NULL)
    {
        mpp_c_timer_ll_revert_cur_timer(&tmp_ptr);
    }
    *self = tmp_ptr;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*------------------------------------------------------------------------- -*/
/*Function that adds a node to the timer linked list.*/
void mpp_c_timer_ll_set_cur_timer(mpp_c_timer_ll_t **self,
                                  mpp_c_timer_t *timer)
{
    /*Local variables*/
    mpp_c_timer_ll_t *tmp_ptr = NULL;  /*Pointer to the mpp_c_timer_ll_t object.*/
    mpp_c_timer_ll_t *tmp_node = NULL; /*Pointer to a mpp_c_timer_ll_t object.*/

    /*Set the local pointer to the mpp_c_timer_ll_t object.*/
    tmp_ptr = *self;

    /*Add a node to the timer linked list.*/
    safemalloc((void **)&tmp_node,sizeof(mpp_c_timer_ll_t));
    tmp_node->cur_timer = timer;
    tmp_node->prev_timer = tmp_ptr;
    tmp_ptr = tmp_node;
    *self = tmp_ptr;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_node = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that removes a node from the timer linked list.*/
void mpp_c_timer_ll_revert_cur_timer(mpp_c_timer_ll_t **self)
{
    /*Local variables*/
    mpp_c_timer_ll_t *tmp_ptr = NULL;  /*Pointer to the mpp_c_timer_ll_t object.*/
    mpp_c_timer_ll_t *tmp_node = NULL; /*Pointer to a mpp_c_timer_ll_t object.*/

    /*Set the local pointer to the mpp_c_timer_ll_t object.*/
    tmp_ptr = *self;

    /*Make sure that the linked list is not already null.*/
    if (tmp_ptr == NULL)
    {
        throw_internal_error("MPP_C_TIMER_LL_REVERT_CUR_TIMER",
                             "current timer node is already null.");
    }

    /*Remove a node to the timer linked list.*/
    tmp_node = tmp_ptr->prev_timer;
    tmp_ptr->cur_timer = NULL;
    safefree((void **)&tmp_ptr);
    tmp_ptr = tmp_node;
    *self = tmp_ptr;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_node = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the current timer for an mpp_c_timer_ll_t
  object.*/
mpp_c_timer_t *mpp_c_timer_ll_get_cur_timer(mpp_c_timer_ll_t const * const self)
{
    /*Make sure that the linked list is not already null.*/
    if (self == NULL)
    {
        throw_internal_error("MPP_C_TIMER_LL_GET_CUR_TIMER",
                             "current timer node is null.");
    }

    return self->cur_timer;
}

/*---------------------------------------------------------------------------*/

