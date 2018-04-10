#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "mpp_c_internal_error.h"
#include "mpp_c_event_t_api.h"
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"
#include "mpp_c_typedefs.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Initialize an mpp_c_event_t object.*/
void mpp_c_event_init(mpp_c_event_t * const * const self)
{
    /*Local variables*/
    mpp_c_event_t *tmp_ptr = NULL; /*Pointer to the mpp_c_event_t object.*/

    /*Set the local pointer to the mpp_c_event_t object.*/
    tmp_ptr = *self;

    /*Make sure that the inputted mpp_c_event_t pointer is not null.*/
    if (tmp_ptr == NULL)
    {
        throw_internal_error("MPP_C_EVENT_INIT","inputted mpp_c_event_t"
                             " pointer is null.  First malloc space.");
    }

    /*Set default values.*/
    snprintf(tmp_ptr->name,MPP_C_EVENT_NAME_LENGTH,"%c",'\0');
    tmp_ptr->event_type = MPP_C_NULL_EVENT;
    tmp_ptr->start_time = 0;
    tmp_ptr->end_time = 0;
    tmp_ptr->num_bytes = 0;
    tmp_ptr->timer_id = MPP_C_NULL_TIMER_ID;

    /*Set the initialization flag to true.*/
    tmp_ptr->is_initialized = MPP_C_TRUE;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Reset an mpp_c_event_t object.*/
void mpp_c_event_reset(mpp_c_event_t * const * const self)
{
    /*Local variables*/
    mpp_c_event_t *tmp_ptr = NULL; /*Pointer to the mpp_c_event_t object.*/

    /*Set the local pointer to the mpp_c_event_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_event_t object has been initialized.*/
    mpp_c_event_check_init_state(tmp_ptr,"MPP_C_EVENT_RESET");

    /*Reset default values.*/
    snprintf(tmp_ptr->name,MPP_C_EVENT_NAME_LENGTH,"%c",'\0');
    tmp_ptr->event_type = MPP_C_NULL_EVENT;
    tmp_ptr->start_time = 0;
    tmp_ptr->end_time = 0;
    tmp_ptr->num_bytes = 0;
    tmp_ptr->timer_id = MPP_C_NULL_TIMER_ID;

    /*Set the initialization flag to false.*/
    tmp_ptr->is_initialized = MPP_C_FALSE;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that sets the starting time for an mpp_c_event_t object.*/
void mpp_c_event_set_start_time(mpp_c_event_t * const * const self,
                                char const * const name,
                                size_t const name_len,
                                int32_t const event_type,
                                int32_t const timer_id)
{
    /*Local variables*/
    mpp_c_event_t *tmp_ptr = NULL; /*Pointer to the mpp_c_event_t object.*/

    /*Set the local pointer to the mpp_c_event_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_event_t object has been initialized.*/
    mpp_c_event_check_init_state(tmp_ptr,"MPP_C_EVENT_SET_START_TIME");

    /*Check inputted string.*/
    check_string_input(name,name_len,"MPP_C_EVENT_SET_START_TIME");

    /*Make sure a start time, end time, or number of bytes has not already
      been set for this object.*/
    if (mpp_c_event_get_start_time(tmp_ptr) != 0 ||
        mpp_c_event_get_end_time(tmp_ptr) != 0 ||
        mpp_c_event_get_num_bytes(tmp_ptr) != 0)
    {
        throw_internal_error("MPP_C_EVENT_SET_START_TIME","event has already"
                             " been started or ended.");
    }

    /*Set the name for the event.  The name will be truncated if it exceeds
      MPP_C_EVENT_NAME_LENGTH characters.*/
    if (name_len > MPP_C_EVENT_NAME_LENGTH)
    {
        print_internal_message("MPP_C_EVENT_SET_START_TIME","inputted name"
                               " exceeds MPP_C_EVENT_NAME_LENGTH characters"
                               " and will be truncated.");
    }
    snprintf(tmp_ptr->name,MPP_C_EVENT_NAME_LENGTH,"%s",name);

    /*Set the event type.*/
    switch (event_type)
    {
        case EVENT_SEND:
            tmp_ptr->event_type = EVENT_SEND;
            break;
        case EVENT_RECV:
            tmp_ptr->event_type = EVENT_RECV;
            break;
        case EVENT_BCAST:
            tmp_ptr->event_type = EVENT_BCAST;
            break;
        case EVENT_REDUCE:
            tmp_ptr->event_type = EVENT_REDUCE;
            break;
        case EVENT_WAIT:
            tmp_ptr->event_type = EVENT_WAIT;
            if (timer_id != MPP_C_NULL_TIMER_ID)
            {
                tmp_ptr->timer_id = timer_id;
            }
            break;
        case EVENT_ALLTOALL:
            tmp_ptr->event_type = EVENT_ALLTOALL;
            break;
        case EVENT_START_TIMER:
            tmp_ptr->event_type = EVENT_START_TIMER;
            if (timer_id != MPP_C_NULL_TIMER_ID)
            {
                tmp_ptr->timer_id = timer_id;
            }
            break;
        case EVENT_STOP_TIMER:
            tmp_ptr->event_type = EVENT_STOP_TIMER;
            if (timer_id != MPP_C_NULL_TIMER_ID)
            {
                tmp_ptr->timer_id = timer_id;
            }
            break;
        default:
            throw_internal_error("MPP_C_EVENT_SET_START_TIME","unsupported"
                                 " event type.");
    }

    /*Set the time at which the event starts.*/
    tmp_ptr->start_time = MPI_Wtime();

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that sets the ending time and number of bytes communicated for an
  mpp_c_event_t object.*/
void mpp_c_event_set_end_time_and_num_bytes(mpp_c_event_t * const * const self,
                                            size_t const comm_num_bytes)
{
    /*Local variables*/
    mpp_c_event_t *tmp_ptr = NULL; /*Pointer to the mpp_c_event_t object.*/

    /*Set the local pointer to the mpp_c_event_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_event_t object has been initialized.*/
    mpp_c_event_check_init_state(tmp_ptr,"MPP_C_EVENT_SET_END_TIME_AND_"
                                 "NUM_BYTES");

    /*Make sure that the start time has been set, and that the end time and
      number of bytes have not already been set.*/
    if (mpp_c_event_get_start_time(tmp_ptr) == 0 ||
        mpp_c_event_get_end_time(tmp_ptr) !=0 ||
        mpp_c_event_get_num_bytes(tmp_ptr) != 0)
    {
        throw_internal_error("MPP_C_EVENT_SET_END_TIME_AND_NUM_BYTES",
                             " event has not been started or already has"
                             " been ended.");
    }

    /*Set the time at which the event ends and the number of bytes
      communicated.*/
    tmp_ptr->end_time = MPI_Wtime();
    tmp_ptr->num_bytes = comm_num_bytes;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that tells whether or not an mpp_c_event_t object has been
  initialized.*/
mpp_c_flag_t mpp_c_event_is_init(mpp_c_event_t const * const self)
{
    /*Local variables*/
    mpp_c_flag_t init_flag = 0; /*Initialization flag.*/

    init_flag = MPP_C_FALSE;
    if (self != NULL)
    {
        if (self->is_initialized)
        {
            init_flag = MPP_C_TRUE;
        }
    }

    return init_flag;
}

/*---------------------------------------------------------------------------*/
/*Function that throws a FATAL error if the inputted mpp_c_event_t object
  is not initialized.*/
void mpp_c_event_check_init_state(mpp_c_event_t const * const self,
                                  char *routine_name)
{
    if (!mpp_c_event_is_init(self))
    {
        throw_internal_error(routine_name,"you must first initialize the"
                             " mpp_c_event_t object.");
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the name of a event for an mpp_c_event_t object.*/
char *mpp_c_event_get_name(mpp_c_event_t * const self)
{
    /*Make sure that the mpp_c_event_t object has been initialized.*/
    mpp_c_event_check_init_state(self,"MPP_C_EVENT_GET_NAME");

    return self->name;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the event type for an mpp_c_event_t object.*/
int32_t mpp_c_event_get_event_type(mpp_c_event_t const * const self)
{
    /*Make sure that the mpp_c_event_t object has been initialized.*/
    mpp_c_event_check_init_state(self,"MPP_C_EVENT_GET_EVENT_TYPE");

    return self->event_type;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the start time for an mpp_c_event_t object.*/
double mpp_c_event_get_start_time(mpp_c_event_t const * const self)
{
    /*Make sure that the mpp_c_event_t object has been initialized.*/
    mpp_c_event_check_init_state(self,"MPP_C_EVENT_GET_START_TIME");

    return self->start_time;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the end time for an mpp_c_event_t object.*/
double mpp_c_event_get_end_time(mpp_c_event_t const * const self)
{
    /*Make sure that the mpp_c_event_t object has been initialized.*/
    mpp_c_event_check_init_state(self,"MPP_C_EVENT_GET_END_TIME");

    return self->end_time;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the number of communicated bytes for an mpp_c_event_t
  object.*/
size_t mpp_c_event_get_num_bytes(mpp_c_event_t const * const self)
{
    /*Make sure that the mpp_c_event_t object has been initialized.*/
    mpp_c_event_check_init_state(self,"MPP_C_EVENT_GET_NUM_BYTES");

    return self->num_bytes;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the timer id associated with the event for an
  mpp_c_event_t object.*/
int32_t mpp_c_event_get_timer_id(mpp_c_event_t const * const self)
{
    /*Make sure that the mpp_c_event_t object has been initialized.*/
    mpp_c_event_check_init_state(self,"MPP_C_EVENT_GET_TIMER_ID");

    return self->timer_id;
}

/*---------------------------------------------------------------------------*/

