#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "helper_functions.h"
#include "mpp_c_event_t_api.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"
#include "mpp_c_pelist_t_api.h"
#include "mpp_c_timer_t_api.h"
#include "mpp_c_typedefs.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Initialize an mpp_c_timer_t object.*/
void mpp_c_timer_init(mpp_c_timer_t * const * const self)
{
    /*Local variables*/
    mpp_c_timer_t *tmp_ptr = NULL; /*Pointer to the mpp_c_timer_t object.*/

    /*Set the local pointer to the mpp_c_timer_t object.*/
    tmp_ptr = *self;

    /*Make sure that the inputted mpp_c_timer_t pointer is not null.*/
    if (tmp_ptr == NULL)
    {
        throw_internal_error("MPP_C_TIMER_INIT","inputted mpp_c_timer_t"
                             " pointer is null.  First malloc space.");
    }

    /*Set default values.*/
    tmp_ptr->has_metadata = MPP_C_FALSE;
    snprintf(tmp_ptr->name,MPP_C_TIMER_NAME_LENGTH,"%c",'\0');
    tmp_ptr->start_time = 0;
    tmp_ptr->end_time = 0;
    tmp_ptr->total_time = 0;
    tmp_ptr->pelist = NULL;
    tmp_ptr->is_on = MPP_C_FALSE;
    tmp_ptr->grain = MPP_C_NULL_TIMER_GRAIN;
    tmp_ptr->sync_at_start = MPP_C_FALSE;
    tmp_ptr->is_detailed = MPP_C_FALSE;
    tmp_ptr->max_events = 0;
    tmp_ptr->cur_num_events = 0;
    tmp_ptr->event_list = NULL;

    /*Set the initialization flag to true.*/
    tmp_ptr->is_initialized = MPP_C_TRUE;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Reset an mpp_c_timer_t object.*/
void mpp_c_timer_reset(mpp_c_timer_t * const * const self)
{
    /*Local variables*/
    mpp_c_timer_t *tmp_ptr = NULL;   /*Pointer to the mpp_c_timer_t object.*/
    mpp_c_event_t *tmp_event = NULL; /*Pointer to an mpp_c_event_t object.*/
    size_t num_events = 0;           /*Number of events.*/
    unsigned int i = 0;              /*Loop variable.*/

    /*Set the local pointer to the mpp_c_timer_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_timer_t object has been initialized.*/
    mpp_c_timer_check_init_state(tmp_ptr,"MPP_C_TIMER_INIT");

    /*Free appropriate memory and reset default values.*/
    if (mpp_c_timer_has_metadata(tmp_ptr) && mpp_c_timer_is_detailed(tmp_ptr))
    {
        num_events = mpp_c_timer_get_cur_num_events(tmp_ptr);
        for (i=0;i<num_events;i++)
        {
            tmp_event = &(tmp_ptr->event_list[i]);
            mpp_c_event_reset(&tmp_event);
        }
        safefree((void **) &(tmp_ptr->event_list));
        tmp_ptr->event_list = NULL;
    }
    tmp_ptr->has_metadata = MPP_C_FALSE;
    snprintf(tmp_ptr->name,MPP_C_TIMER_NAME_LENGTH,"%c",'\0');
    tmp_ptr->start_time = 0;
    tmp_ptr->end_time = 0;
    tmp_ptr->total_time = 0;
    tmp_ptr->pelist = NULL;
    tmp_ptr->is_on = MPP_C_FALSE;
    tmp_ptr->grain = MPP_C_NULL_TIMER_GRAIN;
    tmp_ptr->sync_at_start = MPP_C_FALSE;
    tmp_ptr->is_detailed = MPP_C_FALSE;
    tmp_ptr->max_events = 0;
    tmp_ptr->cur_num_events = 0;

    /*Set the initialization flag to false.*/
    tmp_ptr->is_initialized = MPP_C_FALSE;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_event = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Set the name, grain, pelist, and flag values for an mpp_c_timer_t object.*/
void mpp_c_timer_set_timer_metadata(mpp_c_timer_t * const * const self,
                                    char const *name,
                                    size_t const name_len,
                                    int32_t const grain,
                                    mpp_c_flag_t const sync_flag,
                                    mpp_c_flag_t const detail_flag,
                                    mpp_c_pelist_t const * const pelist)
{
    /*Local variables*/
    mpp_c_timer_t *tmp_ptr = NULL;   /*Pointer to the mpp_c_timer_t object.*/
    mpp_c_event_t *tmp_event = NULL; /*Pointer to an mpp_c_event_t object.*/
    unsigned int i = 0;              /*Loop variable.*/

    /*Set the local pointer to the mpp_c_timer_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_timer_t object has been initialized.*/
    mpp_c_timer_check_init_state(tmp_ptr,"MPP_C_TIMER_SET_TIMER_METADATA");

    /*Check inputted string.*/
    check_string_input(name,name_len,"MPP_C_TIMER_SET_TIMER_METADATA");

    /*Check inputted pelist.*/
    mpp_c_pelist_check_is_set(pelist,"MPP_C_TIMER_SET_TIMER_METADATA");

    /*Set a name for the timer. The name will be truncated if it exceeds
      MPP_C_TIMER_NAME_LENGTH characters.*/
    if (name_len > MPP_C_TIMER_NAME_LENGTH)
    {
        print_internal_message("MPP_C_TIMER_SET_TIMER_METADATA","inputted"
                               " name exceeds MPP_C_TIMER_NAME_LENGTH"
                               " characters and will be truncated.");
    }
    snprintf(tmp_ptr->name,MPP_C_TIMER_NAME_LENGTH,"%s",name);

    /*Set the timer's pelist pointer to point to the inputted pelist.*/
    tmp_ptr->pelist = pelist;

    /*Set the timer's granularity.*/
    switch (grain)
    {
        case MPP_C_RUNTIME_TIMER:
            tmp_ptr->grain = MPP_C_RUNTIME_TIMER;
            break;
        case MPP_C_COMPONENT_TIMER:
            tmp_ptr->grain = MPP_C_COMPONENT_TIMER;
            break;
        case MPP_C_SUBCOMPONENT_TIMER:
            tmp_ptr->grain = MPP_C_SUBCOMPONENT_TIMER;
            break;
        case MPP_C_MODULE_DRIVER_TIMER:
            tmp_ptr->grain = MPP_C_MODULE_DRIVER_TIMER;
            break;
        case MPP_C_MODULE_TIMER:
            tmp_ptr->grain = MPP_C_MODULE_TIMER;
            break;
        case MPP_C_ROUTINE_TIMER:
            tmp_ptr->grain = MPP_C_ROUTINE_TIMER;
            break;
        case MPP_C_LOOP_TIMER:
            tmp_ptr->grain = MPP_C_LOOP_TIMER;
            break;
        case MPP_C_INFRASTRUCTURE_TIMER:
            tmp_ptr->grain = MPP_C_INFRASTRUCTURE_TIMER;
            break;
        default:
            throw_internal_error("MPP_C_TIMER_SET_TIMER_METADATA",
                                 "unsupported timer grain.");
    }

    /*Set the timer's sync at start flag.*/
    if (sync_flag == MPP_C_TRUE)
    {
        tmp_ptr->sync_at_start = MPP_C_TRUE;
    }
    else
    {
        tmp_ptr->sync_at_start = MPP_C_FALSE;
    }

    /*Set the timer's is_detailed flag.  If the timer is detailed, then it
      will log events.*/
    if (detail_flag == MPP_C_TRUE)
    {
        tmp_ptr->is_detailed = MPP_C_TRUE;
        tmp_ptr->max_events = MPP_C_MAX_EVENTS;

        /*Allocate and initialized an array to hold the events.*/
        safemalloc((void **) &(tmp_ptr->event_list),
                   sizeof(mpp_c_event_t)*tmp_ptr->max_events);
        for (i=0;i<tmp_ptr->max_events;i++)
        {
            tmp_event = &(tmp_ptr->event_list[i]);
            mpp_c_event_init(&tmp_event);
        }
        tmp_ptr->cur_num_events = 0;
    }
    else
    {
        tmp_ptr->is_detailed = MPP_C_FALSE;
    }

    /*Set the has metadata flag to true.*/
    tmp_ptr->has_metadata = MPP_C_TRUE;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_event = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that sets the starting time for an mpp_c_timer_t object.*/
void mpp_c_timer_set_start_time(mpp_c_timer_t * const * const self)
{
    /*Local variables*/
    mpp_c_timer_t *tmp_ptr = NULL; /*Pointer to the mpp_c_timer_t object.*/

    /*Set the local pointer to the mpp_c_timer_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_timer_t object has metadata.*/
    mpp_c_timer_check_has_metadata(tmp_ptr,"MPP_C_TIMER_SET_START_TIME");

    /*Make sure that the timer is currently off.*/
    if (mpp_c_timer_is_on(tmp_ptr))
    {
        throw_internal_error("MPP_C_TIMER_SET_START_TIME","timer has already"
                             " been started.");
    }

    /*Set the time at which the timer starts.*/
    tmp_ptr->start_time = MPI_Wtime();

    /*Set the timer to be on.*/
    tmp_ptr->is_on = MPP_C_TRUE;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that sets the ending time and total time for an mpp_c_timer_t
  object.*/
void mpp_c_timer_set_end_time_and_total_time(mpp_c_timer_t * const * const self)
{
    /*Local variables*/
    mpp_c_timer_t *tmp_ptr = NULL; /*Pointer to the mpp_c_timer_t object.*/

    /*Set the local pointer to the mpp_c_timer_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_timer_t object has metadata.*/
    mpp_c_timer_check_has_metadata(tmp_ptr,
                                   "MPP_C_TIMER_SET_END_TIME_AND_TOTAL_TIME");

    /*Make sure that the timer is currently on.*/
    if (!mpp_c_timer_is_on(tmp_ptr))
    {
        throw_internal_error("MPP_C_TIMER_SET_END_TIME_AND_TOTAL_TIME",
                             "timer has not been started or already has been"
                             " ended.");
    }

    /*Set the time at which the timer ends and update the total time that
      the timer has been on.*/
    tmp_ptr->end_time = MPI_Wtime();
    tmp_ptr->total_time = mpp_c_timer_get_total_time(tmp_ptr) + 
                          mpp_c_timer_get_end_time(tmp_ptr) -
                          mpp_c_timer_get_start_time(tmp_ptr);

    /*Set the timer to be off.*/
    tmp_ptr->is_on = MPP_C_FALSE;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that records the start time for an event.*/
void mpp_c_timer_start_new_event(mpp_c_timer_t * const * const self,
                                 char const *name,
                                 size_t const name_len,
                                 int32_t const event_type,
                                 int32_t const timer_id)
{
    /*Local variables*/
    mpp_c_timer_t *tmp_ptr = NULL;   /*Pointer to the mpp_c_timer_t object.*/
    mpp_c_event_t *tmp_event = NULL; /*Pointer to an event.*/
    uint32_t tmp_index = 0;          /*Temporary index variable.*/

    /*Set the local pointer to the mpp_c_timer_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_timer_t object has metadata.*/
    mpp_c_timer_check_has_metadata(tmp_ptr,"MPP_C_TIMER_START_NEW_EVENT");

    /*Make sure that the timer is detailed.*/
    if (mpp_c_timer_is_detailed(tmp_ptr))
    {
        /*Iterate the current number of events.*/
        (tmp_ptr->cur_num_events)++;

        /*Make sure that adding a new event does not exceed the maximum number
          of allowed events.*/
        tmp_index = mpp_c_timer_get_cur_num_events(tmp_ptr) - 1;
        if (tmp_index+1 > mpp_c_timer_get_max_events(tmp_ptr))
        {
            throw_internal_error("MPP_C_TIMER_START_NEW_EVENT","Starting a"
                                 " new event exceeds the maximum number of"
                                 " events allowed.");
        }

        /*Get the starting time for the event.*/
        tmp_event = &(tmp_ptr->event_list[tmp_index]);
        mpp_c_event_set_start_time(&tmp_event,name,name_len,event_type,
                                   timer_id);
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_event = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that records the end time and number of bytes communicated for an
  event.*/
void mpp_c_timer_end_current_event(mpp_c_timer_t * const * const self,
                                   size_t const comm_num_bytes)
{
    /*Local variables*/
    mpp_c_timer_t *tmp_ptr = NULL;   /*Pointer to the mpp_c_timer_t object.*/
    mpp_c_event_t *tmp_event = NULL; /*Pointer to an event.*/
    size_t num_events = 0;           /*Current number of events.*/

    /*Set the local pointer to the mpp_c_timer_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_timer_t object has metadata.*/
    mpp_c_timer_check_has_metadata(tmp_ptr,"MPP_C_TIMER_END_CURRENT_EVENT");

    /*Make sure that the timer is detailed.*/
    if (mpp_c_timer_is_detailed(tmp_ptr))
    {
        /*Get the ending time for the current event and store the number of
          bytes communicated.*/
        num_events = mpp_c_timer_get_cur_num_events(tmp_ptr);
        if (num_events == 0)
        {
            throw_internal_error("MPP_C_TIMER_END_CURRENT_EVENT","No events"
                                 " have been logged yet.");
        }
        else
        {
            tmp_event = &(tmp_ptr->event_list[num_events-1]);
            mpp_c_event_set_end_time_and_num_bytes(&tmp_event,comm_num_bytes);
        }
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_event = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that tells whether or not an mpp_c_timer_t object has been
  initialized.*/
mpp_c_flag_t mpp_c_timer_is_init(mpp_c_timer_t const * const self)
{
    /*Local variables*/
    mpp_c_flag_t init_flag = -1; /*Initialization flag.*/

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
/*Function that throws a FATAL error if the inputted mpp_c_timer_t object
  is not initialized.*/
void mpp_c_timer_check_init_state(mpp_c_timer_t const * const self,
                                  char *routine_name)
{
    if (!mpp_c_timer_is_init(self))
    {
        throw_internal_error(routine_name,"you must first initialize the"
                             " mpp_c_timer_t object.");
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that tells whether or not an mpp_c_timer_t object has metadata.*/
mpp_c_flag_t mpp_c_timer_has_metadata(mpp_c_timer_t const * const self)
{
    /*Make sure that the mpp_c_timer_t object has been initialized.*/
    mpp_c_timer_check_init_state(self,"MPP_C_TIMER_HAS_METADATA");

    return self->has_metadata;
}

/*---------------------------------------------------------------------------*/
/*Function that throws a FATAL error if the inputted mpp_c_timer_t object
  does not have its metadata set.*/
void mpp_c_timer_check_has_metadata(mpp_c_timer_t const * const self,
                                    char *routine_name)
{
    if (!mpp_c_timer_has_metadata(self))
    {
        throw_internal_error(routine_name,"you must first set the timer"
                             " metadata.");
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the name of a timer for an mpp_c_timer_t object.*/
char *mpp_c_timer_get_name(mpp_c_timer_t * const self)
{
    /*Make sure that the mpp_c_timer_t object has metadata.*/
    mpp_c_timer_check_has_metadata(self,"MPP_C_TIMER_GET_NAME");

    return self->name;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the start time for an mpp_c_timer_t object.*/
double mpp_c_timer_get_start_time(mpp_c_timer_t const * const self)
{
    /*Make sure that the mpp_c_timer_t object has metadata.*/
    mpp_c_timer_check_has_metadata(self,"MPP_C_TIMER_GET_START_TIME");

    return self->start_time;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the end time for an mpp_c_timer_t object.*/
double mpp_c_timer_get_end_time(mpp_c_timer_t const * const self)
{
    /*Make sure that the mpp_c_timer_t object has metadata.*/
    mpp_c_timer_check_has_metadata(self,"MPP_C_TIMER_GET_END_TIME");

    return self->end_time;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the total time for an mpp_c_timer_t object.*/
double mpp_c_timer_get_total_time(mpp_c_timer_t const * const self)
{
    /*Make sure that the mpp_c_timer_t object has metadata.*/
    mpp_c_timer_check_has_metadata(self,"MPP_C_TIMER_GET_TOTAL_TIME");

    return self->total_time;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the pelist pointer for an mpp_c_timer_t object.*/
mpp_c_pelist_t* mpp_c_timer_get_pelist(mpp_c_timer_t const * const self)
{
    /*Make sure that the mpp_c_timer_t object has metadata.*/
    mpp_c_timer_check_has_metadata(self,"MPP_C_TIMER_GET_PELIST");

    return self->pelist;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the value of the is_on flag for an mpp_c_timer_t
  object.*/
mpp_c_flag_t mpp_c_timer_is_on(mpp_c_timer_t const * const self)
{
    /*Make sure that the mpp_c_timer_t object has metadata.*/
    mpp_c_timer_check_has_metadata(self,"MPP_C_TIMER_IS_ON");

    return self->is_on;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the grainularity of an mpp_c_timer_t object.*/
int32_t mpp_c_timer_get_grain(mpp_c_timer_t const * const self)
{
    /*Make sure that the mpp_c_timer_t object has metadata.*/
    mpp_c_timer_check_has_metadata(self,"MPP_C_TIMER_GET_GRAIN");

    return self->grain;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the value of the sync_at_start flag for an
  mpp_c_timer_t object.*/
mpp_c_flag_t mpp_c_timer_sync_at_start(mpp_c_timer_t const * const self)
{
    /*Make sure that the mpp_c_timer_t object has metadata.*/
    mpp_c_timer_check_has_metadata(self,"MPP_C_TIMER_SYNC_AT_START");

    return self->sync_at_start;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the value of the is_detailed flag for an
  mpp_c_timer_t object.*/
mpp_c_flag_t mpp_c_timer_is_detailed(mpp_c_timer_t const * const self)
{
    /*Make sure that the mpp_c_timer_t object has metadata.*/
    mpp_c_timer_check_has_metadata(self,"MPP_C_TIMER_IS_DETAILED");

    return self->is_detailed;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the maximum number of allowed events for an
  mpp_c_timer_t object.*/
size_t mpp_c_timer_get_max_events(mpp_c_timer_t const * const self)
{
    /*Make sure that the mpp_c_timer_t object has metadata.*/
    mpp_c_timer_check_has_metadata(self,"MPP_C_TIMER_GET_MAX_EVENTS");

    return self->max_events;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the current number of logged events for an
  mpp_c_timer_t object.*/
size_t mpp_c_timer_get_cur_num_events(mpp_c_timer_t const * const self)
{
    /*Make sure that the mpp_c_timer_t object has metadata.*/
    mpp_c_timer_check_has_metadata(self,"MPP_C_TIMER_GET_CUR_NUM_EVENTS");

    return self->cur_num_events;
}

/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the array of logged events for an
  mpp_c_timer_t object.*/
mpp_c_event_t *mpp_c_timer_get_event_list(mpp_c_timer_t const * const self)
{
    /*Make sure that the mpp_c_timer_t object has metadata.*/
    mpp_c_timer_check_has_metadata(self,"MPP_C_TIMER_GET_EVENT_LIST");

    return self->event_list;
}

/*---------------------------------------------------------------------------*/

