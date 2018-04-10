/** @file */

#ifndef SET_MPP_C_EVENT_T_API_H_
#define SET_MPP_C_EVENT_T_API_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_macros.h"
#include "mpp_c_typedefs.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Structure declarations.*/
/**
    This structure stores all the necessary data for logging a communication
    event.  To use this structure:

    1.) Malloc space.

        > ptr = (mpp_c_event_t *)malloc(sizeof(mpp_c_event_t));

    2.) Initialize the object.

        > mpp_c_event_init(&ptr);

*/
struct mpp_c_event
{
    mpp_c_flag_t is_initialized;        /**<Initialization flag.*/
    char name[MPP_C_EVENT_NAME_LENGTH]; /**<Name of the event.*/
    int32_t event_type;                 /**<Type of event (i.e. send, receive, ...)*/
    double start_time;                  /**<Starting time for the event.*/
    double end_time;                    /**<Ending time for the event.*/
    size_t num_bytes;                   /**<Number of bytes communicated in the event.*/
    int32_t timer_id;                   /**<Id of timer associated with the event.*/
};
typedef struct mpp_c_event mpp_c_event_t;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

/*---------------------------------------------------------------------------*/
/**
    Function that initializes an mpp_c_event_t object.

    \param [in,out] self  An mpp_c_event_t type object.
*/
void mpp_c_event_init(mpp_c_event_t * const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that resets default values for an mpp_c_event_t object.

    \param [in,out] self  An mpp_c_event_t type object.
*/
void mpp_c_event_reset(mpp_c_event_t * const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that sets the name, event_type, and starting time for an
    mpp_c_event_t object.

    \param [in,out] self        An mpp_c_event_t type object.
    \param [in]     name        Name of the event.
    \param [in]     name_len    Length of the event name.
    \param [in]     event_type  MPP_C event type value.
    \param [in]     timer_id    Timer id associated with the event.
*/
void mpp_c_event_set_start_time(mpp_c_event_t * const * const self,
                                char const * const name,
                                size_t const name_len,
                                int32_t const event_type,
                                int32_t const timer_id);

/*---------------------------------------------------------------------------*/
/**
    Function that sets the ending time and number of bytes communicated for an
    mpp_c_event_t object.

    \param [in,out] self            An mpp_c_event_t type object.
    \param [in]     comm_num_bytes  Number of bytes communicated during the
                                    event.
*/
void mpp_c_event_set_end_time_and_num_bytes(mpp_c_event_t * const * const self,
                                            size_t const comm_num_bytes);

/*---------------------------------------------------------------------------*/
/**
    Function that tells whether or not an mpp_c_event_t object has been
    initialized.

    \param  [in] self  An mpp_c_event_t type object.
    \return            A flag telling whether or not the mpp_c_event_t type
                       object has been initialized.
*/
mpp_c_flag_t mpp_c_event_is_init(mpp_c_event_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that throws a FATAL error if the inputted mpp_c_event_t object
    is not initialized.

    \param [in] self          An mpp_c_event_t type object.
    \param [in] routine_name  Name of calling routine.
*/
void mpp_c_event_check_init_state(mpp_c_event_t const * const self,
                                  char *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the name of a event for an mpp_c_event_t object.

    \param  [in] self  An mpp_c_event_t type object.
    \return            The name of the event.
*/
char *mpp_c_event_get_name(mpp_c_event_t * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the event type for an mpp_c_event_t object.

    \param  [in] self  An mpp_c_event_t type object.
    \return            The MPP_C type value for the event.
*/
int32_t mpp_c_event_get_event_type(mpp_c_event_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the start time for an mpp_c_event_t object.

    \param  [in] self  An mpp_c_event_t type object.
    \return            The starting time for the event.
*/
double mpp_c_event_get_start_time(mpp_c_event_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the end time for an mpp_c_event_t object.

    \param  [in] self  An mpp_c_event_t type object.
    \return            The ending time for the event.
*/
double mpp_c_event_get_end_time(mpp_c_event_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the number of communicated bytes for an
    mpp_c_event_t object.

    \param  [in] self  An mpp_c_event_t type object.
    \return            The total number of bytes communicated during the
                       event.
*/
size_t mpp_c_event_get_num_bytes(mpp_c_event_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the timer id associated with the event for an
    mpp_c_event_t object.

    \param  [in] self  An mpp_c_event_t type object.
    \return            The id of the timer associated with the event.
*/
int32_t mpp_c_event_get_timer_id(mpp_c_event_t const * const self);

/*---------------------------------------------------------------------------*/

#endif
