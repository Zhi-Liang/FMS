/** @file */

#ifndef MPP_C_TIMER_T_API_H_
#define MPP_C_TIMER_T_API_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_event_t_api.h"
#include "mpp_c_pelist_t_api.h"
#include "mpp_c_typedefs.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Structure declarations.*/
/**
    This structure defines a "timer".  Timers may be used to time code
    sections, and log communication events.  Event logging only occurs
    if the is_detailed flag is set to MPP_C_TRUE. To use this structure:

    1.) Malloc space.

        > ptr = (mpp_c_timer_t *)malloc(sizeof(mpp_c_timer_t));

    2.) Initialize the object.

        > mpp_c_timer_init(&ptr);

*/
struct mpp_c_timer
{
    mpp_c_flag_t is_initialized;        /**<Initialization flag.*/
    mpp_c_flag_t has_metadata;          /**<Metadata flag.*/
    char name[MPP_C_TIMER_NAME_LENGTH]; /**<Timer name.*/
    double start_time;                  /**<Starting time for a timing.*/
    double end_time;                    /**<Ending time for a timing.*/
    double total_time;                  /**<Total time for all timings.*/
    mpp_c_pelist_t *pelist;             /**<Pointer to the corresponding pelist.*/
    mpp_c_flag_t is_on;                 /**<Is on flag.*/
    int32_t grain;                      /**<Granularity of the timer.*/
    mpp_c_flag_t sync_at_start;         /**<Sync at start flag.*/
    mpp_c_flag_t is_detailed;           /**<Is detailed flag (log events).*/
    size_t max_events;                  /**<Maximum number events allowed to be logged.*/
    size_t cur_num_events;              /**<Current number of logged events.*/
    mpp_c_event_t *event_list;          /**<Array of events.*/
};
typedef struct mpp_c_timer mpp_c_timer_t;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

/*---------------------------------------------------------------------------*/
/**
    Function that initializes an mpp_c_timer_t object.

    \param [in,out] self  An mpp_c_timer_t type object.
*/
void mpp_c_timer_init(mpp_c_timer_t * const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that releases memory and resets default values for an
    mpp_c_timer_t object.

    \param [in,out] self  An mpp_c_timer_t type object.
*/
void mpp_c_timer_reset(mpp_c_timer_t * const * const self);

/*---------------------------------------------------------------------------*/
/**
    Set the name, grain, pelist, and flag values for an mpp_c_timer_t object.

    \param [in,out] self         An mpp_c_timer_t type object.
    \param [in]     name         Name of the timer.
    \param [in]     name_len     Length of the name of the timer.
    \param [in]     grain        Granularity value for the timer.
    \param [in]     sync_flag    Flag denoting whether or not the timer will
                                 be synchronized across MPI ranks when it is
                                 started.
    \param [in]     detail_flag  Flag denoting whether or not the timer is
                                 detailed (event-logging).
    \param [in]     pelist       Pointer to the pelist associated with this
                                 timer.
*/
void mpp_c_timer_set_timer_metadata(mpp_c_timer_t * const * const self,
                                    char const *name,
                                    size_t const name_len,
                                    int32_t const grain,
                                    mpp_c_flag_t const sync_flag,
                                    mpp_c_flag_t const detail_flag,
                                    mpp_c_pelist_t const * const pelist);

/*---------------------------------------------------------------------------*/
/**
    Function that sets the starting time for an mpp_c_timer_t object.

    \param [in,out] self  An mpp_c_timer_t type object.
*/
void mpp_c_timer_set_start_time(mpp_c_timer_t * const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that sets the ending time and total time for an mpp_c_timer_t
    object.

    \param [in,out] self  An mpp_c_timer_t type object.
*/
void mpp_c_timer_set_end_time_and_total_time(mpp_c_timer_t * const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that records the start time for an event.

    \param [in,out] self        An mpp_c_timer_t type object.
    \param [in]     name        Name of the event.
    \param [in]     name_len    Length of the event name.
    \param [in]     event_type  Event type value.
    \param [in]     timer_id    Id of timer associated with the event.
*/
void mpp_c_timer_start_new_event(mpp_c_timer_t * const * const self,
                                 char const *name,
                                 size_t const name_len,
                                 int32_t const event_type,
                                 int32_t const timer_id);

/*---------------------------------------------------------------------------*/
/**
    Function that records the end time and number of bytes communicated for an
    event.

    \param [in,out] self            An mpp_c_timer_t type object.
    \param [in]     comm_num_bytes  Number of bytes communicated in the event.
*/
void mpp_c_timer_end_current_event(mpp_c_timer_t * const * const self,
                                   size_t const comm_num_bytes);

/*---------------------------------------------------------------------------*/
/**
    Function that tells whether or not an mpp_c_timer_t object has been
    initialized.

    \param  [in] self  An mpp_c_timer_t type object.
    \return            A flag denoting whether or not the mpp_c_timer_t
                       object has been initialized.
*/
mpp_c_flag_t mpp_c_timer_is_init(mpp_c_timer_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that throws a FATAL error if the inputted mpp_c_timer_t object
    is not initialized.

    \param [in] self          An mpp_c_timer_t type object.
    \param [in] routine_name  Name of the calling routine.
*/
void mpp_c_timer_check_init_state(mpp_c_timer_t const * const self,
                                  char *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Function that tells whether or not an mpp_c_timer_t object has had its
    metadata set.

    \param [in] self  An mpp_c_timer_t type object.
*/
mpp_c_flag_t mpp_c_timer_has_metadata(mpp_c_timer_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that throws a FATAL error if the inputted mpp_c_timer_t object
    does not have its metadata set.

    \param [in] self          An mpp_c_timer_t type object.
    \param [in] routine_name  Name of the calling routine.
*/
void mpp_c_timer_check_has_metadata(mpp_c_timer_t const * const self,
                                    char *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the name of a timer for an mpp_c_timer_t object.

    \param  [in] self  An mpp_c_timer_t type object.
    \return            The name of the timer.
*/
char *mpp_c_timer_get_name(mpp_c_timer_t * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the start time for an mpp_c_timer_t object.

    \param  [in] self  An mpp_c_timer_t type object.
    \return            The most recent starting time for the timer.
*/
double mpp_c_timer_get_start_time(mpp_c_timer_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the end time for an mpp_c_timer_t object.

    \param  [in] self  An mpp_c_timer_t type object.
    \return            The most recent ending time for the timer.
*/
double mpp_c_timer_get_end_time(mpp_c_timer_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the total time for an mpp_c_timer_t object.

    \param  [in] self  An mpp_c_timer_t type object.
    \return            The current total timing for the timer.
*/
double mpp_c_timer_get_total_time(mpp_c_timer_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the pelist pointer for an mpp_c_timer_t object.

    \param  [in] self  An mpp_c_timer_t type object.
    \return            A pointer to the pelist associated with the timer.
*/
mpp_c_pelist_t* mpp_c_timer_get_pelist(mpp_c_timer_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the value of the is_on flag for an mpp_c_timer_t
    object.

    \param  [in] self  An mpp_c_timer_t type object.
    \return            A flag denoting whether or not the timer is
                       currently on.
*/
mpp_c_flag_t mpp_c_timer_is_on(mpp_c_timer_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the grainularity of an mpp_c_timer_t object.

    \param  [in] self  An mpp_c_timer_t type object.
    \return            The grainularity value for the timer.
*/
int32_t mpp_c_timer_get_grain(mpp_c_timer_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the value of the sync_at_start flag for an
    mpp_c_timer_t object.

    \param  [in] self  An mpp_c_timer_t type object.
    \return            A flag denoting whether or not the timer will be
                       synchronized across ranks when it is started.
*/
mpp_c_flag_t mpp_c_timer_sync_at_start(mpp_c_timer_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the value of the is_detailed flag for an
    mpp_c_timer_t object.

    \param  [in] self  An mpp_c_timer_t type object.
    \return            A flag denoting whether or not the timer supports
                       event-logging.
*/
mpp_c_flag_t mpp_c_timer_is_detailed(mpp_c_timer_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the maximum number of allowed events for an
    mpp_c_timer_t object.

    \param  [in] self  An mpp_c_timer_t type object.
    \return            The maximum number of events that can be logged by
                       the timer.
*/
size_t mpp_c_timer_get_max_events(mpp_c_timer_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the current number of logged events for an
    mpp_c_timer_t object.

    \param  [in] self  An mpp_c_timer_t type object.
    \return            The current number of logged events for the timer.
*/
size_t mpp_c_timer_get_cur_num_events(mpp_c_timer_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns a pointer to the array of logged events for an
    mpp_c_timer_t object.

    \param  [in] self  An mpp_c_timer_t type object.
    \return            A pointer to an array of curently logged events for
                       the timer.
*/
mpp_c_event_t *mpp_c_timer_get_event_list(mpp_c_timer_t const * const self);

/*---------------------------------------------------------------------------*/

#endif
