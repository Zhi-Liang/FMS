/** @file */
/*Header file for mpp_c_context_t methods.*/

#ifndef SET_MPP_C_CONTEXT_T_API_H_
#define SET_MPP_C_CONTEXT_T_API_H_

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "mpp_c_mpi.h"
#include "mpp_c_pelist_t_api.h"
#include "mpp_c_shared_t_api.h"
#include "mpp_c_timer_t_api.h"
#include "mpp_c_typedefs.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Structure declarations.*/
typedef struct mpp_c_context mpp_c_context_t;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

/*---------------------------------------------------------------------------*/
/**
    Initialize an mpp_c_context_t object.

    \param [in,out] self    Context handle which will be allocated and
                            initialized.
    \param [in]     shared  A pointer to an already set mpp_c_shared_t object
                            which contains all data that will be shared by
                            all mpp_c_context_t objects.
*/
void mpp_c_context_init(mpp_c_context_t **self,
                        mpp_c_shared_t const * const shared);

/*---------------------------------------------------------------------------*/
/**
    Free memory and reset an mpp_c_context_t object.

    \param [in,out] self  Context handle which will be deallocated.
*/
void mpp_c_context_reset(mpp_c_context_t **self);

/*---------------------------------------------------------------------------*/
/**
    Function that adds a new send request to an mpp_c_context_t object.

    \param [in,out] self          Context handle.
    \param [in]     comm_request  MPI request handle for a posted MPI_Isend.
*/
void mpp_c_context_add_new_send_request(mpp_c_context_t * const * const self,
                                        MPI_Request const comm_request);

/*---------------------------------------------------------------------------*/
/**
    Function that adds a new receive request to an mpp_c_context_t object.

    \param [in,out] self          Context handle.
    \param [in]     comm_request  MPI request handle for a posted MPI_Irecv.
    \param [in]     request_size  Size of the data sent.
    \param [in]     request_type  MPI type of the data sent.
*/
void mpp_c_context_add_new_recv_request(mpp_c_context_t * const * const self,
                                        MPI_Request const comm_request,
                                        size_t const request_size,
                                        MPI_Datatype const request_type);

/*---------------------------------------------------------------------------*/
/**
    Function that completes all outstanding send requests for an mpp_c_context_t
    mpp_c_context_t object.

    /param [in,out] self  Context handle.
*/
void mpp_c_context_complete_all_send_requests(mpp_c_context_t * const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that completes all outstanding receive requests for an
    mpp_c_context_t object.

    \param [in,out] self  Context handle.
*/
void mpp_c_context_complete_all_recv_requests(mpp_c_context_t * const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that adds a new pelist to the peset array.

    \param [in,out] self             Context handle.
    \param [in]     pelist_name      Name of the pelist.
    \param [in]     pelist_name_len  Length of the pelist name.
    \param [in]     rank_list        Array of MPI ranks from the world pelist.
    \param [in]     rank_list_size   Size of the array of MPI ranks.
*/
void mpp_c_context_add_new_pelist(mpp_c_context_t * const * const self,
                                  char const *pelist_name,
                                  size_t const pelist_name_len,
                                  uint32_t const *rank_list,
                                  size_t const rank_list_size);

/*---------------------------------------------------------------------------*/
/**
    Function that sets the current pelist to point to an existing inputted
    pelist.

    \param [in,out] self            Context handle.
    \param [in]     rank_list       Array of MPI ranks from the world pelist.
    \param [in]     rank_list_size  Size of the array of MPI ranks.
    \param [in]     no_sync         Flag that prevents synchronization of all
                                    ranks on the current pelist.
*/
void mpp_c_context_set_cur_pelist(mpp_c_context_t * const * const self,
                                  uint32_t const *rank_list,
                                  size_t const rank_list_size,
                                  mpp_c_flag_t const no_sync);

/*---------------------------------------------------------------------------*/
/**
    Function that tells whether or not an mpp_c_context_t object has been
    initialized.

    \param  [in] self  Context handle.
    \return            The initialization state for the mpp_c_context_t
                       object.
*/
mpp_c_flag_t mpp_c_context_is_init(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that calls a FATAL error if the inputted mpp_c_context_t object
    is not initialized.

    \param [in] self          Context handle.
    \param [in] routine_name  Name of calling routine.
*/
void mpp_c_context_check_init_state(mpp_c_context_t const * const self,
                                    char *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Function that returns a pointer to the shared context data for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            A pointer to the data shared between all contexts.
*/
mpp_c_shared_t *mpp_c_context_get_shared(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the world rank for an mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            MPI rank id for the process in the world pelist.
*/
uint32_t mpp_c_context_get_world_rank(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**>
    Function that returns maximum number of outstanding MPI send/receive
    requests allowed for an mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            Maximum number of outstanding MPI send or receive
                       requests allowed.
*/
size_t mpp_c_context_get_max_request(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the current index in the send request array for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            Number of outstanding MPI send requests.
*/
size_t mpp_c_context_get_cur_num_send_requests(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the current index in the receive request array for
    an mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            Number of outstandings MPI receive requests.
*/
size_t mpp_c_context_get_cur_num_recv_requests(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the current maximum peset index allowed.

    \param  [in] self  Context handle.
    \return            Maximum number of pelists allowed.
*/
size_t mpp_c_context_get_cur_peset_max(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the total number of current pelists.

    \param  [in] self  Context handle.
    \return            Current number of pelists created.
*/
size_t mpp_c_context_get_cur_num_pelists(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the current pelist pointer.

    \param  [in] self  Context handle.
    \return            Current pelist pointer.
*/
mpp_c_pelist_t *mpp_c_context_get_cur_pelist(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that given a list of ranks in the world pelist, returns a pointer
    to the appropriate pelist.  If a null rank list pointer is passed in, the
    current pelist is returned.  If the inputted rank list does not match an
    existing pelist, then a null pointer is returned.

    \param  [in] self            Context handle.
    \param  [in] rank_list       Array of MPI rank ids from the world pelist.
    \param  [in] rank_list_size  Size of the array of MPI rank ids.
    \param  [in] self_check      Flag denoting whether or not the rank id
                                 of the current process is required to exist
                                 on the inputted rank list.
    \return                      Pointer to the corresponding pelist in the
                                 peset array.
*/
mpp_c_pelist_t *mpp_c_context_get_pelist(mpp_c_context_t const * const self,
                                         uint32_t const *rank_list,
                                         size_t const rank_list_size,
                                         mpp_c_flag_t const self_check);

/*---------------------------------------------------------------------------*/
/**
    Function that returns a pointer to the world pelist.

    \param  [in] self  Context handle.
    \return            Pointer to the world pelist.
*/
mpp_c_pelist_t *mpp_c_context_get_world_pelist(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the name of the world pelist.

    \param  [in] self  Context handle.
    \return            Name of the world pelist.
*/
char *mpp_c_context_get_world_pelist_name(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the length of the name of the world pelist.

    \param [in] self  Context handle.
    \return           Length of the name of the world pelist.
*/
size_t mpp_c_context_get_world_pelist_name_len(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the rank list for the world pelist.

    \param  [in] self  Context handle.
    \return            Pointer to the array containing all MPI rank ids on
                       the world pelist.
*/
uint32_t *mpp_c_context_get_world_pelist_rank_list(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the rank list size for the world pelist.

    \param  [in] self  Context handle.
    \return            Total number of MPI ranks contained on the world
                       pelist.
*/
size_t mpp_c_context_get_world_pelist_rank_list_size(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the root rank id for the world pelist.

    \param  [in] self  Context handle.
    \return            MPI rank id of the "root pe" of the world pelist.
*/
uint32_t mpp_c_context_get_world_pelist_root_rank(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that determines if the local rank is the root rank of the world
    pelist.

    \param  [in] self  Context handle.
    \return            Flag that tells if the current MPI rank is the
                       "root pe" of the world pelist.
*/
mpp_c_flag_t mpp_c_context_is_world_pelist_root_rank(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the communicator id for the world pelist.

    \param  [in] self  Context handle.
    \return            MPI commmunicator id associated with the world pelist.
*/
MPI_Comm mpp_c_context_get_world_pelist_comm_id(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the group id for the world pelist.

    \param  [in] self  Context handle.
    \return            MPI group id associated with the world pelist.
*/
MPI_Group mpp_c_context_get_world_pelist_group_id(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that determines whether an inputted rank exists on the world
    pelist.

    \param  [in] self  Context handle.
    \param  [in] rank  MPI rank id.
    \return            Flag which tells if the inputted MPI rank id exists
                       on the world pelist.
*/
mpp_c_flag_t mpp_c_context_is_rank_on_world_pelist(mpp_c_context_t const * const self,
                                                   uint32_t const rank);

/*---------------------------------------------------------------------------*/
/**
    Function that sets the root rank id for the current pelist.

    \param [in,out] self       Context handle.
    \param [in]     root_rank  MPI rank id that the current pelist's
                               "root pe" will be set to.
*/
void mpp_c_context_set_cur_pelist_root_rank(mpp_c_context_t * const * const self,
                                            uint32_t const root_rank);

/*---------------------------------------------------------------------------*/
/**
    Function that returns a pointer to the current pelist.

    \param  [in] self  Context handle.
    \return            Pointer to the current pelist.
*/
mpp_c_pelist_t *mpp_c_context_get_cur_pelist(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the name of the current pelist.

    \param  [in] self  Context handle.
    \return            Name of the current pelist.
*/
char *mpp_c_context_get_cur_pelist_name(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the length of the name of the current pelist.

    \param  [in] self  Context handle.
    \return            Length of the name of the current pelist.
*/
size_t mpp_c_context_get_cur_pelist_name_len(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the rank list for the current pelist.

    \param  [in] self  Context handle.
    \return            Pointer to the array of MPI rank ids contained on the
                       current pelist.
*/
uint32_t *mpp_c_context_get_cur_pelist_rank_list(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the rank list size for the current pelist.

    \param  [in] self  Context handle.
    \return            Total number of MPI ranks contained on the current
                       pelist.
*/
size_t mpp_c_context_get_cur_pelist_rank_list_size(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the root rank id for the current pelist.

    \param  [in] self  Context handle.
    \return            MPI rank id of the "root pe" of the current pelist.
*/
uint32_t mpp_c_context_get_cur_pelist_root_rank(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that determines if the local rank is the root rank of the current
    pelist.

    \param  [in] self  Context handle.
    \return            Flag telling if the current MPI rank is the
                       "root pe" of the current pelist.
*/
mpp_c_flag_t mpp_c_context_is_cur_pelist_root_rank(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the communicator id for the current pelist.

    \param  [in] self  Context handle.
    \return            MPI communicator id associated with the current pelist.
*/
MPI_Comm mpp_c_context_get_cur_pelist_comm_id(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**Function that returns the group id for the current pelist.

    \param  [in] self  Context handle.
    \return            MPI group id associated with the current pelist.
*/
MPI_Group mpp_c_context_get_cur_pelist_group_id(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that determines whether an inputted rank exists on the curent
    pelist.

    \param  [in] self  Context handle.
    \param  [in] rank  MPI rank id.
    \return            Flag telling if the inputted MPI rank id exists on the
                       current pelist.
*/
mpp_c_flag_t mpp_c_context_is_rank_on_cur_pelist(mpp_c_context_t const * const self,
                                                 uint32_t const rank);

/*---------------------------------------------------------------------------*/
/**
    Function that sets the value of the record_timing_data flag for an
    mpp_c_context_t object.

    \param [in,out] self         Context handle.
    \param [in]     record_flag  Flag telling whether or not to record timing
                                 data.
*/
void mpp_c_context_set_record_timing_data(mpp_c_context_t * const * const self,
                                          mpp_c_flag_t record_flag);

/*---------------------------------------------------------------------------*/
/**
    Function that sets the allowed grainularity for the timers for an
    mpp_c_context_t object.

    \param [in,out] self   Context handle.
    \param [in]     grain  Granularity level for the timers.
*/
void mpp_c_context_set_timer_grain(mpp_c_context_t * const * const self,
                                   int32_t const grain);

/*---------------------------------------------------------------------------*/
/**
    Function that adds a new timer for an mpp_c_context_t object and returns
    an id.

    \param  [in,out] self            Context handle.
    \param  [in]     timer_name      Name of the timer.
    \param  [in]     timer_name_len  Length of the name of the timer.
    \param  [in]     grain           Grainularity value for the timer.
    \param  [in]     sync_flag       Synchronization flag for the timer.
    \param  [in]     detail_flag     Detailed event logging flag for the timer.
    \return                          Id for the timer.
*/
int32_t mpp_c_context_add_new_timer(mpp_c_context_t * const * const self,
                                    char const *timer_name,
                                    size_t const timer_name_len,
                                    int32_t const grain,
                                    mpp_c_flag_t const sync_flag,
                                    mpp_c_flag_t const detail_flag);

/*---------------------------------------------------------------------------*/
/**
    Function that starts a timer for an mpp_c_context_t object.

    \param [in,out] self      Context handle.
    \param [in]     timer_id  Id for a timer.
*/
void mpp_c_context_start_timer(mpp_c_context_t * const * const self,
                               int32_t const timer_id);

/*---------------------------------------------------------------------------*/
/**
    Function that stops a timer for an mpp_c_context_t object.

    \param [in,out] self      Context handle.
    \param [in]     timer_id  Id for a timer.
*/
void mpp_c_context_stop_timer(mpp_c_context_t * const * const self,
                              int32_t const timer_id);

/*---------------------------------------------------------------------------*/
/**
    Function that records the starting time for an event.

    \param [in,out] self            Context handle.
    \param [in,out] timer           Timer that this event is associated with.
    \param [in]     event_name      Name of the event.
    \param [in]     event_name_len  Length of the name of the event.
    \param [in]     event_type      Type parameter for the event.
    \param [in]     timer_id        Id of the timer associated with the event
                                    (wait, start_timer, and stop_timer only).
*/
void mpp_c_context_log_event_start_time(mpp_c_context_t * const * const self,
                                        mpp_c_timer_t * const timer,
                                        char const *event_name,
                                        size_t const event_name_len,
                                        int32_t const event_type,
                                        int32_t const timer_id);

/*---------------------------------------------------------------------------*/
/**
    Function that records the stopping time for an event.

    \param [in,out] self       Context handle.
    \param [in,out] timer      Timer that this event is associated with.
    \param [in]     num_bytes  Total number of bytes communicated in this
                               event.
*/
void mpp_c_context_log_event_stop_time(mpp_c_context_t * const * const self,
                                       mpp_c_timer_t * const timer,
                                       size_t const num_bytes);

/*---------------------------------------------------------------------------*/
/**
    Function that sets the id of the runtime timer for an mpp_c_context_t
    object.

    \param [in,out] self        Context handle.
    \param [in]     runtime_id  Timer id for the "Total runtime" timer.
*/
void mpp_c_context_set_runtime_timer_id(mpp_c_context_t * const * const self,
                                        int32_t const runtime_id);

/*---------------------------------------------------------------------------*/
/**
    Function that gets the value of the record_timing_data flag for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            Flag telling whether timing data is recorded.
*/
mpp_c_flag_t mpp_c_context_record_timing_data(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the grainularity level allowed for timers for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            Maximum grainularity level allowed for all timers.
*/
int32_t mpp_c_context_get_timer_grain(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the value of the sync_all_timers flag for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            Synchronization flag for all timers.
*/
mpp_c_flag_t mpp_c_context_sync_all_timers(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the maximum number of timers allowed for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            Maximum number of timers allowed.
*/
size_t mpp_c_context_get_max_timers(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the current number of timers that exist for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            Current number of created timers.
*/
size_t mpp_c_context_get_cur_num_timers(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns a pointer to the list of timers that exist for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            Array of created timers.
*/
mpp_c_timer_t *mpp_c_context_get_timer_list(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the id of the runtime timer for an mpp_c_context_t
    object.

    \param  [in] self  Context handle.
    \return            Id for the "Total runtime" timer.
*/
int32_t mpp_c_context_get_runtime_timer_id(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Check whether or not the timer already exists in the timer array.

    \param  [in] self            Context handle.
    \param  [in] timer_name      Name of a timer.
    \param  [in] timer_name_len  Length of the name of the timer.
    \return                      Index of the timer in the timer list array.
                                 If no matching timer is found, then the value
                                 MPP_C_NO_MATCH is returned.
*/
uint32_t mpp_c_context_get_timer_list_index(mpp_c_context_t const * const self,
                                            char const *timer_name,
                                            size_t const timer_name_len);

/*---------------------------------------------------------------------------*/
/**
    Function that returns a pointer to the name of the logfile for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            Name of the logfile.
*/
char *mpp_c_context_get_logfile_name(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns a pointer to the logfile file pointer for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            File pointer for the logfile.
*/
FILE *mpp_c_context_get_logfile_ptr(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns a pointer to the name of the etcfile for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            Name of the etcfile.
*/
char *mpp_c_context_get_etcfile_name(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns a pointer to the etcfile file pointer for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            File pointer for the etcfile.
*/
FILE *mpp_c_context_get_etcfile_ptr(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that allocates the namelist buffer to a specific size.

    \param [in,out] self         Context handle.
    \param [in]     buffer_size  Size in bytes that the namelist buffer is
                                 allocated to.
*/
void mpp_c_context_alloc_namelist_buffer(mpp_c_context_t * const * const self,
                                         size_t const buffer_size);

/*---------------------------------------------------------------------------*/
/**
    Function that fills the internal namelist buffer with an inputted string.

    \param [in,out] self         Context handle.
    \param [in]     in_buffer    Inputted string buffer.
    \param [in]     buffer_size  Size of the inputted string buffer.
*/
void mpp_c_context_fill_namelist_buffer(mpp_c_context_t * const * const self,
                                        char const * in_buffer,
                                        size_t const buffer_size);

/*---------------------------------------------------------------------------*/
/**
    Function that frees the namelist buffer.

    \param [in,out] self  Context handle.
*/
void mpp_c_context_free_namelist_buffer(mpp_c_context_t * const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the length of the namelist buffer for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            Length of the namelist buffer.
*/
size_t mpp_c_context_get_namelist_buffer_len(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns a pointer to the namelist buffer for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            Buffer containing the read-in namelist data.
*/
char *mpp_c_context_get_namelist_buffer(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that sets control of the work buffer and reallocates its
    size if necessary.

    \param  [in,out] self            Context handle.
    \param  [in]     requested_size  Size of buffer.
    \return                          A pointer to the work buffer.
*/
void *mpp_c_context_use_work_buffer(mpp_c_context_t * const * const self,
                                   size_t const requested_size);

/*---------------------------------------------------------------------------*/
/**
    Function that releases control of the work buffer.  This flips the
    work_buffer_in_use flag to false.

    \param [in,out] self        Context handle.
    \param [in,out] buffer_ptr  Pointer to the work buffer.
*/
void mpp_c_context_release_work_buffer(mpp_c_context_t * const * const self,
                                       void *buffer_ptr);

/*---------------------------------------------------------------------------*/
/**
    Function that tells whether or not the work buffer is in use for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            Flag telling whether or not the work buffer is currently
                       being used.
*/
mpp_c_flag_t mpp_c_context_is_work_buffer_in_use(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the size in bytes of the work buffer for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            Size in bytes of the work buffer.
*/
size_t mpp_c_context_get_work_buffer_size(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns a pointer to the work buffer for an mpp_c_context_t
    object.

    \param  [in] self  Context handle.
    \return            A pointer to the work buffer.
*/
void *mpp_c_context_get_work_buffer(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns the size in bytes of the input_nml buffer for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            The size in bytes of the input_nml buffer.
*/
size_t mpp_c_context_get_input_nml_buffer_size(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/
/**
    Function that returns a pointer to the input_nml buffer for an
    mpp_c_context_t object.

    \param  [in] self  Context handle.
    \return            A pointer to the input_nml buffer.
*/
char *mpp_c_context_get_input_nml_buffer(mpp_c_context_t const * const self);

/*---------------------------------------------------------------------------*/

#endif
