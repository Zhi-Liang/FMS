#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "helper_functions.h"
#include "mpp_c_context_t_api.h"
#include "mpp_c_event_t_api.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"
#include "mpp_c_parse_nml.h"
#include "mpp_c_pelist_t_api.h"
#include "mpp_c_shared_t_api.h"
#include "mpp_c_timer_ll_t_api.h"
#include "mpp_c_timer_t_api.h"
#include "mpp_c_typedefs.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Structure definition.*/
/**
    This structure defines a "context" for the mpp_c library.  For safety,
    this structure is made opaque.  A pointer ("handle") to this structure
    is a required input for all public facing routines. To use this structure:

    1.) Nullify the pointer.

        > ptr = NULL;

    2.) Initialize it.

        > mpp_c_context_init(&ptr,shared);

        where "shared" is a previously set mpp_c_shared_t object.
*/
struct mpp_c_context
{
    mpp_c_shared_t *shared;           /**<Pointer to data shared between all contexts.*/
    uint32_t world_rank;              /**<MPI rank id in the context's world pelist.*/
    size_t cur_peset_max;             /**<Current maximum size for the peset array.*/
    size_t cur_num_pelists;           /**<Current number of existing pelists in the peset array.*/
    mpp_c_pelist_t *peset;            /**<Array of mpp_pelist_type structures.*/
    mpp_c_pelist_t *cur_pelist;       /**<Pointer to the current pelist.*/
    int32_t timer_grain;              /**<Grainularity level allowed for timers.*/
    size_t max_timers;                /**<Maximum number of timers allowed.*/
    size_t cur_num_timers;            /**<Current number of existing timers.*/
    mpp_c_flag_t record_timing_data;  /**<Flag that denotes whether timings will be recorded.*/
    mpp_c_timer_t *timer_list;        /**<Array of timers.*/
    mpp_c_timer_ll_t *cur_timer_node; /**<Linked list node for the current timer.*/
    int32_t runtime_timer_id;         /**<Id for the total runtime timer.*/
    size_t max_request;               /**<Maximum number of outstanding send/receive requests allowed.*/
    size_t cur_num_send_requests;     /**<Current number of outstanding send requests.*/
    size_t cur_num_recv_requests;     /**<Current number of outstanding receive requests.*/
    MPI_Request *send_request;        /**<Array of outstanding MPI send request handles.*/
    MPI_Request *recv_request;        /**<Array of outstanding MPI receive request handles.*/
    size_t *recv_request_size;        /**<Array of sizes for outstanding MPI receive requests.*/
    MPI_Datatype *recv_request_type;  /**<Array of MPI type parameters for outstanding MPI receive requests.*/
    char *logfile_name;               /**<Name of the logfile.*/
    FILE *logfile_ptr;                /**<Logfile file pointer.*/
    char *etcfile_name;               /**<Name of the etcfile.*/
    FILE *etcfile_ptr;                /**<Etcfile file pointer.*/
    char *namelist_buffer;            /**<Buffer that holds the contents of a .nml file*/
    size_t namelist_buffer_len;       /**<Length of the namelist buffer.*/
    mpp_c_flag_t work_buffer_in_use;  /**<Flag telling if the work buffer is currently in use.*/
    size_t work_buffer_size;          /**<Size of the work buffer in bytes.*/
    void *work_buffer;                /**<Work buffer used to store intermediate results.*/
};

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Initialize an mpp_c_context_t object.*/
void mpp_c_context_init(mpp_c_context_t **self,
                        mpp_c_shared_t const * const shared)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL;      /*Pointer to the mpp_c_context_t object.*/
    mpp_c_pelist_t *tmp_pelist = NULL;    /*Pointer to a pelist.*/
    mpp_c_timer_t *tmp_timer = NULL;      /*Pointer to a timer.*/
    int my_rank = 0;                      /*MPI rank id.*/
    MPI_Comm tmp_comm_id = MPI_COMM_NULL; /*MPI communicator id.*/
    size_t world_num_ranks = 0;           /*Number of MPI ranks.*/
    size_t tmp_max_request = 0;           /*Maximum requests prescribed by the namelist.*/
    int ierr = 0;                         /*MPI error code.*/
    unsigned int i = 0;                   /*Loop variable.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure the inputted mpp_c_context_t pointer is null.*/
    if (tmp_ptr != NULL)
    {
        throw_internal_error("MPP_C_CONTEXT_INIT","inputted mpp_c_context_t"
                             " pointer is not null.  First nullify it.");
    }

    /*Make sure that the inputted mpp_c_shared_t object has been set.*/
    mpp_c_shared_check_is_set(shared,"MPP_C_CONTEXT_INIT");

    /*Allocate space for the mpp_c_context_t object.*/
    safemalloc((void **) &tmp_ptr,sizeof(mpp_c_context_t));

    /*Point to the shared context data.*/
    tmp_ptr->shared = shared;

    /*Set the context's MPI rank in the world pelist.*/
    tmp_pelist = mpp_c_shared_get_world_pelist(shared);
    tmp_comm_id = mpp_c_pelist_get_comm_id(tmp_pelist);
    ierr = MPI_Comm_rank(tmp_comm_id,&my_rank);
    check_MPI_error_code(ierr,"MPI_Comm_rank","MPP_C_CONTEXT_INIT");
    tmp_ptr->world_rank = (uint32_t)my_rank;
    tmp_pelist = NULL;

    /*Set the maximum number of pelists allowed in the peset array, and
      allocate and initialize the array of mpp_c_pelist_t type objects.
      Set the current pelist to be the world pelist.*/
    tmp_ptr->peset = NULL;
    tmp_ptr->cur_peset_max = MPP_C_MAX_NUM_PELISTS;
    safemalloc((void **) &(tmp_ptr->peset),
               sizeof(mpp_c_pelist_t)*(tmp_ptr->cur_peset_max));
    for (i=0;i<(tmp_ptr->cur_peset_max);i++)
    {
        tmp_pelist = &(tmp_ptr->peset[i]);
        mpp_c_pelist_init(&tmp_pelist);
        tmp_pelist = NULL;
    }
    tmp_ptr->cur_num_pelists = 0;
    tmp_ptr->cur_pelist = mpp_c_shared_get_world_pelist(tmp_ptr->shared);

    /*Allocate and initialize an array of timers.*/
    tmp_ptr->timer_list = NULL;
    tmp_ptr->max_timers = MPP_C_MAX_NUM_TIMERS;
    safemalloc((void **) &(tmp_ptr->timer_list),
               sizeof(mpp_c_timer_t)*(tmp_ptr->max_timers));
    for (i=0;i<(tmp_ptr->max_timers);i++)
    {
        tmp_timer = &(tmp_ptr->timer_list[i]);
        mpp_c_timer_init(&tmp_timer);
        tmp_timer = NULL;
    }
    tmp_ptr->timer_grain = MPP_C_ROUTINE_TIMER;
    tmp_ptr->cur_num_timers = 0;
    mpp_c_timer_ll_init(&(tmp_ptr->cur_timer_node));
    tmp_ptr->runtime_timer_id = 0;
    if (mpp_c_shared_get_record_timing_data(tmp_ptr->shared))
    {
        tmp_ptr->record_timing_data = MPP_C_TRUE;
    }
    else
    {
        tmp_ptr->record_timing_data = MPP_C_FALSE;
    }

    /*Allocate and initialize the internal communication request arrays.*/
    tmp_ptr->send_request = NULL;
    tmp_ptr->recv_request = NULL;
    tmp_ptr->recv_request_size = NULL;
    tmp_ptr->recv_request_type = NULL;
    world_num_ranks = mpp_c_pelist_get_rank_list_size(tmp_ptr->cur_pelist);
    tmp_max_request = world_num_ranks*
                      mpp_c_shared_get_request_multiply(tmp_ptr->shared);
    if (tmp_max_request < MPP_C_MAX_REQUESTS)
    {
        tmp_max_request = (size_t)MPP_C_MAX_REQUESTS;
    }
    tmp_ptr->max_request = tmp_max_request;
    safemalloc((void **) &(tmp_ptr->send_request),
               sizeof(MPI_Request)*(tmp_ptr->max_request));
    safemalloc((void **) &(tmp_ptr->recv_request),
               sizeof(MPI_Request)*(tmp_ptr->max_request));
    safemalloc((void **) &(tmp_ptr->recv_request_size),
               sizeof(size_t)*(tmp_ptr->max_request));
    safemalloc((void **) &(tmp_ptr->recv_request_type),
               sizeof(MPI_Datatype)*(tmp_ptr->max_request));
    for (i=0;i<(tmp_ptr->max_request);i++)
    {
        tmp_ptr->send_request[i] = MPI_REQUEST_NULL;
        tmp_ptr->recv_request[i] = MPI_REQUEST_NULL;
        tmp_ptr->recv_request_size[i] = MPP_C_NULL_MESG_SIZE;
        tmp_ptr->recv_request_type[i] = MPP_C_NULL_MESG_TYPE;
    }
    tmp_ptr->cur_num_send_requests = 0;
    tmp_ptr->cur_num_recv_requests = 0;

    /*Set the name for the logfile and the etcfile.*/
    tmp_ptr->logfile_name = NULL;
    safemalloc((void **) &(tmp_ptr->logfile_name),
               sizeof(char)*MPP_C_LOGFILE_NAME_LENGTH);
    snprintf(tmp_ptr->logfile_name,MPP_C_LOGFILE_NAME_LENGTH,
             "logfile.%06u.out",tmp_ptr->world_rank);
    tmp_ptr->logfile_ptr = NULL;
    tmp_ptr->etcfile_name = NULL;
    if (mpp_c_shared_get_etc_unit_is_stderr(tmp_ptr->shared))
    {
        tmp_ptr->etcfile_ptr = stderr;
    }
    else
    {
        safemalloc((void **) &(tmp_ptr->etcfile_name),
                   sizeof(char)*MPP_C_ETCFILE_NAME_LENGTH);
        snprintf(tmp_ptr->etcfile_name,MPP_C_ETCFILE_NAME_LENGTH,
                 "._mpp_c.nonrootpe.msgs.%06u",tmp_ptr->world_rank);
        tmp_ptr->etcfile_ptr = NULL;
    }

    /*Initialize the namelist buffer.*/
    tmp_ptr->namelist_buffer = NULL;
    tmp_ptr->namelist_buffer_len = 0;

    /*Initialize the work buffer.*/
    tmp_ptr->work_buffer = NULL;
    tmp_ptr->work_buffer_in_use = MPP_C_FALSE;
    tmp_ptr->work_buffer_size = MPP_C_WORK_BUFFER_INIT_SIZE;
    safemalloc((void **) &(tmp_ptr->work_buffer),tmp_ptr->work_buffer_size);

    /*Point self to the newly allocated mpp_c_context_t object.*/
    *self = tmp_ptr;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_pelist = NULL;
    tmp_timer = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Release memory and reset default values for an mpp_c_context_t object.*/
void mpp_c_context_reset(mpp_c_context_t **self)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL;   /*Pointer to the mpp_c_context_t object.*/
    mpp_c_pelist_t *tmp_pelist = NULL; /*Pointer to a pelist.*/
    mpp_c_timer_t *tmp_timer = NULL;   /*Pointer to a timer.*/
    char *tmp_char_ptr = NULL;         /*Reusable character pointer.*/
    FILE *tmp_file_ptr = NULL;         /*Reusable file pointer.*/
    size_t num_pelists = 0;            /*Total number of pelists.*/
    size_t num_timers = 0;             /*Total number of timers.*/
    struct stat status;                /*Status structure for a file.*/
    int32_t ioerr = 0;                 /*I/O error code.*/
    unsigned int i = 0;                /*Loop variable.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_CONTEXT_RESET");

    /*Free the work buffer and reset default values.*/
    if (mpp_c_context_is_work_buffer_in_use(tmp_ptr))
    {
        throw_internal_error("MPP_C_CONTEXT_RESET",
                             "the work buffer is still in use.");
    }
    else
    {
        if (tmp_ptr->work_buffer != NULL)
        {
            safefree((void **) &(tmp_ptr->work_buffer));
        }
    }

    /*Free the namelist_buffer and reset default values.*/
    if (tmp_ptr->namelist_buffer != NULL)
    {
        safefree((void **) &(tmp_ptr->namelist_buffer));
    }

    /*Close the etcfile and free its name.*/
    tmp_file_ptr = mpp_c_context_get_etcfile_ptr(tmp_ptr);
    if (tmp_file_ptr != stderr && tmp_file_ptr != NULL)
    {
        ioerr = fclose(tmp_file_ptr);
        if (ioerr != 0)
        {
            throw_internal_error("MPP_C_CONTEXT_RESET",
                                 "the etc file did not close properly.");
        }
    }
    tmp_file_ptr = NULL;
    tmp_ptr->etcfile_ptr = NULL;
    if (tmp_ptr->etcfile_name != NULL)
    {
        safefree((void **) &(tmp_ptr->etcfile_name));
    }

    /*Close the logfile, delete it if it is empty, and free its name.*/
    tmp_char_ptr = mpp_c_context_get_logfile_name(tmp_ptr);
    tmp_file_ptr = mpp_c_context_get_logfile_ptr(tmp_ptr);
    if (tmp_file_ptr != NULL)
    {
        ioerr = fclose(tmp_file_ptr);
        if (ioerr != 0)
        {
            throw_internal_error("MPP_C_CONTEXT_RESET",
                                 "the logfile did not close properly.");
        }
        tmp_ptr->logfile_ptr = NULL;
    }
    tmp_file_ptr = NULL;
    stat(tmp_char_ptr,&status);
    if (status.st_size == 0)
    {
        remove(tmp_char_ptr);
    }
    safefree((void **) &(tmp_ptr->logfile_name));
    tmp_char_ptr = NULL;

    /*Free request arrays and reset default values.*/
    if (mpp_c_context_get_cur_num_send_requests(tmp_ptr) != 0)
    {
        throw_internal_error("MPP_C_CONTEXT_RESET",
                             "there are still outstanding send requests.");
    }
    else
    {
        safefree((void **) &(tmp_ptr->send_request));
    }
    if (mpp_c_context_get_cur_num_recv_requests(tmp_ptr) != 0)
    {
        throw_internal_error("MPP_C_CONTEXT_RESET",
                             "there are still outstanding receive requests.");
    }
    else
    {
        safefree((void **) &(tmp_ptr->recv_request));
        safefree((void **) &(tmp_ptr->recv_request_size));
        safefree((void **) &(tmp_ptr->recv_request_type));
    }

    /*Free all timers and reset default values. To match legacy behavior,
      the first timer is stored at tmp_ptr->timer_list[1], with the first
      spot in the array remaining empty.*/
    mpp_c_timer_ll_reset(&(tmp_ptr->cur_timer_node));
    num_timers = mpp_c_context_get_cur_num_timers(tmp_ptr);
    for (i=1;i<num_timers+1;i++)
    {
        tmp_timer = &(tmp_ptr->timer_list[i]);
        mpp_c_timer_reset(&tmp_timer);
        tmp_timer = NULL;
    }
    safefree((void **) &(tmp_ptr->timer_list));

    /*Free all pelists and the peset array.*/
    tmp_ptr->cur_pelist = NULL;
    num_pelists = mpp_c_context_get_cur_num_pelists(tmp_ptr);
    for (i=0;i<num_pelists;i++)
    {
        tmp_pelist = &(tmp_ptr->peset[i]);
        mpp_c_pelist_reset(&tmp_pelist);
        tmp_pelist = NULL;
    }
    safefree((void **) &(tmp_ptr->peset));

    /*Reset the mpp_c_shared_t pointer.*/
    tmp_ptr->shared = NULL;

    /*Free the mpp_c_context_t object.*/
    safefree((void **) &(tmp_ptr));

    /*Point self to the deallocated mpp_c_context_t object to nullify it.*/
    *self = tmp_ptr;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_pelist = NULL;
    tmp_timer = NULL;
    tmp_char_ptr = NULL;
    tmp_file_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that adds a new send request to an mpp_c_context_t object.*/
void mpp_c_context_add_new_send_request(mpp_c_context_t * const * const self,
                                        MPI_Request const comm_request)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/
    uint32_t tmp_index = 0;          /*Temporary index variable.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,
                                   "MPP_C_CONTEXT_ADD_NEW_SEND_REQUEST");

    /*Make sure that a valid MPI request was inputted.*/
    check_comm_request_input(comm_request,
                             "MPP_C_CONTEXT_ADD_NEW_SEND_REQUEST");

    /*Iterate the current number of send requests.*/
    (tmp_ptr->cur_num_send_requests)++;

    /*Make sure that adding a new request does not exceed the maximum number
      of allowed requests.*/
    tmp_index = mpp_c_context_get_cur_num_send_requests(tmp_ptr) - 1;
    if (tmp_index+1 > mpp_c_context_get_max_request(tmp_ptr))
    {
        throw_internal_error("MPP_C_CONTEXT_ADD_NEW_SEND_REQUEST",
                             "adding a new send request exceeds the"
                             " maximum number of outstanding send requests"
                             " allowed.");
    }

    /*Store the request in the appropriate spot in the send request array.*/
    tmp_ptr->send_request[tmp_index] = comm_request;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that adds a new receive request to an mpp_c_context_t object.*/
void mpp_c_context_add_new_recv_request(mpp_c_context_t * const * const self,
                                        MPI_Request const comm_request,
                                        size_t const request_size,
                                        MPI_Datatype const request_type)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/
    uint32_t tmp_index = 0;          /*Temporary index variable.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,
                                   "MPP_C_CONTEXT_ADD_NEW_RECV_REQUEST");

    /*Make sure that a valid MPI request was inputted.*/
    check_comm_request_input(comm_request,
                             "MPP_C_CONTEXT_ADD_NEW_RECV_REQUEST");

    /*Make sure that a valid request size was inputted.*/
    check_size_input(request_size,"MPP_C_CONTEXT_ADD_NEW_RECV_REQUEST");

    /*Make sure that a valid MPI datatype was inputted.*/
    check_comm_datatype_input(request_type,
                              "MPP_C_CONTEXT_ADD_NEW_RECV_REQUEST");

    /*Iterate the current number of receive requests.*/
    (tmp_ptr->cur_num_recv_requests)++;

    /*Make sure that adding a new request does not exceed the maximum number
      of allowed requests.*/
    tmp_index = mpp_c_context_get_cur_num_recv_requests(tmp_ptr) - 1;
    if (tmp_index+1 > mpp_c_context_get_max_request(tmp_ptr))
    {
        throw_internal_error("MPP_C_CONTEXT_ADD_NEW_RECV_REQUEST",
                             "adding a new receive request exceeds the"
                             " maximum number of outstanding receive"
                             " requests allowed.");
    }

    /*Store the request in the appropriate spot in the receive request
      arrays.*/
    tmp_ptr->recv_request[tmp_index] = comm_request;
    tmp_ptr->recv_request_size[tmp_index] = request_size;
    tmp_ptr->recv_request_type[tmp_index] = request_type;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that completes all outstanding send requests for an mpp_c_context_t
  mpp_c_context_t object.*/
void mpp_c_context_complete_all_send_requests(mpp_c_context_t * const * const self)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/
    size_t num_requests = 0;         /*Number of outstanding requests.*/
    MPI_Status comm_stat;            /*MPI status for completed sends.*/
    int ierr = 0;                    /*MPI error code.*/
    char *event_name = NULL;         /*Event name.*/
    unsigned int i = 0;              /*Loop variable.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,
                                   "MPP_C_CONTEXT_COMPLETE_ALL_SEND_REQUESTS");

    /*Call MPI_Wait on all outstanding MPI send requests.*/
    num_requests = mpp_c_context_get_cur_num_send_requests(tmp_ptr);
    if (num_requests == 0)
    {
/*
        print_internal_message("MPP_C_CONTEXT_COMPLETE_ALL_SEND_REQUESTS",
                               "there are no outstanding send requests");
*/
    }
    else
    {
        /*Log the communication event starting time.*/
        event_name = "Sync_sends";
        mpp_c_context_log_event_start_time(&tmp_ptr,NULL,event_name,
                                           strlen(event_name),EVENT_WAIT,
                                           MPP_C_NULL_TIMER_ID);

        /*Wait on all outstanding requests.*/
        for (i=0;i<num_requests;i++)
        {
            ierr = MPI_Wait(&(tmp_ptr->send_request[i]),&comm_stat);
            check_MPI_error_code(ierr,"MPI_Wait",
                                 "MPP_C_CONTEXT_COMPLETE_ALL_SEND_REQUESTS");
            tmp_ptr->send_request[i] = MPI_REQUEST_NULL;
        }
        tmp_ptr->cur_num_send_requests = 0;

        /*Log the communication event stopping time.*/
        mpp_c_context_log_event_stop_time(&tmp_ptr,NULL,0);
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    event_name = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/* Function that completes all outstanding receive requests for an
    mpp_c_context_t object.*/
void mpp_c_context_complete_all_recv_requests(mpp_c_context_t * const * const self)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/
    size_t num_requests = 0;         /*Number of outstanding requests.*/
    MPI_Status comm_stat;            /*MPI status for completed receives.*/
    int recv_size = 0;               /*Size of received data.*/
    char *event_name = NULL;         /*Event name.*/
    int ierr = 0;                    /*MPI error code.*/
    unsigned int i = 0;              /*Loop variable.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,
                                   "MPP_C_CONTEXT_COMPLETE_ALL_RECV_REQUESTS");

    /*Call MPI_Wait on all outstanding MPI receive requests.*/
    num_requests = mpp_c_context_get_cur_num_recv_requests(tmp_ptr);
    if (num_requests == 0)
    {
/*
        print_internal_message("MPP_C_CONTEXT_COMPLETE_ALL_SEND_REQUESTS",
                               "there are no outstanding receive requests");
*/
    }
    else
    {
        /*Log the communication event starting time.*/
        event_name = "Sync_recvs";
        mpp_c_context_log_event_start_time(&tmp_ptr,NULL,event_name,
                                           strlen(event_name),EVENT_WAIT,
                                           MPP_C_NULL_TIMER_ID);

        for (i=0;i<num_requests;i++)
        {
            ierr = MPI_Wait(&(tmp_ptr->recv_request[i]),&comm_stat);
            check_MPI_error_code(ierr,"MPI_Wait",
                                 "MPP_C_CONTEXT_COMPLETE_ALL_RECV_REQUESTS");
            ierr = MPI_Get_count(&comm_stat,tmp_ptr->recv_request_type[i],
                                 &recv_size);
            check_MPI_error_code(ierr,"MPI_Get_count",
                                 "MPP_C_CONTEXT_COMPLETE_ALL_RECV_REQUESTS");
            if ((size_t)recv_size != tmp_ptr->recv_request_size[i])
            {
                throw_internal_error("MPP_C_CONTEXT_COMPLETE_ALL_RECV_REQUESTS",
                                     " received wrong sized data.");
            }
            tmp_ptr->recv_request[i] = MPI_REQUEST_NULL;
            tmp_ptr->recv_request_size[i] = MPP_C_NULL_MESG_SIZE;
            tmp_ptr->recv_request_type[i] = MPP_C_NULL_MESG_TYPE;
        }
        tmp_ptr->cur_num_recv_requests = 0;

        /*Log the communication event stopping time.*/
        mpp_c_context_log_event_stop_time(&tmp_ptr,NULL,0);
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    event_name = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that adds a new pelist to the peset array.*/
void mpp_c_context_add_new_pelist(mpp_c_context_t * const * const self,
                                  char const *pelist_name,
                                  size_t const pelist_name_len,
                                  uint32_t const *rank_list,
                                  size_t const rank_list_size)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL;   /*Pointer to the mpp_c_context_t object.*/
    mpp_c_pelist_t *tmp_pelist = NULL; /*Pointer to a pelist.*/
    uint32_t tmp_index = 0;            /*Temporary index variable.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_CONTEXT_ADD_NEW_PELIST");

    /*Check string input.*/
    check_string_input(pelist_name,pelist_name_len,
                       "MPP_C_CONTEXT_ADD_NEW_PELIST");

    /*Check array input.*/
    check_array_input((void *)rank_list,rank_list_size,
                      "MPP_C_CONTEXT_ADD_NEW_PELIST");

    /*Check whether or not the pelist already exists in the peset array.*/
    tmp_pelist = mpp_c_context_get_pelist(tmp_ptr,rank_list,
                                          rank_list_size,MPP_C_FALSE);
    if (tmp_pelist != NULL)
    {
/*
        print_internal_message("MPP_C_CONTEXT_ADD_NEW_PELIST",
                               "the pelist already exists in the peset"
                               " array.");
*/
        tmp_pelist = NULL;
        tmp_ptr = NULL;
        return;
    }

    /*Iterate the total number of current pelists.*/
    (tmp_ptr->cur_num_pelists)++;
    tmp_index = mpp_c_context_get_cur_num_pelists(tmp_ptr) - 1;

    /*Make sure that the total number of current pelists does not exceed the
      maximum number of allowed pelists.*/
    if (tmp_index > mpp_c_context_get_cur_peset_max(tmp_ptr)-1)
    {
        throw_internal_error("MPP_C_CONTEXT_ADD_NEW_PELIST",
                             "the number of pelists exceeds the maximum"
                             " number allowed.");
    }

    /*Construct the new pelist.*/
    tmp_pelist = &(tmp_ptr->peset[tmp_index]);
    mpp_c_pelist_set_pelist(&tmp_pelist,pelist_name,pelist_name_len,
                            rank_list,rank_list_size,
                            mpp_c_context_get_world_pelist_comm_id(tmp_ptr));

    /*Nullify the local pointers.*/
    tmp_ptr = NULL;
    tmp_pelist = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that sets the current pelist to match point to an existing pelist.*/
void mpp_c_context_set_cur_pelist(mpp_c_context_t * const * const self,
                                  uint32_t const *rank_list,
                                  size_t const rank_list_size,
                                  mpp_c_flag_t const no_sync)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL;   /*Pointer to the mpp_c_context_t object.*/
    mpp_c_pelist_t *tmp_pelist = NULL; /*Pointer to a pelist.*/
    uint32_t *tmp_rank_list = NULL;    /*Pointer to a rank list.*/
    char *event_name = NULL;           /*Event name.*/
    int ierr = 0;                      /*MPI error code.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,
                                   "MPP_C_CONTEXT_SET_CUR_PELIST");

    /*Check pelist input.*/
    check_pelist_input(rank_list,rank_list_size,
                       "MPP_C_CONTEXT_SET_CUR_PELIST");

    /*Check flag input.*/
    check_flag_input(no_sync,"MPP_C_CONTEXT_SET_CUR_PELIST");

    /*If a null pelist is inputted, then set the current pelist to point to
      the world pelist.*/
    if (rank_list == NULL)
    {
        tmp_ptr->cur_pelist = mpp_c_context_get_world_pelist(tmp_ptr);
    }
    else
    {
        /*Point to the appropriate existing pelist.*/
        tmp_pelist = mpp_c_context_get_pelist(tmp_ptr,rank_list,
                                              rank_list_size,MPP_C_TRUE);
        if (tmp_pelist == NULL)
        {
            throw_internal_error("MPP_C_CONTEXT_SET_CUR_PELIST",
                                 "the inputted pelist does not exist.  You"
                                 " must first create it.");
        }
        tmp_ptr->cur_pelist = tmp_pelist;
    }

    /*Set the root of the pelist to be the first rank id on the rank list.*/
    tmp_rank_list = mpp_c_context_get_cur_pelist_rank_list(tmp_ptr);
    mpp_c_context_set_cur_pelist_root_rank(&tmp_ptr,tmp_rank_list[0]);

    /*Synchronize if appropriate.*/
    if (no_sync != MPP_C_TRUE)
    {
        event_name = "Sync_pelist";
        mpp_c_context_log_event_start_time(&tmp_ptr,NULL,event_name,
                                           strlen(event_name),EVENT_WAIT,
                                           MPP_C_NULL_TIMER_ID);
        ierr = MPI_Barrier(mpp_c_context_get_cur_pelist_comm_id(tmp_ptr));
        check_MPI_error_code(ierr,"MPI_Barrier",
                             "MPP_C_CONTEXT_SET_CUR_PELIST");
        mpp_c_context_log_event_stop_time(&tmp_ptr,NULL,0);
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_pelist = NULL;
    tmp_rank_list = NULL;
    event_name = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that tells whether or not an mpp_c_context_t object has been
  initialized.*/
mpp_c_flag_t mpp_c_context_is_init(mpp_c_context_t const * const self)
{
    /*Local variables*/
    mpp_c_flag_t init_flag = 0; /*Initialization flag the mpp_c_context_t object.*/

    if (self == NULL)
    {
        init_flag = MPP_C_FALSE;
    }
    else
    {
        init_flag = MPP_C_TRUE;
    }

    return init_flag;
}

/*---------------------------------------------------------------------------*/
/*Function that throws a FATAL error if the inputted mpp_c_context_t object
  is not initialized.*/
void mpp_c_context_check_init_state(mpp_c_context_t const * const self,
                                    char *routine_name)
{
    if (!mpp_c_context_is_init(self))
    {
        throw_internal_error(routine_name,"you must first initialize the"
                             "mpp_c_context_t object.");
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the shared context data for an
  mpp_c_context_t object.*/
mpp_c_shared_t *mpp_c_context_get_shared(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_SHARED");

    return self->shared;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the world rank for an mpp_c_context_t object.*/
uint32_t mpp_c_context_get_world_rank(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_WORLD_RANK");

    return self->world_rank;
}

/*---------------------------------------------------------------------------*/
/*Function that returns maximum number of outstanding MPI send/receive
  requests allowed for an mpp_c_context_t object.*/
size_t mpp_c_context_get_max_request(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_MAX_REQUEST");

    return self->max_request;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the current index in the send request array for an
  mpp_c_context_t object.*/
size_t mpp_c_context_get_cur_num_send_requests(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,
                                   "MPP_C_CONTEXT_GET_CUR_NUM_SEND_REQUESTS");

    return self->cur_num_send_requests;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the current index in the receive request array for
  an mpp_c_context_t object.*/
size_t mpp_c_context_get_cur_num_recv_requests(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,
                                   "MPP_C_CONTEXT_GET_CUR_NUM_RECV_REQUESTS");

    return self->cur_num_recv_requests;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the current maximum peset index allowed.*/
size_t mpp_c_context_get_cur_peset_max(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_CUR_PESET_MAX");

    return self->cur_peset_max;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the total number of current pelists.*/
size_t mpp_c_context_get_cur_num_pelists(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_CUR_NUM_PELISTS");

    return self->cur_num_pelists;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the current pelist pointer.*/
mpp_c_pelist_t *mpp_c_context_get_cur_pelist(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_CUR_PELIST");

    return self->cur_pelist;
}

/*---------------------------------------------------------------------------*/
/*Function that given a list of ranks in the world pelist, returns a pointer
  to the appropriate pelist.  If a null rank list pointer is passed in, the
  current pelist is returned.  If the inputted rank list does not match an
  existing pelist, then a null pointer is returned.*/
mpp_c_pelist_t *mpp_c_context_get_pelist(mpp_c_context_t const * const self,
                                         uint32_t const *rank_list,
                                         size_t const rank_list_size,
                                         mpp_c_flag_t const self_check)
{
    /*Local variables*/
    mpp_c_pelist_t *tmp_pelist = NULL; /*Pointer to the appropriate pelist.*/
    uint32_t my_world_rank = 0;        /*World rank id of the process.*/
    size_t num_pelists = 0;            /*Current number of pelists.*/
    uint32_t *tmp_rank_list = NULL;    /*Pointer to a rank list array.*/
    int memcmp_result = 0;             /*Output value from memcmp.*/
    unsigned int i = 0;                /*Loop variable.*/

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_PELIST");

    /*Check pelist input.*/
    check_pelist_input(rank_list,rank_list_size,"MPP_C_CONTEXT_GET_PELIST");

    /*Check flag input.*/
    check_flag_input(self_check,"MPP_C_CONTEXT_GET_PELIST");

    /*Get the index of the pelist in the peset array, then point to it. If
      a null pelist is inputted, point to the current pelist.*/
    if (rank_list == NULL)
    {
        tmp_pelist = mpp_c_context_get_cur_pelist(self);
    }
    else
    {
        /*If necessary, make sure that the world rank of the process exists
          on the inputted rank list.*/
        if (self_check)
        {
            my_world_rank = mpp_c_context_get_world_rank(self);
            if (!is_int_in_int_array(my_world_rank,rank_list,rank_list_size))
            {
                throw_internal_error("MPP_C_CONTEXT_GET_PELIST",
                                     "the current rank must exist on the"
                                     " inputted pelist.");
            }
        }

        /*Check if the inputted pelist is the world pelist.*/
        if (rank_list_size ==
            mpp_c_context_get_world_pelist_rank_list_size(self))
        {
            tmp_rank_list = mpp_c_context_get_world_pelist_rank_list(self);
            memcmp_result = memcmp((void *)tmp_rank_list,(void *)rank_list,
                                   sizeof(uint32_t)*rank_list_size);
            if (memcmp_result == 0)
            {
                tmp_pelist = mpp_c_context_get_world_pelist(self);
                tmp_rank_list = NULL;
                return tmp_pelist;
            }
            tmp_rank_list = NULL;
        }

        /*If the inputted pelist is not the world pelist, then search the
          peset array.  If a match is found, return a pointer to the existing
          pelist.  If not, then return a null pointer.*/
        num_pelists = mpp_c_context_get_cur_num_pelists(self);
        for (i=0;i<num_pelists;i++)
        {
            tmp_pelist = &(self->peset[i]);
            if (rank_list_size == mpp_c_pelist_get_rank_list_size(tmp_pelist))
            {
                tmp_rank_list = mpp_c_pelist_get_rank_list(tmp_pelist);
                memcmp_result = memcmp((void *)tmp_rank_list,(void *)rank_list,
                                       sizeof(uint32_t)*rank_list_size);
                if (memcmp_result == 0)
                {
                    tmp_rank_list = NULL;
                    return tmp_pelist;
                }
            }
        }

        /*No match was found so return a null pointer.*/
        tmp_pelist = NULL;
    }

    /*Nullify local pointers.*/
    tmp_rank_list = NULL;

    return tmp_pelist;
}

/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the world pelist.*/
mpp_c_pelist_t *mpp_c_context_get_world_pelist(mpp_c_context_t const * const self)
{
    /*Local variables*/
    mpp_c_pelist_t *tmp_pelist = NULL; /*Pointer to the world pelist.*/
    mpp_c_shared_t *tmp_shared = NULL; /*Pointer to shared context data.*/

    /*Get the index of the current pelist, then point to it.*/
    tmp_shared = mpp_c_context_get_shared(self);
    tmp_pelist = mpp_c_shared_get_world_pelist(tmp_shared);

    /*Nullify local pointers.*/
    tmp_shared = NULL;

    return tmp_pelist;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the name of the world pelist.*/
char *mpp_c_context_get_world_pelist_name(mpp_c_context_t const * const self)
{
    /*Local variables*/
    char *tmp_name = NULL;             /*Pointer to the name of the world pelist.*/
    mpp_c_pelist_t *tmp_pelist = NULL; /*Pointer to the world pelist.*/

    tmp_pelist = mpp_c_context_get_world_pelist(self);
    tmp_name = mpp_c_pelist_get_name(tmp_pelist);

    /*Nullify local pointers.*/
    tmp_pelist = NULL;

    return tmp_name;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the length of the name of the world pelist.*/
size_t mpp_c_context_get_world_pelist_name_len(mpp_c_context_t const * const self)
{
    /*Local variables*/
    size_t tmp_name_len = 0; /*Length of the name of the world pelist.*/
    char *tmp_name = NULL;   /*Pointer to the name of the world pelist.*/

    /*Get the length of the name of the world pelist.*/
    tmp_name = mpp_c_context_get_world_pelist_name(self);
    tmp_name_len = strlen(tmp_name);

    /*Nullify local pointers.*/
    tmp_name = NULL;

    return tmp_name_len;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the rank list for the world pelist.*/
uint32_t *mpp_c_context_get_world_pelist_rank_list(mpp_c_context_t const * const self)
{
    /*Local variables*/
    uint32_t *tmp_rank_list = NULL;    /*Pointer to the rank list of the world pelist.*/
    mpp_c_pelist_t *tmp_pelist = NULL; /*Pointer to the world pelist.*/

    tmp_pelist = mpp_c_context_get_world_pelist(self);
    tmp_rank_list = mpp_c_pelist_get_rank_list(tmp_pelist);

    /*Nullify local pointers.*/
    tmp_pelist = NULL;

    return tmp_rank_list;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the rank list size for the world pelist.*/
size_t mpp_c_context_get_world_pelist_rank_list_size(mpp_c_context_t const * const self)
{
    /*Local variables*/
    size_t tmp_rank_list_size = 0;     /*Rank list size of the world pelist.*/
    mpp_c_pelist_t *tmp_pelist = NULL; /*Pointer to the world pelist.*/

    tmp_pelist = mpp_c_context_get_world_pelist(self);
    tmp_rank_list_size = mpp_c_pelist_get_rank_list_size(tmp_pelist);

    /*Nullify local pointers.*/
    tmp_pelist = NULL;

    return tmp_rank_list_size;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the root rank id for the world pelist.*/
uint32_t mpp_c_context_get_world_pelist_root_rank(mpp_c_context_t const * const self)
{
    /*Local variables*/
    uint32_t tmp_root_rank = 0;        /*Root rank id of the world pelist.*/
    mpp_c_pelist_t *tmp_pelist = NULL; /*Pointer to the world pelist.*/

    tmp_pelist = mpp_c_context_get_world_pelist(self);
    tmp_root_rank = mpp_c_pelist_get_root_rank(tmp_pelist);

    /*Nullify local pointers.*/
    tmp_pelist = NULL;

    return tmp_root_rank;
}

/*---------------------------------------------------------------------------*/
/*Function that determines if the local rank is the root rank of the world
  pelist.*/
mpp_c_flag_t mpp_c_context_is_world_pelist_root_rank(mpp_c_context_t const * const self)
{
    /*Local variables*/
    mpp_c_flag_t is_root = 0;          /*Is root flag.*/
    mpp_c_pelist_t *tmp_pelist = NULL; /*Pointer to the world pelist.*/
    uint32_t my_rank = 0;              /*MPI rank in the world pelist.*/

    my_rank = mpp_c_context_get_world_rank(self);
    tmp_pelist = mpp_c_context_get_world_pelist(self);
    is_root = mpp_c_pelist_is_root_rank(tmp_pelist,my_rank);

    /*Nullify local pointers.*/
    tmp_pelist = NULL;

    return is_root;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the communicator id for the world pelist.*/
MPI_Comm mpp_c_context_get_world_pelist_comm_id(mpp_c_context_t const * const self)
{
    /*Local variables*/
    MPI_Comm tmp_comm_id = MPI_COMM_NULL; /*Communicator id for the world pelist.*/
    mpp_c_pelist_t *tmp_pelist = NULL;    /*Pointer to the world pelist.*/

    tmp_pelist = mpp_c_context_get_world_pelist(self);
    tmp_comm_id = mpp_c_pelist_get_comm_id(tmp_pelist);

    /*Nullify local pointers.*/
    tmp_pelist = NULL;

    return tmp_comm_id;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the group id for the world pelist.*/
MPI_Group mpp_c_context_get_world_pelist_group_id(mpp_c_context_t const * const self)
{
    /*Local variables*/
    MPI_Group tmp_group_id = MPI_GROUP_NULL; /*Group id for the world pelist.*/
    mpp_c_pelist_t *tmp_pelist = NULL;       /*Pointer to the world pelist.*/

    tmp_pelist = mpp_c_context_get_world_pelist(self);
    tmp_group_id = mpp_c_pelist_get_group_id(tmp_pelist);

    /*Nullify local pointers.*/
    tmp_pelist = NULL;

    return tmp_group_id;
}

/*---------------------------------------------------------------------------*/
/*Function that determines whether an inputted rank exists on the world
  pelist.*/
mpp_c_flag_t mpp_c_context_is_rank_on_world_pelist(mpp_c_context_t const * const self,
                                                   uint32_t const rank)
{
    /*Local variables*/
    mpp_c_flag_t is_rank_on_world_pelist = 0; /*Flag denoting whether the inputted rank is on the current pelist.*/
    size_t tmp_rank_list_size = 0;            /*Current pelist rank list size.*/

    tmp_rank_list_size = mpp_c_context_get_world_pelist_rank_list_size(self);
    if (rank < tmp_rank_list_size)
    {
        is_rank_on_world_pelist = MPP_C_TRUE;
    }
    else
    {
        is_rank_on_world_pelist = MPP_C_FALSE;
    }

    return is_rank_on_world_pelist;
}

/*---------------------------------------------------------------------------*/
/*Function that sets the root rank id for the current pelist.*/
void mpp_c_context_set_cur_pelist_root_rank(mpp_c_context_t * const * const self,
                                            uint32_t const root_rank)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL;   /*Pointer to the mpp_c_context_t object.*/
    mpp_c_pelist_t *tmp_pelist = NULL; /*Pointer to the current pelist.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Set the root rank id of the current pelist.*/
    tmp_pelist = mpp_c_context_get_cur_pelist(tmp_ptr);
    mpp_c_pelist_set_root_rank(&tmp_pelist,root_rank);

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_pelist = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the name of the current pelist.*/
char *mpp_c_context_get_cur_pelist_name(mpp_c_context_t const * const self)
{
    /*Local variables*/
    char *tmp_name = NULL;             /*Pointer to the name of the current pelist.*/
    mpp_c_pelist_t *tmp_pelist = NULL; /*Pointer to the current pelist.*/

    tmp_pelist = mpp_c_context_get_cur_pelist(self);
    tmp_name = mpp_c_pelist_get_name(tmp_pelist);

    /*Nullify local pointers.*/
    tmp_pelist = NULL;

    return tmp_name;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the length of the name of the current pelist.*/
size_t mpp_c_context_get_cur_pelist_name_len(mpp_c_context_t const * const self)
{
    /*Local variables*/
    size_t tmp_name_len = 0; /*Length of the name of the current pelist.*/
    char *tmp_name = NULL;   /*Pointer to the name of the current pelist.*/

    /*Get the length of the name of the current pelist.*/
    tmp_name = mpp_c_context_get_cur_pelist_name(self);
    tmp_name_len = strlen(tmp_name);

    /*Nullify local pointers.*/
    tmp_name = NULL;

    return tmp_name_len;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the rank list for the current pelist.*/
uint32_t *mpp_c_context_get_cur_pelist_rank_list(mpp_c_context_t const * const self)
{
    /*Local variables*/
    uint32_t *tmp_rank_list = NULL;    /*Pointer to the rank list of the current pelist.*/
    mpp_c_pelist_t *tmp_pelist = NULL; /*Pointer to the current pelist.*/

    tmp_pelist = mpp_c_context_get_cur_pelist(self);
    tmp_rank_list = mpp_c_pelist_get_rank_list(tmp_pelist);

    /*Nullify local pointers.*/
    tmp_pelist = NULL;

    return tmp_rank_list;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the rank list size for the current pelist.*/
size_t mpp_c_context_get_cur_pelist_rank_list_size(mpp_c_context_t const * const self)
{
    /*Local variables*/
    size_t tmp_rank_list_size = 0;     /*Rank list size of the current pelist.*/
    mpp_c_pelist_t *tmp_pelist = NULL; /*Pointer to the current pelist.*/

    tmp_pelist = mpp_c_context_get_cur_pelist(self);
    tmp_rank_list_size = mpp_c_pelist_get_rank_list_size(tmp_pelist);

    /*Nullify local pointers.*/
    tmp_pelist = NULL;

    return tmp_rank_list_size;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the root rank id for the current pelist.*/
uint32_t mpp_c_context_get_cur_pelist_root_rank(mpp_c_context_t const * const self)
{
    /*Local variables*/
    uint32_t tmp_root_rank = 0;        /*Root rank id of the current pelist.*/
    mpp_c_pelist_t *tmp_pelist = NULL; /*Pointer to the current pelist.*/

    tmp_pelist = mpp_c_context_get_cur_pelist(self);
    tmp_root_rank = mpp_c_pelist_get_root_rank(tmp_pelist);

    /*Nullify local pointers.*/
    tmp_pelist = NULL;

    return tmp_root_rank;
}

/*---------------------------------------------------------------------------*/
/*Function that determines if the local rank is the root rank of the current
  pelist.*/
mpp_c_flag_t mpp_c_context_is_cur_pelist_root_rank(mpp_c_context_t const * const self)
{
    /*Local variables*/
    mpp_c_flag_t is_root = 0;          /*Is root flag.*/
    mpp_c_pelist_t *tmp_pelist = NULL; /*Pointer to the current pelist.*/
    uint32_t my_rank = 0;              /*MPI rank in the world pelist.*/

    my_rank = mpp_c_context_get_world_rank(self);
    tmp_pelist = mpp_c_context_get_cur_pelist(self);
    is_root = mpp_c_pelist_is_root_rank(tmp_pelist,my_rank);

    /*Nullify local pointers.*/
    tmp_pelist = NULL;

    return is_root;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the communicator id for the current pelist.*/
MPI_Comm mpp_c_context_get_cur_pelist_comm_id(mpp_c_context_t const * const self)
{
    /*Local variables*/
    MPI_Comm tmp_comm_id = MPI_COMM_NULL; /*Communicator id for the current pelist.*/
    mpp_c_pelist_t *tmp_pelist = NULL;    /*Pointer to the current pelist.*/

    tmp_pelist = mpp_c_context_get_cur_pelist(self);
    tmp_comm_id = mpp_c_pelist_get_comm_id(tmp_pelist);

    /*Nullify local pointers.*/
    tmp_pelist = NULL;

    return tmp_comm_id;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the group id for the current pelist.*/
MPI_Group mpp_c_context_get_cur_pelist_group_id(mpp_c_context_t const * const self)
{
    /*Local variables*/
    MPI_Group tmp_group_id = MPI_GROUP_NULL; /*Group id for the current pelist.*/
    mpp_c_pelist_t *tmp_pelist = NULL;       /*Pointer to the current pelist.*/

    tmp_pelist = mpp_c_context_get_cur_pelist(self);
    tmp_group_id = mpp_c_pelist_get_group_id(tmp_pelist);

    /*Nullify local pointers.*/
    tmp_pelist = NULL;

    return tmp_group_id;
}

/*---------------------------------------------------------------------------*/
/*Function that determines whether an inputted rank exists on the curent
  pelist.*/
mpp_c_flag_t mpp_c_context_is_rank_on_cur_pelist(mpp_c_context_t const * const self,
                                                 uint32_t const rank)
{
    /*Local variables*/
    mpp_c_flag_t is_rank_on_cur_pelist = 0; /*Flag denoting whether the inputted rank is on the current pelist.*/
    mpp_c_pelist_t *tmp_pelist = NULL;      /*Pointer to the current pelist.*/

    tmp_pelist = mpp_c_context_get_cur_pelist(self);
    is_rank_on_cur_pelist = mpp_c_pelist_is_rank_on_rank_list(tmp_pelist,rank);

    /*Nullify local pointers.*/
    tmp_pelist = NULL;

    return is_rank_on_cur_pelist;
}

/*---------------------------------------------------------------------------*/
/*Function that sets the value of the record_timing_data flag for an
  mpp_c_context_t object.*/
void mpp_c_context_set_record_timing_data(mpp_c_context_t * const * const self,
                                          mpp_c_flag_t record_flag)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,
                                   "MPP_C_CONTEXT_SET_RECORD_TIMING_DATA");

    /*Check flag input.*/
    check_flag_input(record_flag,"MPP_C_CONTEXT_SET_RECORD_TIMING_DATA");

    /*Set the flag.*/
    tmp_ptr->record_timing_data = record_flag;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that sets the allowed grainularity for the timers for an
  mpp_c_context_t object.*/
void mpp_c_context_set_timer_grain(mpp_c_context_t * const * const self,
                                   int32_t const grain)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_CONTEXT_SET_TIMER_GRAIN");

    /*Set the grainularity for the timers.*/
    switch (grain)
    {
        case MPP_C_RUNTIME_TIMER:
            tmp_ptr->timer_grain = MPP_C_RUNTIME_TIMER;
            break;
        case MPP_C_COMPONENT_TIMER:
            tmp_ptr->timer_grain = MPP_C_COMPONENT_TIMER;
            break;
        case MPP_C_SUBCOMPONENT_TIMER:
            tmp_ptr->timer_grain = MPP_C_SUBCOMPONENT_TIMER;
            break;
        case MPP_C_MODULE_DRIVER_TIMER:
            tmp_ptr->timer_grain = MPP_C_MODULE_DRIVER_TIMER;
            break;
        case MPP_C_MODULE_TIMER:
            tmp_ptr->timer_grain = MPP_C_MODULE_TIMER;
            break;
        case MPP_C_ROUTINE_TIMER:
            tmp_ptr->timer_grain = MPP_C_ROUTINE_TIMER;
            break;
        case MPP_C_LOOP_TIMER:
            tmp_ptr->timer_grain = MPP_C_LOOP_TIMER;
            break;
        case MPP_C_INFRASTRUCTURE_TIMER:
            tmp_ptr->timer_grain = MPP_C_INFRASTRUCTURE_TIMER;
            break;
        default:
            throw_internal_error("MPP_C_CONTEXT_SET_TIMER_GRAIN",
                                 "unsupported timer grain.");
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that adds a new timer for an mpp_c_context_t object and returns
  an id.*/
int32_t mpp_c_context_add_new_timer(mpp_c_context_t * const * const self,
                                    char const *timer_name,
                                    size_t const timer_name_len,
                                    int32_t const grain,
                                    mpp_c_flag_t const sync_flag,
                                    mpp_c_flag_t const detail_flag)
{
    /*Local variables*/
    int32_t timer_id = 0;            /*Id for the timer.*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/
    mpp_c_timer_t *tmp_timer = NULL; /*Pointer to a timer.*/
    uint32_t tmp_index = 0;          /*Temporary index variable.*/
    mpp_c_flag_t tmp_sync_flag;      /*Temporary sync flag.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_CONTEXT_ADD_NEW_TIMER");

    /*Check string input.*/
    check_string_input(timer_name,timer_name_len,
                       "MPP_C_CONTEXT_ADD_NEW_TIMER");

    /*Check flag input.*/
    check_flag_input(sync_flag,"MPP_C_CONTEXT_ADD_NEW_TIMER");
    check_flag_input(detail_flag,"MPP_C_CONTEXT_ADD_NEW_TIMER");

    /*Check whether or not the timer already exists in the timer array.*/
    tmp_index = mpp_c_context_get_timer_list_index(tmp_ptr,timer_name,
                                                   timer_name_len);
    if (tmp_index != MPP_C_NO_MATCH)
    {
/*
        print_internal_message("MPP_C_CONTEXT_ADD_NEW_TIMER",
                               "the timer already exists in the timer_list"
                               " array.");
*/
        timer_id = (int32_t)tmp_index;
    }
    else
    {
        /*Iterate the total number of current timers. To match legacy
          behavior, timer ids must start at 1, so the first spot in the
          timers array will remain empty.*/
        (tmp_ptr->cur_num_timers)++;
        tmp_index = mpp_c_context_get_cur_num_timers(tmp_ptr);

        /*Make sure that the total number of current timers does not exceed
          the maximum number allowed.*/
        if (tmp_index > mpp_c_context_get_max_timers(tmp_ptr) - 1)
        {
            throw_internal_error("MPP_C_CONTEXT_ADD_NEW_TIMER",
                                 "the number of timers exceeds the"
                                 " maximum number allowed.");
        }

        /*Construct the new timer.*/
        tmp_timer = &(tmp_ptr->timer_list[tmp_index]);
        if (mpp_c_context_sync_all_timers(tmp_ptr))
        {
            tmp_sync_flag = MPP_C_TRUE;
        }
        else
        {
            tmp_sync_flag = sync_flag;
        }
        mpp_c_timer_set_timer_metadata(&tmp_timer,timer_name,
                                       timer_name_len,grain,
                                       tmp_sync_flag,detail_flag,
                                       mpp_c_context_get_cur_pelist(tmp_ptr));
        timer_id = (int32_t)tmp_index;
    }

    /*Nullify the local pointers.*/
    tmp_ptr = NULL;
    tmp_timer = NULL;

    return timer_id;
}

/*---------------------------------------------------------------------------*/
/*Function that starts a timer for an mpp_c_context_t object.*/
void mpp_c_context_start_timer(mpp_c_context_t * const * const self,
                               int32_t const timer_id)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/
    mpp_c_timer_t *tmp_timer = NULL; /*Pointer to a timer.*/
    char *event_name = NULL;         /*Name of an event.*/
    int ierr = 0;                    /*MPI error code.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_CONTEXT_START_TIMER");

    /*Make sure that timing data is being recorded.*/
    if (!mpp_c_context_record_timing_data(tmp_ptr))
    {
/*
        print_internal_message("MPP_C_CONTEXT_START_TIMER",
                               "timing data is not being recorded.");
*/
        tmp_ptr = NULL;
        return;
    }

    /*Make sure that a valid timer id was passed in.*/
    if (timer_id < 1 || (uint32_t)timer_id >
        mpp_c_context_get_cur_num_timers(tmp_ptr))
    {
        throw_internal_error("MPP_C_CONTEXT_START_TIMER",
                             "invalid timer id.");
    }

    /*Point to the correct timer.*/
    tmp_timer = &(tmp_ptr->timer_list[timer_id]);

    /*Make sure that the timer has had its metadata set.*/
    if (!mpp_c_timer_has_metadata(tmp_timer))
    {
        throw_internal_error("MPP_C_CONTEXT_START_TIMER","the inputted timer"
                             " has not had its metadata set.");
    }

    /*Make sure that the timer's grain is not too fine.*/
    if (mpp_c_timer_get_grain(tmp_timer) >
        mpp_c_context_get_timer_grain(tmp_ptr))
    {
        tmp_ptr = NULL;
        tmp_timer = NULL;
        return;
    }

    /*Make sure that the current pelist matches the timers pelist.*/
    if (mpp_c_timer_get_pelist(tmp_timer) != 
        mpp_c_context_get_cur_pelist(tmp_ptr))
    {
        throw_internal_error("MPP_C_CONTEXT_START_TIMER",
                             "the current pelist does not match the pelist"
                             " stored in the timer.");
    }

    /*Add the a "start timer" event to the current clock, in order to trace
      timer lineage and get more accurate timing data.*/
    event_name = "Start_timer";
    mpp_c_context_log_event_start_time(&tmp_ptr,NULL,event_name,
                                       strlen(event_name),
                                       EVENT_START_TIMER,timer_id);
    mpp_c_context_log_event_stop_time(&tmp_ptr,NULL,0);
    event_name = NULL;

    /*If necessary, synchronize the timers.*/
    if (mpp_c_timer_sync_at_start(tmp_timer))
    {
        event_name = "Sync_timer";
        mpp_c_context_log_event_start_time(&tmp_ptr,NULL,event_name,
                                           strlen(event_name),EVENT_WAIT,
                                           timer_id);
        ierr = MPI_Barrier(mpp_c_context_get_cur_pelist_comm_id(tmp_ptr));
        check_MPI_error_code(ierr,"MPI_Barrier","MPP_C_CONTEXT_START_TIMER");
        mpp_c_context_log_event_stop_time(&tmp_ptr,NULL,0);
    }

    /*Add the timer to the current timer linked list.*/
    mpp_c_timer_ll_set_cur_timer(&(tmp_ptr->cur_timer_node),tmp_timer);

    /*Start the timer.*/
    mpp_c_timer_set_start_time(&tmp_timer);

    /*Nullify the local pointers.*/
    tmp_ptr = NULL;
    tmp_timer = NULL;
    event_name = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that stops a timer for an mpp_c_context_t object.*/
void mpp_c_context_stop_timer(mpp_c_context_t * const * const self,
                              int32_t const timer_id)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/
    mpp_c_timer_t *tmp_timer = NULL; /*Pointer to a timer.*/
    char *event_name = NULL;         /*Name of an event.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_CONTEXT_STOP_TIMER");

    /*Make sure that timing data is being recorded.*/
    if (!mpp_c_context_record_timing_data(tmp_ptr))
    {
/*
        print_internal_message("MPP_C_CONTEXT_STOP_TIMER",
                               "timers are turned off.");
*/
        tmp_ptr = NULL;
        return;
    }

    /*Make sure that a valid timer id was passed in.*/
    if (timer_id < 1 || (uint32_t)timer_id >
        mpp_c_context_get_cur_num_timers(tmp_ptr))
    {
        throw_internal_error("MPP_C_CONTEXT_STOP_TIMER",
                             "invalid timer id.");
    }

    /*Point to the correct timer.*/
    tmp_timer = &(tmp_ptr->timer_list[timer_id]);

    /*Make sure that the timer has had its metadata set.*/
    if (!mpp_c_timer_has_metadata(tmp_timer))
    {
        throw_internal_error("MPP_C_CONTEXT_STOP_TIMER","the inputted timer"
                             " has not had its metadata set.");
    }

    /*Make sure that the timer's grain is not too fine.*/
    if (mpp_c_timer_get_grain(tmp_timer) >
        mpp_c_context_get_timer_grain(tmp_ptr))
    {
        tmp_ptr = NULL;
        tmp_timer = NULL;
        return;
    }

    /*Make sure that the current pelist matches the timers pelist.*/
    if (mpp_c_timer_get_pelist(tmp_timer) != 
        mpp_c_context_get_cur_pelist(tmp_ptr))
    {
        throw_internal_error("MPP_C_CONTEXT_STOP_TIMER",
                             "the current pelist does not match the pelist"
                             " stored in the timer.");
    }

    /*Stop the timer.*/
    mpp_c_timer_set_end_time_and_total_time(&tmp_timer);

    /*Remove the timer from the current timer linked list.*/
    mpp_c_timer_ll_revert_cur_timer(&(tmp_ptr->cur_timer_node));

    /*Add the a "stop timer" event to the current clock, in order to trace
      timer lineage and get more accurate timing data.*/
    event_name = "Stop_timer";
    mpp_c_context_log_event_start_time(&tmp_ptr,NULL,event_name,
                                       strlen(event_name),
                                       EVENT_STOP_TIMER,timer_id);
    mpp_c_context_log_event_stop_time(&tmp_ptr,NULL,0);

    /*Nullify the local pointers.*/
    tmp_ptr = NULL;
    tmp_timer = NULL;
    event_name = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that records the starting time for an event.*/
void mpp_c_context_log_event_start_time(mpp_c_context_t * const * const self,
                                        mpp_c_timer_t * const timer,
                                        char const *event_name,
                                        size_t const event_name_len,
                                        int32_t const event_type,
                                        int32_t const timer_id)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/
    mpp_c_timer_t *tmp_timer = NULL; /*Pointer to a timer.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,
                                   "MPP_C_CONTEXT_LOG_EVENT_START_TIME");

    /*Check string input.*/
    check_string_input(event_name,event_name_len,
                       "MPP_C_CONTEXT_LOG_EVENT_START_TIME");

    /*Events cannot be logged before the first timer is turned on or after
      the last timer is turned off.*/
    if (tmp_ptr->cur_timer_node == NULL)
    {
/*
        print_internal_message("MPP_C_CONTEXT_LOG_EVENT_START_TIME",
                               "the current timer node is null.");
*/
        tmp_ptr = NULL;
        tmp_timer = NULL;
        return;
    }

    /*Make sure that timing data is being recorded.*/
    if (mpp_c_context_record_timing_data(tmp_ptr))
    {
        /*If a null timer is passed in, then use the current timer from
          the current timer linked list.*/
        if (timer == NULL)
        {
            tmp_timer = mpp_c_timer_ll_get_cur_timer(tmp_ptr->cur_timer_node);
        }
        else
        {
            tmp_timer = timer;
        }

        /*Add a new event to the timer.*/
        mpp_c_timer_start_new_event(&tmp_timer,event_name,event_name_len,
                                    event_type,timer_id);
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_timer = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that records the stopping time for an event.*/
void mpp_c_context_log_event_stop_time(mpp_c_context_t * const * const self,
                                       mpp_c_timer_t * const timer,
                                       size_t const num_bytes)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/
    mpp_c_timer_t *tmp_timer = NULL; /*Pointer to a timer.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,
                                   "MPP_C_CONTEXT_LOG_EVENT_STOP_TIME");

    /*Events cannot be logged before the first timer is turned on or after
      the last timer is turned off.*/
    if (tmp_ptr->cur_timer_node == NULL)
    {
/*
        print_internal_message("MPP_C_CONTEXT_LOG_EVENT_STOP_TIME",
                               "the current timer node is null.");
*/
        tmp_ptr = NULL;
        tmp_timer = NULL;
        return;
    }

    /*Make sure that timing data is being recorded.*/
    if (mpp_c_context_record_timing_data(tmp_ptr))
    {
        /*If a null timer is passed in, then use the current timer from
          the current timer linked list.*/
        if (timer == NULL)
        {
            tmp_timer = mpp_c_timer_ll_get_cur_timer(tmp_ptr->cur_timer_node);
        }
        else
        {
            tmp_timer = timer;
        }

        /*Stop the timer for the event and store the number of bytes
          communicated.*/
        mpp_c_timer_end_current_event(&tmp_timer,num_bytes);
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_timer = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that sets the id of the runtime timer for an mpp_c_context_t
  object.*/
void mpp_c_context_set_runtime_timer_id(mpp_c_context_t * const * const self,
                                        int32_t const runtime_id)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,
                                   "MPP_C_CONTEXT_SET_RUNTIME_TIMER_ID");

    /*Set the id.*/
    tmp_ptr->runtime_timer_id = runtime_id;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that gets the value of the record_timing_data flag for an
  mpp_c_context_t object.*/
mpp_c_flag_t mpp_c_context_record_timing_data(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_RECORD_TIMING_DATA");

    return self->record_timing_data;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the grainularity level allowed for timers for an
  mpp_c_context_t object.*/
int32_t mpp_c_context_get_timer_grain(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_TIMER_GRAIN");

    return self->timer_grain;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the value of the sync_all_timers flag for an
  mpp_c_context_t object.*/
mpp_c_flag_t mpp_c_context_sync_all_timers(mpp_c_context_t const * const self)
{
    /*Local variables*/
    mpp_c_flag_t sync_timers_flag = 0; /*Sync timers flag.*/
    mpp_c_shared_t *tmp_shared = NULL; /*Pointer to an mpp_c_shared_t object.*/

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_SYNC_ALL_TIMERS");

    /*Get the sync timers flag.*/
    tmp_shared = mpp_c_context_get_shared(self);
    sync_timers_flag = mpp_c_shared_get_sync_all_clocks(tmp_shared);

    /*Nullify local pointers.*/
    tmp_shared = NULL;

    return sync_timers_flag;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the maximum number of timers allowed for an
  mpp_c_context_t object.*/
size_t mpp_c_context_get_max_timers(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_MAX_TIMERS");

    return self->max_timers;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the current number of timers that exist for an
  mpp_c_context_t object.*/
size_t mpp_c_context_get_cur_num_timers(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_CUR_NUM_TIMERS");

    return self->cur_num_timers;
}

/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the list of timers that exist for an
  mpp_c_context_t object.*/
mpp_c_timer_t *mpp_c_context_get_timer_list(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_TIMER_LIST");

    return self->timer_list;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the id of the runtime timer for an mpp_c_context_t
  object.*/
int32_t mpp_c_context_get_runtime_timer_id(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_RUNTIME_TIMER_ID");

    return self->runtime_timer_id;
}

/*---------------------------------------------------------------------------*/
/*Check whether or not the timer already exists in the timer array.*/
uint32_t mpp_c_context_get_timer_list_index(mpp_c_context_t const * const self,
                                            char const *timer_name,
                                            size_t const timer_name_len)
{
    /*Local variables*/
    mpp_c_timer_t *tmp_timer = NULL; /*Pointer to a timer.*/
    uint32_t timer_index = 0;        /*Index of the timer in the timer_list array.*/
    size_t num_timers = 0;           /*Number of existing timers.*/
    unsigned int i = 0;              /*Loop variable.*/

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_TIMER_LIST_INDEX");

    /*Check string input.*/
    check_string_input(timer_name,timer_name_len,
                       "MPP_C_CONTEXT_GET_TIMER_LIST_INDEX");

    /*Check if the timer already exists in the timer_list array.*/
    timer_index = (uint32_t)MPP_C_NO_MATCH;
    num_timers = mpp_c_context_get_cur_num_timers(self);
    for (i=1;i<num_timers+1;i++)
    {
        tmp_timer = &(self->timer_list[i]);
        if (strcmp(timer_name,mpp_c_timer_get_name(tmp_timer)) == 0)
        {
            timer_index = (uint32_t)i;
            break;
        }
    }

    /*Nullify local pointers.*/
    tmp_timer = NULL;

    return timer_index;
}
/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the name of the logfile for an
  mpp_c_context_t object.*/
char *mpp_c_context_get_logfile_name(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_LOGFILE_NAME");

    return self->logfile_name;
}

/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the logfile file pointer for an
  mpp_c_context_t object.*/
FILE *mpp_c_context_get_logfile_ptr(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_LOGFILE_PTR");

    return self->logfile_ptr;
}

/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the name of the etcfile for an
  mpp_c_context_t object.*/
char *mpp_c_context_get_etcfile_name(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_ETCFILE_NAME");

    return self->etcfile_name;
}

/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the etcfile file pointer for an
  mpp_c_context_t object.*/
FILE *mpp_c_context_get_etcfile_ptr(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_MASTEGER_GET_ETCFILE_PTR");

    return self->etcfile_ptr;
}

/*---------------------------------------------------------------------------*/
/*Function that allocates the namelist buffer to a specific size.*/
void mpp_c_context_alloc_namelist_buffer(mpp_c_context_t * const * const self,
                                         size_t const buffer_size)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,
                                   "MPP_C_CONTEXT_ALLOC_NAMELIST_BUFFER");

    /*Make sure that the input buffer size is valid.*/
    if (buffer_size == 0 || buffer_size > MPP_C_NAMELIST_BUFFER_MAX_SIZE)
    {
        throw_internal_error("MPP_C_CONTEXT_ALLOC_NAMELIST_BUFFER",
                             "requested buffer size must be greater than"
                             " 0 and less than or equal to"
                             " MPP_C_NAMELIST_BUFFER_MAX_SIZE");
    }

    /*Allocate the namelist buffer and set the length.*/
    if (tmp_ptr->namelist_buffer == NULL)
    {
        safemalloc((void **) &(tmp_ptr->namelist_buffer),
                   sizeof(char)*buffer_size);
        tmp_ptr->namelist_buffer_len = buffer_size;
    }
    else
    {
        throw_internal_error("MPP_C_CONTEXT_ALLOC_NAMELIST_BUFFER",
                             "the namelist buffer pointer is not null.");
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that fills the internal namelist buffer with an inputted string.*/
void mpp_c_context_fill_namelist_buffer(mpp_c_context_t * const * const self,
                                        char const *in_buffer,
                                        size_t const buffer_size)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/
    size_t tmp_size = 0;             /*Size of the namelist buffer.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,
                                   "MPP_C_CONTEXT_FILL_NAMELIST_BUFFER");

    /*Check string input.*/
    check_string_input(in_buffer,buffer_size,
                       "MPP_C_CONTEXT_FILL_NAMELIST_BUFFER");

    /*Make sure that the inputted buffer size is not greater than the namelist
      buffer size.*/
    tmp_size = mpp_c_context_get_namelist_buffer_len(tmp_ptr);
    if (buffer_size > tmp_size)
    {
        throw_internal_error("MPP_C_CONTEXT_FILL_NAMELIST_BUFFER",
                             "the inputted buffer is larger than the namelist"
                             " buffer.");
    }

    /*Copy in the inputted buffer into the namelist buffer.*/
    if (tmp_ptr->namelist_buffer != NULL)
    {
        snprintf(tmp_ptr->namelist_buffer,tmp_size,"%s",in_buffer);
    }
    else
    {
        throw_internal_error("MPP_C_CONTEXT_FILL_NAMELIST_BUFFER",
                             "the namelist buffer pointer is null.");
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that frees the namelist buffer.*/
void mpp_c_context_free_namelist_buffer(mpp_c_context_t * const * const self)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,
                                   "MPP_C_CONTEXT_FREE_NAMELIST_BUFFER");

    /*Free the namelist buffer and set the length to 0.*/
    if (tmp_ptr->namelist_buffer != NULL)
    {
        safefree((void **) &(tmp_ptr->namelist_buffer));
        tmp_ptr->namelist_buffer_len = 0;
    }
    else
    {
        throw_internal_error("MPP_C_CONTEXT_FREE_NAMELIST_BUFFER",
                             "the namelist buffer pointer is null.");
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the length of the namelist buffer for an
  mpp_c_context_t object.*/
size_t mpp_c_context_get_namelist_buffer_len(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,
                                   "MPP_C_CONTEXT_GET_NAMELIST_BUFFER_LEN");

    return self->namelist_buffer_len;
}

/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the namelist buffer for an
  mpp_c_context_t object.*/
char *mpp_c_context_get_namelist_buffer(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_NAMELIST_BUFFER");

    return self->namelist_buffer;
}

/*---------------------------------------------------------------------------*/
/*Function that sets control of the work buffer.*/
void *mpp_c_context_use_work_buffer(mpp_c_context_t * const * const self,
                                    size_t const requested_size)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/
    void *buffer_ptr = NULL;         /*Pointer to work buffer.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_CONTEXT_USE_WORK_BUFFER");

    /*Make sure that a vaild size was inputted.*/
    if (requested_size == 0 || requested_size > MPP_C_WORK_BUFFER_MAX_SIZE)
    {
        throw_internal_error("MPP_C_CONTEXT_USE_WORK_BUFFER",
                             "requested buffer size must be greater than"
                             " 0 and less than MPP_C_MAX_WORK_BUFFER_SIZE");
    }

    /*Reallocate the buffer if necessary and point to it.  Throw an error
      if the buffer is currently in use.*/
    if (!mpp_c_context_is_work_buffer_in_use(tmp_ptr))
    {
        if (requested_size > mpp_c_context_get_work_buffer_size(tmp_ptr))
        {
            safefree((void **) &(tmp_ptr->work_buffer));
            safemalloc((void **) &(tmp_ptr->work_buffer),requested_size);
            tmp_ptr->work_buffer_size = requested_size;
        }
        buffer_ptr = mpp_c_context_get_work_buffer(tmp_ptr);
        tmp_ptr->work_buffer_in_use = MPP_C_TRUE;
    }
    else
    {
        throw_internal_error("MPP_C_CONTEXT_USE_WORK_BUFFER",
                             "the work buffer is already in use.");
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return buffer_ptr;
}

/*---------------------------------------------------------------------------*/
/*Function that releases control of the work buffer.*/
void mpp_c_context_release_work_buffer(mpp_c_context_t * const * const self,
                                       void *buffer_ptr)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,
                                   "MPP_C_CONTEXT_RELEASE_WORK_BUFFER");

    /*Make sure that the inputted buffer_ptr actually points to the work
      buffer.*/
    if (buffer_ptr != mpp_c_context_get_work_buffer(tmp_ptr))
    {
        throw_internal_error("MPP_C_CONTEXT_RELEASE_WORK_BUFFER",
                             "the inputted pointer does not point to the"
                             " work buffer.");
    }

    /*Set the work_buffer_in_use flag to MPP_C_FALSE. Throw an error if the
      buffer was not currently in use.*/
    if (mpp_c_context_is_work_buffer_in_use(tmp_ptr))
    {
        tmp_ptr->work_buffer_in_use = MPP_C_FALSE;
        buffer_ptr = NULL;
    }
    else
    {
        throw_internal_error("MPP_C_CONTEXT_RELEASE_WORK_BUFFER",
                             "the work buffer is not already in use.");
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that tells whether or not the work buffer is in use for an
  mpp_c_context_t object.*/
mpp_c_flag_t mpp_c_context_is_work_buffer_in_use(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_IS_WORK_BUFFER_IN_USE");

    return self->work_buffer_in_use;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the size in bytes of the work buffer for an
  mpp_c_context_t object.*/
size_t mpp_c_context_get_work_buffer_size(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_WORK_BUFFER_SIZE");

    return self->work_buffer_size;
}

/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the work buffer for an mpp_c_context_t
  object.*/
void *mpp_c_context_get_work_buffer(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_WORK_BUFFER");

    return self->work_buffer;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the size in bytes of the input_nml buffer for an
  mpp_c_context_t object.*/
size_t mpp_c_context_get_input_nml_buffer_size(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,
                                   "MPP_C_CONTEXT_GET_INPUT_NML_BUFFER_SIZE");

    return mpp_c_shared_get_input_nml_buffer_size(self->shared);
}

/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the input_nml buffer for an
  mpp_c_context_t object.*/
char *mpp_c_context_get_input_nml_buffer(mpp_c_context_t const * const self)
{
    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(self,"MPP_C_CONTEXT_GET_INPUT_NML_BUFFER");

    return mpp_c_shared_get_input_nml_buffer(self->shared);
}

/*---------------------------------------------------------------------------*/

