#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"
#include "mpp_c_error.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"
#include "mpp_c_pelist_t_api.h"
#include "mpp_c_sync.h"

/*---------------------------------------------------------------------------*/
/*Sychronize ranks on a pelist using a MPI barrier.*/
void mpp_c_sync(int32_t const *rank_list,
                size_t const rank_list_size,
                mpp_c_context_t * const * const context)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL;      /*Pointer to the mpp_c_context_t object.*/
    mpp_c_pelist_t *tmp_pelist = NULL;    /*Pointer to a pelist.*/
    MPI_Comm tmp_comm_id = MPI_COMM_NULL; /*MPI communicator id for the pelist.*/
    char *event_name = NULL;              /*Event name.*/
    char *error_mesg = NULL;              /*Error message pointer.*/
    int32_t ierr = 0;                     /*MPI error code.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *context;

    /*Check pelist input.*/
    check_pelist_input((uint32_t *)rank_list,rank_list_size,"MPP_C_SYNC");

    /*Point at the appropriate pelist.*/
    tmp_pelist = mpp_c_context_get_pelist(tmp_ptr,(uint32_t *)rank_list,
                                          rank_list_size,MPP_C_TRUE);
    if (tmp_pelist == NULL)
    {
        error_mesg = "MPP_C_SYNC: the inputted pelist does not exist."
                     " You must first create it.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }

    /*Perform the sync if the pelist contains more than just the current
      rank.*/
    if (mpp_c_pelist_get_rank_list_size(tmp_pelist) > 1)
    {
        /*Log the communication event starting time.*/
        event_name = "Sync";
        mpp_c_context_log_event_start_time(&tmp_ptr,NULL,event_name,
                                           strlen(event_name),EVENT_WAIT,
                                           MPP_C_NULL_TIMER_ID);

        /*Get the communicator id for the pelist.*/
        tmp_comm_id = mpp_c_pelist_get_comm_id(tmp_pelist);

        /*Synchronize all ranks in the pelist at a MPI barrier.*/
        ierr = MPI_Barrier(tmp_comm_id);
        check_MPI_error_code(ierr,"MPI_Barrier","MPP_C_SYNC");

        /*Log the communication event stopping time.*/
        mpp_c_context_log_event_stop_time(&tmp_ptr,NULL,0);
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_pelist = NULL;
    event_name = NULL;
    error_mesg = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Complete outstanding MPI sends or receives inputted in the request array.*/
void mpp_c_sync_self(int32_t const comm_type,
                     MPI_Request *request,
                     size_t const num_requests,
                     size_t const *msg_size,
                     mpp_c_datatype_t const *msg_type,
                     mpp_c_context_t * const * const context)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/
    char *error_mesg = NULL;         /*Error message pointer.*/
    MPI_Datatype mpi_type_val;       /*MPI type parameter value.*/
    MPI_Status comm_stat;            /*MPI status for completed request.*/
    int32_t recv_size = 0;           /*Size of received data.*/
    int32_t ierr = 0;                /*MPI error code.*/
    char *event_name = NULL;         /*Event name.*/
    int32_t i = 0;                   /*Loop variable.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *context;

    /*Make sure that mpp is initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_SYNC_SELF");

    /*Check array input.*/
    check_array_input((void *)request,num_requests,"MPP_C_SYNC_SELF");
    check_array_input((void *)msg_size,num_requests,"MPP_C_SYNC_SELF");
    check_array_input((void *)msg_type,num_requests,"MPP_C_SYNC_SELF");

    /*Make sure comm_type is a valid value.*/
    if (comm_type != EVENT_SEND && comm_type != EVENT_RECV)
    {
        error_mesg = "MPP_C_SYNC_SELF: invalid value for comm_type.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }

    /*Log the communication event starting time.*/
    event_name = "Sync_self";
    mpp_c_context_log_event_start_time(&tmp_ptr,NULL,event_name,
                                       strlen(event_name),EVENT_WAIT,
                                       MPP_C_NULL_TIMER_ID);

    /*Complete requests.*/
    if (*request != MPP_C_NULL_REQUEST)
    {
        /*Inputted request array.*/
        for (i=0;(size_t)i<num_requests;i++)
        {
            ierr = MPI_Wait(&request[i],&comm_stat);
            check_MPI_error_code(ierr,"MPI_Wait","MPP_C_SYNC_SELF");
            if (comm_type == EVENT_RECV)
            {

                /*Get the MPI type parameter value.*/
                mpi_type_val = mpp_c_datatype_get_MPI_param(msg_type[i]);
                ierr = MPI_Get_count(&comm_stat,mpi_type_val,&recv_size);
                check_MPI_error_code(ierr,"MPI_Get_count","MPP_C_SYNC_SELF");
                if ((size_t)recv_size != msg_size[i])
                {
                    error_mesg = "MPP_C_SYNC_SELF: received wrong sized data.";
                    mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
                }
            }
        }
    }

    /*Log the communication event stopping time.*/
    mpp_c_context_log_event_stop_time(&tmp_ptr,NULL,0);

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    error_mesg = NULL;
    event_name = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
