#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "helper_functions.h"
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"
#include "mpp_c_error.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"
#include "mpp_c_send.h"

/*---------------------------------------------------------------------------*/
/*Non-blocking MPI send to a rank on the world pelist.*/
void mpp_c_send(void **send_data,
                size_t const send_len,
                int32_t const to_rank,
                mpp_c_datatype_t const mpp_type,
                int32_t const tag,
                MPI_Request *request,
                mpp_c_context_t * const * const context)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL;             /*Pointer to the mpp_c_context_t object.*/
    MPI_Datatype mpi_type_val;                   /*MPI type parameter value.*/
    MPI_Comm tmp_comm = MPI_COMM_NULL;           /*World pelist communicator id.*/
    MPI_Request comm_request = MPI_REQUEST_NULL; /*Local MPI request handle.*/
    int32_t ierr = -1;                           /*MPI error code.*/
    char *error_mesg = NULL;                     /*Error message poiner.*/
    char *event_name = NULL;                     /*Event name.*/
    size_t num_bytes = 0;                        /*Number of bytes communicated.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *context;

    /*Make sure mpp is initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_SEND");

    /*Log the communication event starting time.*/
    event_name = "Send";
    mpp_c_context_log_event_start_time(&tmp_ptr,NULL,event_name,
                                       strlen(event_name),EVENT_SEND,
                                       MPP_C_NULL_TIMER_ID);

    /*Perform a non-blocking MPI send.*/
    if (mpp_c_context_is_rank_on_world_pelist(tmp_ptr,(uint32_t)to_rank))
    {
        /*Get the MPI type parameter value.*/
        mpi_type_val = mpp_c_datatype_get_MPI_param(mpp_type);

        /*Get the communicator id for the world pelist.*/
        tmp_comm = mpp_c_context_get_world_pelist_comm_id(tmp_ptr);

        /*Send the data using MPI_Isend.*/
        ierr = MPI_Isend(*send_data,send_len,mpi_type_val,to_rank,tag,
                         tmp_comm,&comm_request);
        check_MPI_error_code(ierr,"MPI_Isend","MPP_C_SEND");
    }
    else
    {
        error_mesg = "MPP_C_SEND: sending to an invalid pe.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }

    /*Process the MPI request.*/
    if (request != NULL)
    {
        *request = comm_request;
    }
    else
    {
        mpp_c_context_add_new_send_request(&tmp_ptr,comm_request);
    }

    /*Log the communication event stopping time.*/
    num_bytes = get_MPI_data_size_in_bytes(mpi_type_val,send_len);
    mpp_c_context_log_event_stop_time(&tmp_ptr,NULL,num_bytes);

    /*Nullify the convenience pointers.*/
    tmp_ptr = NULL;
    error_mesg = NULL;
    event_name = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
