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
#include "mpp_c_recv.h"

/*---------------------------------------------------------------------------*/
/*MPI receive from a rank on the world pelist.*/
void mpp_c_recv(void **recv_data,
                size_t const recv_len,
                int32_t const from_rank,
                mpp_c_datatype_t const mpp_type,
                int32_t const tag,
                MPI_Request *request,
                int32_t const block,
                mpp_c_context_t * const * const context)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL;             /*Pointer to the mpp_c_context_t object.*/
    MPI_Datatype mpi_type_val;                   /*MPI type parameter value.*/
    MPI_Comm tmp_comm = MPI_COMM_NULL;           /*World pelist communicator id.*/
    MPI_Request comm_request = MPI_REQUEST_NULL; /*Local MPI request handle.*/
    MPI_Status comm_stat;                        /*Local MPI status variable.*/
    int32_t recv_size = 0;                       /*Size of data actually received.*/
    int32_t ierr = 0;                            /*MPI error code.*/
    char *error_mesg = NULL;                     /*Error message pointer.*/
    char *event_name = NULL;                     /*Event name.*/
    size_t num_bytes = 0;                        /*Number of bytes communicated.*/

    /*Set the convenience pointer.*/
    tmp_ptr = *context;

    /*Make sure that mpp is initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_RECV");

    /*Log the communication event starting time.*/
    event_name = "Recv";
    mpp_c_context_log_event_start_time(&tmp_ptr,NULL,event_name,
                                       strlen(event_name),EVENT_RECV,
                                       MPP_C_NULL_TIMER_ID);

    /*Get the MPI type parameter value.*/
    mpi_type_val = mpp_c_datatype_get_MPI_param(mpp_type);

    /*Get the communicator id for the world pelist.*/
    tmp_comm = mpp_c_context_get_world_pelist_comm_id(tmp_ptr);

    /*Perform a MPI receive.*/
    if (mpp_c_context_is_rank_on_world_pelist(tmp_ptr,(uint32_t)from_rank))
    {
        if (block == MPP_C_TRUE)
        {
            /*Blocking receive.*/
            ierr = MPI_Recv(*recv_data,recv_len,mpi_type_val,from_rank,tag,
                            tmp_comm,&comm_stat);
            check_MPI_error_code(ierr,"MPI_Recv","MPP_C_RECV");
            ierr = MPI_Get_count(&comm_stat,mpi_type_val,&recv_size);
            check_MPI_error_code(ierr,"MPI_Get_count","MPP_C_RECV");
            if (recv_len != (size_t)recv_size)
            {
                error_mesg = "MPP_C_RECV: received wrong size data.";
                mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
            }
        }
        else
        {
            /*Non-blocking receive.*/
            ierr = MPI_Irecv(*recv_data,recv_len,mpi_type_val,from_rank,tag,
                             tmp_comm,&comm_request);
            check_MPI_error_code(ierr,"MPI_Irecv","MPP_C_RECV");

            /*Process the MPI request.*/
            if (request != NULL)
            {
                *request = comm_request;
            }
            else
            {
                mpp_c_context_add_new_recv_request(&tmp_ptr,comm_request,
                                                   recv_len,mpi_type_val);
            }
        }
    }
    else if (from_rank == ANY_RANK)
    {
        /*Blocking receive from any rank.*/
        ierr = MPI_Recv(*recv_data,recv_len,mpi_type_val,MPI_ANY_SOURCE,tag,
                        tmp_comm,&comm_stat);
        check_MPI_error_code(ierr,"MPI_Recv","MPP_C_RECV");
        ierr = MPI_Get_count(&comm_stat,mpi_type_val,&recv_size);
        check_MPI_error_code(ierr,"MPI_Get_count","MPP_C_RECV");
        if (recv_len != (size_t)recv_size)
        {
            error_mesg = "MPP_C_RECV: received wrong size data.";
            mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
        }
    }
    else
    {
        error_mesg = "MPP_C_RECV: receiving from an invalid pe.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }

    /*Log the communication event stopping time.*/
    num_bytes = get_MPI_data_size_in_bytes(mpi_type_val,recv_len);
    mpp_c_context_log_event_stop_time(&tmp_ptr,NULL,num_bytes);

    /*Nullify the convenience pointers.*/
    tmp_ptr = NULL;
    error_mesg = NULL;
    event_name = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
