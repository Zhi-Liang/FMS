#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "helper_functions.h"
#include "mpp_c_alltoall.h"
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"
#include "mpp_c_error.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"
#include "mpp_c_pelist_t_api.h"

/*---------------------------------------------------------------------------*/
/*Send data from all ranks to all other ranks on a pelist. If a null pelist is
  inputted, then the current pelist is used.*/
void mpp_c_alltoall(void *send_data,
                    size_t const send_len,
                    void **recv_data,
                    size_t const recv_len,
                    size_t const size_per_send,
                    size_t const size_per_recv,
                    mpp_c_datatype_t const mpp_type,
                    int32_t const *rank_list,
                    size_t const rank_list_size,
                    mpp_c_context_t * const * const context)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL;      /*Pointer to the mpp_c_context_t object.*/
    char *error_mesg = NULL;              /*Error message pointer.*/
    char *event_name = NULL;              /*Event name.*/
    size_t num_bytes = 0;                 /*Number of bytes communicated.*/
    mpp_c_pelist_t *tmp_pelist = NULL;    /*Pointer to a pelist.*/
    MPI_Datatype mpi_type_val;            /*MPI type parameter value.*/
    MPI_Comm tmp_comm_id = MPI_COMM_NULL; /*MPI Communicator id.*/
    int32_t ierr = 0;                     /*MPI error code.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *context;

    /*Make sure that mpp is initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_ALLTOALL");

    /*Check array input.*/
    check_array_input((void *)send_data,send_len,"MPP_C_ALLTOALL");
    check_array_input((void *)(*recv_data),recv_len,"MPP_C_ALLTOALL");

    /*Check pelist input.*/
    check_pelist_input((uint32_t *)rank_list,rank_list_size,"MPP_C_ALLTOALL");

    /*Make sure that the send and receive sizes are the same.*/
    if (size_per_send != size_per_recv)
    {
        error_mesg = "MPP_C_ALLTOALL: size_per_send must be equal to"
                     "size_per_recv.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }

    /*Point to the appropriate pelist in the peset array.*/
    tmp_pelist = mpp_c_context_get_pelist(tmp_ptr,(uint32_t *)rank_list,
                                          rank_list_size,MPP_C_TRUE);
    if (tmp_pelist == NULL)
    {
        error_mesg = "MPP_C_ALLTOALL: the inputted pelist does not exist."
                     " You must first create it.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }

    /*Log the communication event starting time.*/
    event_name = "Alltoall";
    mpp_c_context_log_event_start_time(&tmp_ptr,NULL,event_name,
                                       strlen(event_name),EVENT_ALLTOALL,
                                       MPP_C_NULL_TIMER_ID);

    /*Get the communicator id for the pelist.*/
    tmp_comm_id = mpp_c_pelist_get_comm_id(tmp_pelist);

    /*Get the MPI type parameter value.*/
    mpi_type_val = mpp_c_datatype_get_MPI_param(mpp_type);

    /*Send/receive the data.*/
    ierr = MPI_Alltoall(send_data,size_per_send,mpi_type_val,*recv_data,
                        size_per_recv,mpi_type_val,tmp_comm_id);
    check_MPI_error_code(ierr,"MPI_Alltoall","MPP_C_ALLTOALL");

    /*Log the communication event stopping time.*/
    num_bytes = get_MPI_data_size_in_bytes(mpi_type_val,recv_len);
    mpp_c_context_log_event_stop_time(&tmp_ptr,NULL,num_bytes);

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    error_mesg = NULL;
    tmp_pelist = NULL;
    event_name = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
