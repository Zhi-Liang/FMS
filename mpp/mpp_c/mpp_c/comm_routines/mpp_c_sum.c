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
#include "mpp_c_pelist_t_api.h"
#include "mpp_c_sum.h"

/*---------------------------------------------------------------------------*/
/*Sum data across all ranks on a pelist. If a null pelist is inputted, then
  the current pelist is used.*/
void mpp_c_sum(void **sum_data,
               size_t const sum_len,
               mpp_c_datatype_t const mpp_type,
               int32_t const *rank_list,
               size_t const rank_list_size,
               mpp_c_context_t * const * const context)
{
    /*Local variables*/
    void *tmp_ptr = NULL;                 /*Pointer to the data.*/
    mpp_c_context_t *tmp_context = NULL;  /*Pointer to the mpp_c_context_t object.*/
    mpp_c_pelist_t *tmp_pelist = NULL;    /*Pointer to a pelist.*/
    MPI_Datatype mpi_type_val;            /*MPI type parameter value.*/
    void *work_buffer = NULL;             /*Work buffer.*/
    size_t sum_data_byte_size = 0;        /*Size of data being summed in bytes.*/
    MPI_Comm tmp_comm_id = MPI_COMM_NULL; /*MPI Communicator id.*/
    char *event_name = NULL;              /*Event name.*/
    char *error_mesg = NULL;              /*Error message pointer.*/
    int32_t ierr = 0;                     /*MPI error code.*/

    /*Set the local pointer to the data.*/
    tmp_ptr = *sum_data;

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_context = *context;

    /*Make sure that mpp is initialized.*/
    mpp_c_context_check_init_state(tmp_context,"MPP_C_SUM");

    /*Check array input.*/
    check_array_input((void *)tmp_ptr,sum_len,"MPP_C_SUM");

    /*Check pelist input.*/
    check_pelist_input((uint32_t *)rank_list,rank_list_size,"MPP_C_SUM");

    /*Point to the appropriate pelist in the peset array.*/
    tmp_pelist = mpp_c_context_get_pelist(tmp_context,(uint32_t *)rank_list,
                                          rank_list_size,MPP_C_TRUE);
    if (tmp_pelist == NULL)
    {
        error_mesg = "MPP_C_SUM: the inputted pelist does not exist."
                     " You must first create it.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }

    /*Perform the sum if the pelist contains more than just the current
      rank.*/
    if (mpp_c_pelist_get_rank_list_size(tmp_pelist) != 1)
    {
        /*Log the communication event starting time.*/
        event_name = "Sum";
        mpp_c_context_log_event_start_time(&tmp_context,NULL,event_name,
                                           strlen(event_name),EVENT_REDUCE,
                                           MPP_C_NULL_TIMER_ID);

        /*Get the MPI type parameter value.*/
        mpi_type_val = mpp_c_datatype_get_MPI_param(mpp_type);

        /*Get the total size of the data in bytes and allocate the work
          buffer.*/
        sum_data_byte_size = get_MPI_data_size_in_bytes(mpi_type_val,sum_len);
        work_buffer = mpp_c_context_use_work_buffer(&tmp_context,
                                                    sum_data_byte_size);

        /*Get the communicator id for the pelist.*/
        tmp_comm_id = mpp_c_pelist_get_comm_id(tmp_pelist);

        /*Sum the data.*/
        ierr = MPI_Allreduce(tmp_ptr,work_buffer,sum_len,
                             mpi_type_val,MPI_SUM,tmp_comm_id);
        check_MPI_error_code(ierr,"MPI_Allreduce","MPP_C_SUM");

        /*Set the inputted data to be equal to the results of the sum.*/
        memcpy(tmp_ptr,work_buffer,sum_data_byte_size);

        /*Release control of the work buffer.*/
        mpp_c_context_release_work_buffer(&tmp_context,work_buffer);

        /*Log the communication event stopping time.*/
        mpp_c_context_log_event_stop_time(&tmp_context,NULL,
                                          sum_data_byte_size);
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_context = NULL;
    tmp_pelist = NULL;
    event_name = NULL;
    work_buffer = NULL;
    error_mesg = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
