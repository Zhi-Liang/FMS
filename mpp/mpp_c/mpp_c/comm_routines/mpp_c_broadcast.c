#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "helper_functions.h"
#include "mpp_c_broadcast.h"
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"
#include "mpp_c_error.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"
#include "mpp_c_pelist_t_api.h"

/*---------------------------------------------------------------------------*/
/*Broadcast data from one rank to all others in the same pelist.  The
  inputted from_rank should be the rank id of the broadcast root in the
  current pelist.*/
void mpp_c_broadcast(void **bcast_data,
                     size_t const bcast_len,
                     int32_t from_rank,
                     mpp_c_datatype_t const mpp_type,
                     int32_t const *rank_list,
                     size_t const rank_list_size,
                     mpp_c_context_t * const * const context)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL;                   /*Pointer to the mpp_c_context_t object.*/
    mpp_c_pelist_t *tmp_pelist = NULL;                /*Pointer to a pelist.*/
    MPI_Datatype mpi_type_val;                        /*MPI type parameter value.*/
    MPI_Group world_pelist_group_id = MPI_GROUP_NULL; /*Group id for the world pelist.*/
    MPI_Group in_pelist_group_id = MPI_GROUP_NULL;    /*Group id for the inputted pelist.*/
    int32_t in_pelist_bcast_root = 0;                 /*Broadcast root rank id for the inputted pelist.*/
    MPI_Comm in_pelist_comm_id = MPI_COMM_NULL;       /*Communicator id for the inputted pelist.*/
    char *error_mesg = NULL;                          /*Error message pointer.*/
    char *event_name = NULL;                          /*Event name.*/
    size_t num_bytes = 0;                             /*Number of bytes communicated.*/
    int32_t ierr = 0;                                 /*MPI error code.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *context;

    /*Make sure that mpp is initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_BROADCAST");

    /*Check array input.*/
    check_array_input((void *)(*bcast_data),bcast_len,"MPP_C_BROADCAST");

    /*Check pelist input.*/
    check_pelist_input((uint32_t *)rank_list,rank_list_size,"MPP_C_BROADCAST");

    /*Point to the appropriate pelist in the peset array.*/
    tmp_pelist = mpp_c_context_get_pelist(tmp_ptr,(uint32_t *)rank_list,
                                          rank_list_size,MPP_C_TRUE);
    if (tmp_pelist == NULL)
    {
        error_mesg = "MPP_C_BROADCAST: the inputted pelist does not exist."
                     " You must first create it.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }

    /*Make sure that the inputted broadcast root rank is on the appropriate
      pelist.*/
    if (!mpp_c_pelist_is_rank_on_rank_list(tmp_pelist,(uint32_t)from_rank))
    {
        error_mesg = "MPP_C_BROADCAST: the inputted broadcast root must be"
                     " on the pelist which will perform the broadcast.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }

    /*Perform the broadcast if the pelist contains more than just the current
      rank.*/
    if (mpp_c_pelist_get_rank_list_size(tmp_pelist) != 1)
    {
        /*Log the communication event starting time.*/
        event_name = "Broadcast";
        mpp_c_context_log_event_start_time(&tmp_ptr,NULL,event_name,
                                           strlen(event_name),EVENT_BCAST,
                                           MPP_C_NULL_TIMER_ID);

        /*Get the broadcast root's rank id in the inputted pelist.*/
        world_pelist_group_id = mpp_c_context_get_world_pelist_group_id(tmp_ptr);
        in_pelist_group_id = mpp_c_pelist_get_group_id(tmp_pelist);
        ierr = MPI_Group_translate_ranks(world_pelist_group_id,1,
                                         &from_rank,
                                         in_pelist_group_id,
                                         &in_pelist_bcast_root);
        check_MPI_error_code(ierr,"MPI_Group_translate_ranks",
                             "MPP_C_BROADCAST");

        /*Get the communicator id for the inputted pelist.*/
        in_pelist_comm_id = mpp_c_pelist_get_comm_id(tmp_pelist);

        /*Get the MPI type parameter value.*/
        mpi_type_val = mpp_c_datatype_get_MPI_param(mpp_type);

        /*Broadcast the data.*/
        ierr = MPI_Bcast(*bcast_data,bcast_len,mpi_type_val,
                         in_pelist_bcast_root,in_pelist_comm_id);
        check_MPI_error_code(ierr,"MPI_Bcast","MPP_C_BROADCAST");

        /*Log the communication event stopping time.*/
        num_bytes = get_MPI_data_size_in_bytes(mpi_type_val,bcast_len);
        mpp_c_context_log_event_stop_time(&tmp_ptr,NULL,num_bytes);
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_pelist = NULL;
    error_mesg = NULL;
    event_name = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
