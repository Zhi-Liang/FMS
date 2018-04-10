#ifdef _C_ONLY_GNU
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "mpp_c_alltoall.h"
#include "mpp_c_alltoallv.h"
#include "mpp_c_broadcast.h"
#include "mpp_c_chksum.h"
#include "mpp_c_context_t_api.h"
#include "mpp_c_error.h"
#include "mpp_c_exit.h"
#include "mpp_c_gather.h"
#include "mpp_c_gather_pelist.h"
#include "mpp_c_gatherv.h"
#include "mpp_c_init.h"
#include "mpp_c_macros.h"
#include "mpp_c_max.h"
#include "mpp_c_min.h"
#include "mpp_c_mpi.h"
#include "mpp_c_read_nml_file.h"
#include "mpp_c_recv.h"
#include "mpp_c_routine_wrappers_gnu.h"
#include "mpp_c_scatter_pelist.h"
#include "mpp_c_send.h"
#include "mpp_c_shared_t_api.h"
#include "mpp_c_sum.h"
#include "mpp_c_sync.h"

#define MPP_C_NULL_RANK_LIST -1

/*---------------------------------------------------------------------------*/
/*Initial mpp_c.*/
void mpp_init(mpp_c_shared_t **shared,
              mpp_c_context_t **context,
              int const local_comm)
{
    /*Local variables*/
    mpp_c_shared_t *tmp_shared = NULL;   /*Convenience pointer.*/
    mpp_c_context_t *tmp_context = NULL; /*Convenience pointer.*/
    MPI_Comm tmp_comm = MPI_COMM_NULL;   /*MPI communicator id.*/

    /*Set the pointer to the mpp_c_shared_t object.*/
    tmp_shared = *shared;

    /*Set the pointer to the mpp_c_context_t object.*/
    tmp_context = *context;

    /*Get the MPI_Comm value.*/
    tmp_comm = (MPI_Comm)local_comm;

    /*Initialize the mpp_c library.*/
    mpp_c_init(&tmp_shared,&tmp_context,tmp_comm);

    /*Point shared to the newly allocated mpp_c_shared_t object.*/
    *shared = tmp_shared;

    /*Point context to the newly allocated mpp_c_context_t object.*/
    *context = tmp_context;

    /*Nullify local pointers.*/
    tmp_shared = NULL;
    tmp_context = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Finalize mpp_c.*/
void mpp_exit(mpp_c_shared_t **shared,
              mpp_c_context_t **context)
{
    /*Local variables*/
    mpp_c_shared_t *tmp_shared = NULL;   /*Convenience pointer.*/
    mpp_c_context_t *tmp_context = NULL; /*Convenience pointer.*/

    /*Set the pointer to the mpp_c_shared_t object.*/
    tmp_shared = *shared;

    /*Set the pointer to the mpp_c_context_t object.*/
    tmp_context = *context;

    /*Destruct the mpp_c_context_t object.*/
    mpp_c_exit(&tmp_shared,&tmp_context);

    /*Point shared to the deallocated mpp_c_shared_t object, to nullify it.*/
    *shared = tmp_shared;

    /*Point context to the deallocated mpp_c_context_t object, to nullify it.*/
    *context = tmp_context;

    /*Nullify local pointers.*/
    tmp_shared = NULL;
    tmp_context = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Get the rank id of the process in the world pelist.*/
int32_t mpp_pe(mpp_c_context_t const * const context)
{
    return (int32_t)mpp_c_context_get_world_rank(context);
}

/*---------------------------------------------------------------------------*/
/*Get the number the size of the rank list of the current pelist.*/
size_t mpp_npes(mpp_c_context_t const * const context)
{
    return mpp_c_context_get_cur_pelist_rank_list_size(context);
}

/*---------------------------------------------------------------------------*/
/*Get the root rank id of the current pelist.*/
int32_t mpp_root_pe(mpp_c_context_t const * const context)
{
    return (int32_t)mpp_c_context_get_cur_pelist_root_rank(context);
}

/*---------------------------------------------------------------------------*/
/*Set the root rank id of the current pelist.*/
void mpp_set_root_pe(mpp_c_context_t * const * const context,
                     int32_t const root_rank)
{
    mpp_c_context_set_cur_pelist_root_rank(context,(uint32_t)root_rank);

    return;
}

/*---------------------------------------------------------------------------*/
/*Create a new pelist.*/
void mpp_declare_pelist(mpp_c_context_t * const * const context,
                        char const *pelist_name,
                        size_t const pelist_name_len,
                        int32_t const *pelist,
                        size_t const pelist_size)
{
    mpp_c_context_add_new_pelist(context,pelist_name,pelist_name_len,
                                 (uint32_t *)pelist,pelist_size);

    return;
}

/*---------------------------------------------------------------------------*/
/*Set the current pelist.*/
void mpp_set_current_pelist(mpp_c_context_t * const * const context,
                            int32_t const *rank_list,
                            size_t const rank_list_size,
                            int8_t const no_sync_flag)
{
    mpp_c_context_set_cur_pelist(context,(uint32_t *)rank_list,
                                 rank_list_size,no_sync_flag);

    return;
}

/*---------------------------------------------------------------------------*/
/*Get the length of the name of the current pelist.*/
size_t mpp_get_current_pelist_name_len(mpp_c_context_t const * const context)
{
    return mpp_c_context_get_cur_pelist_name_len(context);
}

/*---------------------------------------------------------------------------*/
/*Get the name of the current pelist.*/
char *mpp_get_current_pelist_name(mpp_c_context_t const * const context)
{
    return mpp_c_context_get_cur_pelist_name(context);
}

/*---------------------------------------------------------------------------*/
/*Get the communicator id for the current pelist.*/
int mpp_get_current_pelist_comm_id(mpp_c_context_t const * const context)
{
    return (int)mpp_c_context_get_cur_pelist_comm_id(context);
}

/*---------------------------------------------------------------------------*/
/*Get the group id for the current pelist.*/
int mpp_get_current_pelist_group_id(mpp_c_context_t const * const context)
{
    return (int)mpp_c_context_get_cur_pelist_group_id(context);
}

/*---------------------------------------------------------------------------*/
/*Get the list of world ranks for the current pelist.*/
int32_t *mpp_get_current_pelist(mpp_c_context_t const * const context)
{
    return (int32_t *)mpp_c_context_get_cur_pelist_rank_list(context);
}

/*---------------------------------------------------------------------------*/
/*Set the grainularity for the timers.*/
void mpp_clock_set_grain(mpp_c_context_t * const * const context,
                         int32_t const grain)
{
    mpp_c_context_set_timer_grain(context,grain);

    return;
}

/*---------------------------------------------------------------------------*/
/*Create a new timer and return its id.*/
int32_t mpp_clock_id(mpp_c_context_t * const * const context,
                     char const *timer_name,
                     size_t const timer_name_len,
                     int32_t const grain,
                     int8_t const sync_flag,
                     int8_t const detail_flag)
{
    return mpp_c_context_add_new_timer(context,timer_name,timer_name_len,grain,
                                       sync_flag,detail_flag);
}

/*---------------------------------------------------------------------------*/
/*Start a timer.*/
void mpp_clock_begin(mpp_c_context_t * const * const context,
                     int32_t const timer_id)
{
    mpp_c_context_start_timer(context,timer_id);

    return;
}

/*---------------------------------------------------------------------------*/
/*Stop a timer.*/
void mpp_clock_end(mpp_c_context_t * const * const context,
                   int32_t const timer_id)
{
    mpp_c_context_stop_timer(context,timer_id);

    return;
}

/*---------------------------------------------------------------------------*/
/*Allow timers.*/
void mpp_record_time_start(mpp_c_context_t * const * const context)
{
    mpp_c_context_set_record_timing_data(context,MPP_C_TRUE);

    return;
}

/*---------------------------------------------------------------------------*/
/*Do not allow timers.*/
void mpp_record_time_end(mpp_c_context_t * const * const context)
{
    mpp_c_context_set_record_timing_data(context,MPP_C_FALSE);

    return;
}
/*---------------------------------------------------------------------------*/
/*Read a .nml file and store it in an internal character buffer.*/
void read_input_nml(mpp_c_context_t **context)
{
    mpp_c_read_nml_file(context);

    return;
}

/*---------------------------------------------------------------------------*/
/*Get the length of the namelist buffer.*/
size_t mpp_get_namelist_buffer_len(mpp_c_context_t const * const context)
{
    return mpp_c_context_get_namelist_buffer_len(context);
}

/*---------------------------------------------------------------------------*/
/*Get the namelist buffer.*/
char *mpp_get_namelist_buffer(mpp_c_context_t const * const context)
{
    return mpp_c_context_get_namelist_buffer(context);
}

/*---------------------------------------------------------------------------*/
/*Write out error messages.*/
void mpp_error(int32_t const error_type,
               char const *error_mesg,
               size_t const mesg_len,
               mpp_c_context_t const * const context)
{
    mpp_c_error(error_type,error_mesg,mesg_len,context);

    return;
}

/*---------------------------------------------------------------------------*/
/*Sychronize all ranks on a pelist at a barrier.*/
void mpp_sync(int32_t const *rank_list,
              size_t const rank_list_size,
              mpp_c_context_t * const * const context)
{
    mpp_c_sync(rank_list,rank_list_size,context);

    return;
}

/*---------------------------------------------------------------------------*/
/*Complete send/recv requests.*/
void mpp_sync_self(int32_t const comm_type,
                   int *request,
                   size_t const num_requests,
                   size_t const *msg_size,
                   int32_t const *msg_type,
                   mpp_c_context_t * const * const context)
{
    /*Local variables*/
    char *error_mesg = NULL; /*Error message pointer.*/

    if (request != NULL)
    {
        mpp_c_sync_self(comm_type,request,num_requests,msg_size,msg_type,
                        context);
    }
    else
    {
        if (comm_type == EVENT_SEND)
        {
            mpp_c_context_complete_all_send_requests(context);
        }
        else if (comm_type == EVENT_RECV)
        {
            mpp_c_context_complete_all_recv_requests(context);
        }
        else
        {
            error_mesg = "MPP_SYNC_SELF: invalid value of comm_type.";
            mpp_c_error(FATAL,error_mesg,strlen(error_mesg),*context);
        }

    }

    /*Nullify local pointers.*/
    error_mesg = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Send a message.*/
void mpp_send(void **send_data,
              size_t const send_len,
              int32_t const to_rank,
              int32_t const mpp_type,
              int32_t const tag,
              int *request,
              mpp_c_context_t * const * const context)
{
    mpp_c_send(send_data,send_len,to_rank,mpp_type,tag,
               (MPI_Request *)request,context);

    return;
}

/*---------------------------------------------------------------------------*/
/*Receive a message.*/
void mpp_recv(void **recv_data,
              size_t const recv_len,
              int32_t const from_rank,
              int32_t const mpp_type,
              int32_t const tag,
              int *request,
              int32_t const block,
              mpp_c_context_t * const * const context)
{
    mpp_c_recv(recv_data,recv_len,from_rank,mpp_type,tag,
               (MPI_Request *)request,block,context);

    return;
}

/*---------------------------------------------------------------------------*/
/*Broadcast a message.*/
void mpp_broadcast(void **bcast_data,
                   size_t const bcast_len,
                   int32_t from_rank,
                   int32_t const mpp_type,
                   int32_t const *rank_list,
                   size_t const rank_list_size,
                   mpp_c_context_t * const * const context)
{
    mpp_c_broadcast(bcast_data,bcast_len,from_rank,mpp_type,rank_list,
                    rank_list_size,context);

    return;
}

/*---------------------------------------------------------------------------*/
/*Sum data across ranks.*/
void mpp_sum(void **sum_data,
             size_t const sum_len,
             int32_t const mpp_type,
             int32_t const *rank_list,
             size_t const rank_list_size,
             mpp_c_context_t * const * const context)
{
    mpp_c_sum(sum_data,sum_len,mpp_type,rank_list,rank_list_size,context);

    return;
}

/*---------------------------------------------------------------------------*/
/*Get the maximum value of data across ranks.*/
void mpp_max(void **max_data,
             size_t const max_len,
             int32_t const mpp_type,
             int32_t const *rank_list,
             size_t const rank_list_size,
             mpp_c_context_t * const * const context)
{
    mpp_c_max(max_data,max_len,mpp_type,rank_list,rank_list_size,context);

    return;
}

/*---------------------------------------------------------------------------*/
/*Get the minimum value of data across ranks.*/
void mpp_min(void **min_data,
             size_t const min_len,
             int32_t const mpp_type,
             int32_t const *rank_list,
             size_t const rank_list_size,
             mpp_c_context_t * const * const context)
{
    mpp_c_min(min_data,min_len,mpp_type,rank_list,rank_list_size,context);

    return;
}

/*---------------------------------------------------------------------------*/
/*Calculate a check-sum.*/
int64_t mpp_chksum(void *chksum_data,
                   size_t const chksum_len,
                   size_t const num_bytes,
                   void const *mask_val,
                   int32_t const *rank_list,
                   size_t const rank_list_size,
                   mpp_c_context_t * const * const context)
{
    /*Local variables*/
    int64_t chksum_val = 0; /*Resulting check-sum.*/

    chksum_val = mpp_c_chksum(chksum_data,chksum_len,num_bytes,mask_val,
                              rank_list,rank_list_size,context);

    return chksum_val;
}

/*---------------------------------------------------------------------------*/
/*Gather uniform length data across all ranks on a pelist.*/
void mpp_gather(void *send_data,
                size_t const send_len,
                void **recv_data,
                size_t const recv_len,
                int32_t const mpp_type,
                int32_t *rank_list,
                size_t const rank_list_size,
                mpp_c_context_t * const * const context)
{
    mpp_c_gather(send_data,send_len,recv_data,recv_len,mpp_type,rank_list,
                 rank_list_size,context);

    return;
}

/*---------------------------------------------------------------------------*/
/*Gather variable length data across all ranks on a pelist.*/
void mpp_gatherv(void *send_data,
                 size_t const send_len,
                 void **recv_data,
                 size_t const recv_data_size,
                 size_t const *recv_len_array,
                 size_t const recv_len_array_size,
                 int32_t const mpp_type,
                 int32_t *rank_list,
                 size_t const rank_list_size,
                 mpp_c_context_t * const * const context)
{
    mpp_c_gatherv(send_data,send_len,recv_data,recv_data_size,
                  recv_len_array,recv_len_array_size,mpp_type,rank_list,
                  rank_list_size,context);

    return;
}

/*---------------------------------------------------------------------------*/
/*Perform a generalized gather of array segments across all ranks on a rank
  list.*/
void mpp_gather_pelist(void **gather_data,
                       size_t const gather_data_x_size,
                       size_t const gather_data_y_size,
                       size_t const gather_data_z_size,
                       int32_t const mpp_type,
                       void **array_segment,
                       int32_t const gather_spot_x_start_index,
                       int32_t const gather_spot_x_end_index,
                       int32_t const gather_spot_y_start_index,
                       int32_t const gather_spot_y_end_index,
                       int32_t const gather_spot_z_start_index,
                       int32_t const gather_spot_z_end_index,
                       int32_t const *rank_list,
                       size_t const rank_list_size,
                       int32_t const rank_list_root_flag,
                       int32_t const root_gather_spot_x_offset,
                       int32_t const root_gather_spot_y_offset,
                       int32_t const root_gather_spot_z_offset,
                       mpp_c_context_t * const * const context)
{
    mpp_c_gather_pelist(gather_data,gather_data_x_size,gather_data_y_size,
                        gather_data_z_size,mpp_type,array_segment,
                        gather_spot_x_start_index,gather_spot_x_end_index,
                        gather_spot_y_start_index,gather_spot_y_end_index,
                        gather_spot_z_start_index,gather_spot_z_end_index,
                        rank_list,rank_list_size,rank_list_root_flag,
                        root_gather_spot_x_offset,root_gather_spot_y_offset,
                        root_gather_spot_z_offset,context);

    return;
}

/*---------------------------------------------------------------------------*/
/*Perform a generalized scatter of array segments across all ranks on a rank
  list.*/
void mpp_scatter_pelist(void **scatter_data,
                        size_t const scatter_data_x_size,
                        size_t const scatter_data_y_size,
                        size_t const scatter_data_z_size,
                        int32_t const mpp_type,
                        void **array_segment,
                        int32_t const scatter_spot_x_start_index,
                        int32_t const scatter_spot_x_end_index,
                        int32_t const scatter_spot_y_start_index,
                        int32_t const scatter_spot_y_end_index,
                        int32_t const scatter_spot_z_start_index,
                        int32_t const scatter_spot_z_end_index,
                        int32_t const *rank_list,
                        size_t const rank_list_size,
                        int32_t const rank_list_root_flag,
                        int32_t const root_scatter_spot_x_offset,
                        int32_t const root_scatter_spot_y_offset,
                        int32_t const root_scatter_spot_z_offset,
                        mpp_c_context_t * const * const context)
{
    mpp_c_scatter_pelist(scatter_data,scatter_data_x_size,scatter_data_y_size,
                         scatter_data_z_size,mpp_type,array_segment,
                         scatter_spot_x_start_index,scatter_spot_x_end_index,
                         scatter_spot_y_start_index,scatter_spot_y_end_index,
                         scatter_spot_z_start_index,scatter_spot_z_end_index,
                         rank_list,rank_list_size,rank_list_root_flag,
                         root_scatter_spot_x_offset,root_scatter_spot_y_offset,
                         root_scatter_spot_z_offset,context);

    return;
}

/*---------------------------------------------------------------------------*/
/*Perform an alltoall communication across a pelist.*/
void mpp_alltoall(void *send_data,
                  size_t const send_len,
                  void **recv_data,
                  size_t const recv_len,
                  size_t const size_per_send,
                  size_t const size_per_recv,
                  int32_t const mpp_type,
                  int32_t const *rank_list,
                  size_t const rank_list_size,
                  mpp_c_context_t * const * const context)
{
    mpp_c_alltoall(send_data,send_len,recv_data,recv_len,size_per_send,
                   size_per_recv,mpp_type,rank_list,rank_list_size,context);

    return;
}

/*---------------------------------------------------------------------------*/
/*Perform a variable alltoall communication across a pelist.*/
void mpp_alltoallv(void *send_data,
                   int32_t *send_len,
                   int32_t *send_displace,
                   void **recv_data,
                   int32_t *recv_len,
                   int32_t *recv_displace,
                   int32_t const mpp_type,
                   int32_t const *rank_list,
                   size_t const rank_list_size,
                   mpp_c_context_t * const * const context)
{
    mpp_c_alltoallv(send_data,send_len,send_displace,recv_data,recv_len,
                    recv_displace,mpp_type,rank_list,rank_list_size,
                    context);

    return;
}

/*---------------------------------------------------------------------------*/

#endif
