#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "helper_functions.h"
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"
#include "mpp_c_error.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_gatherv.h"
#include "mpp_c_macros.h"
#include "mpp_c_recv.h"
#include "mpp_c_send.h"

/*---------------------------------------------------------------------------*/
/*Gather variable length data from all ranks on a pelist to the first rank
  of the pelist. If a null pelist is inputted, then the current pelist is
  used.*/
void mpp_c_gatherv(void *send_data,
                   size_t const send_len,
                   void **recv_data,
                   size_t const recv_data_size,
                   size_t const *recv_len_array,
                   size_t const recv_len_array_size,
                   mpp_c_datatype_t const mpp_type,
                   int32_t *rank_list,
                   size_t const rank_list_size,
                   mpp_c_context_t * const * const context)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/
    uint32_t gatherv_root = 0;       /*Root rank for the gatherv.*/
    uint32_t my_rank = 0;            /*Rank id for the current process.*/
    char *error_mesg = NULL;         /*Error message pointer.*/
    uint32_t *tmp_rank_list = NULL;  /*Pointer to rank list of the pelist.*/
    size_t num_ranks = 0;            /*Size of the pelist.*/
    size_t num_bytes = 0;            /*Size of mpp_type in bytes.*/
    size_t total_recv_size = 0;      /*Size of all gathered data.*/
    unsigned char *data_ptr = NULL;  /*Pointer to the data.*/
    unsigned int i = 0;              /*Loop variable.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *context;

    /*Make sure that mpp is initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_GATHERV");

    /*Set the pelist.  If necessary use the internal work buffer.*/
    if (rank_list == NULL)
    {
        num_ranks = mpp_c_context_get_cur_pelist_rank_list_size(tmp_ptr);
        tmp_rank_list = (uint32_t *)mpp_c_context_use_work_buffer(&tmp_ptr,
                                                   sizeof(uint32_t)*num_ranks);
        for (i=0;i<num_ranks;i++)
        {
            tmp_rank_list[i] = i +
                               mpp_c_context_get_cur_pelist_root_rank(tmp_ptr);
        }
    }
    else
    {
        check_size_input(rank_list_size,"MPP_C_GATHERV");
        num_ranks = rank_list_size;
        tmp_rank_list = (uint32_t *)rank_list;
    }

    /*Make sure that the rank id of the current process is included in the
      tmp_rank_list array.*/
    my_rank = mpp_c_context_get_world_rank(tmp_ptr);
    if (!is_int_in_int_array(my_rank,tmp_rank_list,num_ranks))
    {
        error_mesg = "MPP_C_GATHERV: the current rank is not included in"
                     " the gatherv.  This may cause a send request to be"
                     " posted without a matching receive request.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }

    /*Make sure that all ranks whose rank ids are contained in the
      tmp_rank_list are present. If MPI is used and any ranks are missing,
      this routine should hang because there will be some outstanding
      send/receive requests that cannot be completed.*/

    /*Set the gatherv root to be the rank id in the world pelist of the first
      element of the pelist.*/
    gatherv_root = tmp_rank_list[0];

    /*Perform the gatherv.*/
    if (my_rank == gatherv_root)
    {
        /*Check array input.*/
        check_array_input((void *)(*recv_data),recv_data_size,"MPP_C_GATHERV");
        check_array_input((void *)(recv_len_array),recv_len_array_size,
                          "MPP_C_GATHERV");

        /*Make sure that the length of the recv_len_array matches the total
          number of ranks in the pelist.*/
        if (recv_len_array_size != num_ranks)
        {
            error_mesg = "MPP_C_GATHERV: the recv_len_array is not the correct"
                         " size.";
            mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
        }

        /*Make sure that the recv_data array is large enough to hold all the
          data that is to be gathered.*/
        total_recv_size = 0;
        for (i=0;i<num_ranks;i++)
        {
            total_recv_size = total_recv_size + recv_len_array[i];
        }
        if (recv_data_size < total_recv_size)
        {
            error_mesg = "MPP_C_GATHERV: the recv_data array is not large"
                         " enough to store all the data that will be"
                         " gathered.";
            mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
        }

        /*Make sure that the inputted send_len value is not greater than the
          expected size stored in the recv_len_array array.*/
        if (send_len > recv_len_array[0])
        {
            error_mesg = "MPP_C_GATHERV: send_len is greater than the"
                         " expected size stored in recv_len_array[0]"
                         " for the gather root rank.";
            mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
        }

        /*Gather the data.*/
        data_ptr = (unsigned char *)(*recv_data);
        num_bytes = mpp_c_datatype_get_num_bytes(mpp_type);
        if (recv_len_array[0] > 0)
        {
            check_pointer_input((void *)send_data,"MPP_C_GATHERV");
            memcpy((void *)data_ptr,send_data,num_bytes*recv_len_array[0]);
        }
        for (i=1;i<num_ranks;i++)
        {
            data_ptr = data_ptr + num_bytes*recv_len_array[i-1];
            if (recv_len_array[i] > 0)
            {
                mpp_c_recv((void **)&data_ptr,recv_len_array[i],
                           tmp_rank_list[i],mpp_type,MPP_C_GATHERV_TAG,
                           NULL,MPP_C_FALSE,&tmp_ptr);
            }
        }
        mpp_c_context_complete_all_recv_requests(&tmp_ptr);
    }
    else
    {
        if (send_len > 0)
        {
            /*Check input array.*/
            check_array_input((void *)send_data,send_len,"MPP_C_GATHERV");

            /*Send the data to the gather root.*/
            mpp_c_send((void **)&send_data,send_len,gatherv_root,mpp_type,
                       MPP_C_GATHERV_TAG,NULL,&tmp_ptr);
        }
        mpp_c_context_complete_all_send_requests(&tmp_ptr);
    }

    /*If necessary release the internal work buffer.*/
    if (rank_list == NULL)
    {
        mpp_c_context_release_work_buffer(&tmp_ptr,(void *)tmp_rank_list);
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    error_mesg = NULL;
    tmp_rank_list = NULL;
    data_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
