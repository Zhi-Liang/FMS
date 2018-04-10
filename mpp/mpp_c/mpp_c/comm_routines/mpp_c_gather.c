#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "helper_functions.h"
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"
#include "mpp_c_error.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_gather.h"
#include "mpp_c_macros.h"
#include "mpp_c_recv.h"
#include "mpp_c_send.h"

/*---------------------------------------------------------------------------*/
/*Gather data from all ranks on a pelist to the first rank of the pelist. If
  a null pelist is inputted, then the current pelist is used.*/
void mpp_c_gather(void *send_data,
                  size_t const send_len,
                  void **recv_data,
                  size_t const recv_len,
                  mpp_c_datatype_t const mpp_type,
                  int32_t *rank_list,
                  size_t const rank_list_size,
                  mpp_c_context_t * const * const context)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/
    uint32_t gather_root = 0;        /*Root rank for the gather.*/
    uint32_t my_rank = 0;            /*Rank id for the current process.*/
    char *error_mesg = NULL;         /*Error message pointer.*/
    uint32_t *tmp_rank_list = NULL;  /*Pointer to rank list of the pelist.*/
    size_t num_ranks = 0;            /*Size of the pelist.*/
    size_t num_bytes = 0;            /*Size of mpp_type in bytes.*/
    unsigned char *data_ptr = NULL;  /*Pointer to the data.*/
    unsigned int i = 0;              /*Loop variable.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *context;

    /*Make sure that mpp is initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_GATHER");

    /*Check array input.*/
    check_array_input((void *)send_data,send_len,"MPP_C_GATHER");
    check_array_input((void *)(*recv_data),recv_len,"MPP_C_GATHER");

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
        check_size_input(rank_list_size,"MPP_C_GATHER");
        num_ranks = rank_list_size;
        tmp_rank_list = (uint32_t *)rank_list;
    }

    /*Make sure that the rank id of the current process is included in the
      tmp_rank_list array.*/
    my_rank = mpp_c_context_get_world_rank(tmp_ptr);
    if (!is_int_in_int_array(my_rank,tmp_rank_list,num_ranks))
    {
        error_mesg = "MPP_C_GATHER: the current rank is not included in"
                     " the gather.  This will cause a send request to be"
                     " posted without a matching receive request.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }

    /*Make sure that the recv_data array is large enough to hold all the
      data that will be sent.*/
    if (recv_len < send_len*num_ranks)
    {
        error_mesg = "MPP_C_GATHER: the recv_data array is not large enough.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }

    /*Set the gather root to be the rank id in the world pelist of the first
      element of the pelist.*/
    gather_root = tmp_rank_list[0];

    /*Perform the gather.*/
    if (my_rank == gather_root)
    {
        data_ptr = (unsigned char *)(*recv_data);
        num_bytes = mpp_c_datatype_get_num_bytes(mpp_type);
        memcpy((void *)data_ptr,send_data,num_bytes*send_len);
        for (i=1;i<num_ranks;i++)
        {
            data_ptr = data_ptr + num_bytes*send_len;
            mpp_c_recv((void **)&data_ptr,send_len,tmp_rank_list[i],
                       mpp_type,MPP_C_GATHER_TAG,NULL,MPP_C_FALSE,
                       &tmp_ptr);
        }
        mpp_c_context_complete_all_recv_requests(&tmp_ptr);
    }
    else
    {
        mpp_c_send((void **)&send_data,send_len,gather_root,mpp_type,
                   MPP_C_GATHER_TAG,NULL,&tmp_ptr);
        mpp_c_context_complete_all_send_requests(&tmp_ptr);
    }

    /*If necessary, release the internal work buffer.*/
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
