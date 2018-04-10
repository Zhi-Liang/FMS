#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "helper_functions.h"
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"
#include "mpp_c_error.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_gather_pelist.h"
#include "mpp_c_macros.h"
#include "mpp_c_max.h"
#include "mpp_c_recv.h"
#include "mpp_c_send.h"

/*---------------------------------------------------------------------------*/
/*Gather array segments onto a rank of a pelist from all other ranks in the
  pelist and store them in the gather_data array.  Conceptually, the gather
                            can be viewed like the picture, where say rank 3
 |-----------|-----------|  fills in the remaining values of its gather_array
 | array     | array     |  with array segments received from the other ranks
 | segment   | segment   |  in the inputted pelist (i.e. rank 0, rank 1,
 | gathered  | gathered  |  and rank 2 in this example.) The starting and
 | from      | from      |  ending indices of each spot in the gather_data
 | rank 0    | rank 1    |  array that will be filled by each rank's array
 |-----------|-----------|  segment are first sent to the root rank, then
 | array     | array     |  the array segment itself is sent and stored
 | segment   | segment   |  in the gather_data array in the spot described
 | gathered  | gathered  |  by the sent indices (plus any offsets).
 | from      | from      |
 | rank 2    | rank 3    |
 |-----------|-----------|*/

void mpp_c_gather_pelist(void **gather_data,
                         size_t const gather_data_x_size,
                         size_t const gather_data_y_size,
                         size_t const gather_data_z_size,
                         mpp_c_datatype_t const mpp_type,
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
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL;         /*Pointer to the mpp_c_context_t object.*/
    char *error_mesg = NULL;                 /*Error message pointer.*/
    int32_t gather_root = 0;                 /*Root rank for the gather.*/
    int32_t *gather_root_ptr = NULL;         /*Pointer to the gather root.*/
    size_t num_dims = 0;                     /*Number of dimensions of gather_data.*/
    size_t spot_index_size = 0;              /*Total number of gather spot indices.*/
    uint32_t *loc_spot_index = NULL;         /*Array of local gather spot indices.*/
    uint32_t *all_spot_index = NULL;         /*Array of all gather spot indices.*/
    uint32_t *spot_ptr = NULL;               /*Pointer to indices in the spot arrays.*/
    size_t x_spot_size = 0;                  /*Size of the x-dimension of the array segment.*/
    size_t y_spot_size = 0;                  /*Size of the y-dimension of the array segment.*/
    size_t z_spot_size = 0;                  /*Size of the z-dimension of the array segment.*/
    size_t array_segment_size = 0;           /*Total size of the array segment.*/
    size_t num_bytes = 0;                    /*Size of gather_data type in bytes.*/
    unsigned char **gather_buffer = NULL;    /*Buffer for gather_data.*/
    unsigned char *gather_data_ptr = NULL;   /*Pointer to gather_data.*/
    unsigned char *array_segment_ptr = NULL; /*Pointer to the array segment.*/
    int32_t gx_index_start = 0;              /*Temporary index variable.*/
    int32_t gx_index_end = 0;                /*Temporary index variable.*/
    int32_t gy_index_start = 0;              /*Temporary index variable.*/
    int32_t gy_index_end = 0;                /*Temporary index variable.*/
    int32_t gz_index_start = 0;              /*Temporary index variable.*/
    int32_t gz_index_end = 0;                /*Temporary index variable.*/
    unsigned int i = 0;                      /*Loop variable.*/
    unsigned int j = 0;                      /*Loop variable.*/
    unsigned int k = 0;                      /*Temporary index variable.*/
    unsigned int x = 0;                      /*Temporary index variable.*/
    unsigned int y = 0;                      /*Temporary index variable.*/
    unsigned int z = 0;                      /*Temporary index variable.*/

    /*Check inputted arrays.*/
/*  check_array_input();*/
/*  check_array_input();*/

    /*Point local pointer at the mpp_c_context_t object.*/
    tmp_ptr = *context;

    /*Make sure that mpp is initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_GATHER_PELIST");

    /*Make sure the inputted pelist is not null.*/
    if (rank_list == NULL)
    {
        error_mesg = "MPP_C_GATHER_PELIST: a null pelist passed in.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }

    /*Make sure only one gather root has been inputted.*/
    if (rank_list_root_flag)
    {
        gather_root = (int32_t)mpp_c_context_get_world_rank(tmp_ptr);
    }
    else
    {
        gather_root = -1;
    }
    gather_root_ptr = &gather_root;
    mpp_c_max((void **)(&gather_root_ptr),1,MPP_INT32,rank_list,
              rank_list_size,&tmp_ptr);
    if (gather_root == -1)
    {
        error_mesg = "MPP_C_GATHER_PELIST: gather root not specified.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }
    if (rank_list_root_flag && gather_root != 
        (int32_t)mpp_c_context_get_world_rank(tmp_ptr))
    {
        error_mesg = "MPP_C_GATHER_PELIST: more than one gather root"
                     " specified.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }

    /*Send the indices to the gather root. Pack the inputted indices into an
      array to minimize the total number of sends/receives.*/
    num_dims = 3;
    spot_index_size = 2*num_dims;
    safemalloc((void **)&loc_spot_index,sizeof(uint32_t)*spot_index_size);
    loc_spot_index[0] = (uint32_t)gather_spot_x_start_index;
    loc_spot_index[1] = (uint32_t)gather_spot_x_end_index;
    loc_spot_index[2] = (uint32_t)gather_spot_y_start_index;
    loc_spot_index[3] = (uint32_t)gather_spot_y_end_index;
    loc_spot_index[4] = (uint32_t)gather_spot_z_start_index;
    loc_spot_index[5] = (uint32_t)gather_spot_z_end_index;
    if (rank_list_root_flag)
    {
        /*Allocate an array to store all the spot indices on the gather root.*/
        safemalloc((void **)&all_spot_index,sizeof(uint32_t)*spot_index_size
                                            *rank_list_size);

        /*Loop through the pelist to gather the spot indices.*/
        for (i=0;i<rank_list_size;i++)
        {
            if (rank_list[i] == gather_root)
            { 
                /*For the gather root, directly place the spot indices
                into the appropriate spot in the all_spot_index array.*/
                for (j=0;j<spot_index_size;j++)
                {
                    all_spot_index[j+spot_index_size*i] = loc_spot_index[j];
                }
            } 
            else
            {
               /*Receive the spot indices from all other ranks in th pelist.*/
               spot_ptr = &(all_spot_index[spot_index_size*i]);
               mpp_c_recv((void **)(&spot_ptr),spot_index_size,rank_list[i],
                          MPP_UINT32,MPP_C_GATHER_PELIST_TAG,NULL,
                          MPP_C_FALSE,&tmp_ptr);
            }
        }
        mpp_c_context_complete_all_recv_requests(&tmp_ptr);

        /*Make sure that all spot indices are inside the range of the
          gather_data array.*/
        for (i=0;i<rank_list_size;i++)
        {
            for (j=0;j<spot_index_size;j++)
            {
                if (j < spot_index_size/num_dims)
                {
                    all_spot_index[j+spot_index_size*i] += 
                                                    root_gather_spot_x_offset;
                    if (all_spot_index[j+spot_index_size*i] >= 
                        gather_data_x_size)
                    {
                        error_mesg = "MPP_C_GATHER_PELIST: gather_data"
                                     " x-dimension index out of range.";
                        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),
                                    tmp_ptr);
                    }
                }
                else if (j >= spot_index_size/num_dims && j < 
                         2*spot_index_size/num_dims)
                {
                    all_spot_index[j+spot_index_size*i] += 
                                                    root_gather_spot_y_offset;
                    if (all_spot_index[j+spot_index_size*i] >= 
                        gather_data_y_size)
                    {
                        error_mesg = "MPP_C_GATHER_PELIST: gather_data"
                                     " y-dimension index out of range.";
                        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),
                                    tmp_ptr);
                    }
                }
                else if (j >= 2*spot_index_size/num_dims)
                {
                    all_spot_index[j+spot_index_size*i] += 
                                                    root_gather_spot_z_offset;
                    if (all_spot_index[j+spot_index_size*i] >= 
                        gather_data_z_size)
                    {
                        error_mesg = "MPP_C_GATHER_PELIST: gather_data"
                                     " z-dimension index out of range.";
                        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),
                                    tmp_ptr);
                    }
                }
            }
        }
    }
    else
    {
        /*Send the local spot indices to the gather root.*/
        spot_ptr = &(loc_spot_index[0]);
        mpp_c_send((void **)(&spot_ptr),spot_index_size,gather_root,
                   MPP_UINT32,MPP_C_GATHER_PELIST_TAG,NULL,&tmp_ptr);
        mpp_c_context_complete_all_send_requests(&tmp_ptr);
    }

    /*Gather the array segments into the appropriate spots in the gather_data
      array.*/
    if (rank_list_root_flag)
    {
        /*Allocate an array of pointers, which will point to buffers that will
          hold the array segments received from the other ranks by the gather
          root.*/
        safemalloc((void **)&gather_buffer,
                   sizeof(unsigned char *)*rank_list_size);

        /*Point to the first index of the gather_data array.*/
        gather_data_ptr = (unsigned char *)(*gather_data);

        /*Calculate the number of bytes contained in the inputted mpp_type
          variable.*/
        num_bytes = mpp_c_datatype_get_num_bytes(mpp_type);

        /*Loop through the pelist to perform the gather of the data.*/
        for (i=0;i<rank_list_size;i++)
        {
            /*Get the starting and ending indices and sizes for the spots
              where the array segments will be placed.*/
            gx_index_start = all_spot_index[i*spot_index_size];
            gx_index_end = all_spot_index[i*spot_index_size+1];
            gy_index_start = all_spot_index[i*spot_index_size+2];
            gy_index_end = all_spot_index[i*spot_index_size+3];
            gz_index_start = all_spot_index[i*spot_index_size+4];
            gz_index_end = all_spot_index[i*spot_index_size+5];
            x_spot_size = gx_index_end - gx_index_start + 1;
            y_spot_size = gy_index_end - gy_index_start + 1;
            z_spot_size = gz_index_end - gz_index_start + 1;
            array_segment_size = x_spot_size*y_spot_size*z_spot_size;

            if (rank_list[i] == gather_root)
            {
                /*For the gather root, directly place the array segment into
                the appropriate spot in the gather_data array.*/
                array_segment_ptr = (unsigned char *)(*array_segment);
                for (j=0;j<array_segment_size;j++)
                {
                    x = j/(z_spot_size*y_spot_size);
                    y = (j-x*z_spot_size*y_spot_size)/z_spot_size;
                    z = j - x*z_spot_size*y_spot_size - y*z_spot_size;
                    x = x + gx_index_start;
                    y = y + gy_index_start;
                    z = z + gz_index_start;
                    k = x*gather_data_z_size*gather_data_y_size +
                        y*gather_data_z_size + z;
                    memcpy(gather_data_ptr+k*num_bytes,
                           array_segment_ptr+j*num_bytes,num_bytes);
                }
            }
            else
            {
               /*Receive the array segments from the other ranks and store
                 each in a contiguous buffer.*/
               gather_buffer[i] = NULL;
               safemalloc((void **)(&(gather_buffer[i])),num_bytes*
                          array_segment_size);
               mpp_c_recv((void **)&(gather_buffer[i]),array_segment_size,
                          rank_list[i],mpp_type,MPP_C_GATHER_PELIST_TAG,
                          NULL,MPP_C_FALSE,&tmp_ptr);
            }
        }
        mpp_c_context_complete_all_recv_requests(&tmp_ptr);

        /*Place the received array segments in the appropriate spots in the
          gather data array.*/
        for (i=0;i<rank_list_size;i++)
        {
            if (rank_list[i] != gather_root)
            {
                array_segment_ptr = (unsigned char *)(gather_buffer[i]);
                gx_index_start = all_spot_index[i*spot_index_size];
                gx_index_end = all_spot_index[i*spot_index_size+1];
                gy_index_start = all_spot_index[i*spot_index_size+2];
                gy_index_end = all_spot_index[i*spot_index_size+3];
                gz_index_start = all_spot_index[i*spot_index_size+4];
                gz_index_end = all_spot_index[i*spot_index_size+5];
                x_spot_size = gx_index_end - gx_index_start + 1;
                y_spot_size = gy_index_end - gy_index_start + 1;
                z_spot_size = gz_index_end - gz_index_start + 1;
                array_segment_size = x_spot_size*y_spot_size*z_spot_size;
                for (j=0;j<array_segment_size;j++)
                {
                    x = j/(z_spot_size*y_spot_size);
                    y = (j-x*z_spot_size*y_spot_size)/z_spot_size;
                    z = j - x*z_spot_size*y_spot_size - y*z_spot_size;
                    x = x + gx_index_start;
                    y = y + gy_index_start;
                    z = z + gz_index_start;
                    k = x*gather_data_z_size*gather_data_y_size +
                        y*gather_data_z_size + z;
                    memcpy(gather_data_ptr+k*num_bytes,
                           array_segment_ptr+j*num_bytes,num_bytes);
                }
            }
        }
    }
    else
    {
        /*Calculate the sizes of the array segment and send it to the 
          gather root.*/
        x_spot_size = (size_t)(gather_spot_x_end_index-
                               gather_spot_x_start_index) + 1 ;
        y_spot_size = (size_t)(gather_spot_y_end_index-
                               gather_spot_y_start_index) + 1;
        z_spot_size = (size_t)(gather_spot_z_end_index-
                               gather_spot_z_start_index) + 1;
        array_segment_size = x_spot_size*y_spot_size*z_spot_size;
        mpp_c_send(array_segment,array_segment_size,gather_root,mpp_type,
                   MPP_C_GATHER_PELIST_TAG,NULL,&tmp_ptr);
        mpp_c_context_complete_all_send_requests(&tmp_ptr);
    }

    /*Free local allocatables.*/
    safefree((void **)&loc_spot_index);
    if (rank_list_root_flag)
    {
        safefree((void **)&all_spot_index);
        for (i=0;i<rank_list_size;i++)
        {
            if (rank_list[i] != gather_root)
            {
                safefree((void **)(&(gather_buffer[i])));
            }
        }
        safefree((void **)&gather_buffer);
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    error_mesg = NULL;
    gather_root_ptr = NULL;
    spot_ptr = NULL;
    gather_data_ptr = NULL;
    array_segment_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
