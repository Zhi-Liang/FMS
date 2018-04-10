#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "helper_functions.h"
#include "mpp_c_context_t_api.h"
#include "mpp_c_datatype_t_api.h"
#include "mpp_c_error.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_max.h"
#include "mpp_c_recv.h"
#include "mpp_c_scatter_pelist.h"
#include "mpp_c_send.h"

/*---------------------------------------------------------------------------*/
/*Scatter a data array from a rank into array segments on all other ranks on
  a pelist. Conceptually, the scatter can be viewed like the picture, where
                            say rank 1 fills each of the array segments on
 |-----------|-----------|  ranks 0, 2, and 3 by sending sections of its
 | array     | array     |  inputted scatter_data array. The starting and
 | segment   | segment   |  ending indices of each spot in the scatter_data
 | scattered | scattered |  array that will be sent to the each other rank
 | to        | to        |  are first scattered by the scattering root rank,
 | rank 0    | rank 1    |  and then the data is scattered and stored in
 |-----------|-----------|  each ranks array_segment array.
 | array     | array     |
 | segment   | segment   |
 | scattered | scattered |
 | to        | to        |
 | rank 2    | rank 3    |
 |-----------|-----------|*/

void mpp_c_scatter_pelist(void **scatter_data,
                         size_t const scatter_data_x_size,
                         size_t const scatter_data_y_size,
                         size_t const scatter_data_z_size,
                         mpp_c_datatype_t const mpp_type,
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
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL;         /*Pointer to the mpp_c_context_t object.*/
    char *error_mesg = NULL;                 /*Error message pointer.*/
    int32_t scatter_root = 0;                /*Root rank for the scatter.*/
    int32_t *scatter_root_ptr = NULL;        /*Pointer to the scatter root.*/
    size_t num_dims = 0;                     /*Number of dimensions of scatter_data.*/
    size_t spot_index_size = 0;              /*Total number of scatter spot indices.*/
    uint32_t *loc_spot_index = NULL;         /*Array of local scatter spot indices.*/
    uint32_t *all_spot_index = NULL;         /*Array of all scatter spot indices.*/
    uint32_t *spot_ptr = NULL;               /*Pointer to indices in the spot arrays.*/
    size_t x_spot_size = 0;                  /*Size of the x-dimension of the array segment.*/
    size_t y_spot_size = 0;                  /*Size of the y-dimension of the array segment.*/
    size_t z_spot_size = 0;                  /*Size of the z-dimension of the array segment.*/
    size_t array_segment_size = 0;           /*Total size of the array segment.*/
    size_t num_bytes = 0;                    /*Size of scatter_data type in bytes.*/
    unsigned char **scatter_buffer = NULL;   /*Buffer for scatter_data.*/
    unsigned char *scatter_data_ptr = NULL;  /*Pointer to scatter_data.*/
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
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_SCATTER_PELIST");

    /*Make sure the inputted pelist is not null.*/
    if (rank_list == NULL)
    {
        error_mesg = "MPP_C_SCATTER_PELIST: a null pelist passed in.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }

    /*Make sure only one scatter root has been inputted.*/
    if (rank_list_root_flag)
    {
        scatter_root = (int32_t)mpp_c_context_get_world_rank(tmp_ptr);
    }
    else
    {
        scatter_root = -1;
    }
    scatter_root_ptr = &scatter_root;
    mpp_c_max((void **)(&scatter_root_ptr),1,MPP_INT32,rank_list,
              rank_list_size,&tmp_ptr);
    if (scatter_root == -1)
    {
        error_mesg = "MPP_C_SCATTER_PELIST: scatter root not specified.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }
    if (rank_list_root_flag && scatter_root != 
        (int32_t)mpp_c_context_get_world_rank(tmp_ptr))
    {
        error_mesg = "MPP_C_SCATTER_PELIST: more than one scatter root\
                      specified.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }

    /*Send the indices to the scatter root. Pack the inputted indices into an
      array to minimize the total number of sends/receives.*/
    num_dims = 3;
    spot_index_size = 2*num_dims;
    safemalloc((void **)&loc_spot_index,sizeof(uint32_t)*spot_index_size);
    loc_spot_index[0] = (uint32_t)scatter_spot_x_start_index;
    loc_spot_index[1] = (uint32_t)scatter_spot_x_end_index;
    loc_spot_index[2] = (uint32_t)scatter_spot_y_start_index;
    loc_spot_index[3] = (uint32_t)scatter_spot_y_end_index;
    loc_spot_index[4] = (uint32_t)scatter_spot_z_start_index;
    loc_spot_index[5] = (uint32_t)scatter_spot_z_end_index;
    if (rank_list_root_flag)
    {
        /*Allocate an array to store all the spot indices on the scatter root.*/
        safemalloc((void **)&all_spot_index,sizeof(uint32_t)*spot_index_size
                                            *rank_list_size);

        /*Loop through the pelist to scatter the spot indices.*/
        for (i=0;i<rank_list_size;i++)
        {
            if (rank_list[i] == scatter_root)
            {
                /*For the scatter root, directly place the spot indices
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
                           MPP_UINT32,MPP_C_SCATTER_PELIST_TAG,NULL,
                           MPP_C_FALSE,&tmp_ptr);
            }
        }
        mpp_c_context_complete_all_recv_requests(&tmp_ptr);

        /*Add indice offsets and make sure that all spot indices are inside
          the range of the scatter_data array.*/
        for (i=0;i<rank_list_size;i++)
        {
            for (j=0;j<spot_index_size;j++)
            {
                if (j < spot_index_size/num_dims)
                {
                    all_spot_index[j+spot_index_size*i] += 
                                                    root_scatter_spot_x_offset;
                    if (all_spot_index[j+spot_index_size*i] >= 
                        scatter_data_x_size)
                    {
                        error_mesg = "MPP_C_SCATTER_PELIST: scatter_data\
                                      x-dimension index out of range.";
                        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),
                                    tmp_ptr);
                    }
                }
                else if (j >= spot_index_size/num_dims && j < 
                         2*spot_index_size/num_dims)
                {
                    all_spot_index[j+spot_index_size*i] += 
                                                    root_scatter_spot_y_offset;
                    if (all_spot_index[j+spot_index_size*i] >= 
                        scatter_data_y_size)
                    {
                        error_mesg = "MPP_C_SCATTER_PELIST: scatter_data\
                                      y-dimension index out of range.";
                        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),
                                    tmp_ptr);
                    }
                }
                else if (j >= 2*spot_index_size/num_dims)
                {
                    all_spot_index[j+spot_index_size*i] += 
                                                    root_scatter_spot_z_offset;
                    if (all_spot_index[j+spot_index_size*i] >= 
                        scatter_data_z_size)
                    {
                        error_mesg = "MPP_C_SCATTER_PELIST: scatter_data\
                                      z-dimension index out of range.";
                        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),
                                    tmp_ptr);
                    }
                }
            }
        }
    }
    else
    {
        /*Send the local spot indices to the scatter root.*/
        spot_ptr = &(loc_spot_index[0]);
        mpp_c_send((void **)(&spot_ptr),spot_index_size,scatter_root,
                   MPP_UINT32,MPP_C_SCATTER_PELIST_TAG,NULL,&tmp_ptr);
        mpp_c_context_complete_all_send_requests(&tmp_ptr);
    }

    /*Scatter sections of the scatter_data array into the appropriate array
      segments on the other ranks in the pelist.*/
    if (rank_list_root_flag)
    {
        /*Allocate an array of pointers, which will point to buffers that will
          hold the scatter_data array sections that will be sent to the other
          ranks by the scatter root.*/
        safemalloc((void **)&scatter_buffer,sizeof(unsigned char *)*rank_list_size);

        /*Point to the first index of the scatter_data array.*/
        scatter_data_ptr = (unsigned char *)(*scatter_data);

        /*Calculate the number of bytes contained in the inputted mpp_type
          variable.*/
        num_bytes = mpp_c_datatype_get_num_bytes(mpp_type);

        /*Loop through the pelist to perform the scatter of the data.*/
        for (i=0;i<rank_list_size;i++)
        {
            /*Get the starting and ending indices and sizes for the segments
              of the scatter data array that will be sent.*/
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

            if (rank_list[i] == scatter_root)
            {
                /*For the scatter root, directly place the appropriate
                  scatter_data elements into the array segment.*/
                array_segment_ptr = (unsigned char *)(*array_segment);
                for (j=0;j<array_segment_size;j++)
                {
                    x = j/(z_spot_size*y_spot_size);
                    y = (j-x*z_spot_size*y_spot_size)/z_spot_size;
                    z = j - x*z_spot_size*y_spot_size - y*z_spot_size;
                    x = x + gx_index_start;
                    y = y + gy_index_start;
                    z = z + gz_index_start;
                    k = x*scatter_data_z_size*scatter_data_y_size +
                        y*scatter_data_z_size + z;
                    memcpy(array_segment_ptr+j*num_bytes,
                           scatter_data_ptr+k*num_bytes,num_bytes);
                }
            }
            else
            {
                /*Place the scatter_data array sections that will be sent to
                  the other ranks into a contiguous buffer and send.*/
                scatter_buffer[i] = NULL;
                safemalloc((void **)(&(scatter_buffer[i])),num_bytes*
                           array_segment_size);
                array_segment_ptr = (unsigned char *)scatter_buffer[i];
                for (j=0;j<array_segment_size;j++)
                {
                    x = j/(z_spot_size*y_spot_size);
                    y = (j-x*z_spot_size*y_spot_size)/z_spot_size;
                    z = j - x*z_spot_size*y_spot_size - y*z_spot_size;
                    x = x + gx_index_start;
                    y = y + gy_index_start;
                    z = z + gz_index_start;
                    k = x*scatter_data_z_size*scatter_data_y_size +
                        y*scatter_data_z_size + z;
                    memcpy(array_segment_ptr+j*num_bytes,
                           scatter_data_ptr+k*num_bytes,num_bytes);
                }
               mpp_c_send((void **)&(scatter_buffer[i]),array_segment_size,
                          rank_list[i],mpp_type,MPP_C_SCATTER_PELIST_TAG,
                          NULL,&tmp_ptr);
            }
        }
        mpp_c_context_complete_all_send_requests(&tmp_ptr);
    }
    else
    {
        /*Calculate the sizes of the array segment and receive it from the
          scatter root.*/
        x_spot_size = (size_t)(scatter_spot_x_end_index-
                               scatter_spot_x_start_index) + 1 ;
        y_spot_size = (size_t)(scatter_spot_y_end_index-
                               scatter_spot_y_start_index) + 1;
        z_spot_size = (size_t)(scatter_spot_z_end_index-
                               scatter_spot_z_start_index) + 1;
        array_segment_size = x_spot_size*y_spot_size*z_spot_size;
        mpp_c_recv(array_segment,array_segment_size,scatter_root,mpp_type,
                   MPP_C_SCATTER_PELIST_TAG,NULL,MPP_C_FALSE,
                   &tmp_ptr);
        mpp_c_context_complete_all_recv_requests(&tmp_ptr);
    }

    /*Free local allocatables.*/
    safefree((void **)&loc_spot_index);
    if (rank_list_root_flag)
    {
        safefree((void **)&all_spot_index);
        for (i=0;i<rank_list_size;i++)
        {
            if (rank_list[i] != scatter_root)
            {
                safefree((void **)(&(scatter_buffer[i])));
            }
        }
        safefree((void **)&scatter_buffer);
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    error_mesg = NULL;
    scatter_root_ptr = NULL;
    spot_ptr = NULL;
    scatter_data_ptr = NULL;
    array_segment_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
