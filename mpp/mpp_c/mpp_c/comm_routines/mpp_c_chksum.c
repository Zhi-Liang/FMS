#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "mpp_c_chksum.h"
#include "mpp_c_context_t_api.h"
#include "mpp_c_error.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_sum.h"

/*---------------------------------------------------------------------------*/
/*Perform a check-sum across all ranks of a pelist.  If a null pelist is
  inputted, then the current pelist is used.*/
int64_t mpp_c_chksum(void const *chksum_data,
                     size_t const chksum_len,
                     size_t const num_bytes,
                     void const *mask_val,
                     int32_t const *rank_list,
                     size_t const rank_list_size,
                     mpp_c_context_t * const * const context)
{
    /*Local variables*/
    int64_t tot_chksum = 0;          /*Resulting check-sum value.*/
    mpp_c_context_t *tmp_ptr = NULL; /*Pointer to the mpp_c_context_t object.*/
    uint64_t tmp_chksum = 0;         /*Temporary check-sum value.*/
    uint64_t tmp_elem = 0;           /*Temporary sum element.*/
    uint64_t tmp_mask = 0;           /*Temporary mask value.*/
    void *sum_ptr = NULL;            /*Pointer used to sum over all ranks in the pelist.*/
    char *error_mesg = NULL;         /*Error message pointer.*/
    unsigned int i = 0;              /*Loop variable.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *context;

    /*Check array input.*/
    check_array_input(chksum_data,chksum_len,"MPP_C_CHKSUM");

    /*Check pelist input.*/
    check_pelist_input((uint32_t *)rank_list,rank_list_size,"MPP_C_CHKSUM");

    /*Get the mask value if necessary.*/
    if (mask_val != NULL)
    {
        if (num_bytes == 4)
        {
            tmp_mask = (uint64_t) (*((uint32_t *)mask_val));
        }
        else if (num_bytes == 8)
        {
            tmp_mask = *((uint64_t *)mask_val);
        }
        else
        {
            error_mesg = "MPP_C_CHKSUM: unsupported type.  Only 4 and 8 byte"
                         " types are currently supported.";
            mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
        }
    }

    /*Perform the check-sum.*/
    tmp_chksum = 0;
    for (i=0;i<chksum_len;i++)
    {
        if (num_bytes == 4)
        {
            tmp_elem = (uint64_t) (((uint32_t *)chksum_data)[i]);
        }
        else if (num_bytes == 8)
        {
            tmp_elem = (((uint64_t *)chksum_data)[i]);
        }
        if (mask_val != NULL)
        {
            if (tmp_elem != tmp_mask)
            {
                tmp_chksum = tmp_chksum + tmp_elem;
            }
        }
        else
        {
            tmp_chksum = tmp_chksum + tmp_elem;
        }
    }
    sum_ptr = &tmp_chksum;
    mpp_c_sum(&sum_ptr,1,MPP_UINT64,rank_list,rank_list_size,&tmp_ptr);
    tot_chksum = (int64_t)tmp_chksum;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    sum_ptr = NULL;
    error_mesg = NULL;

    return tot_chksum;
}

/*---------------------------------------------------------------------------*/
