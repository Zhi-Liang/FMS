#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "helper_functions.h"
#include "mpp_c_broadcast.h"
#include "mpp_c_context_t_api.h"
#include "mpp_c_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"
#include "mpp_c_read_nml_file.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Function that reads a .nml namelist file, and stores the contents of the
  file in the mpp_c_context_t object's namelist buffer.*/
void mpp_c_read_nml_file(mpp_c_context_t **context)
{
    /*Local variables.*/
    mpp_c_context_t *tmp_ptr = NULL;                    /*Pointer to the mpp_c_context_t object.*/
    char *error_mesg = NULL;                            /*Error message pointer.*/
    char *pelist_name = NULL;                           /*Name of the current pelist.*/
    char nml_file_name[MPP_C_MAX_NML_FILE_NAME_LENGTH]; /*Namelist file name.*/
    int32_t file_not_found = 0;                         /*File exists flag.*/
    size_t nml_file_len = 0;                            /*Length of the namelist file in bytes.*/
    struct stat status;                                 /*Status struct for a file.*/
    FILE *tmp_file_ptr = NULL;                          /*Temporary file pointer.*/
    char *tmp_buff = NULL;                              /*Local buffer to hold the namelist file contents.*/
    int32_t ioerr = 0;                                  /*I/O error code.*/
    void *data_ptr = NULL;                              /*Pointer to data that is broadcasted.*/
    int32_t from_rank = 0;                              /*Rank id in the world pelist that will be the broadcast root.*/
    uint64_t i = 0;                                     /*Loop variable.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *context;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,"MPP_C_CONTEXT_READ_NML_FILE");

    /*If the namelist buffer was already allocated, then free it.*/
    if (mpp_c_context_get_namelist_buffer(tmp_ptr) != NULL)
    {
        mpp_c_context_free_namelist_buffer(&tmp_ptr);
    }

    /*Get the size of the .nml file, which will be used to size the namelist
      buffer.*/
    file_not_found = MPP_C_FALSE;
    if (mpp_c_context_is_cur_pelist_root_rank(tmp_ptr))
    {
        /*Get the name of the current pelist.*/
        pelist_name = mpp_c_context_get_cur_pelist_name(tmp_ptr);

        /*Get the name of .nml file and open it.*/
        snprintf(nml_file_name,64,"input_%s.nml",pelist_name);
        if (stat(nml_file_name,&status) != 0)
        {
            /*Set the file not found flag to true.*/
            file_not_found = MPP_C_TRUE;
        }
        else
        {
            /*Open the file.*/
            tmp_file_ptr = fopen(nml_file_name,"r");

            /*Get the size of the .nml file in bytes.*/
            nml_file_len = (size_t)status.st_size+1;
        }
    }

    /*If the file was not found, then return.*/
    from_rank = (int32_t)mpp_c_context_get_cur_pelist_root_rank(tmp_ptr);
    data_ptr = (void *)&file_not_found;
    mpp_c_broadcast(&data_ptr,1,from_rank,MPP_INT32,NULL,0,&tmp_ptr);
    if (file_not_found)
    {
        error_mesg = "MPP_C_READ_NML_FILE: namelist file does not exist.";
        mpp_c_error(NOTE,error_mesg,strlen(error_mesg),tmp_ptr);
        data_ptr = NULL;
        error_mesg = NULL;
        tmp_ptr = NULL;
        return;
    }

    /*Broadcast the size of the file to all other ranks on the current
      pelist.*/
    data_ptr = (void *)&nml_file_len;
    mpp_c_broadcast(&data_ptr,1,from_rank,MPP_UINT64,NULL,0,&tmp_ptr);

    /*Allocate the namelist buffer.*/
    mpp_c_context_alloc_namelist_buffer(&tmp_ptr,nml_file_len);

    /*Allocate and read the contents of the file into a temporary buffer,
      and null terminate the buffer.*/
    safemalloc((void **) &tmp_buff,sizeof(char)*nml_file_len);
    if (mpp_c_context_is_cur_pelist_root_rank(tmp_ptr))
    {
        for (i=0;i<nml_file_len;i++)
        {
            tmp_buff[i] = getc(tmp_file_ptr);
        }
        tmp_buff[nml_file_len-1] = '\0';

        /*Close the namelist file.*/
        fclose(tmp_file_ptr);
    }

    /*Broadcast the temporary buffer to all other ranks on the current
      pelist.*/
    mpp_c_broadcast((void **) &tmp_buff,nml_file_len,from_rank,
                    MPP_CHAR,NULL,0,&tmp_ptr);

    /*Copy the temporary buffer into the namelist buffer.*/
    mpp_c_context_fill_namelist_buffer(&tmp_ptr,tmp_buff,nml_file_len);

    /*Free the temporary buffer.*/
    safefree((void **) &tmp_buff);

    /*Write out the namelist.*/
    if (mpp_c_context_is_cur_pelist_root_rank(tmp_ptr))
    {
        tmp_file_ptr = NULL;
        tmp_file_ptr = mpp_c_context_get_logfile_ptr(tmp_ptr);
        if (tmp_file_ptr == NULL)
        {
            tmp_file_ptr = fopen(mpp_c_context_get_logfile_name(tmp_ptr),"a");
        }
        fprintf(tmp_file_ptr,"============================================="
                "==================================\n");
        fprintf(tmp_file_ptr,"MPP_C_READ_NML_FILE: %s\n%s\n",nml_file_name,
                mpp_c_context_get_namelist_buffer(tmp_ptr));
        ioerr = fclose(tmp_file_ptr);
        if (ioerr != 0)
        {
            error_mesg = "MPP_C_CONTEXT_READ_INPUT_NML: error closing logfile";
            mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
        }
        tmp_file_ptr = NULL;
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    error_mesg = NULL;
    pelist_name = NULL;
    tmp_file_ptr = NULL;
    tmp_buff = NULL;
    data_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/

