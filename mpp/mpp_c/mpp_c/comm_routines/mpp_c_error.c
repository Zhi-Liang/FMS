#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpp_c_context_t_api.h"
#include "mpp_c_error.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"

/*---------------------------------------------------------------------------*/
/*Print out an error message and abort if necessary.*/
void mpp_c_error(int32_t const error_type,
                 char const *error_mesg,
                 size_t const mesg_len,
                 mpp_c_context_t const * const context)
{
    /*Local variables*/
    uint32_t my_rank = 0;                      /*Convenience variable.*/
    char message[MPP_C_MAX_ERROR_MESG_LENGTH]; /*Local message buffer.*/
    FILE *tmp_file_ptr = NULL;                 /*Temporary file pointer.*/
    int32_t ioerr = 0;                         /*I/O error code.*/

    /*Make sure the mpp_c_context_t object has been initialized.*/
    if (context == NULL)
    {
        throw_internal_error("MPP_C_ERROR","you must first call mpp_init.");
    }

    /*Check string input.*/
    check_string_input(error_mesg,mesg_len,"MPP_C_ERROR");

    /*Write out a message if the inputted error message will be truncated.*/
    if (mesg_len > MPP_C_MAX_ERROR_MESG_LENGTH)
    {
        print_internal_message("MPP_C_ERROR","error message length exceeds"
                               " maximum allowed.  The error message will be"
                               " truncated");
    }

    /*If necessary, open the etcfile.*/
    if (mpp_c_context_is_cur_pelist_root_rank(context))
    {
        tmp_file_ptr = stdout;
    }
    else
    {
        if (mpp_c_context_get_etcfile_ptr(context) == NULL)
        {
            tmp_file_ptr = fopen(mpp_c_context_get_etcfile_name(context),"a");
        }
    }

    /*Set the convenience variable.*/
    my_rank = mpp_c_context_get_world_rank(context);


    /*Place the message in a null terminated local character array buffer.*/
    snprintf(message,MPP_C_MAX_ERROR_MESG_LENGTH,"%s",error_mesg);

    /*Print out FATALS and WARNINGS to stdout and stderr.  Print out NOTES
      to stdout.*/
    if (error_type == FATAL)
    {
        fprintf(tmp_file_ptr,"FATAL from rank %d: %s\n",my_rank,message);
        fprintf(stderr,"FATAL from rank %d: %s\n",my_rank,message);
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }
    else if (error_type == WARNING)
    {
        fprintf(tmp_file_ptr,"WARNING from rank %d: %s\n",my_rank,message);
        fprintf(stderr,"WARNING from rank %d: %s\n",my_rank,message);
    }
    else if (error_type == NOTE)
    {
        fprintf(tmp_file_ptr,"NOTE from rank %d: %s\n",my_rank,message);
    }
    else
    {
        fprintf(tmp_file_ptr,"FATAL from rank %d: MPP_C_ERROR: unsupported"
                             " error code.\n",my_rank);
        fprintf(stderr,"FATAL from rank %d: MPP_C_ERROR: unsupported error"
                       " code.\n",my_rank);
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }

    /*Close the etcfile.*/
    if (tmp_file_ptr != stdout)
    {
        ioerr = fclose(tmp_file_ptr);
        if (ioerr != 0)
        {
            fprintf(stdout,"FATAL from rank %d: The etcfile did not close"
                           "properly\n",my_rank);
            fprintf(stderr,"FATAL from rank %d: The etcfile did not close"
                           "properly\n",my_rank);
            MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
        }
    }

    /*Nullify local pointers.*/
    tmp_file_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/

