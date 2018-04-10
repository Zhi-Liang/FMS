#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "helper_functions.h"
#include "mpp_c_context_t_api.h"
#include "mpp_c_error.h"
#include "mpp_c_init.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"
#include "mpp_c_shared_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Function that initializes the mpp_c library.*/
void mpp_c_init(mpp_c_shared_t **shared,
                mpp_c_context_t **context,
                MPI_Comm local_comm)
{
    /*Local variables.*/
    mpp_c_shared_t *tmp_shared = NULL;    /*Pointer to the mpp_c_shared_t object.*/
    mpp_c_context_t *tmp_context = NULL;  /*Pointer to the mpp_c_context_t object.*/
    MPI_Comm tmp_comm_id = MPI_COMM_NULL; /*MPI communicator id.*/
    int mpi_init_flag = 0;                /*MPI initialization flag.*/
    char *error_mesg = NULL;              /*Error message pointer.*/
    char *tmp_char_ptr = NULL;            /*Resuable character pointer.*/
    size_t tmp_size = 0;                  /*Resuable size.*/
    uint32_t *tmp_rank_list = NULL;       /*Rank list for a pelist.*/
    FILE *tmp_file_ptr = NULL;            /*Reusable file pointer.*/
    int32_t tmp_timer_id = 0;             /*Timer id.*/
    struct stat status;                   /*Status structure for a file.*/
    int32_t ierr = 0;                     /*MPI error code.*/
    int32_t ioerr = 0;                    /*I/O error code.*/

    /*If necessary, initialize MPI.*/
    ierr = MPI_Initialized(&mpi_init_flag);
    check_MPI_error_code(ierr,"MPI_Initialized","MPP_C_INIT");
    if (mpi_init_flag)
    {
        print_internal_message("MPP_C_INIT","MPI has already been"
                               " initialized.  The inputted communicator"
                               " will be used as the mpp world.");
        tmp_comm_id = local_comm;
    }
    else
    {
        ierr = MPI_Init(NULL,NULL);
        check_MPI_error_code(ierr,"MPI_Init","MPP_C_INIT");
        tmp_comm_id = MPI_COMM_WORLD;
    }

    /*Set the local pointer to the mpp_c_shared_t object.*/
    tmp_shared = *shared;

    /*Allocate, initialize, and set the mpp_c_shared_t object.*/
    if (tmp_shared != NULL)
    {
        throw_internal_error("MPP_C_INIT","the inputted mpp_c_shared_t"
                             " pointer is not null.");
    }
    safemalloc((void **) &tmp_shared,sizeof(mpp_c_shared_t));
    mpp_c_shared_init(&tmp_shared);
    mpp_c_shared_set_shared_data(&tmp_shared,tmp_comm_id);
    *shared = tmp_shared;

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_context = *context;

    /*Initialize an mpp_c_shared_t object.*/
    mpp_c_context_init(&tmp_context,tmp_shared);
    *context = tmp_context;

    /*Define the "myself" pelist.*/
    tmp_char_ptr = "myself";
    tmp_size = 1;
    safemalloc((void **) &tmp_rank_list,sizeof(uint32_t));
    tmp_rank_list[0] = mpp_c_context_get_world_rank(tmp_context);
    mpp_c_context_add_new_pelist(&tmp_context,tmp_char_ptr,
                                 strlen(tmp_char_ptr),tmp_rank_list,tmp_size);
    tmp_char_ptr = NULL;
    safefree((void **) &tmp_rank_list);

    /*Overwrite any previous log files.*/
    tmp_char_ptr = mpp_c_context_get_logfile_name(tmp_context);
    if (stat(tmp_char_ptr,&status) == 0)
    {
        tmp_file_ptr = fopen(tmp_char_ptr,"w+");
        ioerr = fclose(tmp_file_ptr);
        if (ioerr != 0)
        {
            error_mesg = "the logfile did not close properly.";
            mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_context);
        }
        tmp_file_ptr = NULL;
    }
    tmp_char_ptr = NULL;

    /*Overwrite any previous etc files.*/
    if (mpp_c_context_get_etcfile_ptr(tmp_context) != stderr)
    {
        tmp_char_ptr = mpp_c_context_get_etcfile_name(tmp_context);
        if (stat(tmp_char_ptr,&status) == 0)
        {
            tmp_file_ptr = fopen(tmp_char_ptr,"w+");
            ioerr = fclose(tmp_file_ptr);
            if (ioerr != 0)
            {
                error_mesg = "the etcfile did not close properly.";
                mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_context);
            }
            tmp_file_ptr = NULL;
        }
        tmp_char_ptr = NULL;
    }

    /*Write the contexts of the input.nml file and an initialization message
      to the root rank's logfile.*/
    if (mpp_c_context_is_cur_pelist_root_rank(tmp_context))
    {
        tmp_char_ptr = mpp_c_context_get_logfile_name(tmp_context);
        tmp_file_ptr = fopen(tmp_char_ptr,"a");
        fprintf(tmp_file_ptr,"============================================="
                "==================================\n");
        fprintf(tmp_file_ptr,"INPUT.NML: input.nml\n%s\n",
                mpp_c_shared_get_input_nml_buffer(tmp_shared));
        fprintf(tmp_file_ptr,"MPP_C Library version: 0.0\n");
        fprintf(tmp_file_ptr,"MPP_C Started with NPES = %lu\n",
                mpp_c_context_get_world_pelist_rank_list_size(tmp_context));
#ifdef use_libMPI
        fprintf(tmp_file_ptr,"Using MPI library for message passing ...\n");
#endif
        ioerr = fclose(tmp_file_ptr);
        if (ioerr != 0)
        {
            error_mesg = "the logfile did not close properly.";
            mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_context);
        }
        tmp_file_ptr = NULL;
        tmp_char_ptr = NULL;
    }

    /*Initialize and start the total runtime timer.*/
    tmp_char_ptr = "Total runtime";
    tmp_timer_id = mpp_c_context_add_new_timer(&tmp_context,tmp_char_ptr,
                                               strlen(tmp_char_ptr),
                                               MPP_C_RUNTIME_TIMER,
                                               MPP_C_TRUE,MPP_C_FALSE);
    mpp_c_context_set_runtime_timer_id(&tmp_context,tmp_timer_id);
    mpp_c_context_start_timer(&tmp_context,tmp_timer_id);
    tmp_char_ptr = NULL;

    /*Nullify local pointers.*/
    tmp_shared = NULL;
    tmp_context = NULL;
    error_mesg = NULL;
    tmp_char_ptr = NULL;
    tmp_rank_list = NULL;
    tmp_file_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/

