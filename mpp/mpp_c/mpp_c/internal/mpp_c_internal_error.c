#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpp_c_internal_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"

/*---------------------------------------------------------------------------*/
/*Function that collects a routine name and error message, combines them,
  and then prints them to stderr and aborts.*/
void throw_internal_error(char const *routine_name,
                          char const *error_text)
{
    /*Local variables*/
    char tmp_routine_name[MPP_C_MAX_ROUTINE_NAME_LENGTH]; /*Calling routine name.*/
    char tmp_error_text[MPP_C_MAX_ERROR_MESG_LENGTH];     /*Error text.*/
    int world_rank = 0;                                   /*Process rank in MPI_COMM_WORLD.*/
    int mpi_is_init = 0;                                  /*MPI initialization flag.*/
    int ierr = 0;                                         /*MPI error code.*/

    /*Set the calling routine name pointer.*/
    if (routine_name == NULL)
    {
        fprintf(stderr,"MPP_C Internal Error(throw_internal_error):"
                       " null routine name was inputted.\n");
        exit(EXIT_FAILURE);
    }
    snprintf(tmp_routine_name,MPP_C_MAX_ROUTINE_NAME_LENGTH,"%s",routine_name);

    /*Set the error text pointer.*/
    if (error_text == NULL)
    {
        fprintf(stderr,"MPP_C Internal Error(throw_internal_error):"
                       " null error message was inputted.\n");
        exit(EXIT_FAILURE);
    }
    snprintf(tmp_error_text,MPP_C_MAX_ROUTINE_NAME_LENGTH,"%s",error_text);

    /*Get the MPI initialization state.*/
    ierr = MPI_Initialized(&mpi_is_init);
    if (ierr != MPI_SUCCESS)
    {
        fprintf(stderr,"MPP_C Internal Error(throw_internal_error):"
                       " MPI_Initialized exited with error code %d.\n",ierr);
        exit(EXIT_FAILURE);
    }

    /*Print out the error and abort.*/
    if (mpi_is_init)
    {
        ierr = MPI_Comm_rank(MPI_COMM_WORLD,&world_rank);
        if (ierr != MPI_SUCCESS)
        {
            fprintf(stderr,"MPP_C Internal Error(throw_internal_error):"
                           "MPI_Comm_rank exited with error code %d.\n",ierr);
            exit(EXIT_FAILURE);
        }
        fprintf(stderr,"MPP_C Internal Error(%s) from rank %d in"
                       " MPI_COMM_WORLD: %s.\n",tmp_routine_name,world_rank,
                       tmp_error_text);
        MPI_Abort(MPI_COMM_WORLD,EXIT_FAILURE);
    }
    else
    {
        fprintf(stderr,"MPP_C Internal Error(%s): %s.\n",tmp_routine_name,
                tmp_error_text);
        exit(EXIT_FAILURE);
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that collects a routine name and warning message, combines them,
  and then prints them to stdout.*/
void print_internal_message(char const *routine_name,
                            char const *mesg_text)
{
    /*Local variables*/
    char tmp_routine_name[MPP_C_MAX_ROUTINE_NAME_LENGTH]; /*Calling routine name.*/
    char tmp_mesg_text[MPP_C_MAX_ERROR_MESG_LENGTH];      /*Error text.*/
    int world_rank = 0;                                   /*Process rank in MPI_COMM_WORLD.*/
    int mpi_is_init = 0;                                  /*MPI initialization flag.*/
    int ierr = 0;                                         /*MPI error code.*/

    /*Set the calling routine name pointer.*/
    if (routine_name == NULL)
    {
        throw_internal_error("PRINT_INTERNAL_MESSAGE",
                             "null routine name was inputted.");
    }
    snprintf(tmp_routine_name,MPP_C_MAX_ROUTINE_NAME_LENGTH,"%s",routine_name);

    /*Set the message text pointer.*/
    if (mesg_text == NULL)
    {
        throw_internal_error("PRINT_INTERNAL_MESSAGE",
                             "null message was inputted.\n");
    }
    snprintf(tmp_mesg_text,MPP_C_MAX_ROUTINE_NAME_LENGTH,"%s",mesg_text);

    /*Get the MPI initialization state.*/
    ierr = MPI_Initialized(&mpi_is_init);
    check_MPI_error_code(ierr,"MPI_Initialized","PRINT_INTERNAL_MESSAGE");

    /*Print out the error and abort.*/
    if (mpi_is_init)
    {
        ierr = MPI_Comm_rank(MPI_COMM_WORLD,&world_rank);
        check_MPI_error_code(ierr,"MPI_Comm_rank","PRINT_INTERNAL_MESSAGE");
        fprintf(stdout,"MPP_C Internal Message(%s) from rank %d in"
                       " MPI_COMM_WORLD: %s.\n",tmp_routine_name,world_rank,
                       tmp_mesg_text);
    }
    else
    {
        fprintf(stdout,"MPP_C Internal Message(%s): %s.\n",tmp_routine_name,
                tmp_mesg_text);
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that checks if an inputted flag variable is either MPP_C_TRUE
  or MPP_C_FALSE.  If not, then an internal error is thrown.*/
void check_flag_input(mpp_c_flag_t const input_flag,
                      char const *routine_name)
{
    if (input_flag != MPP_C_TRUE && input_flag != MPP_C_FALSE)
    {
        throw_internal_error(routine_name,"the inputted flag variable is"
                             " neither MPP_C_TRUE or MPP_C_FALSE.");
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that checks if an inputted size variable is zero.  If so, then
  an internal error is thrown.*/
void check_size_input(size_t const input_size,
                      char const *routine_name)
{
    if (input_size == 0)
    {
        throw_internal_error(routine_name,"the inputted size variable is"
                             " zero.");
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that checks if an inputted pointer is null.  If so, then an
  internal error is thrown.*/
void check_pointer_input(void const * const ptr,
                         char const *routine_name)
{
    if (ptr == NULL)
    {
        throw_internal_error(routine_name,"the inputted pointer is null.");
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that checks whether an inputted array is valid.  The inputted
  pointer must not be null and the inputted array size must be greater than
  zero and less than MPP_C_MAX_ARRAY_SIZE to be valid.*/
void check_array_input(void const * const array_ptr,
                       size_t const array_size,
                       char const *routine_name)
{
    /*Check the pointer.*/
    check_pointer_input(array_ptr,routine_name);

    /*Check the array size.*/
    check_size_input(array_size,routine_name);
    if (array_size > MPP_C_MAX_ARRAY_SIZE)
    {
        throw_internal_error(routine_name,"the inpuuted array size is greater"
                             " than MPP_C_MAX_ARRAY_SIZE");
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that checks whether an inputted string is valid.  The inputted
  character pointer must not be null, have a length greater than 0, and
  be null terminated in order to be deemed valid.*/
void check_string_input(char const *input_str,
                        size_t const input_str_length,
                        char const *routine_name)
{
    check_array_input((void *)input_str,input_str_length,routine_name);
    if (input_str[input_str_length] != '\0')
    {
        throw_internal_error(routine_name,"the inputted string is not null"
                             " terminated.");
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that checks whether a inputted pelist is valid.  The inputted
  pointer is allowed to be null, but if it is not null, then the inputted
  size must be greater than zero.*/
void check_pelist_input(uint32_t const * const rank_list,
                        size_t const rank_list_size,
                        char const *routine_name)
{
    if (rank_list != NULL && rank_list_size == 0)
    {
        throw_internal_error(routine_name,"the inputted pelist has a size"
                             " of zero.");
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that checks whether an inputted MPI communicator is valid.  The
  inputted communicator must not be equal to MPI_COMM_NULL in order to be
  deemed valid.*/
void check_comm_id_input(MPI_Comm const comm_id,
                         char const *routine_name)
{
    if (comm_id == MPI_COMM_NULL)
    {
        throw_internal_error(routine_name,"the inputted MPI communicator is"
                             " equal to MPI_COMM_NULL.");
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that checks whether an inputted MPI request is valid.  The
  inputted request must not be equal to MPI_REQUEST_NULL in order to be
  deemed valid.*/
void check_comm_request_input(MPI_Request const comm_request,
                              char const *routine_name)
{
    if (comm_request == MPI_REQUEST_NULL)
    {
        throw_internal_error(routine_name,"the inputted MPI request is"
                             " equal to MPI_REQUEST_NULL.");
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that checks whether an inputted MPI datatype is valid.  The
  inputted datatype must be one of MPI_INT32_T, MPI_UINT32_T, MPI_INT64_T,
  MPI_UINT64_T, MPI_FLOAT, MPI_DOUBLE, or MPI_CHAR to be deemed valid.*/
void check_comm_datatype_input(MPI_Datatype const comm_datatype,
                               char const *routine_name)
{
    switch (comm_datatype)
    {
        case MPI_INT32_T:
            break;
        case MPI_UINT32_T:
            break;
        case MPI_INT64_T:
            break;
        case MPI_UINT64_T:
            break;
        case MPI_FLOAT:
            break;
        case MPI_DOUBLE:
            break;
        case MPI_CHAR:
            break;
        default:
            throw_internal_error(routine_name,"the inputted MPI datatype"
                                 " is not supported.");
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Check the error code returned from a call to a MPI routine.  If the error
  code does not equal MPI_SUCCESS, then throw an internal error.*/
void check_MPI_error_code(int error_code,
                          char const *mpi_func_name,
                          char const *routine_name)
{
    /*Local variables*/
    char error_mesg[MPP_C_MAX_ERROR_MESG_LENGTH]; /*Error message.*/

    if (error_code != MPI_SUCCESS)
    {
        snprintf(error_mesg,MPP_C_MAX_ERROR_MESG_LENGTH,"%s returned an"
                 " MPI error code: %d.",mpi_func_name,error_code);
        throw_internal_error(routine_name,error_mesg);
    }

    return;
}
/*---------------------------------------------------------------------------*/

