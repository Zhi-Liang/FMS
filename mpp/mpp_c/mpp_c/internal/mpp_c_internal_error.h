/** @file */
/*Header file for mpp_c internal errorr function.*/

#ifndef SET_MPP_C_INTERNAL_ERROR_H_
#define SET_MPP_C_INTERNAL_ERROR_H_

#include "mpp_c_mpi.h"
#include "mpp_c_typedefs.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

/*---------------------------------------------------------------------------*/
/**
    Print out an internal error message and abort.

    \param [in] routine_name  Name of the routine where the error occurred.
    \param [in] error_text    Description of the error.
*/
void throw_internal_error(char const *routine_name,
                          char const *error_text);

/*---------------------------------------------------------------------------*/
/**
    Function that collects a routine name and warning message, combines them,
    and then prints them to stdout.

    \param [in] routine_name  Name of the calling routine.
    \param [in] mesg_text     Message text.
*/
void print_internal_message(char const *routine_name,
                            char const *mesg_text);

/*---------------------------------------------------------------------------*/
/**
    Function that checks if an inputted flag variable is either MPP_C_TRUE
    or MPP_C_FALSE.  If not, then an internal error is thrown.

    \param [in] input_flag    A mpp_c_flag_t variable.
    \param [in] routine_name  Name of the calling routine.
*/
void check_flag_input(mpp_c_flag_t const input_flag,
                      char const *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Function that checks if an inputted size variable is zero.  If so, then
    an internal error is thrown.

    \param [in] input_size    A size_t variable.
    \param [in] routine_name  Name of the calling routine.
*/
void check_size_input(size_t const input_size,
                      char const *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Function that checks if an inputted pointer is null.  If so, then an
    internal error is thrown.

    \param [in] ptr           A pointer.
    \param [in] routine_name  Name of the calling routine.
*/
void check_pointer_input(void const * const ptr,
                         char const *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Check whether an inputted array is valid.  The inputted pointer must not
    be null and the inputted array size must be greater than zero and less
    than MPP_C_MAX_ARRAY_SIZE to be valid.

    \param [in] array_ptr     An array.
    \param [in] array_size    Size of the inputted array.
    \param [in] routine_name  Name of the calling routine.
*/
void check_array_input(void const * const array_ptr,
                       size_t const array_size,
                       char const *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Check whether n inputted string is valid.  The inputted character pointer
    must not be null, have a length greater than 0, and be null terminated in
    order to be deemed valid.

    \param [in] input_str         A string.
    \param [in] input_str_length  Length of the inputted string.
    \param [in] routine_name      Name of the calling routine.
*/
void check_string_input(char const *input_str,
                        size_t const input_str_length,
                        char const *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Check whether a inputted pelist is valid.  The inputted pointer is allowed
    to be null, but if it is not null, then the inputted size must be greater
    than zero.

    \param [in] rank_list       An array of rank ids.
    \param [in] rank_list_size  Length of the inputted array of rank ids.
    \param [in] routine_name    Name of the calling routine.
*/
void check_pelist_input(uint32_t const * const rank_list,
                        size_t const rank_list_size,
                        char const *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Function that checks whether an inputted MPI communicator is valid.  The
    inputted communicator must not be equal to MPI_COMM_NULL in order to be
    deemed valid.

    \param [in] comm_id       MPI communicator id.
    \param [in] routine_name  Name of the calling routine.
*/
void check_comm_id_input(MPI_Comm const comm_id,
                         char const *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Function that checks whether an inputted MPI request is valid.  The
    inputted request must not be equal to MPI_REQUEST_NULL in order to be
    deemed valid.

    \param [in] comm_request  MPI request id.
    \param [in] routine_name  Name of the calling routine.
*/
void check_comm_request_input(MPI_Request const comm_request,
                              char const *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Function that checks whether an inputted MPI datatype is valid.  The
    inputted datatype must be one of MPI_INT32_T, MPI_UINT32_T, MPI_INT64_T,
    MPI_UINT64_T, MPI_FLOAT, MPI_DOUBLE, or MPI_CHAR to be deemed valid.

    \param [in] comm_request  MPI datatype.
    \param [in] routine_name  Name of the calling routine.
*/
void check_comm_datatype_input(MPI_Datatype const comm_datatype,
                               char const *routine_name);

/*---------------------------------------------------------------------------*/
/**
    Check the error code returned from a call to a MPI routine.  If the error
    code does not equal MPI_SUCCESS, then throw an internal error.

    \param [in] error_code     MPI error code.
    \param [in] mpi_func_name  Name of the MPI routine that returned the error
                               code.
    \param [in] routine_name   Name of the calling routine.
*/
void check_MPI_error_code(int error_code,
                          char const *mpi_func_name,
                          char const *routine_name);

/*---------------------------------------------------------------------------*/

#endif
