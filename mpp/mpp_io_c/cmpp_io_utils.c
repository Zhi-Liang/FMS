#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include "cmpp_io_attribute_t_api.h"
#include "cmpp_io_debug.h"
#include "cmpp_io_dimension_t_api.h"
#include "cmpp_io_utils.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_variable_t_api.h"

/*---------------------------------------------------------------------------*/
/*This function checks for the existence of the file and returns true or
  false.

  Note: if the following operation is to open, and you wish for this
  be atomic, you should use some other method, like open with flags ...
*/
bool cmpp_io_inquire(char const * const fname,
                     struct stat *res_stat)
{
    /*Local variables*/
    struct stat fstatus;
    int statres;
    bool res;

    /*Reset the value of errno.*/
    errno = 0;

    /*Perform a stat call on the inputted file.*/
    statres = stat(fname,
                   &fstatus);

    /*Initialize the res flag to false.*/
    res = false;

    /*Check for errors.*/
    if (errno == 0 && statres == 0)
    {
        /*If there are no errors, then the file exists.*/
        res = true;
    }
    else
    {
        switch (errno)
        {
            case 0:
                /*This case should never occur if statres is not 0.*/
                break;

            case ENOENT:
                /*The stat call exited ok, but the file name does not
                  exit.*/
                break;

            case ENOTDIR:
                /*A file that isn't a directory was specified when a
                  directory is required.*/
                fatal("passed fname=%s, returned with path error.",
                      fname);
                break;

            case EACCES:
                /*Permission denied.*/
                fatal("passed fname=%s, returned with access permission"
                          " error (errno=%d, errst=%s).",
                      fname,
                      errno,
                      strerror(errno));
                break;

            default:
                /*Default message for less common errors.*/
                fatal("passed fname=%s, returned with errno=%d, errst=%s.\n",
                      fname,
                      errno,
                      strerror(errno));
                break;
        }
    }

    /*If a pointer to a non-null structure was passed in, fill the
      structure with the structured returned from stat.*/
    if (res_stat != NULL)
    {
        *res_stat = fstatus;
    }

    return res;
}

/*---------------------------------------------------------------------------*/
/*Calculate the size of the file.*/
size_t cmpp_io_file_size(char const * const fsize,
                         char const * const fname)
{
    /*Local variables*/
    struct stat st;
    int64_t size = 0;
    char lastchar;

    /* get last char as upper */
    if (fsize != NULL)
    {
        lastchar = (char)toupper(fsize[strlen(fsize)-1]);
    }
    else
    {
        fatal("null string at address %p passed in to fsize.",
              (void *) &fsize);
    }

    if (strcmp(fsize,"file") == 0)
    {
        /* get stats of fname */
        if (cmpp_io_inquire(fname,
                            &st))
        {
            size = st.st_size;
        }
    }
    else if ((lastchar >= 'A') && (lastchar <= 'Z'))
    {
        /* suffix handling */
        switch(lastchar)
        {
            case 'K':
                size = size*1024;
                break;

            case 'M':
                size = size*(1024*1024);
                break;

            case 'G':
                size = size*(1024*1024*1024);
                break;

            default:
                break;
        }
    }
    else
    {
        /* read filesize as int*/
        sscanf(fsize,
               "%"PRId64,
               &size);
    }

    if (size == 0)
    {
        size = 65536;
    }

    return (size_t)size;
}

/*---------------------------------------------------------------------------*/
/*Set the mode that will be used when the file is opended with fopen.*/
void cmpp_io_get_stream_fmode(int const action,
                              char *mode)
{
    /*Set mode to the correct string based on the inputted action.*/
    switch (action)
    {
        case CMPP_RDONLY:
            strcpy(mode,
                   "r");
            break;

        case CMPP_OVERWR:
            strcpy(mode,
                   "w");
            break;

        case CMPP_APPEND:
            strcpy(mode,
                   "a");
            break;

        case CMPP_WRONLY:
            strcpy(mode,
                   "w");
            break;

        default:
            fatal("the inputted action flag %d is not currently implemented.",
                  action);
            break;
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*If reading, make sure that the inputted file exists.  If writing, make sure
  that the file does not already exist, unless overwriting is specified.*/
void cmpp_io_check_file_status_and_action(char const * const fname,
                                          int const action)
{
    /*Make sure that the file status is compatible with the inputted action.*/
    if (!cmpp_io_inquire(fname,
                         NULL))
    {
        if (action == CMPP_APPEND || action == CMPP_RDONLY)
        {
            fatal("file %s does not exist.",
                  fname);
        }
    }
    else
    {
        if (action == CMPP_WRONLY)
        {
            fatal("you will overwrite existing file %s.",
                  fname);
        }
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Delete a file.*/
void cmpp_io_delete_file(char const *fileName)
{
    /*Local variables*/
    int err;

    /*Delete the file.*/
    err = unlink(fileName);
    error_check(err == 0,
                "unlink for file %s returned error=%d, errstr=%s.\n",
                fileName,
                err,
                strerror(err));

    return;
}

/*---------------------------------------------------------------------------*/
/*Add a pointer to an array of pointers and return its index.*/
int add_pointer_to_array(void **ptr_array,
                         int const ptr_array_size,
                         int * const num_array_spots_filled,
                         void *ptr)
{
    /*Check inputs.*/
    error_check(ptr_array,
                "inputted pointer array at address %p is null.",
                (void *) &ptr_array);

    error_check(num_array_spots_filled,
                "inputted pointer to the number of array spots that"
                    " are currently filled at address %p is null.",
                (void *) &num_array_spots_filled);

    error_check(*num_array_spots_filled >= 0,
                "inputted number of array spots filled (%d) must be >= 0.",
                *num_array_spots_filled);

    error_check(ptr,
                "inputted pointer (that will be added to the pointer array)"
                    " at address %p is null.",
                (void *) &ptr);

    /*Iterate the number of spots filled in the array.  Throw an error
      if this number exceeds the size of the pointer array.*/
    int index = *num_array_spots_filled;
    (*num_array_spots_filled)++;
    error_check(*num_array_spots_filled <= ptr_array_size,
                "the number of array spots filled (%d) exceeds the"
                    " size of the inputted array (%d).",
                *num_array_spots_filled,
                ptr_array_size);

    /*Place the pointer into the array.*/
    error_check(ptr_array[index] == NULL,
                "index %d in the inputted pointer array is not null.  An"
                    " existing pointer cannot be overwritten to guard"
                    " against memory leaks.",
                index);
    ptr_array[index] = ptr;

    return index;
}

/*---------------------------------------------------------------------------*/
/*Given the index of a pointer in an array of pointers, return the pointer.*/
void *get_pointer_by_index(void **ptr_array,
                           int const ptr_array_size,
                           int const ptr_index)
{
    /*Check inputs.*/
    error_check(ptr_array,
                "inputted pointer array at address %p is null.",
                (void *) &ptr_array);

    error_check(ptr_index >= 0 && ptr_index < ptr_array_size,
                "inputted array index (%d) must be >= 0 and < the size"
                   " of the array (%d).",
                ptr_index,
                ptr_array_size);

    return ptr_array[ptr_index];
}

/*---------------------------------------------------------------------------*/
/*Given the index of a pointer in an array of pointers, remove the pointer,
  performing any cleanup as necessary.  Reset the index to a value of -1.*/
void remove_pointer_from_array(void **ptr_array,
                               int const ptr_array_size,
                               int * const ptr_index,
                               cleanup_t *cfunc)
{
    /*Check inputs.*/
    error_check(ptr_index,
                "inputted pointer to the array index at address %p is null.",
                (void *) &ptr_index);

    /*Remove the pointer.*/
    void *ptr = get_pointer_by_index(ptr_array,
                                     ptr_array_size,
                                     *ptr_index);

    error_check(ptr,
                "pointer at index %d is null.",
                *ptr_index)

    switch (cfunc->type)
    {
        case ATT_CLEANUP:
            cfunc->fp.att_cleanup((cmpp_io_attribute_t **)(&ptr));
            break;

        case DIM_CLEANUP:
            cfunc->fp.dim_cleanup((cmpp_io_dimension_t **)(&ptr));
            break;

        case VAR_CLEANUP:
            cfunc->fp.var_cleanup((cmpp_io_variable_t **)(&ptr));
            break;

        case NETCDF_FILE_CLEANUP:
            cfunc->fp.netcdf_file_cleanup((cmpp_io_netcdf_file_t **)(&ptr));
            break;

        case REGULAR_FILE_CLEANUP:
            cfunc->fp.regular_file_cleanup((cmpp_io_regular_file_t **)(&ptr));
            break;

        case HDF5_FILE_CLEANUP:
            cfunc->fp.hdf5_file_cleanup((cmpp_io_hdf5_file_t **)(&ptr));
            break;

        default:
            fatal("inputted cleanup type (%d) not recognized.  Must be"
                      " one of: ATT_CLEANUP (%d), DIM_CLEANUP (%d),"
                      " VAR_CLEANUP (%d), NETCDF_FILE_CLEANUP (%d),"
                      " REGULAR_FILE_CLEANUP (%d), or"
                      " HDF5_FILE_CLEANUP (%d).",
                  cfunc->type,
                  ATT_CLEANUP,
                  DIM_CLEANUP,
                  VAR_CLEANUP,
                  NETCDF_FILE_CLEANUP,
                  REGULAR_FILE_CLEANUP,
                  HDF5_FILE_CLEANUP);
    }
    ptr_array[*ptr_index] = NULL;
    *ptr_index = CMPP_IO_INDEX_NOT_FOUND;

    return;
}

/*---------------------------------------------------------------------------*/
/*Search a pointer array for a pointer that corresponds to a certain string
  or int.  If found, then return the index of the pointer in the array.  If
  not, then instead return CMPP_IO_INDEX_NOT_FOUND.*/
int find_pointer_in_array(void **ptr_array,
                          int const ptr_search_size,
                          void * const in_val,
                          compare_t *cfunc,
                          int *empty_index)
{
    /*Local variables*/
    size_t in_val_num_bytes;
    int i;
    void *buf = NULL;
    size_t buf_num_bytes;
    int val;
    int res;

    /*Check inputs.*/
    error_check(ptr_array,
                "inputted pointer array at address %p is null.",
                (void *) &ptr_array);

    error_check(ptr_search_size >= 0,
                "inputted number of elements of the pointer array that"
                    " will be searched (%d) must be >= 0.",
                ptr_search_size);

    error_check(in_val,
                "inputted value pointer at address %p is null.",
                (void *) &in_val);

    error_check(cfunc,
                "inputted compare structure at address %p is null.",
                (void *) &cfunc);

    error_check(empty_index,
                "inputted empty index pointer at address %p is null.",
                (void *) &empty_index);

    /*Store the number of bytes of the inputted value.*/
    if (cfunc->type == ATT_NAME || cfunc->type == DIM_NAME ||
            cfunc->type == VAR_NAME || cfunc->type == REGULAR_FILE_NAME ||
            cfunc->type == NETCDF_FILE_NAME || cfunc->type == HDF5_FILE_NAME)
    {
        in_val_num_bytes = sizeof(char)*strlen((char *)in_val);
    }
    else if (cfunc->type == DIM_NC_ID || cfunc->type == VAR_NC_ID)
    {
        in_val_num_bytes = sizeof(int);
    }
    else
    {
        fatal("unrecognized compare type (%d).  This should be one of:"
                  " ATT_NAME (%d), DIM_NAME (%d), VAR_NAME (%d),"
                  " REGULAR_FILE_NAME (%d), NETCDF_FILE_NAME (%d),"
                  " HDF5_FILE_NAME (%d), DIM_NC_ID (%d), or VAR_NC_ID (%d).",
              cfunc->type,
              ATT_NAME,
              DIM_NAME,
              VAR_NAME,
              REGULAR_FILE_NAME,
              NETCDF_FILE_NAME,
              HDF5_FILE_NAME,
              DIM_NC_ID,
              VAR_NC_ID);
    }

    /*Initialize the empty index placeholder.  This is implemented so that it
      if there are "holes" (empty spots) in the array, they may be refilled.*/
    *empty_index = (int)CMPP_IO_INDEX_NOT_FOUND;

    /*Search for the pointer with the matching value.*/
    for (i=0;i<ptr_search_size;i++)
    {
        if (ptr_array[i])
        {
            switch (cfunc->type)
            {
                case ATT_NAME:
                    buf = (void *)(cfunc->fp.att_name((cmpp_io_attribute_t *)(ptr_array[i])));
                    buf_num_bytes = sizeof(char)*strlen((char *)buf);
                    break;

                case DIM_NAME:
                    buf = (void *)(cfunc->fp.dim_name((cmpp_io_dimension_t *)(ptr_array[i])));
                    buf_num_bytes = sizeof(char)*strlen((char *)buf);
                    break;

                case VAR_NAME:
                    buf = (void *)(cfunc->fp.var_name((cmpp_io_variable_t *)(ptr_array[i])));
                    buf_num_bytes = sizeof(char)*strlen((char *)buf);
                    break;

                case REGULAR_FILE_NAME:
                    buf = (void *)(cfunc->fp.regular_file_name((cmpp_io_regular_file_t *)(ptr_array[i])));
                    buf_num_bytes = sizeof(char)*strlen((char *)buf);
                    break;

                case NETCDF_FILE_NAME:
                    buf = (void *)(cfunc->fp.netcdf_file_name((cmpp_io_netcdf_file_t *)(ptr_array[i])));
                    buf_num_bytes = sizeof(char)*strlen((char *)buf);
                    break;

                case HDF5_FILE_NAME:
                    buf = (void *)(cfunc->fp.hdf5_file_name((cmpp_io_hdf5_file_t *)(ptr_array[i])));
                    buf_num_bytes = sizeof(char)*strlen((char *)buf);
                    break;

                case DIM_NC_ID:
                    val = cfunc->fp.dim_id((cmpp_io_dimension_t *)(ptr_array[i]));
                    buf = (void *)(&val);
                    buf_num_bytes = sizeof(int);
                    break;

                case VAR_NC_ID:
                    val = cfunc->fp.var_id((cmpp_io_variable_t *)(ptr_array[i]));
                    buf = (void *)(&val);
                    buf_num_bytes = sizeof(int);
                    break;
            }

            if (buf_num_bytes == in_val_num_bytes)
            {
                /*Compare the values.  If they match, return the index.*/
                res = memcmp(in_val,
                             buf,
                             buf_num_bytes);
                if (res == 0)
                {
                    buf = NULL;
                    return i;
                }
            }
        }
        else if (*empty_index == CMPP_IO_INDEX_NOT_FOUND)
        {
            *empty_index = i;
        }
    }
    buf = NULL;

    return CMPP_IO_INDEX_NOT_FOUND;
}

/*---------------------------------------------------------------------------*/
