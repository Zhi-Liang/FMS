#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cmpp_io_debug.h"
#include "cmpp_io_file_props_t_api.h"
#include "cmpp_io_helper_functions.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_regular_file_t_api.h"
#include "cmpp_io_utils.h"

/*Helper macros.*/
#define null_regular_file_object(ptr) \
    error_check(ptr, \
                "the cmpp_io_regular_file_t pointer at address %p is null." \
                    "  Please first initialize the object by calling" \
                    " cmpp_io_regular_file_create.", \
                (void *) &ptr);

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Structure definitions.*/

struct cmpp_io_regular_file_type
{
    cmpp_io_file_props_t *file_props; /**<File properties (i.e., name,
                                          is_writer, is_reader, ...).*/
    FILE *file_ptr; /**<File pointer.*/
};

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Create a cmpp_io_regular_file_t object.*/
cmpp_io_regular_file_t *cmpp_io_regular_file_create(cmpp_io_file_props_t * const file_props)
{
    /*Local variables*/
    cmpp_io_regular_file_t *fptr = NULL;

    /*Malloc the structure and set default values.*/
    cmpp_io_safemalloc((void **)(&fptr),
                       sizeof(cmpp_io_regular_file_t));
    cmpp_io_safememset(fptr,
                       0,
                       sizeof(cmpp_io_regular_file_t));

    /*Store the file properties or throw a fatal error if the inputted
      pointer is null.*/
    if (file_props)
    {
        fptr->file_props = file_props;
    }
    else
    {
        fatal("null file properties object at address %p was passed in.",
              (void *) &file_props);
    }

    /*Nullify the FILE pointer.*/
    fptr->file_ptr = NULL;

    return fptr;
}

/*---------------------------------------------------------------------------*/
/*Destroy a cmpp_io_regular_file_t object.*/
void cmpp_io_regular_file_destroy(cmpp_io_regular_file_t **self)
{
    /*Local variables*/
    cmpp_io_regular_file_t *fptr = NULL;

    /*Set the local pointer to the cmpp_io_regular_file_t object.*/
    fptr = *self;

    /*Make sure that the cmpp_io_regular_file_t object has been initialized.*/
    null_regular_file_object(fptr);

    /*Make sure that the file is not still open.*/
/*
    Per Zhi's decision.
    error_check(!cmpp_io_file_props_get_is_open(fptr->file_props),
                "file %s cannot be destroyed because it is still open.",
                cmpp_io_file_props_get_name(fptr->file_props));
*/
    if (cmpp_io_file_props_get_is_open(fptr->file_props))
    {
        warn("file %s is being destroyed because it is still open.",
             cmpp_io_file_props_get_name(fptr->file_props));
    }

    /*Free malloced members.*/
    cmpp_io_safefree((void **)(&(fptr->file_props)));

    /*Free the cmpp_io_regular_file_t object.*/
    cmpp_io_safefree((void **)(&fptr));

    /*Point the inputted pointer to the deallocated object.*/
    *self = fptr;

    return;
}

/*---------------------------------------------------------------------------*/
/*Return a pointer to the file properties object for a cmpp_io_regular_file_t
  object.*/
cmpp_io_file_props_t *cmpp_io_regular_file_get_file_props(cmpp_io_regular_file_t * const self)
{
    /*Make sure that the cmpp_io_regular_file_t object has been initialized.*/
    null_regular_file_object(self);

    return self->file_props;
}

/*---------------------------------------------------------------------------*/
/*Return the FILE pointer associated with the open file.*/
FILE *cmpp_io_regular_file_get_file_ptr(cmpp_io_regular_file_t * const self)
{
    /*Make sure that the cmpp_io_regular_file_t object has been initialized.*/
    null_regular_file_object(self);

    /*Make sure that the file is open.*/
    error_check(cmpp_io_file_props_get_is_open(self->file_props),
                "regular file %s is not currently open, thus it does not"
                    " have a valid FILE pointer associated with it.",
                cmpp_io_file_props_get_name(self->file_props));

    return self->file_ptr;
}

/*---------------------------------------------------------------------------*/
/*Return the name of the file.*/
char *cmpp_io_regular_file_get_name(cmpp_io_regular_file_t * const self)
{
    /*Make sure that the cmpp_io_regular_file_t object has been initialized.*/
    null_regular_file_object(self);

    return cmpp_io_file_props_get_name(self->file_props);
}

/*---------------------------------------------------------------------------*/
/*Open a regular file.*/
void cmpp_io_regular_file_open(cmpp_io_regular_file_t * const * const self)
{
    /*Local variables*/
    cmpp_io_regular_file_t *fptr = NULL;
    char *file_name = NULL;
    int action;
    char file_mode[3];

    /*Set the local pointer to the cmpp_io_regular_file_t object.*/
    fptr = *self;

    /*Make sure that the cmpp_io_regular_file_t object has been initialized.*/
    null_regular_file_object(fptr);

    /*Make sure that the file isn't already open.*/
    file_name = cmpp_io_file_props_get_name(fptr->file_props);
    error_check(!cmpp_io_file_props_get_is_open(fptr->file_props),
                "regular file %s is already open.",
                file_name);

    /*Check if the file exists, and throw an error if an older file will
      be overwritten unintentionally.*/
    action = cmpp_io_file_props_get_action(fptr->file_props);
    cmpp_io_check_file_status_and_action(file_name,
                                         action);

    /*Get the character corresponding to the file action.*/
    cmpp_io_get_stream_fmode(action,
                             file_mode);

    /*Open the file and store the file pointer.*/
    fptr->file_ptr = fopen(file_name,
                           file_mode);
    error_check(fptr->file_ptr,
                "fopen failed for regular file %s.",
                file_name);

    /*Mark the file as open.*/
    cmpp_io_file_props_set_is_open(&(fptr->file_props),
                                   true);
    file_name = NULL;
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Flush a regular file.*/
void cmpp_io_regular_file_flush(cmpp_io_regular_file_t const * const self)
{
    /*Local variables.*/
    char *file_name = NULL;

    /*Make sure that the cmpp_io_regular_file_t object has been initialized.*/
    null_regular_file_object(self);

    /*Make sure that the file is open.*/
    file_name = cmpp_io_file_props_get_name(self->file_props);
    error_check(cmpp_io_file_props_get_is_open(self->file_props),
                "regular file %s is not currently open.",
                file_name);

    /*Print a warning if the file is read only.*/
    if (cmpp_io_file_props_get_action(self->file_props) == (int)CMPP_RDONLY)
    {
        warn("the file %s cannot be flushed because it was opened in"
                 "read-only mode.",
             file_name);
    }

    /*Flush the file.*/
    fflush(self->file_ptr);
    file_name = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Close a regular file.*/
void cmpp_io_regular_file_close(cmpp_io_regular_file_t * const * const self)
{
    /*Local variables*/
    cmpp_io_regular_file_t *fptr = NULL;
    char *file_name = NULL;

    /*Set the local pointer to the cmpp_io_regular_file_t object.*/
    fptr = *self;

    /*Make sure that the cmpp_io_regular_file_t object has been initialized.*/
    null_regular_file_object(fptr);

    /*Make sure that the file is open.*/
    file_name = cmpp_io_file_props_get_name(fptr->file_props);
    error_check(cmpp_io_file_props_get_is_open(fptr->file_props),
                "regular file %s is not currently open.",
                file_name);

    /*Close the file.*/
    error_check(fclose(fptr->file_ptr) == 0,
                "fclose failed for regular file %s.",
                file_name);
    fptr->file_ptr = NULL;

    /*Mark the file as closed.*/
    cmpp_io_file_props_set_is_open(&(fptr->file_props),
                                   false);
    file_name = NULL;
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Delete a regular file.*/
void cmpp_io_regular_file_delete(cmpp_io_regular_file_t const * const self)
{
    /*Local variables*/
    char *file_name = NULL;

    /*Make sure that the cmpp_io_regular_file_t object has been initialized.*/
    null_regular_file_object(self);

    /*Make sure that the file is closed.*/
    file_name = cmpp_io_file_props_get_name(self->file_props);
    error_check(!cmpp_io_file_props_get_is_open(self->file_props),
                "regular file %s is currently open.",
                file_name);

    /*Delete the file.*/
    cmpp_io_delete_file(file_name);
    file_name = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Perform a fortran style read of a list of values from a regular file.*/
void cmpp_io_regular_file_fread(cmpp_io_regular_file_t const * const self,
                                int const num_vals,
                                int const data_type,
                                int const val_max_width,
                                char const delimiter,
                                void **data_buf,
                                bool const last_read)
{
    /*Local variables*/
    char *file_name = NULL;
    char val[CMPP_IO_FREAD_MAX_VAL_WIDTH];
    int counter;
    int i;
    int cur_char;
    bool start;
    char *bptr = NULL;
    size_t val_len;
    char *end = NULL;
    long int lval;
    int ival;
    float fval;
    double dval;

    /*Make sure that the cmpp_io_regular_file_t object has been initialized.*/
    null_regular_file_object(self);

    /*Make sure that the file is open.*/
    file_name = cmpp_io_file_props_get_name(self->file_props);
    error_check(cmpp_io_file_props_get_is_open(self->file_props),
                "regular file %s is not currently open.",
                file_name);

    /*Make sure that the inputted data buffer is not null.*/
    error_check(data_buf,
                "inputted buffer at address %p is null.",
                (void *) &data_buf);

    /*Make sure that the number of values to be read is valid.*/
    error_check(num_vals > 0 && num_vals <= CMPP_IO_FREAD_MAX_NUM_VALS,
                "the inputted number of values to be read (%d) from file"
                    " %s must be > 0 and < CMPP_IO_FREAD_MAX_NUM_VALS (%d).",
                num_vals,
                file_name,
                CMPP_IO_FREAD_MAX_NUM_VALS);

    /*Make sure the width of each value does not exceed the size of the
      internal buffer.*/
    error_check(val_max_width > 0 && val_max_width < CMPP_IO_FREAD_MAX_VAL_WIDTH,
                "maximum width of a value (%d) in file %s must be > 0 and"
                    " < CMPP_IO_FREAD_MAX_VAL_WIDTH (%d).",
                val_max_width,
                file_name,
                CMPP_IO_FREAD_MAX_VAL_WIDTH);

    /*Zero out the counters.*/
    counter = 0;
    i = 0;

    /*Initialize character buffers.*/
    memset(val,
           '\0',
           sizeof(char)*CMPP_IO_FREAD_MAX_VAL_WIDTH);
    cur_char = '\0';

    /*Loop until all values are found.*/
    start = false;
    bptr = (char *)(*data_buf);
    while (counter < num_vals)
    {
        /*Get the next character.*/
        cur_char = fgetc(self->file_ptr);
        error_check(cur_char != EOF,
                    "end of file %s has been reached when only %d/%d values"
                        " have been found.",
                    file_name,
                    counter,
                    num_vals);

        if (cur_char == ' ' || cur_char == '\t' || cur_char == '\n' ||
                cur_char == delimiter)
        {
            if (start)
            {
                /*Store the read in string.*/
                val[i] = '\0';
                val_len = strlen(val);
                error_check(val_len <= (size_t)val_max_width,
                            "read in string %s in file %s is longer than"
                                " the inputted maximum width %d.",
                            val,
                            file_name,
                            val_max_width);

                errno = 0;
                switch(data_type)
                {
                    case CMPP_IO_CHAR:
                        snprintf((char *)bptr,
                                 val_len+1,
                                 "%s",
                                 val);
                        bptr = (char *)(*(data_buf+counter+1));
                        break;

                    case CMPP_IO_INT:
                        lval = strtol(val,
                                      (char **)(&end),
                                      10);
                        error_check(*end == '\0' && errno == 0,
                                    "strtol failed for string %s in file %s.",
                                    val,
                                    file_name);

                        /*Cast to int.*/
                        error_check(lval >= INT_MIN && lval <= INT_MAX,
                                    "read in value (%ld) in file %s is not"
                                        " representable as an int.",
                                    lval,
                                    file_name);
                        ival = (int)lval;

                        memcpy(bptr,
                               &ival,
                               sizeof(int));
                        bptr += sizeof(int);
                        break;

                    case CMPP_IO_FLOAT:
                        fval = strtof(val,
                                      (char **)(&end));
                        error_check(*end == '\0' && errno == 0,
                                    "strtof failed for string %s in file %s.",
                                    val,
                                    file_name);
                        memcpy(bptr,
                               &fval,
                               sizeof(float));
                        bptr += sizeof(float);
                        break;

                    case CMPP_IO_DOUBLE:
                        dval = strtod(val,
                                      (char **)(&end));
                        error_check(*end == '\0' && errno == 0,
                                    "strtod failed for string %s in file %s.",
                                    val,
                                    file_name);
                        memcpy(bptr,
                               &dval,
                               sizeof(double));
                        bptr += sizeof(double);
                        break;

                    default:
                        fatal("unrecognized data type (%d).  Must be one of"
                                  " CMPP_IO_CHAR (%d), CMPP_IO_INT (%d),"
                                  " CMPP_IO_FLOAT (%d), or CMPP_IO_DOUBLE (%d).",
                              data_type,
                              CMPP_IO_CHAR,
                              CMPP_IO_INT,
                              CMPP_IO_FLOAT,
                              CMPP_IO_DOUBLE);
                }

                /*Iterate counter, and reset val and associated flags/
                  iterators.*/
                counter++;
                start = false;
                memset(val,
                       '0',
                       sizeof(char)*CMPP_IO_FREAD_MAX_VAL_WIDTH);
                i = 0;
            }
        }
        else
        {
            /*Store the read in non-whitespace or non-delimiting character.*/
            if (!start)
            {
                start = true;
            }

            error_check(cur_char >= CHAR_MIN && cur_char <= CHAR_MAX,
                        "read in character %d must be >= %d and <= %d in"
                            " order to fit into a char.",
                        cur_char,
                        CHAR_MIN,
                        CHAR_MAX);
            val[i] = (char)cur_char;
            i++;
        }
    }
    bptr = NULL;
    end = NULL;

    /*Read to the start of the next new line.*/
    while (cur_char != '\n' && !last_read)
    {
        cur_char = fgetc(self->file_ptr);
        error_check(cur_char != EOF,
                    "end of file %s has been reached.  If this is the last"
                        " line that will be read from the file, then"
                        " please pass in true for last_read (%s).",
                    file_name,
                    last_read ? "true" : "false");
        continue;
    }
    file_name = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
