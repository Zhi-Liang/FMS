#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "cmpp_io_context_t_api.h"
#include "cmpp_io_debug.h"
#include "cmpp_io_file_props_t_api.h"
#include "cmpp_io_hdf5_file_t_api.h"
#include "cmpp_io_helper_functions.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_netcdf_file_t_api.h"
#include "cmpp_io_regular_file_t_api.h"
#include "cmpp_io_utils.h"

/*Helper macros.*/
#define null_context_object(ptr) \
    error_check(ptr, \
                "the cmpp_io_context_t pointer at address %p is null." \
                    "  Please first initialized the object by calling" \
                    " cmpp_io_context_create.", \
                (void *) &ptr);

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Structure definitions.*/

struct cmpp_io_context_type
{
    /*netcdf files.*/
    cmpp_io_netcdf_file_t **netcdf_files; /**<Array of netCDF files.*/
    int max_netcdf_files_allowed; /**<Maximum number of netCDF files
                                      allowed.*/
    int cur_num_netcdf_files; /**<Current number of netcdf files.*/

    /*regular files.*/
    cmpp_io_regular_file_t **regular_files; /**<Array of regular files.*/
    int max_regular_files_allowed; /**<Maximum number of regular files
                                       allowed.*/
    int cur_num_regular_files; /**<Current number of regular files.*/

    /*hdf5 files.*/
    cmpp_io_hdf5_file_t **hdf5_files; /**<Array of hdf5 files.*/
    int max_hdf5_files_allowed; /**<Maximum number of hdf5 files
                                    allowed.*/
    int cur_num_hdf5_files; /**<Current number of hdf5 files.*/

    /*Namelist parameters.*/
    bool debug_flag; /**<Flag telling whether or not to
                         print debugging information.*/
    bool verbose_flag; /**<Flag telling whether or not to
                           print verbose messages.*/
    int header_buffer_val; /**<*/
    int shuffle; /**<*/
    int deflate; /**<*/
    int deflate_level; /**<*/
};

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Create a cmpp_io_context_t object.*/
cmpp_io_context_t *cmpp_io_context_create(int const max_num_netcdf_files,
                                          int const max_num_regular_files,
                                          int const max_num_hdf5_files,
                                          bool const debug_flag,
                                          bool const verbose_flag,
                                          int const header_buffer_val,
                                          int const shuffle,
                                          int const deflate,
                                          int const deflate_level)
{
    /*Local variables*/
    cmpp_io_context_t *cptr = NULL;
    int i;

    /*Make sure that the inputted maximum number of netCDF files is valid.*/
    error_check((max_num_netcdf_files >= 0) &&
                    (max_num_netcdf_files <=
                    (int)CMPP_IO_MAX_NUM_NETCDF_FILES_ALLOWED),
                "the inputted maximum number of netCDF files allowed (%d)"
                    " must be > 0 and <="
                    " CMPP_IO_MAX_NUM_NETCDF_FILES_ALLOWED (%d).",
                max_num_netcdf_files,
                (int)CMPP_IO_MAX_NUM_NETCDF_FILES_ALLOWED);

    /*Make sure that the inputted maximum number of regular files is valid.*/
    error_check((max_num_regular_files >= 0) &&
                    (max_num_regular_files <=
                    (int)CMPP_IO_MAX_NUM_REGULAR_FILES_ALLOWED),
                "the inputted maximum number of regular files allowed (%d)"
                    " must be > 0 and <="
                    " CMPP_IO_MAX_NUM_REGULAR_FILES_ALLOWED (%d).",
                max_num_regular_files,
                (int)CMPP_IO_MAX_NUM_REGULAR_FILES_ALLOWED);

    /*Make sure that the inputted maximum number of hdf5 files is valid.*/
    error_check((max_num_hdf5_files >= 0) &&
                    (max_num_hdf5_files <=
                    (int)CMPP_IO_MAX_NUM_HDF5_FILES_ALLOWED),
                "the inputted maximum number of hdf5 files allowed (%d)"
                    " must be > 0 and <="
                    " CMPP_IO_MAX_NUM_HDF5_FILES_ALLOWED (%d).",
                max_num_hdf5_files,
                (int)CMPP_IO_MAX_NUM_HDF5_FILES_ALLOWED);

    /*Make sure that the inputted deflate and deflate level values are
      valid.*/
    if (deflate != 0)
    {
        error_check(deflate_level >= 0 && deflate_level <= 9,
                    "if the deflate parameter (%d) does not equal zero, then"
                        "the deflate level parameter (%d) must be >= 0 and"
                        " <= 9.",
                    deflate,
                    deflate_level);
    }

    /*Malloc space and set all values to zero.*/
    cmpp_io_safemalloc((void **) (&cptr),
                       sizeof(cmpp_io_context_t));
    cmpp_io_safememset(cptr,
                       0,
                       sizeof(cmpp_io_context_t));

    /*Malloc an array of cmpp_io_netcdf_file_t type objects.*/
    cptr->netcdf_files = NULL;
    cptr->max_netcdf_files_allowed = max_num_netcdf_files;
    if (cptr->max_netcdf_files_allowed > 0)
    {
        cmpp_io_safemalloc((void **) &(cptr->netcdf_files),
                           sizeof(cmpp_io_netcdf_file_t *)*
                               ((size_t)cptr->max_netcdf_files_allowed));
        for (i=0;i<(cptr->max_netcdf_files_allowed);i++)
        {
            cptr->netcdf_files[i] = NULL;
        }
    }

    /*Malloc an array of cmpp_io_regular_file_t type objects.*/
    cptr->regular_files = NULL;
    cptr->max_regular_files_allowed = max_num_regular_files;
    if (cptr->max_regular_files_allowed > 0)
    {
        cmpp_io_safemalloc((void **) &(cptr->regular_files),
                           sizeof(cmpp_io_regular_file_t *)*
                               ((size_t)cptr->max_regular_files_allowed));
        for (i=0;i<(cptr->max_regular_files_allowed);i++)
        {
            cptr->regular_files[i] = NULL;
        }
    }

    /*Malloc an array of cmpp_io_hdf5_file_t type objects.*/
    cptr->hdf5_files = NULL;
    cptr->max_hdf5_files_allowed = max_num_hdf5_files;
    if (cptr->max_hdf5_files_allowed > 0)
    {
        cmpp_io_safemalloc((void **) &(cptr->hdf5_files),
                           sizeof(cmpp_io_hdf5_file_t *)*
                               ((size_t)cptr->max_hdf5_files_allowed));
        for (i=0;i<(cptr->max_hdf5_files_allowed);i++)
        {
            cptr->hdf5_files[i] = NULL;
        }
    }

    /*Store inputted parameters.*/
    cptr->debug_flag = debug_flag;
    cptr->verbose_flag = verbose_flag;
    cptr->header_buffer_val = header_buffer_val;
    cptr->shuffle = shuffle;
    cptr->deflate = deflate;
    cptr->deflate_level = deflate_level;

    return cptr;
}

/*---------------------------------------------------------------------------*/
/*Destroy a cmpp_io_context_t object.*/
void cmpp_io_context_destroy(cmpp_io_context_t **self)
{
    /*Local variables*/
    cmpp_io_context_t *cptr = NULL;
    int i;

    /*Set the local pointer to the cmpp_io_context_t object.*/
    cptr = *self;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    null_context_object(cptr);

    /*Free all netcdf file objects.*/
    if (cptr->netcdf_files != NULL)
    {
        for (i=0;i<cptr->cur_num_netcdf_files;i++)
        {
            if (cptr->netcdf_files[i] != NULL)
            {
                cmpp_io_netcdf_file_destroy(&(cptr->netcdf_files[i]));
            }
        }
        cmpp_io_safefree((void **) &(cptr->netcdf_files));
    }

    /*Free all regular file objects.*/
    if (cptr->regular_files != NULL)
    {
        for (i=0;i<cptr->cur_num_regular_files;i++)
        {
            if (cptr->regular_files[i] != NULL)
            {
                cmpp_io_regular_file_destroy(&(cptr->regular_files[i]));
            }
        }
        cmpp_io_safefree((void **) &(cptr->regular_files));
    }

    /*Free all hdf5 file objects.*/
    if (cptr->hdf5_files != NULL)
    {
        for (i=0;i<cptr->cur_num_hdf5_files;i++)
        {
            if (cptr->hdf5_files[i] != NULL)
            {
                cmpp_io_hdf5_file_destroy(&(cptr->hdf5_files[i]));
            }
        }
        cmpp_io_safefree((void **) &(cptr->hdf5_files));
    }

    /*Free the cmpp_io_context_t object.*/
    cmpp_io_safefree((void **) (&cptr));

    /*Point the inputted pointer to the deallocated object.*/
    *self = cptr;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the context's netcdf files array.*/
cmpp_io_netcdf_file_t **cmpp_io_context_get_netcdf_files(cmpp_io_context_t * const self)
{
    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    return self->netcdf_files;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the maximum number of netcdf files allowed in the
  context's netcdf files array.*/
int cmpp_io_context_get_max_netcdf_files_allowed(cmpp_io_context_t const * const self)
{
    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    return self->max_netcdf_files_allowed;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the current number of netcdf files contained in the
  context's file array.*/
int cmpp_io_context_get_cur_num_netcdf_files(cmpp_io_context_t const * const self)
{
    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    return self->cur_num_netcdf_files;
}

/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the context's regular files array.*/
cmpp_io_regular_file_t **cmpp_io_context_get_regular_files(cmpp_io_context_t * const self)
{
    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    return self->regular_files;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the maximum number of regular files allowed in the
  context's regular files array.*/
int cmpp_io_context_get_max_regular_files_allowed(cmpp_io_context_t const * const self)
{
    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    return self->max_regular_files_allowed;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the current number of regular files contained in the
  context's file array.*/
int cmpp_io_context_get_cur_num_regular_files(cmpp_io_context_t const * const self)
{
    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    return self->cur_num_regular_files;
}

/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the context's hdf5 files array.*/
cmpp_io_hdf5_file_t **cmpp_io_context_get_hdf5_files(cmpp_io_context_t * const self)
{
    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    return self->hdf5_files;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the maximum number of hdf5 files allowed in the
  context's hdf5 files array.*/
int cmpp_io_context_get_max_hdf5_files_allowed(cmpp_io_context_t const * const self)
{
    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    return self->max_hdf5_files_allowed;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the current number of hdf5 files contained in the
  context's file array.*/
int cmpp_io_context_get_cur_num_hdf5_files(cmpp_io_context_t const * const self)
{
    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    return self->cur_num_hdf5_files;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the value of the debug flag for a cmpp_io_context_t
  object.*/
bool cmpp_io_context_get_debug_flag(cmpp_io_context_t const * const self)
{
    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    return self->debug_flag;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the value of the verbose flag for a cmpp_io_context_t
  object.*/
bool cmpp_io_context_get_verbose_flag(cmpp_io_context_t const * const self)
{
    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    return self->verbose_flag;
}

/*---------------------------------------------------------------------------*/
/*???*/
int cmpp_io_context_get_header_buffer_val(cmpp_io_context_t const * const self)
{
    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    return self->header_buffer_val;
}

/*---------------------------------------------------------------------------*/
/*???*/
int cmpp_io_context_get_shuffle(cmpp_io_context_t const * const self)
{
    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    return self->shuffle;
}

/*---------------------------------------------------------------------------*/
/*???*/
int cmpp_io_context_get_deflate(cmpp_io_context_t const * const self)
{
    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    return self->deflate;
}

/*---------------------------------------------------------------------------*/
/*???*/
int cmpp_io_context_get_deflate_level(cmpp_io_context_t const * const self)
{
    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    return self->deflate_level;
}

/*---------------------------------------------------------------------------*/
/*Given the name and type of a file, return it's index in the context's
  corresponding files array.*/
int cmpp_io_context_get_file_index(cmpp_io_context_t const * const self,
                                   char const * const fname,
                                   int const ftype,
                                   int *empty_index)
{
    /*Local variables*/
    compare_t cfunc;
    int index;
    void **array = NULL;
    int array_size;

    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    switch (ftype)
    {
        case CMPP_ASCII:
            cfunc.type = REGULAR_FILE_NAME;
            cfunc.fp.regular_file_name = cmpp_io_regular_file_get_name;
            array = (void **)(self->regular_files);
            array_size = self->cur_num_regular_files;
            break;

        case CMPP_NETCDF:
            cfunc.type = NETCDF_FILE_NAME;
            cfunc.fp.netcdf_file_name = cmpp_io_netcdf_file_get_name;
            array = (void **)(self->netcdf_files);
            array_size = self->cur_num_netcdf_files;
            break;

        case CMPP_HDF5:
            cfunc.type = HDF5_FILE_NAME;
            cfunc.fp.hdf5_file_name = cmpp_io_hdf5_file_get_name;
            array = (void **)(self->hdf5_files);
            array_size = self->cur_num_hdf5_files;
            break;

        default:
            fatal("inputted file type (%d) not recognized.  Must be one"
                      " of: CMPP_ASCII (%d), CMPP_NETCDF (%d), or"
                      " CMPP_HDF5 (%d).",
                  ftype,
                  CMPP_ASCII,
                  CMPP_NETCDF,
                  CMPP_HDF5);
    }

    index = find_pointer_in_array(array,
                                  array_size,
                                  (void *) fname,
                                  &cfunc,
                                  empty_index);
    array = NULL;

    return index;
}

/*---------------------------------------------------------------------------*/
/*Add a pointer to a file to the context's corresponding files arrays and
  return its array index.*/
int cmpp_io_context_add_file(cmpp_io_context_t * const * const self,
                             void * const fptr,
                             int const file_type)
{
    /*Local variables*/
    cmpp_io_context_t *cptr = NULL;
    void **array = NULL;
    int *array_size = NULL;
    int max_array_size;
    char *file_name = NULL;
    int file_index;
    int empty_index;

    /*Set the local pointer to the cmpp_io_context_t object.*/
    cptr = *self;

    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(cptr);

    /*Check if the file already exists in the inputted context's array.  If
      so, throw a fatal error.*/
    switch (file_type)
    {
        case CMPP_ASCII:
            array = (void **)(cptr->regular_files);
            array_size = &(cptr->cur_num_regular_files);
            max_array_size = cptr->max_regular_files_allowed;
            file_name = cmpp_io_regular_file_get_name((cmpp_io_regular_file_t *)fptr);
            break;

        case CMPP_NETCDF:
            array = (void **)(cptr->netcdf_files);
            array_size = &(cptr->cur_num_netcdf_files);
            max_array_size = cptr->max_netcdf_files_allowed;
            file_name = cmpp_io_netcdf_file_get_name((cmpp_io_netcdf_file_t *)fptr);
            break;

        case CMPP_HDF5:
            array = (void **)(cptr->hdf5_files);
            array_size = &(cptr->cur_num_hdf5_files);
            max_array_size = cptr->max_hdf5_files_allowed;
            file_name = cmpp_io_hdf5_file_get_name((cmpp_io_hdf5_file_t *)fptr);
            break;

        default:
            fatal("inputted file type (%d) not recognized.  Must be one"
                      " of: CMPP_ASCII (%d), CMPP_NETCDF (%d), or"
                      " CMPP_HDF5 (%d).",
                  file_type,
                  CMPP_ASCII,
                  CMPP_NETCDF,
                  CMPP_HDF5);
    }

    file_index = cmpp_io_context_get_file_index(cptr,
                                                file_name,
                                                file_type,
                                                &empty_index);

#ifdef NOLEGACY
    error_check(file_index == CMPP_IO_INDEX_NOT_FOUND,
                "the file %s already exists for the inputted context."
                    "  This means the file has most likely already been"
                    " opened.",
                file_name);
    file_name = NULL;
#else
    /*Per Zhi's decision.  Almost certainly going to lead to memory leaks
      or crashes.*/
    if (file_index != CMPP_IO_INDEX_NOT_FOUND)
    {
        warn("the file %s was already opened.  To match legacy behavior,"
                 " another file separate file structure will be allocated."
                 "  This will almost certainly cause a memory leak, unless"
                 " all individual file structures are freed.",
             file_name);
    }
#endif

    if (empty_index == CMPP_IO_INDEX_NOT_FOUND)
    {
        /*Iterate the number of netcdf files and make sure that the number
          of files does not exceed the maximum number allowed.*/
        file_index = add_pointer_to_array(array,
                                          max_array_size,
                                          array_size,
                                          fptr);
    }
    else
    {
        /*Set the file index to be an empty spot in the array.*/
        file_index = empty_index;
        array[file_index] = fptr;
    }
    cptr = NULL;
    array = NULL;
    array_size = NULL;

    return file_index;
}

/*---------------------------------------------------------------------------*/
/*Return a pointer to the file object stored at the inputted index in the
  inputted context's corresponding files array.*/
void *cmpp_io_context_get_file_ptr(cmpp_io_context_t * const self,
                                   int const file_index,
                                   int const file_type)
{
    /*Local variables*/
    void *ptr = NULL;
    void **array = NULL;
    int array_size;

    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(self);

    switch (file_type)
    {
        case CMPP_ASCII:
            array = (void **)(self->regular_files);
            array_size = self->cur_num_regular_files;
            break;

        case CMPP_NETCDF:
            array = (void **)(self->netcdf_files);
            array_size = self->cur_num_netcdf_files;
            break;

        case CMPP_HDF5:
            array = (void **)(self->hdf5_files);
            array_size = self->cur_num_hdf5_files;
            break;

        default:
            fatal("inputted file type (%d) not recognized.  Must be one"
                      " of: CMPP_ASCII (%d), CMPP_NETCDF (%d), or"
                      " CMPP_HDF5 (%d).",
                  file_type,
                  CMPP_ASCII,
                  CMPP_NETCDF,
                  CMPP_HDF5);
    }

    ptr = get_pointer_by_index(array,
                               array_size,
                               file_index);
    array = NULL;

    return ptr;
}

/*---------------------------------------------------------------------------*/
/*Remove a file object from the inputted context's corresponding files
  array, free the object, and reset the inputted file_index to
  CMPP_IO_INDEX_NOT_FOUND.*/
void cmpp_io_context_remove_file(cmpp_io_context_t * const * const self,
                                 int * const file_index,
                                 int const file_type)
{
    /*Local variables*/
    cmpp_io_context_t *cptr = NULL;
    cleanup_t cfunc;
    void **array = NULL;
    int array_size;

    /*Set the local pointer to the cmpp_io_context_t object.*/
    cptr = *self;

    /*Make sure that the cmpp_io_context_t object has been initialized.*/
    null_context_object(cptr);

    switch (file_type)
    {
        case CMPP_ASCII:
            cfunc.type = REGULAR_FILE_CLEANUP;
            cfunc.fp.regular_file_cleanup = cmpp_io_regular_file_destroy;
            array = (void **)(cptr->regular_files);
            array_size = cptr->cur_num_regular_files;
            break;

        case CMPP_NETCDF:
            cfunc.type = NETCDF_FILE_CLEANUP;
            cfunc.fp.netcdf_file_cleanup = cmpp_io_netcdf_file_destroy;
            array = (void **)(cptr->netcdf_files);
            array_size = cptr->cur_num_netcdf_files;
            break;

        case CMPP_HDF5:
            cfunc.type = HDF5_FILE_CLEANUP;
            cfunc.fp.hdf5_file_cleanup = cmpp_io_hdf5_file_destroy;
            array = (void **)(cptr->hdf5_files);
            array_size = cptr->cur_num_hdf5_files;
            break;

        default:
            fatal("inputted file type (%d) not recognized.  Must be one"
                      " of: CMPP_ASCII (%d), CMPP_NETCDF (%d), or"
                      " CMPP_HDF5 (%d).",
                  file_type,
                  CMPP_ASCII,
                  CMPP_NETCDF,
                  CMPP_HDF5);
    }

    remove_pointer_from_array(array,
                              array_size,
                              file_index,
                              &cfunc);
    cptr = NULL;
    array = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
