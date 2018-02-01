#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "cmpp_io_attribute_t_api.h"
#include "cmpp_io_debug.h"
#include "cmpp_io_dimension_t_api.h"
#include "cmpp_io_file_props_t_api.h"
#include "cmpp_io_helper_functions.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_netcdf_file_t_api.h"
#include "cmpp_io_netcdf_utils.h"
#include "cmpp_io_utils.h"
#include "cmpp_io_variable_t_api.h"
#include "netcdf.h"

/*Helper macros.*/
#define null_netcdf_file_object(ptr) \
    error_check(ptr, \
                "the cmpp_io_netcdf_file_t pointer at address %p is null." \
                    "  Please first initialize the object by calling" \
                    " cmpp_io_netcdf_file_create.", \
                (void *) &ptr);

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Structure definitions.*/

struct cmpp_io_netcdf_file_type
{
    cmpp_io_file_props_t *file_props; /**<File properties (i.e., name,
                                          is_writer, is_reader, ...).*/
    int ncid; /**<netcdf file id.*/
    bool is_in_define_mode; /**<Flag telling if the netcdf file is
                                in define mode.*/
    int max_num_global_attributes; /**<Maximum number of
                                       global attributes that are
                                       allowed in the file.*/
    int max_num_dimensions; /**<Maximum number of
                                dimensions that are allowed
                                in the file.*/
    int max_num_variables; /**<Maximum number of
                               variables that are allowed
                               in the file.*/
    int max_num_attributes_per_variable; /**<Maximum number of attributes
                                             that are allowed per variable
                                             in the file.*/
    int max_num_dimensions_per_variable; /**<Maximum number of dimensions
                                             that are allowed per
                                             variable in the file.*/
    size_t max_metadata_num_bytes; /**<Maximum allowed size of
                                       the total metadata for a
                                       file in bytes.*/
    int num_global_attributes; /**<Number of global attributes
                                   in the file.*/
    int num_dimensions; /**<Number of dimensions in
                            the file.*/
    int num_variables; /**<Number of fields in the
                           file.*/
    size_t metadata_num_bytes; /**<Size of the total metadata
                                   in bytes.*/
    cmpp_io_attribute_t **global_attributes; /**<Array of global attributes
                                                 in the file.*/
    cmpp_io_dimension_t **dimensions; /**<Array of dimensions in the
                                          file.*/
    cmpp_io_variable_t **variables; /**<Array of variables in the
                                        file.*/
};

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Create a cmpp_io_netcdf_file_t object.*/
cmpp_io_netcdf_file_t *cmpp_io_netcdf_file_create(cmpp_io_file_props_t * const file_props,
                                                  int const max_num_global_attributes,
                                                  int const max_num_dimensions,
                                                  int const max_num_variables,
                                                  int const max_num_attributes_per_variable,
                                                  int const max_num_dimensions_per_variable,
                                                  size_t const max_metadata_num_bytes)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    char *file_name = NULL;
    int i;

    /*Malloc the structure and set default values.*/
    cmpp_io_safemalloc((void **)(&fptr),
                       sizeof(cmpp_io_netcdf_file_t));
    cmpp_io_safememset(fptr,
                       0,
                       sizeof(cmpp_io_netcdf_file_t));

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

    /*Store the maximum sizes.*/
    file_name = cmpp_io_file_props_get_name(file_props);
    error_check((max_num_global_attributes >= 0) &&
                    (max_num_global_attributes <=
                    (int)CMPP_IO_MAX_GLOBAL_ATTS_PER_FILE),
                "the inputted maximum number of global attributes (%d)"
                    " for netcdf file %s must be >=0 and <="
                    " CMPP_IO_MAX_GLOBAL_ATTS_PER_FILE (%d).",
                max_num_global_attributes,
                file_name,
                (int)CMPP_IO_MAX_GLOBAL_ATTS_PER_FILE);
    fptr->max_num_global_attributes = max_num_global_attributes;

    error_check((max_num_dimensions >= 0) &&
                    (max_num_dimensions <= (int)CMPP_IO_MAX_DIMS_PER_FILE),
                "the inputted maximum number of dimensions (%d)"
                    " for netcdf file %s must be >=0 and <="
                    " CMPP_IO_MAX_DIMS_PER_FILE (%d).",
                max_num_dimensions,
                file_name,
                (int)CMPP_IO_MAX_DIMS_PER_FILE);
    fptr->max_num_dimensions = max_num_dimensions;

    error_check((max_num_variables >= 0) &&
                    (max_num_variables <= (int)CMPP_IO_MAX_VARS_PER_FILE),
                "the inputted maximum number of variables (%d)"
                    " for netcdf file %s must be >=0 and <="
                    " CMPP_IO_MAX_VARS_PER_FILE (%d).",
                max_num_variables,
                file_name,
                (int)CMPP_IO_MAX_VARS_PER_FILE);
    fptr->max_num_variables = max_num_variables;

    error_check((max_num_attributes_per_variable >= 0) &&
                    (max_num_attributes_per_variable <=
                    (int)CMPP_IO_MAX_ATTS_PER_VAR),
                "the inputted maximum number of attributes"
                    " per variable (%d)"
                    " for netcdf file %s must be >=0 and <="
                    " CMPP_IO_MAX_ATTS_PER_VAR (%d).",
                max_num_attributes_per_variable,
                file_name,
                (int)CMPP_IO_MAX_ATTS_PER_VAR);
    fptr->max_num_attributes_per_variable = max_num_attributes_per_variable;

    error_check((max_num_dimensions_per_variable >= 0) &&
                    (max_num_dimensions_per_variable <=
                    (int)CMPP_IO_MAX_DIMS_PER_VAR),
                "the inputted maximum number of dimensions"
                    " per variable (%d)"
                    " for netcdf file %s must be >=0 and <="
                    " CMPP_IO_MAX_DIMS_PER_VAR (%d).",
                max_num_dimensions_per_variable,
                file_name,
                (int)CMPP_IO_MAX_DIMS_PER_VAR);
    fptr->max_num_dimensions_per_variable = max_num_dimensions_per_variable;

    error_check(max_metadata_num_bytes <=
                    (size_t)CMPP_IO_MAX_METADATA_BYTES_PER_FILE,
                "the inputted maximum metadata size in bytes (%zu)"
                    " for netcdf file %s must be >=0 and <="
                    " CMPP_IO_MAX_METADATA_BYTES_PER_FILE (%zu).",
                max_metadata_num_bytes,
                file_name,
                (size_t)CMPP_IO_MAX_METADATA_BYTES_PER_FILE);
    fptr->max_metadata_num_bytes = max_metadata_num_bytes;

    /*Allocate arrays of global attribute, dimension, and variable object
      pointers.*/
    if (fptr->max_num_global_attributes > 0)
    {
        cmpp_io_safemalloc((void **) (&(fptr->global_attributes)),
                           sizeof(cmpp_io_attribute_t *)*
                               ((size_t)fptr->max_num_global_attributes));
        for (i=0;i<fptr->max_num_global_attributes;i++)
        {
            fptr->global_attributes[i] = NULL;
        }
    }
    if (fptr->max_num_dimensions > 0)
    {
        cmpp_io_safemalloc((void **) (&(fptr->dimensions)),
                           sizeof(cmpp_io_dimension_t *)*
                               ((size_t)fptr->max_num_dimensions));
        for (i=0;i<fptr->max_num_dimensions;i++)
        {
            fptr->dimensions[i] = NULL;
        }
    }
    if (fptr->max_num_variables > 0)
    {
        cmpp_io_safemalloc((void **) (&(fptr->variables)),
                           sizeof(cmpp_io_variable_t *)*
                               ((size_t)fptr->max_num_variables));
        for (i=0;i<fptr->max_num_variables;i++)
        {
            fptr->variables[i] = NULL;
        }
    }

    return fptr;
}

/*---------------------------------------------------------------------------*/
/*Destroy a cmpp_io_netcdf_file_t object.*/
void cmpp_io_netcdf_file_destroy(cmpp_io_netcdf_file_t **self)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    int i;

    /*Set the local pointer to the cmpp_io_netcdf_file_t object.*/
    fptr = *self;

    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(fptr);

    /*Make sure that the file is not still open.*/
/*
    Per Zhi's decision.

    error_check(!cmpp_io_file_props_get_is_open(fptr->file_props),
                "netcdf file %s cannot be destroyed because it is"
                    " still open.",
                cmpp_io_file_props_get_name(fptr->file_props));
*/
    if (cmpp_io_file_props_get_is_open(fptr->file_props))
    {
        warn("netcdf file %s is being destroyed while it is"
                 " still open.",
             cmpp_io_file_props_get_name(fptr->file_props));
    }

    /*Free malloced members.*/
    cmpp_io_file_props_destroy(&(fptr->file_props));
    if (fptr->variables != NULL)
    {
        for (i=0;i<fptr->num_variables;i++)
        {
            if (fptr->variables[i] != NULL)
            {
                cmpp_io_variable_destroy(&(fptr->variables[i]));
            }
        }
        cmpp_io_safefree((void **) &(fptr->variables));
    }
    if (fptr->dimensions != NULL)
    {
        for (i=0;i<fptr->num_dimensions;i++)
        {
            if (fptr->dimensions[i] != NULL)
            {
                cmpp_io_dimension_destroy(&(fptr->dimensions[i]));
            }
        }
        cmpp_io_safefree((void **) &(fptr->dimensions));
    }
    if (fptr->global_attributes != NULL)
    {
        for (i=0;i<fptr->num_global_attributes;i++)
        {
            if (fptr->global_attributes[i] != NULL)
            {
                cmpp_io_attribute_destroy(&(fptr->global_attributes[i]));
            }
        }
        cmpp_io_safefree((void **) &(fptr->global_attributes));
    }

    /*Free the cmpp_io_netcdf_file_t object.*/
    cmpp_io_safefree((void **)(&fptr));

    /*Point the inputted pointer to the deallocated object.*/
    *self = fptr;

    return;
}

/*---------------------------------------------------------------------------*/
/*Return a pointer to the file properties object for a cmpp_io_netcdf_file_t
  object.*/
cmpp_io_file_props_t *cmpp_io_netcdf_file_get_file_props(cmpp_io_netcdf_file_t * const self)
{
    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(self);

    return self->file_props;
}

/*---------------------------------------------------------------------------*/
/*Return the name of the file.*/
char *cmpp_io_netcdf_file_get_name(cmpp_io_netcdf_file_t * const self)
{
    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(self);

    return cmpp_io_file_props_get_name(self->file_props);
}

/*---------------------------------------------------------------------------*/
/*Return the netcdf file id associated with the open file.*/
int cmpp_io_netcdf_file_get_ncid(cmpp_io_netcdf_file_t const * const self)
{
    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(self);

    /*Make sure that the file is open.*/
    error_check(cmpp_io_file_props_get_is_open(self->file_props),
                "netcdf file %s is not currently open, thus it does not"
                    " have a valid netcdf id associated with it.",
                cmpp_io_file_props_get_name(self->file_props));

    return self->ncid;
}

/*---------------------------------------------------------------------------*/
/*Return a flag telling if the netcdf file is in define mode.*/
bool cmpp_io_netcdf_file_get_is_in_define_mode(cmpp_io_netcdf_file_t const * const self)
{
    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(self);

    /*Make sure that the file is open.*/
    error_check(cmpp_io_file_props_get_is_open(self->file_props),
                "netcdf file %s is not currently open, thus it does not"
                    " have a (define or data) mode.",
                cmpp_io_file_props_get_name(self->file_props));

    return self->is_in_define_mode;
}

/*---------------------------------------------------------------------------*/
/*Return the number of dimensions currently in the file.*/
int cmpp_io_netcdf_file_get_num_dimensions(cmpp_io_netcdf_file_t const * const self)
{
    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(self);

    return self->num_dimensions;
}

/*---------------------------------------------------------------------------*/
/*Open a netcdf file.*/
void cmpp_io_netcdf_file_open(cmpp_io_netcdf_file_t * const * const self)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    char *file_name = NULL;
    int action;
    size_t fsize;
    int ncflags;
    size_t initial_size;
    int err;
    int old_fill_mode;

    /*Set the local pointer to the cmpp_io_netcdf_file_t object.*/
    fptr = *self;

    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(fptr);

    /*Make sure that the file isn't already open.*/
    file_name = cmpp_io_file_props_get_name(fptr->file_props);
    error_check(!cmpp_io_file_props_get_is_open(fptr->file_props),
                "netcdf file %s is already open.",
                file_name);

    /*Check if the file exists, and throw an error if an older file will
      be overwritten unintentionally.*/
    action = cmpp_io_file_props_get_action(fptr->file_props);
    cmpp_io_check_file_status_and_action(file_name,
                                         action);

    /*Get the netcdf chunk size for the file.*/
    fsize = cmpp_io_get_netCDF_chunksize(file_name);

    /*Get the default netCDF flags for the file.*/
    ncflags = cmpp_io_get_ncflags();

    /*Open the file and store its netcdf id.*/
    initial_size = 0;
    switch (action)
    {
        case CMPP_RDONLY:
            err = nc__open(file_name,
                           (ncflags | NC_NOWRITE),
                           &fsize,
                           &(fptr->ncid));
            fptr->is_in_define_mode = false;
            break;
        case CMPP_WRONLY:
            err = nc__create(file_name,
                             (ncflags | NC_NOCLOBBER),
                             initial_size,
                             &fsize,
                             &(fptr->ncid));
            fptr->is_in_define_mode = true;
            break;
        case CMPP_OVERWR:
            err = nc__create(file_name,
                             (ncflags | NC_CLOBBER),
                             initial_size,
                             &fsize,
                             &(fptr->ncid));
            fptr->is_in_define_mode = true;
            break;
        case CMPP_APPEND:
            err = nc__open(file_name,
                           NC_WRITE,
                           &fsize,
                           &(fptr->ncid));
            fptr->is_in_define_mode = false;
            break;
    }
    check_netcdf_call(err);

    /*Switch to "no-fill" mode for non-readonly files.*/
    if (action != CMPP_RDONLY)
    {
        check_netcdf_call(nc_set_fill(fptr->ncid,
                                      NC_NOFILL,
                                      &old_fill_mode));
    }

    /*Mark the file as open.*/
    cmpp_io_file_props_set_is_open(&(fptr->file_props),
                                   true);
    file_name = NULL;
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Flush a netcdf file.*/
void cmpp_io_netcdf_file_flush(cmpp_io_netcdf_file_t const * const self)
{
    /*Local variables.*/
    char *file_name = NULL;
    int err;

    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(self);

    /*Make sure that the file is open.*/
    file_name = cmpp_io_file_props_get_name(self->file_props);
    error_check(cmpp_io_file_props_get_is_open(self->file_props),
                "netcdf file %s is not currently open.",
                file_name);

    /*Make sure that the file is in data mode.*/
    error_check(!(self->is_in_define_mode),
                "the netcdf file %s is currently in define mode.  Please"
                    " switch the file to data mode so that the flush may"
                    " be performed.",
                file_name);

    /*Print a warning if the file is read only.*/
    if (cmpp_io_file_props_get_action(self->file_props) == (int)CMPP_RDONLY)
    {
        warn("the file %s cannot be flushed because it was opened in"
                 "read-only mode.",
             file_name);
    }

    /*Flush the file.*/
    err = nc_sync(self->ncid);
    check_netcdf_call(err);
    file_name = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Close a netcdf file.*/
void cmpp_io_netcdf_file_close(cmpp_io_netcdf_file_t * const * const self)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    char *file_name = NULL;
    int err;

    /*Set the local pointer to the cmpp_io_netcdf_file_t object.*/
    fptr = *self;

    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(fptr);

    /*Make sure that the file is open.*/
    file_name = cmpp_io_file_props_get_name(fptr->file_props);
    error_check(cmpp_io_file_props_get_is_open(fptr->file_props),
                "netcdf file %s is not currently open.",
                file_name);

    /*Close the file.*/
    err = nc_close(fptr->ncid);
    check_netcdf_call(err);

    /*Mark the file as closed.*/
    cmpp_io_file_props_set_is_open(&(fptr->file_props),
                                   false);
    file_name = NULL;
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Delete a netcdf file.*/
void cmpp_io_netcdf_file_delete(cmpp_io_netcdf_file_t const * const self)
{
    /*Local variables*/
    char *file_name = NULL;

    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(self);

    /*Make sure that the file is closed.*/
    file_name = cmpp_io_file_props_get_name(self->file_props);
    error_check(!cmpp_io_file_props_get_is_open(self->file_props),
                "netcdf file %s is currently open.",
                file_name);

    /*Delete the file.*/
    cmpp_io_delete_file(file_name);
    file_name = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Add a pointer to the file's global attributes, dimensions, or variables
  array and return its index.*/
int cmpp_io_netcdf_file_add_ptr(cmpp_io_netcdf_file_t * const * const self,
                                void * const ptr,
                                int const ptr_type)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    int index;

    /*Set the local pointer to the cmpp_io_netcdf_file_t object.*/
    fptr = *self;

    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(fptr);

    /*Add the pointer to the appropriate array.*/
    switch (ptr_type)
    {
         case ATT_PTR:
             index = add_pointer_to_array((void **)(fptr->global_attributes),
                                          fptr->max_num_global_attributes,
                                          &(fptr->num_global_attributes),
                                          ptr);
             break;
         case DIM_PTR:
             index = add_pointer_to_array((void **)(fptr->dimensions),
                                          fptr->max_num_dimensions,
                                          &(fptr->num_dimensions),
                                          ptr);
             break;
         case VAR_PTR:
             index = add_pointer_to_array((void **)(fptr->variables),
                                          fptr->max_num_variables,
                                          &(fptr->num_variables),
                                          ptr);
             break;
         default:
             fatal("inputted pointer type (%d) not recognized.  Must be"
                       " one of: ATT_PTR (%d), DIM_PTR (%d), or VAR_PTR"
                       " (%d).",
                   ptr_type,
                   ATT_PTR,
                   DIM_PTR,
                   VAR_PTR);
    }
    fptr = NULL;

    return index;
}

/*---------------------------------------------------------------------------*/
/*Remove a pointer from the file's global attributes, dimensions, or variables
  array and set its index to CMPP_IO_INDEX_NOT_FOUND.*/
void cmpp_io_netcdf_file_remove_ptr(cmpp_io_netcdf_file_t * const * const self,
                                    int * const index,
                                    int const ptr_type)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cleanup_t cfunc;
    void **array = NULL;
    int array_size;

    /*Set the local pointer to the cmpp_io_netcdf_file_t object.*/
    fptr = *self;

    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(fptr);

    /*Remove the pointer from the appropriate array.*/
    switch (ptr_type)
    {
         case ATT_PTR:
             cfunc.type = ATT_CLEANUP;
             cfunc.fp.att_cleanup = cmpp_io_attribute_destroy;
             array = (void **)(fptr->global_attributes);
             array_size = fptr->max_num_global_attributes;
             break;

         case DIM_PTR:
             cfunc.type = DIM_CLEANUP;
             cfunc.fp.dim_cleanup = cmpp_io_dimension_destroy;
             array = (void **)(fptr->dimensions);
             array_size = fptr->max_num_dimensions;
             break;

         case VAR_PTR:
             cfunc.type = VAR_CLEANUP;
             cfunc.fp.var_cleanup = cmpp_io_variable_destroy;
             array = (void **)(fptr->variables);
             array_size = fptr->max_num_variables;
             break;

         default:
             fatal("inputted pointer type (%d) not recognized.  Must be"
                       " one of: ATT_PTR (%d), DIM_PTR (%d), or VAR_PTR"
                       " (%d).",
                   ptr_type,
                   ATT_PTR,
                   DIM_PTR,
                   VAR_PTR);
    }

    remove_pointer_from_array(array,
                              array_size,
                              index,
                              &cfunc);
    fptr = NULL;
    array = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Return a pointer to a global attribute, dimension, or variable stored
  at the inputted index in the file's global attributes, dimensions, or
  variables array.*/
void *cmpp_io_netcdf_file_get_ptr(cmpp_io_netcdf_file_t const * const self,
                                  int const index,
                                  int const ptr_type)
{
    /*Local variables*/
    void *ptr = NULL;
    void **array = NULL;
    int array_size;

    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(self);

    /*Get the pointer from the appropriate array.*/
    switch (ptr_type)
    {
         case ATT_PTR:
             array = (void **)(self->global_attributes);
             array_size = self->num_global_attributes;
             break;

         case DIM_PTR:
             array = (void **)(self->dimensions);
             array_size = self->num_dimensions;
             break;

         case VAR_PTR:
             array = (void **)(self->variables);
             array_size = self->num_variables;
             break;

         default:
             fatal("inputted pointer type (%d) not recognized.  Must be"
                       " one of: ATT_PTR (%d), DIM_PTR (%d), or VAR_PTR"
                       " (%d).",
                   ptr_type,
                   ATT_PTR,
                   DIM_PTR,
                   VAR_PTR);
    }

    ptr = get_pointer_by_index(array,
                               array_size,
                               index);
    array = NULL;

    return ptr;
}

/*---------------------------------------------------------------------------*/
/*Given the name of a global attribute, dimension, or variable, return its
  index if in the file's corresponding array.  If not found, return
  CMPP_IO_INDEX_NOT_FOUND.*/
int cmpp_io_netcdf_file_get_ptr_index(cmpp_io_netcdf_file_t const * const self,
                                      char * const val,
                                      int const ptr_type)
{
    /*Local variables*/
    compare_t cfunc;
    int index;
    void **array = NULL;
    int array_size;
    int empty_index;

    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(self);

    /*Search for the pointer.*/
    switch (ptr_type)
    {
         case ATT_PTR:
             cfunc.type = ATT_NAME;
             cfunc.fp.att_name = cmpp_io_attribute_get_name;
             array = (void **)(self->global_attributes);
             array_size = self->num_global_attributes;
             break;

         case DIM_PTR:
             cfunc.type = DIM_NAME;
             cfunc.fp.dim_name = cmpp_io_dimension_get_name;
             array = (void **)(self->dimensions);
             array_size = self->num_dimensions;
             break;

         case VAR_PTR:
             cfunc.type = VAR_NAME;
             cfunc.fp.var_name = cmpp_io_variable_get_name;
             array = (void **)(self->variables);
             array_size = self->num_variables;
             break;

         default:
             fatal("inputted pointer type (%d) not recognized.  Must be"
                       " one of: ATT_PTR (%d), DIM_PTR (%d), or VAR_PTR"
                       " (%d).",
                   ptr_type,
                   ATT_PTR,
                   DIM_PTR,
                   VAR_PTR);
    }

    index = find_pointer_in_array(array,
                                  array_size,
                                  (void *) val,
                                  &cfunc,
                                  &empty_index);
    array = NULL;

    return index;
}

/*---------------------------------------------------------------------------*/
/*Given the netcdf id of a dimension or variable, return
  a pointer to the object in the file's corresponding array.  If not found,
  then throw an error.*/
void *cmpp_io_netcdf_file_get_ptr_by_netcdf_id(cmpp_io_netcdf_file_t const * const self,
                                               int netcdf_id,
                                               int const ptr_type)
{
    /*Local variables*/
    compare_t cfunc;
    int index;
    void **array = NULL;
    int array_size;
    int empty_index;
    void *ptr = NULL;

    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(self);

    /*Search for the pointer.*/
    switch (ptr_type)
    {
         case DIM_PTR:
             cfunc.type = DIM_NC_ID;
             cfunc.fp.dim_id = cmpp_io_dimension_get_netcdf_id;
             array = (void **)(self->dimensions);
             array_size = self->num_dimensions;
             break;

         case VAR_PTR:
             cfunc.type = VAR_NC_ID;
             cfunc.fp.var_id = cmpp_io_variable_get_netcdf_id;
             array = (void **)(self->variables);
             array_size = self->num_variables;
             break;

         default:
             fatal("inputted pointer type (%d) not recognized.  Must be"
                       " one of: DIM_PTR (%d) or VAR_PTR (%d).",
                   ptr_type,
                   DIM_PTR,
                   VAR_PTR);
    }

    index = find_pointer_in_array(array,
                                  array_size,
                                  (void *) &netcdf_id,
                                  &cfunc,
                                  &empty_index);

    if (index != CMPP_IO_INDEX_NOT_FOUND)
    {
        /*Convert from index to pointer.*/
        ptr = cmpp_io_netcdf_file_get_ptr(self,
                                          index,
                                          ptr_type);
    }
    else
    {
        fatal("netcdf id %d is not defined in file %s.",
              netcdf_id,
              cmpp_io_file_props_get_name(self->file_props));
    }

    return ptr;
}

/*---------------------------------------------------------------------------*/
/*Switch the netcdf file from define mode to data mode.*/
void cmpp_io_netcdf_file_enter_data_mode(cmpp_io_netcdf_file_t * const * const self,
                                         int const header_buffer_val)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    char *file_name = NULL;
    int err;

    /*Set the local pointer to the cmpp_io_netcdf_file_t object.*/
    fptr = *self;

    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(fptr);

    /*Make sure that the file is open.*/
    file_name = cmpp_io_file_props_get_name(fptr->file_props);
    error_check(cmpp_io_file_props_get_is_open(fptr->file_props),
                "netcdf file %s is not currently open.",
                file_name);

    /*Make sure that the file is in define mode.*/
    error_check(fptr->is_in_define_mode,
                "the netcdf file %s is not currently in define mode, so"
                    " it may not be switched to data mode.",
                file_name);
    file_name = NULL;

    /*Switch the file into data mode.*/
    if (header_buffer_val > 0)
    {
        err = nc__enddef(fptr->ncid,
                         (size_t)header_buffer_val,
                         4,
                         0,
                         4);
    }
    else
    {
        err = nc_enddef(fptr->ncid);
    }
    check_netcdf_call(err);

    /*Set the is_in_define_mode flag to false.*/
    fptr->is_in_define_mode = false;
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Switch the netcdf file from data mode to define mode.*/
void cmpp_io_netcdf_file_enter_define_mode(cmpp_io_netcdf_file_t * const * const self)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    char *file_name = NULL;
    int err;

    /*Set the local pointer to the cmpp_io_netcdf_file_t object.*/
    fptr = *self;

    /*Make sure that the cmpp_io_netcdf_file_t object has been initialized.*/
    null_netcdf_file_object(fptr);

    /*Make sure that the file is open.*/
    file_name = cmpp_io_file_props_get_name(fptr->file_props);
    error_check(cmpp_io_file_props_get_is_open(fptr->file_props),
                "netcdf file %s is not currently open.",
                file_name);

    /*Make sure that the file is in data mode.*/
    error_check(!fptr->is_in_define_mode,
                "the netcdf file %s is not currently in data mode, so"
                    " it may not be switched to define mode.",
                file_name);
    file_name = NULL;

    /*Switch the file into define mode.*/
    err = nc_redef(fptr->ncid);
    check_netcdf_call(err);

    /*Set the is_in_define_mode flag to true.*/
    fptr->is_in_define_mode = true;
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
