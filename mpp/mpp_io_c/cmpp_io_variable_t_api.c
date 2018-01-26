#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "cmpp_io_attribute_t_api.h"
#include "cmpp_io_debug.h"
#include "cmpp_io_dimension_t_api.h"
#include "cmpp_io_helper_functions.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_netcdf_utils.h"
#include "cmpp_io_utils.h"
#include "cmpp_io_variable_t_api.h"
#include "netcdf.h"

/*Helper macros.*/
#define null_var_object(ptr) \
    error_check(ptr, \
                "the cmpp_io_variable_t pointer at address %p is null." \
                    "  Please first initialize the object by calling" \
                    " cmpp_io_variable_create.", \
                (void *) &ptr);

#define buffered_data_check(ptr) \
    error_check(cmpp_io_variable_get_is_data_buffered(ptr), \
                "data has not yet been buffered for variable %s.  Please" \
                    " buffer data to for this variable by calling" \
                    " cmpp_io_variable_buffer_data.", \
                ptr->name);

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Structure definitions.*/

struct cmpp_io_variable_type
{
    bool is_data_buffered; /**<Flag telling if the data has
                               been buffered for the object.*/
    int max_num_attributes; /**<Maximum number of allowed
                                attributes for the variable.*/
    char name[CMPP_IO_MAX_VAR_NAME_LEN]; /**<Name of the variable.*/
    nc_type type_in_file; /**<Type that the variable will be
                              written to the netCDF file as.*/
    int num_dimensions; /**<Number of dimensions for the
                            variable.*/
    int num_attributes; /**<Number of attributes for the
                            variable.*/
    cmpp_io_dimension_t **dimensions; /**<Array of dimensions for the
                                          variable.*/
    cmpp_io_attribute_t **attributes; /**<Array of attributes for the
                                          variable.*/
    int netcdf_id; /**<netCDF id for the variable.*/
    void *data; /**<Data for variable.*/
    size_t *corner_indices; /**<Array of the corner indices for the
                                variable's data.*/
    size_t *edge_lengths; /**<Array of the corner indices for the
                              variable's data.*/
    nc_type type_in_mem; /**<Type of the data in memory.*/
};

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Create a cmpp_io_variable_t object.*/
cmpp_io_variable_t *cmpp_io_variable_create(char const * const name,
                                            nc_type const type_in_file,
                                            int const netcdf_id,
                                            cmpp_io_dimension_t * const * const dimensions,
                                            int const num_dimensions,
                                            int const max_num_attributes)
{
    /*Local variables*/
    cmpp_io_variable_t *vptr = NULL;
    int i;

    /*Malloc the structure and set default values.*/
    cmpp_io_safemalloc((void **) &vptr,
                       sizeof(cmpp_io_variable_t));
    cmpp_io_safememset(vptr,
                       0,
                       sizeof(cmpp_io_variable_t));

    /*Store the inputted variable name.  If the length of the name exceeds
      CMPP_IO_MAX_VAR_NAME_LEN characters, then throw a fatal error.*/
    safe_string_copy(name,
                     (size_t)CMPP_IO_MAX_VAR_NAME_LEN,
                     vptr->name);

    /*Store the type that the variable will be written to the netcdf file
      as.*/
    vptr->type_in_file = type_in_file;

    /*Store the netcdf id of the variable.*/
    vptr->netcdf_id = netcdf_id;

    /*Store the number of dimensions associated with the variable.*/
    vptr->num_dimensions = num_dimensions;

    if (num_dimensions > 0)
    {
        /*Store the inputted dimension pointers in an array.*/
        cmpp_io_safemalloc((void **) &(vptr->dimensions),
                           sizeof(cmpp_io_dimension_t *)*((size_t)num_dimensions));
        for (i=0;i<num_dimensions;i++)
        {
            vptr->dimensions[i] = dimensions[i];
        }
    }

    /*Store the maximum number of attributes that can be associated with
      the variable.*/
    vptr->max_num_attributes = max_num_attributes;

    if (max_num_attributes > 0)
    {
        /*Allocate an array of attribute object pointers where attributes
          added to the variable will be stored.*/
        cmpp_io_safemalloc((void **) &(vptr->attributes),
                           sizeof(cmpp_io_attribute_t *)*((size_t)max_num_attributes));
        for (i=0;i<max_num_attributes;i++)
        {
            vptr->attributes[i] = NULL;
        }
    }

    /*Nullify pointers.*/
    vptr->data = NULL;
    vptr->corner_indices = NULL;
    vptr->edge_lengths = NULL;

    /*Set flags to false.*/
    vptr->is_data_buffered = false;

    return vptr;
}

/*---------------------------------------------------------------------------*/
/*Destroy a cmpp_io_variable_t object.*/
void cmpp_io_variable_destroy(cmpp_io_variable_t **self)
{
    /*Local variables*/
    cmpp_io_variable_t *vptr = NULL;
    int i;

    /*Set the local pointer to the cmpp_io_variable_t object.*/
    vptr = *self;

    /*Make sure that the cmpp_io_variable_t object has been initialized.*/
    null_var_object(vptr);

    /*Nullify pointers and free malloced arrays.*/
    if (vptr->dimensions != NULL)
    {
        for (i=0;i<vptr->num_dimensions;i++)
        {
            if (vptr->dimensions[i] != NULL)
            {
                vptr->dimensions[i] = NULL;
            }
        }
        cmpp_io_safefree((void **) &(vptr->dimensions));
    }
    if (vptr->attributes != NULL)
    {
        for (i=0;i<vptr->num_attributes;i++)
        {
            if (vptr->attributes[i] != NULL)
            {
                cmpp_io_attribute_destroy(&(vptr->attributes[i]));
            }
        }
        cmpp_io_safefree((void **) &(vptr->attributes));
    }
    if (vptr->data != NULL)
    {
        cmpp_io_safefree((void **) &(vptr->data));
    }
    if (vptr->corner_indices != NULL)
    {
        cmpp_io_safefree((void **) &(vptr->corner_indices));
    }
    if (vptr->edge_lengths != NULL)
    {
        cmpp_io_safefree((void **) &(vptr->edge_lengths));
    }

    /*Free the cmpp_io_variable_t object.*/
    cmpp_io_safefree((void **) (&vptr));

    /*Point the inputted pointer at the deallocated object.*/
    *self = vptr;

    return;
}

/*---------------------------------------------------------------------------*/
/*Return the value of the flag telling if the data has been buffered for a
  cmpp_io_variable_t object.*/
bool cmpp_io_variable_get_is_data_buffered(cmpp_io_variable_t const * const self)
{
    /*Make sure that the cmpp_io_variable_t object has been initialized.*/
    null_var_object(self);

    return self->is_data_buffered;
}

/*---------------------------------------------------------------------------*/
/*Return the maximum number of attributes allowed for a cmpp_io_variable_t
  object.*/
int cmpp_io_variable_get_max_num_attributes(cmpp_io_variable_t const * const self)
{
    /*Make sure that the cmpp_io_variable_t object has been initialized.*/
    null_var_object(self);

    return self->max_num_attributes;
}

/*---------------------------------------------------------------------------*/
/*Return the name of the variable for a cmpp_io_variable_t object.*/
char *cmpp_io_variable_get_name(cmpp_io_variable_t * const self)
{
    /*Make sure that the cmpp_io_variable_t object has been initialized.*/
    null_var_object(self);

    return self->name;
}

/*---------------------------------------------------------------------------*/
/*Return the type that the variable data will be stored as in the netCDF
  file for a cmpp_io_variable_t object.*/
nc_type cmpp_io_variable_get_type_in_file(cmpp_io_variable_t const * const self)
{
    /*Make sure that the cmpp_io_variable_t object has been initialized.*/
    null_var_object(self);

    return self->type_in_file;
}

/*---------------------------------------------------------------------------*/
/*Return the number of dimensions for a cmpp_io_variable_t object.*/
int cmpp_io_variable_get_num_dimensions(cmpp_io_variable_t const * const self)
{
    /*Make sure that the cmpp_io_variable_t object has been initialized.*/
    null_var_object(self);

    return self->num_dimensions;
}

/*---------------------------------------------------------------------------*/
/*Return the number of attributes for a cmpp_io_variable_t object.*/
int cmpp_io_variable_get_num_attributes(cmpp_io_variable_t const * const self)
{
    /*Make sure that the cmpp_io_variable_t object has been initialized.*/
    null_var_object(self);

    return self->num_attributes;
}
/*---------------------------------------------------------------------------*/
/*Return a pointer to the dimensions array for a cmpp_io_variable_t object.*/
cmpp_io_dimension_t **cmpp_io_variable_get_dimensions(cmpp_io_variable_t * const self)
{
    /*Make sure that the cmpp_io_variable_t object has been initialized.*/
    null_var_object(self);

    return self->dimensions;
}

/*---------------------------------------------------------------------------*/
/*Return a pointer to the attributes array for a cmpp_io_variable_t object.*/
cmpp_io_attribute_t **cmpp_io_variable_get_attributes(cmpp_io_variable_t * const self)
{
    /*Make sure that the cmpp_io_variable_t object has been initialized.*/
    null_var_object(self);

    return self->attributes;
}

/*---------------------------------------------------------------------------*/
/*Return the netCDF id for a cmpp_io_variable_t object.*/
int cmpp_io_variable_get_netcdf_id(cmpp_io_variable_t const * const self)
{
    /*Make sure that the cmpp_io_variable_t object has been initialized.*/
    null_var_object(self);

    return self->netcdf_id;
}

/*---------------------------------------------------------------------------*/
/*Return a pointer to the buffered data for a cmpp_io_variable_t object.*/
void *cmpp_io_variable_get_data(cmpp_io_variable_t * const self)
{
    /*Make sure that the cmpp_io_variable_t object has its data buffered.*/
    buffered_data_check(self);

    return self->data;
}

/*---------------------------------------------------------------------------*/
/*Return the array of buffered corner indices for data stored in
  a cmpp_io_variable_t object.*/
size_t *cmpp_io_variable_get_corner_indices(cmpp_io_variable_t * const self)
{
    /*Make sure that the cmpp_io_variable_t object has its data buffered.*/
    buffered_data_check(self);

    return self->corner_indices;
}

/*---------------------------------------------------------------------------*/
/*Return the array of buffered edge lengths for data stored in
  a cmpp_io_variable_t object.*/
size_t *cmpp_io_variable_get_edge_lengths(cmpp_io_variable_t * const self)
{
    /*Make sure that the cmpp_io_variable_t object has its data buffered.*/
    buffered_data_check(self);

    return self->edge_lengths;
}

/*---------------------------------------------------------------------------*/
/*Return the type that the buffered data is stored as in memory for a
  cmpp_io_variable_t object.*/
nc_type cmpp_io_variable_get_type_in_mem(cmpp_io_variable_t const * const self)
{
    /*Make sure that the cmpp_io_variable_t object has its data buffered.*/
    buffered_data_check(self);

    return self->type_in_mem;
}

/*---------------------------------------------------------------------------*/
/*Add an attribute to a cmpp_io_variable_t object's attributes array
  and return its index.*/
int cmpp_io_variable_add_attribute(cmpp_io_variable_t * const * const self,
                                   cmpp_io_attribute_t * const attribute)
{
    /*Local variables*/
    cmpp_io_variable_t *vptr = NULL;
    int attribute_index;

    /*Set the local pointer to the cmpp_io_variable_t object.*/
    vptr = *self;

    /*Make sure that the cmpp_io_variable_t object has been initialized.*/
    null_var_object(vptr);

    /*Add the attribute.*/
    attribute_index = add_pointer_to_array((void **)(vptr->attributes),
                                           vptr->max_num_attributes,
                                           &(vptr->num_attributes),
                                           (void *)attribute);
    vptr = NULL;

    return attribute_index;
}

/*---------------------------------------------------------------------------*/
/*Remove an attribute from a cmpp_io_variable_t object's attributes array
  and set its index to CMPP_IO_ATT_NOT_FOUND.*/
void cmpp_io_variable_remove_attribute(cmpp_io_variable_t * const * const self,
                                       int * const attribute_index)
{
    /*Local variables*/
    cmpp_io_variable_t *vptr = NULL;
    cleanup_t cfunc;

    /*Set the local pointer to the cmpp_io_variable_t object.*/
    vptr = *self;

    /*Make sure that the cmpp_io_variable_t object has been initialized.*/
    null_var_object(vptr);

    /*Destroy the attribute.*/
    cfunc.type = ATT_CLEANUP;
    cfunc.fp.att_cleanup = cmpp_io_attribute_destroy;
    remove_pointer_from_array((void **)(vptr->attributes),
                              vptr->max_num_attributes,
                              attribute_index,
                              &cfunc);
    vptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Return a pointer to an attribute or dimension stored at the inputted
  index in the variable's attributes or dimensions arrays.*/
void *cmpp_io_variable_get_ptr(cmpp_io_variable_t const * const self,
                               int const index,
                               int const ptr_type)
{
    /*Local variables*/
    void *ptr = NULL;

    /*Make sure that the cmpp_io_variable_t object has been initialized.*/
    null_var_object(self);

    /*Get the pointer from the appropriate array.*/
    switch (ptr_type)
    {
        case ATT_PTR:
            ptr = get_pointer_by_index((void **) (self->attributes),
                                       self->num_attributes,
                                       index);
            break;
        case DIM_PTR:
            ptr = get_pointer_by_index((void **) (self->dimensions),
                                       self->num_dimensions,
                                       index);
            break;
        default:
            fatal("inputted pointer type (%d) not recognized.  Must be"
                      " one of: ATT_PTR (%d) or DIM_PTR (%d).",
                  ptr_type,
                  ATT_PTR,
                  DIM_PTR);
    }

    return ptr;
}

/*---------------------------------------------------------------------------*/
/*Given the name of an attribute or dimension, return its index if in the
  variable's corresponding array.  If not found, return
  CMPP_IO_INDEX_NOT_FOUND.*/
int cmpp_io_variable_get_ptr_index(cmpp_io_variable_t const * const self,
                                   char * const val,
                                   int const ptr_type)
{
    /*Local variables*/
    compare_t cfunc;
    int index;
    void **array = NULL;
    int array_size;
    int empty_index;

    /*Make sure that the cmpp_io_variable_t object has been initialized.*/
    null_var_object(self);

    /*Search for the pointer.*/
    switch (ptr_type)
    {
         case ATT_PTR:
             cfunc.type = ATT_NAME;
             cfunc.fp.att_name = cmpp_io_attribute_get_name;
             array = (void **)(self->attributes);
             array_size = self->num_attributes;
             break;

         case DIM_PTR:
             cfunc.type = DIM_NAME;
             cfunc.fp.dim_name = cmpp_io_dimension_get_name;
             array = (void **)(self->dimensions);
             array_size = self->num_dimensions;
             break;

         default:
             fatal("inputted pointer type (%d) not recognized.  Must be"
                       " one of: ATT_PTR (%d) or DIM_PTR (%d).",
                   ptr_type,
                   ATT_PTR,
                   DIM_PTR);
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
/*Buffer variable data for a cmpp_io_variable_t object.  This data will be
  written out later by calling cmpp_io_varaible_write_buffered_data.*/
void cmpp_io_variable_buffer_data(cmpp_io_variable_t * const * const self,
                                  size_t const * const corner_indices,
                                  size_t const * const edge_lengths,
                                  void const * const data,
                                  nc_type const type_in_mem,
                                  bool const overwrite)
{
    /*Local variables*/
    cmpp_io_variable_t *vptr = NULL;
    cmpp_io_dimension_t *dptr = NULL;
    size_t type_bytes;
    size_t num_bytes;
    int i;
    size_t dim_length;
    size_t num_vals;

    /*set the local pointer to the cmpp_io_variable_t object.*/
    vptr = *self;

    /*Make sure that any previous data is allowed to be overwritten.*/
    if (cmpp_io_variable_get_is_data_buffered(vptr))
    {
        error_check(overwrite,
                    "data is already currently buffered for variable %s."
                        "  To overwrite the currently buffered data, please"
                        " pass in a value of true for the overwrite"
                        " parameter.",
                    vptr->name);

        /*Free space malloced for data that will be overwritten.*/
        if (vptr->corner_indices != NULL)
        {
            cmpp_io_safefree((void **) &(vptr->corner_indices));
        }
        if (vptr->edge_lengths != NULL)
        {
            cmpp_io_safefree((void **) &(vptr->edge_lengths));
        }
        cmpp_io_safefree((void **) &(vptr->data));
    }

    /*Make sure that the inputted data type in memory is valid.  Get the
      size of the type in bytes.*/
    switch (type_in_mem)
    {
        case NC_INT:
            type_bytes = sizeof(int);
            break;
        case NC_FLOAT:
            type_bytes = sizeof(float);
            break;
        case NC_DOUBLE:
            type_bytes = sizeof(double);
            break;
        default:
            fatal("the inputted type that the data is stored in memory as"
                      " %d is not currently supported.",
                  type_in_mem);
    }
    vptr->type_in_mem = type_in_mem;

    /*Store the corner indices and edge lengths for the data.  If the
      number of dimension for the variable is zero, then leave the
      object's corner indices and edge lengths unallocated.*/
    if (vptr->num_dimensions != 0)
    {
        /*Make sure that the inputted corner indices and edge lengths
          arrays are not null.*/
        error_check(corner_indices,
                    "the inputted corner indices array at address %p is"
                        " null.  This array should be the same size as"
                        " the number of dimensions (%d) associated with"
                        " variable %s.",
                    (void *) &corner_indices,
                    vptr->num_dimensions,
                    vptr->name);
        error_check(edge_lengths,
                    "the inputted edge lengths array at address %p is"
                        " null.  This array should be the same size as"
                        " the number of dimensions (%d) associated with"
                        " variable %s.",
                    (void *) &edge_lengths,
                    vptr->num_dimensions,
                    vptr->name);

        /*Make sure that the corner indices and edge lengths are compatible
          with the variable's dimensions.*/
        for (i=0;i<vptr->num_dimensions;i++)
        {
            dptr = cmpp_io_variable_get_ptr(vptr,
                                            i,
                                            DIM_PTR);
            error_check(corner_indices[i] >= 0,
                        "input corner index (%zu) for dimension %s"
                            " of variable %s must be >= 0.",
                        corner_indices[i],
                        cmpp_io_dimension_get_name(dptr),
                        vptr->name);

            if (cmpp_io_dimension_get_is_unlimited(dptr))
            {
                error_check(edge_lengths[i] == 1,
                            "inputted edge length (%zu) for unlimited"
                                " dimension %s of variable %s must = 1.",
                            edge_lengths[i],
                            cmpp_io_dimension_get_name(dptr),
                            vptr->name);
            }
            else
            {
                error_check(edge_lengths[i] >= 1,
                            "input edge length (%zu) for dimension %s"
                                " of variable %s must >= 1.",
                            edge_lengths[i],
                            cmpp_io_dimension_get_name(dptr),
                            vptr->name);
                dim_length = cmpp_io_dimension_get_length(dptr);
                error_check(corner_indices[i] + edge_lengths[i] <= dim_length,
                            "corner index (%zu) + edge length (%zu)"
                                " > dimension size (%zu) for dimension %s"
                                " of variable %s.",
                            corner_indices[i],
                            edge_lengths[i],
                            dim_length,
                            cmpp_io_dimension_get_name(dptr),
                            vptr->name);
            }
        }
        dptr = NULL;

        /*Allocate and copy in the corner indices and edge lengths.*/
        num_bytes = sizeof(size_t)*((size_t)(vptr->num_dimensions));
        cmpp_io_safemalloc((void **) &(vptr->corner_indices),
                           num_bytes);
        memcpy(vptr->corner_indices,
               corner_indices,
               num_bytes);
        cmpp_io_safemalloc((void **) &(vptr->edge_lengths),
                           num_bytes);
        memcpy(vptr->edge_lengths,
               edge_lengths,
               num_bytes);
    }

    /*Make sure that the inputted data pointer is not null.*/
    error_check(data,
                "the inputted data pointer at address %p is null.",
                (void *) &data);

    /*Allocate space and copy in the data.  If the number of dimensions
      for the variable is zero, then a single scalar value of the inputted
      type is buffered.*/
    if (vptr->num_dimensions == 0)
    {
        num_vals = 1;
    }
    else
    {
        num_vals = 0;
        for (i=0;i<vptr->num_dimensions;i++)
        {
            num_vals = num_vals + vptr->edge_lengths[i];
        }
    }
    num_bytes = num_vals*type_bytes;
    cmpp_io_safemalloc((void **) &(vptr->data),
                       num_bytes);
    memcpy(vptr->data,
           data,
           num_bytes);

    /*Set the is_data_buffered flag to true.*/
    vptr->is_data_buffered = true;
    vptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Write out variable data that was previously buffered to a netCDF file.
  Free the malloced data arrays.*/
void cmpp_io_variable_write_buffered_data(cmpp_io_variable_t * const * const self,
                                          int const ncid,
                                          bool const free_data)
{
    /*Local variables*/
    cmpp_io_variable_t *vptr = NULL;

    /*Set the local pointer to the cmpp_io_variable_t object.*/
    vptr = *self;

    /*Write out the data.*/
    write_netcdf_variable_data(ncid,
                               vptr->netcdf_id,
                               vptr->corner_indices,
                               vptr->edge_lengths,
                               vptr->data,
                               vptr->type_in_mem);

    if (free_data)
    {
        /*Free buffered data, corner indices, and edge lengths.*/
        cmpp_io_safefree((void **) &(vptr->data));
        if (vptr->corner_indices != NULL)
        {
            cmpp_io_safefree((void **) &(vptr->corner_indices));
        }
        if (vptr->edge_lengths != NULL)
        {
            cmpp_io_safefree((void **) &(vptr->edge_lengths));
        }

        /*Set the is_data_buffered flag to false.*/
        vptr->is_data_buffered = false;
    }
    vptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
