#include <stdlib.h>
#include <string.h>
#include "cmpp_io_attribute_t_api.h"
#include "cmpp_io_debug.h"
#include "cmpp_io_helper_functions.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_netcdf_utils.h"
#include "netcdf.h"

/*Helper macros.*/
#define null_att_object(ptr) \
    error_check(ptr, \
                "the cmpp_io_attribute_t pointer at address %p is null." \
                    "  Please first initialize the object by calling" \
                    " cmpp_io_attribute_create.", \
                (void *) &ptr);

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Structure definitions.*/

struct cmpp_io_attribute_type
{
    char name[CMPP_IO_MAX_ATT_NAME_LEN]; /**<Name of the attribute.*/
    nc_type type_in_file; /**<Type of the attribute
                              when stored in the netCDF
                              file.*/
    nc_type type_in_mem; /**<Type of the attribute
                             when stored in memory.*/
    size_t num_values; /**<Number of values stored in
                           the attribute.  For a string,
                           this is the number of
                           characters in the string.*/
    void *values; /**<Array of attribute
                      values.*/
};

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Create a cmpp_io_attribute_t object.*/
cmpp_io_attribute_t *cmpp_io_attribute_create(char const * const name,
                                              nc_type const type_in_file,
                                              nc_type const type_in_mem,
                                              size_t const num_values,
                                              void const * const values)
{
    /*Local variables*/
    cmpp_io_attribute_t *aptr = NULL;
    size_t values_num_bytes;

    /*Malloc the structure and set default values.*/
    cmpp_io_safemalloc((void **)(&aptr),
                       sizeof(cmpp_io_attribute_t));
    cmpp_io_safememset(aptr,
                       0,
                       sizeof(cmpp_io_attribute_t));

    /*Store the inputted attribute name in the inputted cmpp_io_attribute_t
      object.  If the length of the name exceeds CMPP_IO_MAX_ATT_NAME_LEN
      characters, then throw a fatal error.*/
    safe_string_copy(name,
                     (size_t)CMPP_IO_MAX_ATT_NAME_LEN,
                     aptr->name);

    /*Make sure that the inputted type_in_file and type_in_mem values
      are valid and compatible.  If so, then store them.*/
    check_netcdf_attribute_types(type_in_mem,
                                 type_in_file);
    aptr->type_in_file = type_in_file;
    aptr->type_in_mem = type_in_mem;

    /*Make sure that the inputted number of values for the attribute
      is greater than zero.  If so, then store the number of values.*/
    error_check((size_t)num_values > 0,
                "the inputted number of values for attribute %s is zero.",
                aptr->name);
    aptr->num_values = num_values;

    /*Make sure that the inputted values of the attribute are not
      null.*/
    error_check(values,
                "the inputted array of values for attribute %s is null.",
                aptr->name);

    /*Malloc space and store the inputted values.*/
    switch (aptr->type_in_mem)
    {
        case NC_CHAR:
            cmpp_io_safemalloc((void **) &(aptr->values),
                               sizeof(char)*((size_t)CMPP_IO_MAX_ATT_STR_LEN));
            safe_string_copy((char *)values,
                             (size_t)CMPP_IO_MAX_ATT_STR_LEN,
                             (char *)aptr->values);
            aptr->num_values = strlen(aptr->values) + 1;
            break;
        case NC_INT:
            values_num_bytes = sizeof(int)*num_values;
            cmpp_io_safemalloc((void **) &(aptr->values),
                               values_num_bytes);
            memcpy(aptr->values,
                   values,
                   values_num_bytes);
            break;
        case NC_FLOAT:
            values_num_bytes = sizeof(float)*num_values;
            cmpp_io_safemalloc((void **) &(aptr->values),
                               values_num_bytes);
            memcpy(aptr->values,
                   values,
                   values_num_bytes);
            break;
        case NC_DOUBLE:
            values_num_bytes = sizeof(double)*num_values;
            cmpp_io_safemalloc((void **) &(aptr->values),
                               values_num_bytes);
            memcpy(aptr->values,
                   values,
                   values_num_bytes);
            break;
    }

    return aptr;
}

/*---------------------------------------------------------------------------*/
/*Destroy a cmpp_io_attribute_t object.*/
void cmpp_io_attribute_destroy(cmpp_io_attribute_t **self)
{
    /*Local variables*/
    cmpp_io_attribute_t *aptr = NULL;

    /*Set the local pointer to the cmpp_io_attribute_t object.*/
    aptr = *self;

    /*Make sure that the cmpp_io_attribute_t object has been initialized.*/
    null_att_object(aptr);

    /*Free malloced arrays.*/
    if (aptr->values != NULL)
    {
        cmpp_io_safefree((void **)(&(aptr->values)));
    }

    /*Free the cmpp_io_attribute_t object.*/
    cmpp_io_safefree((void **)(&aptr));

    /*Point the inputted pointer to the deallocated object.*/
    *self = aptr;

    return;
}

/*---------------------------------------------------------------------------*/
/*Return a pointer to the name of a cmpp_io_attribute_t object.*/
char *cmpp_io_attribute_get_name(cmpp_io_attribute_t * const self)
{
    /*Make sure that the cmpp_io_attribute_t object has been initialized.*/
    null_att_object(self);

    return self->name;
}

/*---------------------------------------------------------------------------*/
/*Return the type that the attribute will store as inside the netCDF file
  for a cmpp_io_attribute_t object.*/
nc_type cmpp_io_attribute_get_type_in_file(cmpp_io_attribute_t const * const self)
{
    /*Make sure that the cmpp_io_attribute_t object has been initialized.*/
    null_att_object(self);

    return self->type_in_file;
}

/*---------------------------------------------------------------------------*/
/*Return the type that the attribute will store as in memory for a
  cmpp_io_attribute_t object.*/
nc_type cmpp_io_attribute_get_type_in_mem(cmpp_io_attribute_t const * const self)
{
    /*Make sure that the cmpp_io_attribute_t object has been initialized.*/
    null_att_object(self);

    return self->type_in_mem;
}

/*---------------------------------------------------------------------------*/
/*Return the number of values stored in the attribute for a
  cmpp_io_attribute_t object.*/
size_t cmpp_io_attribute_get_num_values(cmpp_io_attribute_t const * const self)
{
    /*Make sure that the cmpp_io_attribute_t object has been initialized.*/
    null_att_object(self);

    return self->num_values;
}

/*---------------------------------------------------------------------------*/
/*Return a pointer to an array of values stored in the attribute for a
  cmpp_io_attribute_t object.*/
void *cmpp_io_attribute_get_values(cmpp_io_attribute_t * const self)
{
    /*Make sure that the cmpp_io_attribute_t object has been initialized.*/
    null_att_object(self);

    return self->values;
}

/*---------------------------------------------------------------------------*/
