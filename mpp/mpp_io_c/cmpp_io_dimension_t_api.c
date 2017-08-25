#include <stdbool.h>
#include <stdlib.h>
#include "cmpp_io_debug.h"
#include "cmpp_io_dimension_t_api.h"
#include "cmpp_io_helper_functions.h"
#include "cmpp_io_macros.h"

/*Helper macros.*/
#define null_dim_object(ptr) \
    error_check(ptr, \
                "the cmpp_io_dimension_t pointer at address %p is null." \
                    "  Please first initialized the object by calling" \
                    " cmpp_io_dimension_create.", \
                (void *) &ptr);

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Structure definitions.*/

struct cmpp_io_dimension_type
{
    char name[CMPP_IO_MAX_DIM_NAME_LEN]; /**<Name of the dimension.*/
    size_t length; /**<Length of the dimension.*/
    bool is_unlimited; /**<Flag telling if the dimension
                           is an unlimited dimension.*/
    int current_level; /**<Current level, if the dimension
                           is unlimited.*/
    int netcdf_id; /**<netCDF id for the dimension.*/
};

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Create a cmpp_io_dimension_t object.*/
cmpp_io_dimension_t *cmpp_io_dimension_create(char const * const name,
                                              size_t const length,
                                              int const netcdf_id,
                                              bool const is_unlimited,
                                              int const current_level)
{
    /*Local variables*/
    cmpp_io_dimension_t *dptr = NULL;

    /*Malloc the structure and set default values.*/
    cmpp_io_safemalloc((void **) (&dptr),
                       sizeof(cmpp_io_dimension_t));
    cmpp_io_safememset(dptr,
                       0,
                       sizeof(cmpp_io_dimension_t));

    /*Store the inputted dimension name in the inputted cmpp_io_dimension_t
      object.  If the length of the name exceeds CMPP_IO_MAX_DIM_NAME_LEN
      characters, then throw a fatal error.*/
    safe_string_copy(name,
                     (size_t)CMPP_IO_MAX_DIM_NAME_LEN,
                     dptr->name);

    /*Store the dimension length.*/
    if (length != (size_t)NC_UNLIMITED)
    {
        error_check(length > 0,
                    "the inputted length for dimension %s is zero.",
                    dptr->name);
    }
    dptr->length = length;

    /*Store the dimension's netcdf id, flag telling if the dimension
      is unlimited, and current dimension level.*/
    dptr->netcdf_id = netcdf_id;
    dptr->is_unlimited = is_unlimited;
    dptr->current_level = current_level;

    return dptr;
}

/*---------------------------------------------------------------------------*/
/*Destroy a cmpp_io_dimension_t object.*/
void cmpp_io_dimension_destroy(cmpp_io_dimension_t **self)
{
    /*Local variables*/
    cmpp_io_dimension_t *dptr = NULL;

    /*Set the local pointer to the cmpp_io_dimension_t object.*/
    dptr = *self;

    /*Make sure that the cmpp_io_dimension_t object has been initialized.*/
    null_dim_object(dptr);

    /*Free the cmpp_io_dimension_t object.*/
    cmpp_io_safefree((void **) (&dptr));

    /*Point the inputted pointer at the deallocated object.*/
    *self = dptr;

    return;
}

/*---------------------------------------------------------------------------*/
/*Return the name of the dimension for the cmpp_io_dimension_t object.*/
char *cmpp_io_dimension_get_name(cmpp_io_dimension_t * const self)
{
    /*Make sure that the cmpp_io_dimension_t object has been initialized.*/
    null_dim_object(self);

    return self->name;
}

/*---------------------------------------------------------------------------*/
/*Return the length of the dimension for the cmpp_io_dimension_t object.*/
size_t cmpp_io_dimension_get_length(cmpp_io_dimension_t const * const self)
{
    /*Make sure that the cmpp_io_dimension_t object has been initialized.*/
    null_dim_object(self);

    return self->length;
}

/*---------------------------------------------------------------------------*/
/*Return the value of the flag telling if the dimension is the unlimited
  dimension the cmpp_io_dimension_t object.*/
bool cmpp_io_dimension_get_is_unlimited(cmpp_io_dimension_t const * const self)
{
    /*Make sure that the cmpp_io_dimension_t object has been initialized.*/
    null_dim_object(self);

    return self->is_unlimited;
}

/*---------------------------------------------------------------------------*/
/*Return the current level if the dimension is unlimited.*/
int cmpp_io_dimension_get_current_level(cmpp_io_dimension_t const * const self)
{
    /*Make sure that the cmpp_io_dimension_t object is an unlimited
      dimension.*/
    error_check(cmpp_io_dimension_get_is_unlimited(self),
                "dimension %s is not unlimited.",
                self->name);

    return self->current_level;
}

/*---------------------------------------------------------------------------*/
/*Return the netCDF id of the dimension for the cmpp_io_dimension_t object.*/
int cmpp_io_dimension_get_netcdf_id(cmpp_io_dimension_t const * const self)
{
    /*Make sure that the cmpp_io_dimension_t object has been initialized.*/
    null_dim_object(self);

    return self->netcdf_id;
}

/*---------------------------------------------------------------------------*/
/*Advance the current level of the an unlimited dimension by one.*/
void cmpp_io_dimension_advance_level(cmpp_io_dimension_t * const * const self)
{
    /*Local variables*/
    cmpp_io_dimension_t *dptr = NULL;

    /*Set the local pointer to the cmpp_io_dimension_t object.*/
    dptr = *self;

    /*Make sure that the cmpp_io_dimension_t object is an unlimited
      dimension.*/
    error_check(cmpp_io_dimension_get_is_unlimited(dptr),
                "dimension %s is not unlimited.",
                dptr->name);

    /*Iterate the dimension's current level.*/
    (dptr->current_level)++;

    /*Nullify local pointers.*/
    dptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
