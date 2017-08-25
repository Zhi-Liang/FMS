#include <stdlib.h>
#include "cmpp_io_debug.h"
#include "cmpp_io_file_props_t_api.h"
#include "cmpp_io_hdf5_file_t_api.h"
#include "cmpp_io_helper_functions.h"

/*Helper macros.*/
#define null_hdf5_file_object(ptr) \
    error_check(ptr, \
                "the cmpp_io_hdf5_file_t pointer at address %p is null." \
                    "  Please first initialize the object by calling" \
                    " cmpp_io_hdf5_file_create.", \
                (void *) &ptr);

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Structure definitions.*/

struct cmpp_io_hdf5_file_type
{
    cmpp_io_file_props_t *file_props; /**<File properties (i.e., name,
                                          is_writer, is_reader, ...).*/
};

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Create a cmpp_io_hdf5_file_t object.*/
cmpp_io_hdf5_file_t *cmpp_io_hdf5_file_create(cmpp_io_file_props_t * const file_props)
{
    /*Local variables*/
    cmpp_io_hdf5_file_t *fptr = NULL;

    /*Malloc the structure and set default values.*/
    cmpp_io_safemalloc((void **)(&fptr),
                       sizeof(cmpp_io_hdf5_file_t));
    cmpp_io_safememset(fptr,
                       0,
                       sizeof(cmpp_io_hdf5_file_t));

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

    return fptr;
}

/*---------------------------------------------------------------------------*/
/*Destroy a cmpp_io_hdf5_file_t object.*/
void cmpp_io_hdf5_file_destroy(cmpp_io_hdf5_file_t **self)
{
    /*Local variables*/
    cmpp_io_hdf5_file_t *fptr = NULL;

    /*Set the local pointer to the cmpp_io_hdf5_file_t object.*/
    fptr = *self;

    /*Make sure that the cmpp_io_hdf5_file_t object has been initialized.*/
    null_hdf5_file_object(fptr);

    /*Free malloced arrays.*/
    if (fptr->file_props != NULL)
    {
        cmpp_io_safefree((void **)(&(fptr->file_props)));
    }

    /*Free the cmpp_io_hdf5_file_t object.*/
    cmpp_io_safefree((void **)(&fptr));

    /*Point the inputted pointer to the deallocated object.*/
    *self = fptr;

    return;
}

/*---------------------------------------------------------------------------*/
/*Return a pointer to the file properties object for a cmpp_io_hdf5_file_t
  object.*/
cmpp_io_file_props_t *cmpp_io_hdf5_file_get_file_props(cmpp_io_hdf5_file_t * const self)
{
    /*Make sure that the cmpp_io_hdf5_file_t object has been initialized.*/
    null_hdf5_file_object(self);

    return self->file_props;
}

/*---------------------------------------------------------------------------*/
/*Return the file name.*/
char *cmpp_io_hdf5_file_get_name(cmpp_io_hdf5_file_t * const self)
{
    /*Make sure that the cmpp_io_hdf5_file_t object has been initialized.*/
    null_hdf5_file_object(self);

    return cmpp_io_file_props_get_name(self->file_props);
}

/*---------------------------------------------------------------------------*/
