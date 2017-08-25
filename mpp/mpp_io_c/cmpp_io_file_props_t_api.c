#include <stdbool.h>
#include <stdlib.h>
#include "cmpp_io_debug.h"
#include "cmpp_io_file_props_t_api.h"
#include "cmpp_io_helper_functions.h"
#include "cmpp_io_macros.h"

/*Helper macros.*/
#define null_file_props_object(ptr) \
    error_check(ptr, \
                "the cmpp_io_file_props_t pointer at address %p is null." \
                    "  Please first initialize the object by calling" \
                    " cmpp_io_file_props_create.", \
                (void *) &ptr);

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Structure definitions.*/

struct cmpp_io_file_props_type
{
    bool is_open; /**<Flag telling if the file is open.*/
    char name[CMPP_IO_MAX_FILE_NAME_LENGTH]; /**<Name of the file.*/
    int action; /**<Action to take on the file.*/
};

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Create a cmpp_io_file_props_t object.*/
cmpp_io_file_props_t *cmpp_io_file_props_create(char * const name,
                                                int const action)
{
    /*Local variables*/
    cmpp_io_file_props_t *fptr = NULL;

    /*Malloc the structure and set default values.*/
    cmpp_io_safemalloc((void **)(&fptr),
                       sizeof(cmpp_io_file_props_t));
    cmpp_io_safememset(fptr,
                       0,
                       sizeof(cmpp_io_file_props_t));

    /*Store the name of the file.  Throw a fatal error if the name exceeds
      CMPP_IO_MAX_FILE_NAME_LENGTH characters.*/
    safe_string_copy(name,
                     (size_t)CMPP_IO_MAX_FILE_NAME_LENGTH,
                     fptr->name);

    /*Store the file action.  Throw a fatal error if the action is not
      recognized.*/
    switch (action)
    {
        case CMPP_RDONLY:
        case CMPP_WRONLY:
        case CMPP_OVERWR:
        case CMPP_APPEND:
            fptr->action = action;
            break;
        default:
            fatal("the inputted action (%d) for file %s must be either"
                      " CMPP_RDONLY (%d), CMPP_WRONLY (%d), CMPP_OVERWR"
                      " (%d), or CMPP_APPEND (%d).",
                  action,
                  fptr->name,
                  (int)CMPP_RDONLY,
                  (int)CMPP_WRONLY,
                  (int)CMPP_OVERWR,
                  (int)CMPP_APPEND);
    }

    /*Mark the file as closed.*/
    fptr->is_open = false;

    return fptr;
}

/*---------------------------------------------------------------------------*/
/*Destroy a cmpp_io_file_props_t object.*/
void cmpp_io_file_props_destroy(cmpp_io_file_props_t **self)
{
    /*Local variables*/
    cmpp_io_file_props_t *fptr = NULL;

    /*Set the local pointer to the cmpp_io_file_props_t object.*/
    fptr = *self;

    /*Make sure that the cmpp_io_file_props_t object has been initialized.*/
    null_file_props_object(fptr);

    /*Free the cmpp_io_file_props_t object.*/
    cmpp_io_safefree((void **)(&fptr));

    /*Point the inputted pointer to the deallocated object.*/
    *self = fptr;

    return;
}

/*---------------------------------------------------------------------------*/
/*Return a flag telling if the file is open.*/
bool cmpp_io_file_props_get_is_open(cmpp_io_file_props_t const * const self)
{
    /*Make sure that the cmpp_io_file_props_t object has been initialized.*/
    null_file_props_object(self);

    return self->is_open;
}

/*---------------------------------------------------------------------------*/
/*Return the file name.*/
char *cmpp_io_file_props_get_name(cmpp_io_file_props_t * const self)
{
    /*Make sure that the cmpp_io_file_props_t object has been initialized.*/
    null_file_props_object(self);

    return self->name;
}

/*---------------------------------------------------------------------------*/
/*Return the file action.*/
int cmpp_io_file_props_get_action(cmpp_io_file_props_t const * const self)
{
    /*Make sure that the cmpp_io_file_props_t object has been initialized.*/
    null_file_props_object(self);

    return self->action;
}

/*---------------------------------------------------------------------------*/
/*Set the flag telling if the file is open.*/
void cmpp_io_file_props_set_is_open(cmpp_io_file_props_t * const * const self,
                                    bool const is_open)
{
    /*Local variables*/
    cmpp_io_file_props_t *fptr = NULL;

    /*Set the local pointer to the cmpp_io_file_props_t object.*/
    fptr = *self;

    /*Make sure that the cmpp_io_file_props_t object has been initialized.*/
    null_file_props_object(fptr);

    /*Set the is_open flag.*/
    fptr->is_open = is_open;
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
