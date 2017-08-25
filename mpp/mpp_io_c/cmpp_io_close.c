#include <stdbool.h>
#include <stdlib.h>
#include "cmpp_io_close.h"
#include "cmpp_io_context_t_api.h"
#include "cmpp_io_debug.h"
#include "cmpp_io_file_props_t_api.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_netcdf_file_t_api.h"
#include "cmpp_io_regular_file_t_api.h"

/*---------------------------------------------------------------------------*/
/*Close a netcdf file and remove it from the context.*/
void cmpp_io_close_netcdf_file(cmpp_io_context_t * const * const context,
                               int * const file_index,
                               int const action,
                               bool const do_warn)
{
    /*Local variables*/
    cmpp_io_context_t *cptr = NULL;
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_file_props_t *file_props = NULL;

    /*Set the local pointer to the cmpp_io_context_t object.*/
    cptr = *context;

    /*Get a pointer to the file object.*/
    fptr = cmpp_io_context_get_file_ptr(cptr,
                                        *file_index,
                                        CMPP_NETCDF);

    if (fptr != NULL)
    {
        /*If necessary, put the file into data mode so that it can be
          flushed.*/
        if (cmpp_io_netcdf_file_get_is_in_define_mode(fptr))
        {
            cmpp_io_netcdf_file_enter_data_mode(&fptr,
                                                cmpp_io_context_get_header_buffer_val(cptr));
        }

        /*Flush the file.*/
        file_props = cmpp_io_netcdf_file_get_file_props(fptr);
        if (cmpp_io_file_props_get_action(file_props) != CMPP_RDONLY)
        {
            cmpp_io_netcdf_file_flush(fptr);
        }
        file_props = NULL;

        /*Close the file.*/
        cmpp_io_netcdf_file_close(&fptr);

        /*If necessary, delete the file.*/
        if (action == (int)CMPP_DELETE_FILE)
        {
            cmpp_io_netcdf_file_delete(fptr);
        }

        /*Remove the netcdf file from the context's netcdf files array and
          reset the file index to be CMPP_IO_INDEX_NOT_FOUND.*/
        cmpp_io_context_remove_file(&cptr,
                                    file_index,
                                    CMPP_NETCDF);
    }
    else
    {
        if (do_warn)
        {
            warn("the inputted file index (%d) corresponds to a netcdf file"
                     " that has already been closed.  No action will be taken"
                     " on the file.",
                 *file_index);
        }
    }
    cptr = NULL;
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Close a regular file and remove it from the context.*/
void cmpp_io_close_regular_file(cmpp_io_context_t * const * const context,
                                int * const file_index,
                                int const action,
                                bool const do_warn)
{
    /*Local variables*/
    cmpp_io_context_t *cptr = NULL;
    cmpp_io_regular_file_t *fptr = NULL;
    cmpp_io_file_props_t *file_props = NULL;

    /*Set the local pointer to the cmpp_io_context_t object.*/
    cptr = *context;

    /*Get a pointer to the file object.*/
    fptr = cmpp_io_context_get_file_ptr(cptr,
                                        *file_index,
                                        CMPP_ASCII);

    if (fptr != NULL)
    {
        /*Flush the file.*/
        file_props = cmpp_io_regular_file_get_file_props(fptr);
        if (cmpp_io_file_props_get_action(file_props) != CMPP_RDONLY)
        {
            cmpp_io_regular_file_flush(fptr);
        }
        file_props = NULL;

        /*Close the file.*/
        cmpp_io_regular_file_close(&fptr);

        /*If necessary, delete the file.*/
        if (action == (int)CMPP_DELETE_FILE)
        {
            cmpp_io_regular_file_delete(fptr);
        }

        /*Remove the regular file from the context's regular files array and
          reset the file index to be CMPP_IO_FILE_NOT_FOUND.*/
        cmpp_io_context_remove_file(&cptr,
                                    file_index,
                                    CMPP_ASCII);
    }
    else
    {
        if (do_warn)
        {
            warn("the inputted file index (%d) corresponds to a regular file"
                     " that has already been closed.  No action will be taken"
                     " on the file.",
                 *file_index);
        }
    }
    cptr = NULL;
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
