#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include "cmpp_io_context_t_api.h"
#include "cmpp_io_dimension_t_api.h"
#include "cmpp_io_dimension_utils.h"
#include "cmpp_io_debug.h"
#include "cmpp_io_file_props_t_api.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_netcdf_file_t_api.h"
#include "cmpp_io_netcdf_utils.h"
#include "cmpp_io_utils.h"

/*---------------------------------------------------------------------------*/
/*Read in a dimension from a netCDF file and store its values in a dimension
  object.*/
int cmpp_io_read_netcdf_dimension(cmpp_io_context_t * const context,
                                  int const file_index,
                                  int const netcdf_dimension_id)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_file_props_t *file_props = NULL;
    cmpp_io_dimension_t *dptr = NULL;
    int ncid;
    char name_buf[CMPP_IO_MAX_DIM_NAME_LEN];
    size_t dim_length;
    bool is_unlimited;
    int current_level;
    int dimension_index;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get the netcdf id of the file.*/
    ncid = cmpp_io_netcdf_file_get_ncid(fptr);

    /*If necessary, put the file in data mode.*/
    if (cmpp_io_netcdf_file_get_is_in_define_mode(fptr))
    {
        cmpp_io_netcdf_file_enter_data_mode(&fptr,
                                            cmpp_io_context_get_header_buffer_val(context));
    }

    /*Read in the dimension values.*/
    file_props = cmpp_io_netcdf_file_get_file_props(fptr);
    read_netcdf_dimension(ncid,
                          netcdf_dimension_id,
                          name_buf,
                          (size_t)CMPP_IO_MAX_DIM_NAME_LEN,
                          &dim_length,
                          &is_unlimited,
                          cmpp_io_file_props_get_name(file_props));

    /*Create the dimension object.*/
    if (is_unlimited)
    {
        current_level = (int)dim_length;
    }
    else
    {
        current_level = 0;
    }
    dptr = cmpp_io_dimension_create(name_buf,
                                    dim_length,
                                    netcdf_dimension_id,
                                    is_unlimited,
                                    current_level);

    /*Place the dimension object into the appropriate array.*/
    dimension_index = cmpp_io_netcdf_file_add_ptr(&fptr,
                                                  dptr,
                                                  DIM_PTR);
    dptr = NULL;
    fptr = NULL;
    file_props = NULL;

    return dimension_index;
}

/*---------------------------------------------------------------------------*/
/*Write out a dimension from a netCDF file and store its values in a
  dimension object if necessary.*/
int cmpp_io_write_netcdf_dimension(cmpp_io_context_t * const context,
                                   int const file_index,
                                   char * const name,
                                   size_t const length,
                                   bool const buffer_values)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_file_props_t *file_props = NULL;
    cmpp_io_dimension_t *dptr = NULL;
    int ncid;
    int netcdf_dimension_id;
    bool is_unlimited;
    int dimension_index;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get the netcdf id of the file.*/
    ncid = cmpp_io_netcdf_file_get_ncid(fptr);

    /*If necessary, put the file in define mode.*/
    if (!cmpp_io_netcdf_file_get_is_in_define_mode(fptr))
    {
        cmpp_io_netcdf_file_enter_define_mode(&fptr);
    }

    /*Write out the dimension values to the file.*/
    file_props = cmpp_io_netcdf_file_get_file_props(fptr);
    netcdf_dimension_id = write_netcdf_dimension(ncid,
                                                 name,
                                                 length,
                                                 cmpp_io_file_props_get_name(file_props));

    if (buffer_values)
    {
        /*Determine if the dimension is unlimited.*/
        if (length == (size_t)NC_UNLIMITED)
        {
            is_unlimited = true;
        }
        else
        {
            is_unlimited = false;
        }

        /*Create the dimension object.*/
        dptr = cmpp_io_dimension_create(name,
                                        length,
                                        netcdf_dimension_id,
                                        is_unlimited,
                                        0);

        /*Place the dimension object into the appropriate array.*/
        dimension_index = cmpp_io_netcdf_file_add_ptr(&fptr,
                                                      dptr,
                                                      DIM_PTR);
        dptr = NULL;
    }
    else
    {
        dimension_index = (int)CMPP_IO_INDEX_NOT_FOUND;
    }
    fptr = NULL;
    file_props = NULL;

    return dimension_index;
}

/*---------------------------------------------------------------------------*/
/*Advance the current level of an unlimited dimension by one.*/
void cmpp_io_advance_netcdf_dimension_level(cmpp_io_context_t * const context,
                                            int const file_index,
                                            int const dimension_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_dimension_t *dptr = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get a pointer to the dimension object.*/
    dptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       dimension_index,
                                       DIM_PTR);

    /*Advance the dimension level.*/
    cmpp_io_dimension_advance_level(&dptr);
    dptr = NULL;
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Free space that was allocated for a dimension.  Set the inputted dimension
  index to CMPP_IO_DIM_NOT_FOUND.*/
void cmpp_io_free_netcdf_dimension(cmpp_io_context_t * const context,
                                   int const file_index,
                                   int * const dimension_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Remove the dimension from memory.*/
    cmpp_io_netcdf_file_remove_ptr(&fptr,
                                   dimension_index,
                                   DIM_PTR);
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Given the name of a dimension, return the index of the dimension in the
  file's dimensions array.*/
int cmpp_io_get_netcdf_dimension_index(cmpp_io_context_t * const context,
                                       int const file_index,
                                       char * const dim_name)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    int dimension_index;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    dimension_index = cmpp_io_netcdf_file_get_ptr_index(fptr,
                                                        dim_name,
                                                        DIM_PTR);
    fptr = NULL;

    return dimension_index;
}

/*---------------------------------------------------------------------------*/
/*Given a file index, the index of variable in the file's variables array,
  and the index of a dimension in the variable's dimensions array, return
  the corresponding index of the dimension in file's dimensions array.*/
int cmpp_io_convert_netcdf_variable_dimension_index(cmpp_io_context_t * const context,
                                                    int const file_index,
                                                    int const variable_index,
                                                    int const dimension_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    cmpp_io_dimension_t *dptr = NULL;
    cmpp_io_dimension_t *fdptr = NULL;
    int num_dims;
    int i;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get a pointer to the variable in the file's variables array.*/
    vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       variable_index,
                                       VAR_PTR);

    /*Get a pointer to the dimension in the variable's dimensions array.*/
    dptr = cmpp_io_variable_get_ptr(vptr,
                                    dimension_index,
                                    DIM_PTR);

    /*Loop through the dimensions in the file's dimensions array and look
      for a matching pointer.*/
    num_dims = cmpp_io_netcdf_file_get_num_dimensions(fptr);
    for (i=0;i<num_dims;i++)
    {
        fdptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                            i,
                                            DIM_PTR);
        if (dptr == fdptr)
        {
            fptr = NULL;
            vptr = NULL;
            dptr = NULL;
            fdptr = NULL;
            return i;
        }
    }

    /*The code should never reach this part.*/
    cmpp_io_file_props_t *file_props = NULL;
    file_props = cmpp_io_netcdf_file_get_file_props(fptr);
    fatal("dimension %s for variable %s does not match any dimension in"
              " file %s.",
          cmpp_io_dimension_get_name(dptr),
          cmpp_io_variable_get_name(vptr),
          cmpp_io_file_props_get_name(file_props));

    fptr = NULL;
    vptr = NULL;
    dptr = NULL;
    fdptr = NULL;
    file_props = NULL;

    return CMPP_IO_INDEX_NOT_FOUND;
}

/*---------------------------------------------------------------------------*/
/*Get the name of a dimension.*/
char *cmpp_io_get_netcdf_dimension_name(cmpp_io_context_t * const context,
                                        int const file_index,
                                        int const dimension_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_dimension_t *dptr = NULL;
    char *dim_name = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get the dimension name.*/
    dptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       dimension_index,
                                       DIM_PTR);
    dim_name = cmpp_io_dimension_get_name(dptr);
    dptr = NULL;
    fptr = NULL;

    return dim_name;
}

/*---------------------------------------------------------------------------*/
/*Get the length of a dimension.*/
size_t cmpp_io_get_netcdf_dimension_length(cmpp_io_context_t * const context,
                                           int const file_index,
                                           int const dimension_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_dimension_t *dptr = NULL;
    size_t length;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get the dimension length.*/
    dptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       dimension_index,
                                       DIM_PTR);
    length = cmpp_io_dimension_get_length(dptr);
    dptr = NULL;
    fptr = NULL;

    return length;
}

/*---------------------------------------------------------------------------*/
/*Get a flag telling if the dimension is unlimited.*/
bool cmpp_io_get_netcdf_dimension_is_unlimited(cmpp_io_context_t * const context,
                                               int const file_index,
                                               int const dimension_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_dimension_t *dptr = NULL;
    bool is_unlimited;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Determine if the dimension is unlimited.*/
    dptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       dimension_index,
                                       DIM_PTR);
    is_unlimited = cmpp_io_dimension_get_is_unlimited(dptr);
    dptr = NULL;
    fptr = NULL;

    return is_unlimited;
}

/*---------------------------------------------------------------------------*/
/*Get the current level for a dimension.*/
int cmpp_io_get_netcdf_dimension_current_level(cmpp_io_context_t * const context,
                                               int const file_index,
                                               int const dimension_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_dimension_t *dptr = NULL;
    int current_level;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get the dimension's current level.*/
    dptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       dimension_index,
                                       DIM_PTR);
    current_level = cmpp_io_dimension_get_current_level(dptr);
    dptr = NULL;
    fptr = NULL;

    return current_level;
}

/*---------------------------------------------------------------------------*/
/*Get the netCDF id for a dimension.*/
int cmpp_io_get_netcdf_dimension_id(cmpp_io_context_t * const context,
                                    int const file_index,
                                    int const dimension_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_dimension_t *dptr = NULL;
    int netcdf_id;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get the dimension's current level.*/
    dptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       dimension_index,
                                       DIM_PTR);
    netcdf_id = cmpp_io_dimension_get_netcdf_id(dptr);
    dptr = NULL;
    fptr = NULL;

    return netcdf_id;
}

/*---------------------------------------------------------------------------*/
