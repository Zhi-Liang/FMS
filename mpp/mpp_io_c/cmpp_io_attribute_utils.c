#include <stdbool.h>
#include <stdlib.h>
#include "cmpp_io_attribute_t_api.h"
#include "cmpp_io_attribute_utils.h"
#include "cmpp_io_context_t_api.h"
#include "cmpp_io_debug.h"
#include "cmpp_io_helper_functions.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_netcdf_file_t_api.h"
#include "cmpp_io_netcdf_utils.h"
#include "cmpp_io_utils.h"
#include "cmpp_io_variable_t_api.h"
#include "netcdf.h"

static cmpp_io_attribute_t *get_att_ptr_from_index(cmpp_io_netcdf_file_t * const fptr,
                                                   int const variable_index,
                                                   int const attribute_index);

/*---------------------------------------------------------------------------*/
/*Read in an attribute from a netCDF file and store its values in an
  attribute object.*/
int cmpp_io_read_netcdf_attribute(cmpp_io_context_t * const context,
                                  int const file_index,
                                  int const variable_index,
                                  int const netcdf_attribute_id)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_file_props_t *file_props = NULL;
    cmpp_io_variable_t* vptr = NULL;
    cmpp_io_attribute_t *aptr = NULL;
    int netcdf_variable_id;
    int ncid;
    char name_buf[CMPP_IO_MAX_ATT_NAME_LEN];
    nc_type type_in_file;
    nc_type type_in_mem;
    size_t num_values;
    void *values_buf = NULL;
    int attribute_index;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get the netcdf id of the variable.*/
    if (variable_index == (int)NC_GLOBAL)
    {
        netcdf_variable_id = variable_index;
    }
    else
    {
        vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                           variable_index,
                                           VAR_PTR);
        netcdf_variable_id = cmpp_io_variable_get_netcdf_id(vptr);
    }

    /*Get the netCDF id of the file.*/
    ncid = cmpp_io_netcdf_file_get_ncid(fptr);

    /*If necessary, put the file in data mode.*/
    if (cmpp_io_netcdf_file_get_is_in_define_mode(fptr))
    {
        cmpp_io_netcdf_file_enter_data_mode(&fptr,
                                            cmpp_io_context_get_header_buffer_val(context));
    }

    /*Read in the attribute values.*/
    file_props = cmpp_io_netcdf_file_get_file_props(fptr);
    read_netcdf_attribute(ncid,
                          netcdf_variable_id,
                          netcdf_attribute_id,
                          name_buf,
                          (size_t)CMPP_IO_MAX_ATT_NAME_LEN,
                          &type_in_file,
                          &type_in_mem,
                          &num_values,
                          &values_buf,
                          cmpp_io_file_props_get_name(file_props));

    /*Create an attribute object.*/
    aptr = cmpp_io_attribute_create(name_buf,
                                    type_in_file,
                                    type_in_mem,
                                    num_values,
                                    values_buf);
    cmpp_io_safefree((void **) &values_buf);

    /*Place the attribute object into the appropriate array.*/
    if (variable_index == (int)NC_GLOBAL)
    {
        attribute_index = cmpp_io_netcdf_file_add_ptr(&fptr,
                                                      aptr,
                                                      ATT_PTR);
    }
    else
    {
        attribute_index = cmpp_io_variable_add_attribute(&vptr,
                                                         aptr);
        vptr = NULL;
    }
    aptr = NULL;
    fptr = NULL;
    file_props = NULL;

    return attribute_index;
}

/*---------------------------------------------------------------------------*/
/*Write out an attribute to a netcdf file and store its values in an
  attribute object if necessary.*/
int cmpp_io_write_netcdf_attribute(cmpp_io_context_t * const context,
                                   int const file_index,
                                   int const variable_index,
                                   nc_type const type_in_file,
                                   size_t const num_values,
                                   char * const name,
                                   void * const values,
                                   nc_type const type_in_mem,
                                   bool const buffer_values)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_file_props_t *file_props = NULL;
    cmpp_io_variable_t *vptr = NULL;
    cmpp_io_attribute_t *aptr = NULL;
    int netcdf_variable_id;
    int ncid;
    int attribute_index;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get the netcdf variable id for the attribute.*/
    if (variable_index == (int)NC_GLOBAL)
    {
        netcdf_variable_id = variable_index;
    }
    else
    {
        vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                           variable_index,
                                           VAR_PTR);
        netcdf_variable_id = cmpp_io_variable_get_netcdf_id(vptr);
    }

    /*Get the netCDF id of the file.*/
    ncid = cmpp_io_netcdf_file_get_ncid(fptr);

    /*If necessary, put the file in define mode.*/
    if (!cmpp_io_netcdf_file_get_is_in_define_mode(fptr))
    {
        cmpp_io_netcdf_file_enter_define_mode(&fptr);
    }

    /*Write the attribute to the file.*/
    file_props = cmpp_io_netcdf_file_get_file_props(fptr);
    write_netcdf_attribute(ncid,
                           netcdf_variable_id,
                           type_in_mem,
                           name,
                           type_in_file,
                           num_values,
                           values,
                           cmpp_io_file_props_get_name(file_props));

    if (buffer_values)
    {
        /*Create an attribute object.*/
        aptr = cmpp_io_attribute_create(name,
                                        type_in_file,
                                        type_in_mem,
                                        num_values,
                                        values);

        /*Place the attribute object into the appropriate array.*/
        if (variable_index == (int)NC_GLOBAL)
        {
            attribute_index = cmpp_io_netcdf_file_add_ptr(&fptr,
                                                          aptr,
                                                          ATT_PTR);
        }
        else
        {
            attribute_index = cmpp_io_variable_add_attribute(&vptr,
                                                             aptr);
            vptr = NULL;
        }
        aptr = NULL;
    }
    else
    {
        attribute_index = (int)CMPP_IO_INDEX_NOT_FOUND;
    }
    fptr = NULL;
    file_props = NULL;

    return attribute_index;
}

/*---------------------------------------------------------------------------*/
/*Free space that was allocated for an attribute.  Set the inputted attribute
  index to CMPP_IO_ATT_NOT_FOUND.*/
void cmpp_io_free_netcdf_attribute(cmpp_io_context_t * const context,
                                   int const file_index,
                                   int const variable_index,
                                   int * const attribute_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    if (variable_index == (int)NC_GLOBAL)
    {
        /*Remove the global attribute from memory.*/
        cmpp_io_netcdf_file_remove_ptr(&fptr,
                                       attribute_index,
                                       ATT_PTR);
    }
    else
    {
        /*Remove the variable attribute from memory.*/
        vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                           variable_index,
                                           VAR_PTR);
        cmpp_io_variable_remove_attribute(&vptr,
                                          attribute_index);
        vptr = NULL;
    }
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Given the name of an attribute, return the index of the attribute in the
  file's (if global) or corresponding variable's attributes array.*/
int cmpp_io_get_netcdf_attribute_index(cmpp_io_context_t * const context,
                                       int const file_index,
                                       int const variable_index,
                                       char * const att_name)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    int att_index;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    if (variable_index == (int)NC_GLOBAL)
    {
        att_index = cmpp_io_netcdf_file_get_ptr_index(fptr,
                                                      att_name,
                                                      ATT_PTR);
    }
    else
    {
        vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                           variable_index,
                                           VAR_PTR);
        att_index = cmpp_io_variable_get_ptr_index(vptr,
                                                   att_name,
                                                   ATT_PTR);
        vptr = NULL;
    }
    fptr = NULL;

    return att_index;
}

/*---------------------------------------------------------------------------*/
/*Get the name of an attribute.*/
char *cmpp_io_get_netcdf_attribute_name(cmpp_io_context_t * const context,
                                        int const file_index,
                                        int const variable_index,
                                        int const attribute_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_attribute_t *aptr = NULL;
    char *att_name = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get the attribute name.*/
    aptr = get_att_ptr_from_index(fptr,
                                  variable_index,
                                  attribute_index);
    att_name = cmpp_io_attribute_get_name(aptr);
    aptr = NULL;
    fptr = NULL;

    return att_name;
}

/*---------------------------------------------------------------------------*/
/*Get the type in the file of an attribute.*/
nc_type cmpp_io_get_netcdf_attribute_type_in_file(cmpp_io_context_t * const context,
                                                  int const file_index,
                                                  int const variable_index,
                                                  int const attribute_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_attribute_t *aptr = NULL;
    int type_in_file;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get the type.*/
    aptr = get_att_ptr_from_index(fptr,
                                  variable_index,
                                  attribute_index);
    type_in_file = cmpp_io_attribute_get_type_in_file(aptr);
    aptr = NULL;
    fptr = NULL;

    return type_in_file;
}

/*---------------------------------------------------------------------------*/
/*Get the type in memory of an attribute.*/
nc_type cmpp_io_get_netcdf_attribute_type_in_mem(cmpp_io_context_t * const context,
                                                 int const file_index,
                                                 int const variable_index,
                                                 int const attribute_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_attribute_t *aptr = NULL;
    int type_in_mem;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get the type.*/
    aptr = get_att_ptr_from_index(fptr,
                                  variable_index,
                                  attribute_index);
    type_in_mem = cmpp_io_attribute_get_type_in_mem(aptr);
    aptr = NULL;
    fptr = NULL;

    return type_in_mem;
}

/*---------------------------------------------------------------------------*/
/*Get the number of attribute values.*/
size_t cmpp_io_get_netcdf_attribute_num_values(cmpp_io_context_t * const context,
                                               int const file_index,
                                               int const variable_index,
                                               int const attribute_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_attribute_t *aptr = NULL;
    size_t num_values;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get the number of values.*/
    aptr = get_att_ptr_from_index(fptr,
                                  variable_index,
                                  attribute_index);
    num_values = cmpp_io_attribute_get_num_values(aptr);
    aptr = NULL;
    fptr = NULL;

    return num_values;
}

/*---------------------------------------------------------------------------*/
/*Get the attribute values.*/
void *cmpp_io_get_netcdf_attribute_values(cmpp_io_context_t * const context,
                                          int const file_index,
                                          int const variable_index,
                                          int const attribute_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_attribute_t *aptr = NULL;
    void *values = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get the values.*/
    aptr = get_att_ptr_from_index(fptr,
                                  variable_index,
                                  attribute_index);
    values = cmpp_io_attribute_get_values(aptr);
    aptr = NULL;
    fptr = NULL;

    return values;
}

/*---------------------------------------------------------------------------*/
/*Given a pointer to a file object, variable index, and attribute index,
  return a pointer to the attribute object.*/
static cmpp_io_attribute_t *get_att_ptr_from_index(cmpp_io_netcdf_file_t * const fptr,
                                                   int const variable_index,
                                                   int const attribute_index)
{
    /*Local variables*/
    cmpp_io_attribute_t *aptr = NULL;
    cmpp_io_variable_t *vptr = NULL;

    if (variable_index == (int)NC_GLOBAL)
    {
        aptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                           attribute_index,
                                           ATT_PTR);
    }
    else
    {
        vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                           variable_index,
                                           VAR_PTR);
        aptr = cmpp_io_variable_get_ptr(vptr,
                                        attribute_index,
                                        ATT_PTR);
        vptr = NULL;
    }

    return aptr;
}

/*---------------------------------------------------------------------------*/
