#include <stdbool.h>
#include <stdlib.h>
#include "cmpp_io_context_t_api.h"
#include "cmpp_io_debug.h"
#include "cmpp_io_helper_functions.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_netcdf_file_t_api.h"
#include "cmpp_io_netcdf_utils.h"
#include "cmpp_io_utils.h"
#include "cmpp_io_variable_t_api.h"
#include "cmpp_io_variable_utils.h"
#include "netcdf.h"

/*---------------------------------------------------------------------------*/
/*Read in metadata for a variable from a netcdf file and store the data
  in a variable object.*/
int cmpp_io_read_netcdf_variable_metadata(cmpp_io_context_t * const context,
                                          int const file_index,
                                          int const netcdf_variable_id)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    int ncid;
    int err;
    int num_dimensions;
    int *dimension_ids = NULL;
    char name_buf[CMPP_IO_MAX_VAR_NAME_LEN];
    nc_type type_in_file;
    int num_attributes;
    cmpp_io_dimension_t **dimensions = NULL;
    int i;
    int variable_index;

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

    /*Get the number of dimensions associated with the variable.*/
    err = nc_inq_varndims(ncid,
                          netcdf_variable_id,
                          &num_dimensions);
    check_netcdf_call(err);

    if (num_dimensions > 0)
    {
        /*Allocate space to hold the netcdf ids of each dimension
          associated with the variable.*/
        cmpp_io_safemalloc((void **) &dimension_ids,
                           sizeof(int)*((size_t)num_dimensions));
    }

    /*Read in the variable metadata (except for the attributes).*/
    read_netcdf_variable_metadata(ncid,
                                  netcdf_variable_id,
                                  name_buf,
                                  (size_t)CMPP_IO_MAX_VAR_NAME_LEN,
                                  &type_in_file,
                                  NULL,
                                  dimension_ids,
                                  &num_attributes);

    if (num_dimensions > 0)
    {
        /*Allocate an array of dimension object pointers.*/
        cmpp_io_safemalloc((void **) &dimensions,
                           sizeof(cmpp_io_dimension_t *)*((size_t)num_dimensions));

        /*Get pointers to the dimensions associated with the read-in
          netcdf dimension ids.*/
        for (i=0;i<num_dimensions;i++)
        {
            dimensions[i] = cmpp_io_netcdf_file_get_ptr_by_netcdf_id(fptr,
                                                                     dimension_ids[i],
                                                                     DIM_PTR);
        }
        cmpp_io_safefree((void **) &dimension_ids);
    }

    /*Create a variable object.*/
    vptr = cmpp_io_variable_create(name_buf,
                                   type_in_file,
                                   netcdf_variable_id,
                                   dimensions,
                                   num_dimensions,
                                   num_attributes);
    if (num_dimensions > 0)
    {
        cmpp_io_safefree((void **) &dimensions);
    }

    /*Place the variable into the appropriate array.*/
    variable_index = cmpp_io_netcdf_file_add_ptr(&fptr,
                                                 vptr,
                                                 VAR_PTR);
    vptr = NULL;
    fptr = NULL;

    return variable_index;
}

/*---------------------------------------------------------------------------*/
/*Write out metadata for a variable to a netcdf file and store the data in
  a variable object if necessary.*/
int cmpp_io_write_netcdf_variable_metadata(cmpp_io_context_t * const context,
                                           int const file_index,
                                           nc_type const type_in_file,
                                           char * const name,
                                           int * const dimension_indices,
                                           int const num_dimensions,
                                           int const max_num_attributes,
                                           bool const buffer_metadata)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    int ncid;
    cmpp_io_dimension_t **dimensions = NULL;
    int *dimension_ids = NULL;
    int i;
    int netcdf_variable_id;
    int variable_index;

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

    if (num_dimensions > 0)
    {
        /*Allocate space to hold dimension object pointers and the
          netcdf dimension id associated with the inputted dimensions.*/
        cmpp_io_safemalloc((void **) &dimensions,
                           sizeof(cmpp_io_dimension_t *)*((size_t)num_dimensions));
        cmpp_io_safemalloc((void **) &dimension_ids,
                           sizeof(int)*((size_t)num_dimensions));

        for (i=0;i<num_dimensions;i++)
        {
            dimensions[i] = cmpp_io_netcdf_file_get_ptr(fptr,
                                                        dimension_indices[i],
                                                        DIM_PTR);
            dimension_ids[i] = cmpp_io_dimension_get_netcdf_id(dimensions[i]);
        }
    }

    /*Write out the variable metadata to the file.*/
    netcdf_variable_id = write_netcdf_variable_metadata(ncid,
                                                        type_in_file,
                                                        name,
                                                        dimension_ids,
                                                        num_dimensions);
    if (num_dimensions > 0)
    {
        cmpp_io_safefree((void **) &dimension_ids);

#ifdef use_netCDF3
        /*Set the shuffle and deflate parameters for the variable.*/
        int shuffle;
        int deflate;
        int err;
        shuffle = cmpp_io_context_get_shuffle(context);
        deflate = cmpp_io_context_get_deflate(context);
        if (shuffle != 0 || deflate != 0)
        {
            err = nc_def_var_deflate(ncid,
                                     netcdf_variable_id,
                                     shuffle,
                                     deflate,
                                     cmpp_io_context_get_deflate_level(context));
            check_netcdf_call(err);
        }
#endif
    }

    if (buffer_metadata)
    {
        /*Create a variable object.*/
        vptr = cmpp_io_variable_create(name,
                                       type_in_file,
                                       netcdf_variable_id,
                                       dimensions,
                                       num_dimensions,
                                       max_num_attributes);

        /*Place the variable into the appropriate array.*/
        variable_index = cmpp_io_netcdf_file_add_ptr(&fptr,
                                                     vptr,
                                                     VAR_PTR);
        vptr = NULL;
    }
    else
    {
        variable_index = (int)CMPP_IO_INDEX_NOT_FOUND;
    }

    if (num_dimensions > 0)
    {
        cmpp_io_safefree((void **) &dimensions);
    }
    fptr = NULL;

    return variable_index;
}

/*---------------------------------------------------------------------------*/
/*Read in data for a variable from a netcdf file and store the data
  in the space associated with the inputted data pointer.*/
void cmpp_io_read_netcdf_variable_data(cmpp_io_context_t * const context,
                                       int const file_index,
                                       int const variable_index,
                                       size_t * const corner_indices,
                                       size_t * const edge_lengths,
                                       void * const * const data,
                                       nc_type const type_in_mem)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    int netcdf_variable_id;
    int ncid;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get the netcdf id of the variable.*/
    vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       variable_index,
                                       VAR_PTR);
    netcdf_variable_id = cmpp_io_variable_get_netcdf_id(vptr);
    vptr = NULL;

    /*Get the netcdf id of the file.*/
    ncid = cmpp_io_netcdf_file_get_ncid(fptr);

    /*If necessary, put the file in data mode.*/
    if (cmpp_io_netcdf_file_get_is_in_define_mode(fptr))
    {
        cmpp_io_netcdf_file_enter_data_mode(&fptr,
                                            cmpp_io_context_get_header_buffer_val(context));
    }

    /*Read in the data from the file.*/
    read_netcdf_variable_data(ncid,
                              netcdf_variable_id,
                              corner_indices,
                              edge_lengths,
                              data,
                              type_in_mem);
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Write out data for a variable to a netcdf file.*/
void cmpp_io_write_netcdf_variable_data(cmpp_io_context_t * const context,
                                        int const file_index,
                                        int const variable_index,
                                        size_t * const corner_indices,
                                        size_t * const edge_lengths,
                                        void * const data,
                                        nc_type const type_in_mem)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    int netcdf_variable_id;
    int ncid;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get the netcdf id of the variable.*/
    vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       variable_index,
                                       VAR_PTR);
    netcdf_variable_id = cmpp_io_variable_get_netcdf_id(vptr);
    vptr = NULL;

    /*Get the netcdf id of the file.*/
    ncid = cmpp_io_netcdf_file_get_ncid(fptr);

    /*If necessary, put the file in data mode.*/
    if (cmpp_io_netcdf_file_get_is_in_define_mode(fptr))
    {
        cmpp_io_netcdf_file_enter_data_mode(&fptr,
                                            cmpp_io_context_get_header_buffer_val(context));
    }

    /*Read in the data from the file.*/
    write_netcdf_variable_data(ncid,
                               netcdf_variable_id,
                               corner_indices,
                               edge_lengths,
                               data,
                               type_in_mem);
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Buffer data for a variable in a netcdf file.*/
void cmpp_io_buffer_netcdf_variable_data(cmpp_io_context_t * const context,
                                         int const file_index,
                                         int const variable_index,
                                         size_t * const corner_indices,
                                         size_t * const edge_lengths,
                                         void * const data,
                                         nc_type const type_in_mem,
                                         bool const overwrite)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get a pointer to the variable object.*/
    vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       variable_index,
                                       VAR_PTR);
    /*Buffer the data.*/
    cmpp_io_variable_buffer_data(&vptr,
                                 corner_indices,
                                 edge_lengths,
                                 data,
                                 type_in_mem,
                                 overwrite);
    vptr = NULL;
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Write out data that was previously buffered for a variable in a file.*/
void cmpp_io_write_buffered_netcdf_variable_data(cmpp_io_context_t * const context,
                                                 int const file_index,
                                                 int const variable_index,
                                                 bool const free_data)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    int ncid;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get a pointer to the variable object.*/
    vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       variable_index,
                                       VAR_PTR);

    /*Get the netcdf id of the file.*/
    ncid = cmpp_io_netcdf_file_get_ncid(fptr);

    /*If necessary, put the file in data mode.*/
    if (cmpp_io_netcdf_file_get_is_in_define_mode(fptr))
    {
        cmpp_io_netcdf_file_enter_data_mode(&fptr,
                                            cmpp_io_context_get_header_buffer_val(context));
    }

    /*Write out the data.*/
    cmpp_io_variable_write_buffered_data(&vptr,
                                         ncid,
                                         free_data);
    vptr = NULL;
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Free space that was allocated for a variable.  Set the inputted variable
  index to CMPP_IO_VAR_NOT_FOUND.*/
void cmpp_io_free_netcdf_variable(cmpp_io_context_t * const context,
                                  int const file_index,
                                  int * const variable_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Free space and reset the variable_index.*/
    cmpp_io_netcdf_file_remove_ptr(&fptr,
                                   variable_index,
                                   VAR_PTR);
    fptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Given the name of a variable, return the index of the variable in the
  file's variables array.*/
int cmpp_io_get_netcdf_variable_index(cmpp_io_context_t * const context,
                                      int const file_index,
                                      char * const var_name)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    int variable_index;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    variable_index = cmpp_io_netcdf_file_get_ptr_index(fptr,
                                                       var_name,
                                                       VAR_PTR);
    fptr = NULL;

    return variable_index;
}

/*---------------------------------------------------------------------------*/
/*Return a flag telling if data has been buffered for a variable in a netcdf
  file.*/
bool cmpp_io_get_netcdf_variable_is_data_buffered(cmpp_io_context_t * const context,
                                                  int const file_index,
                                                  int const variable_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    bool is_data_buffered;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get a pointer to the variable object.*/
    vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       variable_index,
                                       VAR_PTR);

    /*Get a the flag telling if the data has been buffered.*/
    is_data_buffered = cmpp_io_variable_get_is_data_buffered(vptr);
    vptr = NULL;
    fptr = NULL;

    return is_data_buffered;
}

/*---------------------------------------------------------------------------*/
/*Return the maximum number of attributes allowed for the inputted variable in
  a netcdf file.*/
int cmpp_io_get_netcdf_variable_max_num_attributes(cmpp_io_context_t * const context,
                                                   int const file_index,
                                                   int const variable_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    int max_num_attributes;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get a pointer to the variable object.*/
    vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       variable_index,
                                       VAR_PTR);

    /*Get the maximum number of attributes allowed for the variable.*/
    max_num_attributes = cmpp_io_variable_get_max_num_attributes(vptr);
    vptr = NULL;
    fptr = NULL;

    return max_num_attributes;
}

/*---------------------------------------------------------------------------*/
/*Return the name of a variable in a netcdf file.*/
char *cmpp_io_get_netcdf_variable_name(cmpp_io_context_t * const context,
                                       int const file_index,
                                       int const variable_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    char *name = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get a pointer to the variable object.*/
    vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       variable_index,
                                       VAR_PTR);

    /*Get the number of attributes associated with the variable.*/
    name = cmpp_io_variable_get_name(vptr);
    vptr = NULL;
    fptr = NULL;

    return name;
}

/*---------------------------------------------------------------------------*/
/*Return the type that data that was buffered for a variable in a netcdf
  file is stored in the file as.*/
nc_type cmpp_io_get_netcdf_variable_type_in_file(cmpp_io_context_t * const context,
                                                 int const file_index,
                                                 int const variable_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    nc_type type_in_file;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get a pointer to the variable object.*/
    vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       variable_index,
                                       VAR_PTR);

    /*Get type of the bufffered data.*/
    type_in_file = cmpp_io_variable_get_type_in_file(vptr);
    vptr = NULL;
    fptr = NULL;

    return type_in_file;
}

/*---------------------------------------------------------------------------*/
/*Return the number of dimensions of a variable in a netcdf file.*/
int cmpp_io_get_netcdf_variable_num_dimensions(cmpp_io_context_t * const context,
                                               int const file_index,
                                               int const variable_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    int num_dimensions;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get a pointer to the variable object.*/
    vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       variable_index,
                                       VAR_PTR);

    /*Get the number of attributes associated with the variable.*/
    num_dimensions = cmpp_io_variable_get_num_dimensions(vptr);
    vptr = NULL;
    fptr = NULL;

    return num_dimensions;
}

/*---------------------------------------------------------------------------*/
/*Return the number of attributes associated with the inputted variable in
  a netcdf file.*/
int cmpp_io_get_netcdf_variable_num_attributes(cmpp_io_context_t * const context,
                                               int const file_index,
                                               int const variable_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    int num_attributes;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get a pointer to the variable object.*/
    vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       variable_index,
                                       VAR_PTR);

    /*Get the number of attributes associated with the variable.*/
    num_attributes = cmpp_io_variable_get_num_attributes(vptr);
    vptr = NULL;
    fptr = NULL;

    return num_attributes;
}

/*---------------------------------------------------------------------------*/
/*Return the netcdf id for the inputted variable in a netcdf file.*/
int cmpp_io_get_netcdf_variable_id(cmpp_io_context_t * const context,
                                   int const file_index,
                                   int const variable_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    int netcdf_id;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get a pointer to the variable object.*/
    vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       variable_index,
                                       VAR_PTR);

    /*Get the netcdf id of the variable.*/
    netcdf_id = cmpp_io_variable_get_netcdf_id(vptr);
    vptr = NULL;
    fptr = NULL;

    return netcdf_id;
}

/*---------------------------------------------------------------------------*/
/*Return a pointer to data that was buffered for a variable in a netcdf
  file.*/
void *cmpp_io_get_netcdf_variable_buffered_data(cmpp_io_context_t * const context,
                                                int const file_index,
                                                int const variable_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    void *data_ptr = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get a pointer to the variable object.*/
    vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       variable_index,
                                       VAR_PTR);

    /*Get a pointer to the data.*/
    data_ptr = cmpp_io_variable_get_data(vptr);
    vptr = NULL;
    fptr = NULL;

    return data_ptr;
}

/*---------------------------------------------------------------------------*/
/*Return a pointer to an array of corner indices that was buffered for a
  variable in a netcdf file.*/
size_t *cmpp_io_get_netcdf_variable_corner_indices(cmpp_io_context_t * const context,
                                                   int const file_index,
                                                   int const variable_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    size_t *corner_indices = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get a pointer to the variable object.*/
    vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       variable_index,
                                       VAR_PTR);

    /*Get a pointer to the corner indices.*/
    corner_indices = cmpp_io_variable_get_corner_indices(vptr);
    vptr = NULL;
    fptr = NULL;

    return corner_indices;
}

/*---------------------------------------------------------------------------*/
/*Return a pointer to an array of edge lengths that was buffered for a
  variable in a netcdf file.*/
size_t *cmpp_io_get_netcdf_variable_edge_lengths(cmpp_io_context_t * const context,
                                                 int const file_index,
                                                 int const variable_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    size_t *edge_lengths = NULL;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get a pointer to the variable object.*/
    vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       variable_index,
                                       VAR_PTR);

    /*Get a pointer to the edge lengths.*/
    edge_lengths = cmpp_io_variable_get_edge_lengths(vptr);
    vptr = NULL;
    fptr = NULL;

    return edge_lengths;
}

/*---------------------------------------------------------------------------*/
/*Return the type that data that was buffered for a variable in a netcdf
  file is stored in memory as.*/
nc_type cmpp_io_get_netcdf_variable_type_in_mem(cmpp_io_context_t * const context,
                                                int const file_index,
                                                int const variable_index)
{
    /*Local variables*/
    cmpp_io_netcdf_file_t *fptr = NULL;
    cmpp_io_variable_t *vptr = NULL;
    nc_type type_in_mem;

    /*Get a pointer to the file in the context's files array.*/
    fptr = cmpp_io_context_get_file_ptr(context,
                                        file_index,
                                        CMPP_NETCDF);

    /*Get a pointer to the variable object.*/
    vptr = cmpp_io_netcdf_file_get_ptr(fptr,
                                       variable_index,
                                       VAR_PTR);

    /*Get type of the bufffered data.*/
    type_in_mem = cmpp_io_variable_get_type_in_mem(vptr);
    vptr = NULL;
    fptr = NULL;

    return type_in_mem;
}

/*---------------------------------------------------------------------------*/
