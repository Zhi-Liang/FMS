#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cmpp_io_debug.h"
#include "cmpp_io_utils.h"
#include "cmpp_io_helper_functions.h"
#include "cmpp_io_macros.h"
#include "cmpp_io_netcdf_utils.h"
#include "netcdf.h"

#define XLG_STR_LEN 512

/*---------------------------------------------------------------------------*/
/*Check the error code returned from a netCDF call.*/
void check_netcdf_call(int const res)
{
    vcheck_netcdf_call(res,
                       "",
                       "");

    return;
}

/*---------------------------------------------------------------------------*/
/*Print a verbose error message and abort if an error occurred during a
  netCDF call.*/
void vcheck_netcdf_call(int const res,
                        char * const prefixf,
                        char * const suffixf,
                        ...)
{
    if (res != NC_NOERR)
    {
        size_t untruncated;
        char msgbase[XLG_STR_LEN];
        char msgformat[2*XLG_STR_LEN];

        /* packs base msg of nc err code and err string */
        untruncated = (size_t)snprintf(msgbase,
                                       sizeof(msgbase)-1,
                                       "Netcdf call failed with code = %d.\n\t%s. ",
                                       res,
                                       nc_strerror(res));

        if (untruncated >= sizeof(msgbase))
        {
            snprintf(msgbase,
                     sizeof(msgbase)-1,
                     "Netcdf call failed with code = %d (strmsg truncated due"
                     " to excessive length).\n",
                     res);
        }

        /* packs prefixf+base+suffixf as a format for the variadic print */
        untruncated = (size_t)snprintf(msgformat,
                                       sizeof(msgformat)-1,
                                       "%s%s%s\n",
                                       prefixf,
                                       msgbase,
                                       suffixf);

        if (untruncated >= sizeof(msgformat))
        {
            snprintf(msgformat,
                     sizeof(msgformat)-1,
                     "%s (prefix+suffix stripped due to excessive length).\n",
                     msgbase);
        }

        va_list args;
        va_start(args,
                 suffixf);
        vfprintf(stderr,
                 msgformat,
                 args);
        va_end(args);
        exit(EXIT_FAILURE);
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Make sure that the two inputted types are compatible for an attribute.*/
void check_netcdf_attribute_types(nc_type const type_in_mem,
                                  nc_type const type_in_file)
{
    /*Local variables*/
    char *file_type_str = NULL;
    char *mem_type_str = NULL;
    bool error;

    switch (type_in_file)
    {
        case NC_CHAR:
            file_type_str = "NC_CHAR";
            break;
        case NC_BYTE:
            file_type_str = "NC_BYTE";
            break;
        case NC_SHORT:
            file_type_str = "NC_SHORT";
            break;
        case NC_INT:
            file_type_str = "NC_INT";
            break;
        case NC_FLOAT:
            file_type_str = "NC_FLOAT";
            break;
        case NC_DOUBLE:
            file_type_str = "NC_DOUBLE";
            break;
        default:
            file_type_str = "unknown";
    }

    switch (type_in_mem)
    {
        case NC_CHAR:
            mem_type_str = "NC_CHAR";
            break;
        case NC_BYTE:
            mem_type_str = "NC_BYTE";
            break;
        case NC_SHORT:
            mem_type_str = "NC_SHORT";
            break;
        case NC_INT:
            mem_type_str = "NC_INT";
            break;
        case NC_FLOAT:
            mem_type_str = "NC_FLOAT";
            break;
        case NC_DOUBLE:
            mem_type_str = "NC_DOUBLE";
            break;
        default:
            mem_type_str = "unknown";
            break;
    }

    error = false;
    switch (type_in_mem)
    {
        case NC_CHAR:
        case NC_INT:
            if (type_in_file != type_in_mem)
            {
                error = true;
            }
            break;
        case NC_FLOAT:
            switch (type_in_file)
            {
                case NC_FLOAT:
                case NC_SHORT:
                case NC_BYTE:
                    break;
                default:
                    error = true;
                    break;
            }
            break;
        case NC_DOUBLE:
            switch (type_in_file)
            {
                case NC_DOUBLE:
                case NC_FLOAT:
                case NC_SHORT:
                case NC_BYTE:
                    break;
                default:
                    error = true;
                    break;
            }
            break;
        default:
            fatal("the inputted type in memory %s is not currently"
                      " supported.",
                  mem_type_str);
            break;
    }

    if (error)
    {
        fatal("the inputted type in memory %s is not compatiable with"
                  " the inputted type in the file %s.",
              mem_type_str,
              file_type_str);
    }
    mem_type_str = NULL;
    file_type_str = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Make sure that the two inputted types are compatible for a variable.*/
void check_netcdf_variable_types(nc_type const type_in_mem,
                                 nc_type const type_in_file)
{
    /*Local variables*/
    char *file_type_str = NULL;
    char *mem_type_str = NULL;
    bool error;

    switch (type_in_file)
    {
        case NC_INT:
            file_type_str = "NC_INT";
            break;
        case NC_FLOAT:
            file_type_str = "NC_FLOAT";
            break;
        case NC_DOUBLE:
            file_type_str = "NC_DOUBLE";
            break;
        default:
            file_type_str = "unknown";
            break;
    }

    switch (type_in_mem)
    {
        case NC_INT:
            mem_type_str = "NC_INT";
            break;
        case NC_FLOAT:
            mem_type_str = "NC_FLOAT";
            break;
        case NC_DOUBLE:
            mem_type_str = "NC_DOUBLE";
            break;
        default:
            mem_type_str = "unknown";
            break;
    }

    error = false;
    switch (type_in_mem)
    {
        case NC_INT:
        case NC_FLOAT:
            if (type_in_file != type_in_mem)
            {
                error = true;
            }
            break;
        case NC_DOUBLE:
            switch (type_in_file)
            {
                case NC_DOUBLE:
                case NC_FLOAT:
                    break;
                default:
                    error = true;
                    break;
            }
            break;
        default:
            error = true;
            break;
    }

    if (error)
    {
        fatal("the inputted type in memory %s is not compatiable with"
                  " the inputted type in the file %s.",
              mem_type_str,
              file_type_str);
    }
    mem_type_str = NULL;
    file_type_str = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Set the netCDF flags for a file.*/
int cmpp_io_get_ncflags(void)
{
    /*Local variables*/
    int ncflags = 0;

    /*Flags for netCDF classic mode.*/
    ncflags = NC_NETCDF4 | NC_CLASSIC_MODEL;

    return ncflags;
}

/*---------------------------------------------------------------------------*/
/*Determine the chunksize for the inputted netCDF file.*/
size_t cmpp_io_get_netCDF_chunksize(char const * const fname)
{
    /*Local variables*/
    size_t fsize;
    char *sbuf = NULL;
    char *sptr = NULL;
    char *ncblk = NULL;

    /*Create a string for an environment variable for the netCDF blocksize
      for the inputted file.*/
    cmpp_io_safemalloc((void **)(&sbuf),
                       sizeof(char)*(strlen(fname)+10));
    sprintf(sbuf,
            "NC_BLKSZ_%s",
            fname);

    /*Check if the environment variable defined by the above string exists.
      If it doesn't, then check for the general netCDF blocksize environment
      variable.*/
    sptr = getenv(sbuf);
    cmpp_io_safefree((void **)(&sbuf));
    if (sptr == NULL)
    {
        sptr = getenv("NC_BLKSZ");
    }

    /*If no netCDF blocksize environment variable was found, then use a
      default value of 64k.*/
    if (sptr == NULL)
    {
        cmpp_io_safemalloc((void **)(&ncblk),
                           sizeof(char)*5);
        snprintf(ncblk,
                 5,
                 "64k");
    }
    else
    {
        ncblk = sptr;
    }

    /*Get the chunksize for the netCDF file.*/
    fsize = cmpp_io_file_size(ncblk,
                              fname);

    /*If necessary, free memory held by ncblk.*/
    if (sptr == NULL)
    {
        cmpp_io_safefree((void **) (&ncblk));
    }

    /*Nullify local pointers.*/
    sptr = NULL;
    ncblk = NULL;

    return fsize;
}
/*---------------------------------------------------------------------------*/
/*Read in an attribute from a netCDF file.*/
void read_netcdf_attribute(int const ncid,
                           int const netcdf_variable_id,
                           int const netcdf_attribute_id,
                           char * const name_buf,
                           size_t const name_buf_size,
                           nc_type * const type_in_file,
                           nc_type * const type_in_mem,
                           size_t * const num_values,
                           void **values_buf,
                           char const * const file_name)
{
    /*Local variables*/
    char tmp_name[NC_MAX_NAME+1];
    char tmp_str[CMPP_IO_MAX_ATT_STR_LEN];
    size_t val_bytes;
    int err;
    int num_atts;

    /*Make sure that the inputted netcdf attribute id is valid.*/
    if (netcdf_variable_id == NC_GLOBAL)
    {
        err = nc_inq_natts(ncid,
                           &num_atts);
        check_netcdf_call(err);
        error_check((netcdf_attribute_id >= 0) &&
                        (netcdf_attribute_id < num_atts),
                    "the inputted netcdf attribute id (%d) must be >= 0 and"
                        " < the total number of global attributes (%d) in"
                        " netcdf file %s.",
                    netcdf_attribute_id,
                    num_atts,
                    file_name);
    }
    else
    {
        err = nc_inq_varnatts(ncid,
                              netcdf_variable_id,
                              &num_atts);
        check_netcdf_call(err);
        error_check((netcdf_attribute_id >= 0) &&
                        (netcdf_attribute_id < num_atts),
                    "the inputted netcdf attribute id (%d) must be >= 0 and"
                        " < the total number of attributes (%d) for variable"
                        " (%d) in netcdf file %s.",
                    netcdf_attribute_id,
                    num_atts,
                    netcdf_variable_id,
                    file_name);
    }

    /*Get the name of the attribute.*/
    err = nc_inq_attname(ncid,
                         netcdf_variable_id,
                         netcdf_attribute_id,
                         tmp_name);
    check_netcdf_call(err);

    /*Copy the attribute name into the inputted name buffer.  If the
      length of the name exceeds the passed in size of the name buffer,
      then throw a fatal error.*/
    safe_string_copy(tmp_name,
                     name_buf_size,
                     name_buf);

    /*Get the netCDF type and number of values stored in the attribute.*/
    err = nc_inq_att(ncid,
                     netcdf_variable_id,
                     name_buf,
                     type_in_file,
                     num_values);
    check_netcdf_call(err);
    *type_in_mem = *type_in_file;

    /*Read in the attribute values.*/
    switch (*type_in_file)
    {
        case NC_CHAR:
            /*Do not read in string-valued attributes if they are longer
              than CMPP_IO_MAX_STR_ATT_LEN characters.  Instead store
              the string value "unknown".*/
            if (*num_values >= (size_t)CMPP_IO_MAX_ATT_STR_LEN)
            {
                note("the attribute (%s) in netcdf file %s exceeds"
                         " CMPP_IO_MAX_STR_ATT_LEN"
                         " (%zu) characters.  This attribute will not be"
                         " read.",
                     name_buf,
                     file_name,
                     (size_t)CMPP_IO_MAX_ATT_STR_LEN);
                *num_values = 8;
                snprintf(tmp_str,
                         (size_t)CMPP_IO_MAX_ATT_STR_LEN,
                         "unknown");
                err = NC_NOERR;
            }
            else
            {
                err = nc_get_att_text(ncid,
                                      netcdf_variable_id,
                                      name_buf,
                                      tmp_str);
                *num_values = *num_values + 1;
            }
            val_bytes = sizeof(char)*(*num_values);
            cmpp_io_safemalloc((void **) values_buf,
                               val_bytes);
            snprintf((char *)(*values_buf),
                     *num_values,
                     "%s",
                     tmp_str);
            break;
        case NC_INT:
            val_bytes = sizeof(int)*(*num_values);
            cmpp_io_safemalloc((void **) values_buf,
                               val_bytes);
            err = nc_get_att_int(ncid,
                                 netcdf_variable_id,
                                 name_buf,
                                 (int *)(*values_buf));
            break;
        case NC_FLOAT:
            val_bytes = sizeof(float)*(*num_values);
            cmpp_io_safemalloc((void **) values_buf,
                               val_bytes);
            err = nc_get_att_float(ncid,
                                   netcdf_variable_id,
                                   name_buf,
                                   (float *)(*values_buf));
            break;
        case NC_DOUBLE:
            val_bytes = sizeof(double)*(*num_values);
            cmpp_io_safemalloc((void **) values_buf,
                               val_bytes);
            err = nc_get_att_double(ncid,
                                    netcdf_variable_id,
                                    name_buf,
                                    (double *)(*values_buf));
            break;
        default:
            fatal("the type (%d) that attribute %s is stored in the"
                      " netcdf file %s as must be one of: NC_CHAR (%d),"
                      " NC_INT (%d), NC_FLOAT (%d) or"
                      " NC_DOUBLE (%d).",
                  ((int)(*type_in_file)),
                  name_buf,
                  file_name,
                  (int)NC_CHAR,
                  (int)NC_INT,
                  (int)NC_FLOAT,
                  (int)NC_DOUBLE);
    }
    check_netcdf_call(err);

    return;
}

/*---------------------------------------------------------------------------*/
/*Write out an attribute to a netCDF file.*/
void write_netcdf_attribute(int const ncid,
                            int const variable_id,
                            nc_type const type_in_mem,
                            char * const name,
                            nc_type const type_in_file,
                            size_t const num_values,
                            void * const values,
                            char const * const file_name)
{
    /*Local variables*/
    char name_buf[CMPP_IO_MAX_ATT_NAME_LEN];
    char string_val_buf[CMPP_IO_MAX_ATT_STR_LEN];
    int err;

    /*Make sure that the length of the string does not exceed
      CMPP_IO_MAX_ATT_NAME_LEN characters and is null terminated.*/
    safe_string_copy(name,
                     (size_t)CMPP_IO_MAX_ATT_NAME_LEN,
                     name_buf);

    /*Make sure that the inputted type_in_file and type_in_mem values
      are valid and compatible.*/
    check_netcdf_attribute_types(type_in_mem,
                                 type_in_file);

    /*Make sure that the inputted number of values for the attribute
      is greater than zero.*/
    error_check((num_values > 0) &&
                    (num_values <= ((size_t)CMPP_IO_MAX_ATT_NUM_VALS)),
                "the inputted number of values (%zu) for attribute %s in"
                    " netcdf file %s must be > 0 and <="
                    " CMPP_IO_MAX_ATT_NUM_VALS (%zu).",
                num_values,
                name,
                file_name,
                (size_t)CMPP_IO_MAX_ATT_NUM_VALS);

    /*Make sure that the inputted values of the attribute are not
      null.*/
    error_check(values,
                "the inputted array of values for attribute %s"
                    " in netcdf file %s is null.",
                name,
                file_name);

    /*Write the attribute to the file.  If the attribute is a string, make
      sure that it does not exceed CMPP_IO_MAX_ATT_STR_LEN characters
      and is null terminated.*/
    switch (type_in_mem)
    {
        case NC_CHAR:
            safe_string_copy((char *)values,
                             (size_t)CMPP_IO_MAX_ATT_STR_LEN,
                             string_val_buf);
            err = nc_put_att_text(ncid,
                                  variable_id,
                                  name,
                                  num_values,
                                  string_val_buf);
            break;
        case NC_INT:
            err = nc_put_att_int(ncid,
                                 variable_id,
                                 name,
                                 type_in_file,
                                 num_values,
                                 (int *)values);
            break;
        case NC_FLOAT:
            err = nc_put_att_float(ncid,
                                   variable_id,
                                   name,
                                   type_in_file,
                                   num_values,
                                   (float *)values);
            break;
        case NC_DOUBLE:
            err = nc_put_att_double(ncid,
                                    variable_id,
                                    name,
                                    type_in_file,
                                    num_values,
                                    (double *)values);
            break;
    }
    check_netcdf_call(err);

    return;
}

/*---------------------------------------------------------------------------*/
/*Read in a dimension from a netcdf file.*/
void read_netcdf_dimension(int const ncid,
                           int const netcdf_dimension_id,
                           char * const name_buf,
                           size_t const name_buf_size,
                           size_t * const length,
                           bool * const is_unlimited,
                           char const * const file_name)
{
    /*Local variables*/
    int err;
    char tmp_name[NC_MAX_NAME+1];
    int num_unlimited_dims;
    int *unlimited_dim_ids = NULL;
    int i;
    int num_dims;

    /*Make sure that the inputted netcdf attribute id is valid.*/
    err = nc_inq_ndims(ncid,
                       &num_dims);
    check_netcdf_call(err);
    error_check((netcdf_dimension_id >= 0) &&
                    (netcdf_dimension_id < num_dims),
                "the inputted netcdf dimension id (%d) must be >= 0 and"
                    " < the total number of dimensions (%d) in"
                    " netcdf file %s.",
                netcdf_dimension_id,
                num_dims,
                file_name);

    /*Read in the dimension name and length.*/
    err = nc_inq_dim(ncid,
                     netcdf_dimension_id,
                     tmp_name,
                     length);
    check_netcdf_call(err);

    /*Copy the dimension name into the inputted buffer. If the
      length of the name exceeds the inputted buffer size (name_buf_size),
      then throw a fatal error.*/
    safe_string_copy(tmp_name,
                     name_buf_size,
                     name_buf);

    /*Check if this dimension is unlimited.*/
#ifdef use_netCDF3
    num_unlimited_dims = 1;
    cmpp_io_safemalloc((void **) &unlimited_dim_ids,
                       sizeof(int));
    err = nc_inq_unlimdim(ncid,
                          &unlimited_dim_ids);
#else
    err = nc_inq_unlimdims(ncid,
                           &num_unlimited_dims,
                           NULL);
    check_netcdf_call(err);
    cmpp_io_safemalloc((void **) &unlimited_dim_ids,
                       sizeof(int)*((size_t)num_unlimited_dims));
    err = nc_inq_unlimdims(ncid,
                           &num_unlimited_dims,
                           unlimited_dim_ids);
#endif
    check_netcdf_call(err);
    *is_unlimited = false;
    for (i=0;i<num_unlimited_dims;i++)
    {
        if (netcdf_dimension_id == unlimited_dim_ids[i])
        {
            *is_unlimited = true;
            break;
        }
    }
    cmpp_io_safefree((void **) &unlimited_dim_ids);

    return;
}

/*---------------------------------------------------------------------------*/
/*Write out a netCDF dimension name and length to a netCDF file.  Return the
  netCDF id for the dimension.*/
int write_netcdf_dimension(int const ncid,
                           char * const name,
                           size_t const length,
                           char const * const file_name)
{
    /*Local variables*/
    char name_buf[CMPP_IO_MAX_ATT_NAME_LEN];
    int err;
    int netcdf_id;

    /*Make sure that the length of the inputted name does not exceed
      CMPP_IO_MAX_ATT_NAME_LEN characters and null terminate it.*/
    safe_string_copy(name,
                     (size_t)CMPP_IO_MAX_DIM_NAME_LEN,
                     name_buf);

    if (length != (size_t)NC_UNLIMITED)
    {
        /*Make sure that the inputted number of values for the attribute
          is greater than zero.*/
        error_check((length > 0) && (length <= ((size_t)CMPP_IO_MAX_DIM_LEN)),
                    "the inputted length (%zu) for dimension %s in"
                        " netcdf file %s must be > 0 and <="
                        " CMPP_IO_MAX_DIM_LEN (%zu).",
                    length,
                    name,
                    file_name,
                    (size_t)CMPP_IO_MAX_DIM_LEN);
    }

    /*Write out the dimension.*/
    err = nc_def_dim(ncid,
                     name_buf,
                     length,
                     &netcdf_id);
    check_netcdf_call(err);

    return netcdf_id;
}

/*---------------------------------------------------------------------------*/
/*Read in the metadata for a variable in a netCDF file.*/
void read_netcdf_variable_metadata(int const ncid,
                                   int const netcdf_variable_id,
                                   char * const name_buf,
                                   size_t const name_buf_size,
                                   nc_type * const type_in_file,
                                   int * const num_dimensions,
                                   int * const dimension_ids,
                                   int * const num_attributes)
{
    /*Local variables*/
    int err;
    char tmp_name[NC_MAX_NAME+1];

    /*Read in the variable metadata from the file.*/
    err = nc_inq_var(ncid,
                     netcdf_variable_id,
                     tmp_name,
                     type_in_file,
                     num_dimensions,
                     dimension_ids,
                     num_attributes);
    check_netcdf_call(err);

    /*Copy the variable name into the inputted buffer. If the
      length of the name exceeds the inputted buffer size (name_buf_size),
      then throw a fatal error.*/
    safe_string_copy(tmp_name,
                     name_buf_size,
                     name_buf);

    return;
}

/*---------------------------------------------------------------------------*/
/*Write out metadata for a variable in a netCDF file.*/
int write_netcdf_variable_metadata(int const ncid,
                                   nc_type const type_in_file,
                                   char * const name,
                                   int * const dimension_ids,
                                   int const num_dimensions)
{
    /*Local variables*/
    char name_buf[CMPP_IO_MAX_VAR_NAME_LEN];
    int err;
    int netcdf_variable_id;

    /*Make sure that the length of the inputted name does not exceed
      CMPP_IO_MAX_VAR_NAME_LEN characters and null terminate it.*/
    safe_string_copy(name,
                     (size_t)CMPP_IO_MAX_VAR_NAME_LEN,
                     name_buf);

    /*Make sure that compatible dimesion id array and number of dimensions
      were passed in.*/
    error_check((num_dimensions >= 0) ||
                    (num_dimensions <= (int)NC_MAX_VAR_DIMS),
                "the inputted number of dimensions (%d) for variable %s"
                    " must be >=0 and <= NC_MAX_VAR_DIMS (%d).",
                num_dimensions,
                name,
                (int)NC_MAX_VAR_DIMS);
    if (num_dimensions > 0)
    {
        error_check(dimension_ids,
                    "the inputted array of dimension ids for variable %s"
                        " cannot be null if the inputted number of dimensions"
                        " (%d) is > 0.",
                    name,
                    num_dimensions);
    }

    /*Write out the variable metadata to the file.*/
    err = nc_def_var(ncid,
                     name_buf,
                     type_in_file,
                     num_dimensions,
                     dimension_ids,
                     &netcdf_variable_id);
    check_netcdf_call(err);

    return netcdf_variable_id;
}

/*---------------------------------------------------------------------------*/
/*Read in variable data from a netcdf file.*/
void read_netcdf_variable_data(int const ncid,
                               int const netcdf_variable_id,
                               size_t const * const corner_indices,
                               size_t const * const edge_lengths,
                               void * const * const data,
                               nc_type const type_in_mem)
{
    /*Local variables*/
    int err;
    void *ptr = NULL;

    /*Read in the data.*/
    ptr = *data;
    switch (type_in_mem)
    {
        case NC_INT:
            err = nc_get_vara_int(ncid,
                                  netcdf_variable_id,
                                  corner_indices,
                                  edge_lengths,
                                  (int *)ptr);
            break;
        case NC_FLOAT:
            err = nc_get_vara_float(ncid,
                                    netcdf_variable_id,
                                    corner_indices,
                                    edge_lengths,
                                    (float *)ptr);
            break;
        case NC_DOUBLE:
            err = nc_get_vara_double(ncid,
                                     netcdf_variable_id,
                                     corner_indices,
                                     edge_lengths,
                                     (double *)ptr);
            break;
        case NC_CHAR:
            err = nc_get_vara_text(ncid,
                                   netcdf_variable_id,
                                   corner_indices,
                                   edge_lengths,
                                   (char *)ptr);
            break;
        default:
            fatal("the inputted type that the data is stored in memory"
                      " as (%d) must be one of NC_INT (%d), NC_FLOAT (%d),"
                      " NC_DOUBLE (%d), or NC_CHAR (%d).",
                  type_in_mem,
                  (int)NC_INT,
                  (int)NC_FLOAT,
                  (int)NC_DOUBLE,
                  (int)NC_CHAR);
    }
    check_netcdf_call(err);
    ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Write out variable data to a netcdf file.*/
void write_netcdf_variable_data(int const ncid,
                                int const netcdf_variable_id,
                                size_t const * const corner_indices,
                                size_t const * const edge_lengths,
                                void * const data,
                                nc_type const type_in_mem)
{
    /*Local variables*/
    int err;

    /*Write out the data.*/
    switch (type_in_mem)
    {
        case NC_INT:
            err = nc_put_vara_int(ncid,
                                  netcdf_variable_id,
                                  corner_indices,
                                  edge_lengths,
                                  (int *)data);
            break;
        case NC_FLOAT:
            err = nc_put_vara_float(ncid,
                                    netcdf_variable_id,
                                    corner_indices,
                                    edge_lengths,
                                    (float *)data);
            break;
        case NC_DOUBLE:
            err = nc_put_vara_double(ncid,
                                     netcdf_variable_id,
                                     corner_indices,
                                     edge_lengths,
                                     (double *)data);
            break;
        default:
            fatal("the inputted type that the data is stored in memory"
                      " as (%d) must be one of NC_INT (%d), NC_FLOAT (%d),"
                      " or NC_DOUBLE (%d).",
                  type_in_mem,
                  (int)NC_INT,
                  (int)NC_FLOAT,
                  (int)NC_DOUBLE);
    }
    check_netcdf_call(err);

    return;
}

/*---------------------------------------------------------------------------*/
