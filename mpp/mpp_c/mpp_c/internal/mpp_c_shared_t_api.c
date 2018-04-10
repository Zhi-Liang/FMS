#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "helper_functions.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"
#include "mpp_c_parse_nml.h"
#include "mpp_c_pelist_t_api.h"
#include "mpp_c_shared_t_api.h"
#include "mpp_c_typedefs.h"
#include "json.h"
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Initialize an mpp_c_shared_t object.*/
void mpp_c_shared_init(mpp_c_shared_t * const * const self)
{
    /*Local variables*/
    mpp_c_shared_t *tmp_ptr = NULL; /*Pointer to the mpp_c_shared_t object.*/

    /*Set the local pointer to the mpp_c_shared_t object.*/
    tmp_ptr = *self;

    /*Make sure that the inputted mpp_c_shared_t pointer is not null.*/
    if (tmp_ptr == NULL)
    {
        throw_internal_error("MPP_C_SHARED_INIT","inputted mpp_c_shared_t"
                             " pointer is null.  First malloc space.");
    }

    /*Set default values.*/
    tmp_ptr->is_set = MPP_C_FALSE;
    tmp_ptr->world_pelist = NULL;
    tmp_ptr->input_nml_buffer = NULL;
    tmp_ptr->input_json_buffer = NULL;
    tmp_ptr->input_nml_buffer_size = 0;
    tmp_ptr->input_json_buffer_size = 0;
    tmp_ptr->etc_unit_is_stderr = MPP_C_FALSE;
    tmp_ptr->request_multiply = 0;
    tmp_ptr->record_timing_data = MPP_C_FALSE;
    tmp_ptr->sync_all_clocks = MPP_C_FALSE;

    /*Set the initialization flag to true.*/
    tmp_ptr->is_initialized = MPP_C_TRUE;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Reset an mpp_c_shared_t object.*/
void mpp_c_shared_reset(mpp_c_shared_t * const * const self)
{
    /*Local variables*/
    mpp_c_shared_t *tmp_ptr = NULL; /*Pointer to the mpp_c_shared_t object.*/

    /*Set the local pointer to the mpp_c_shared_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_shared_t object has been initialized.*/
    mpp_c_shared_check_init_state(tmp_ptr,"MPP_C_SHARED_RESET");

    /*Reset default values.*/
    if (tmp_ptr->is_set)
    {
        mpp_c_pelist_reset(&(tmp_ptr->world_pelist));
        safefree((void **) &(tmp_ptr->world_pelist));
        safefree((void **) &(tmp_ptr->input_nml_buffer));
        safefree((void **) &(tmp_ptr->input_json_buffer));

    }
    tmp_ptr->is_set = MPP_C_FALSE;
    tmp_ptr->input_nml_buffer_size = 0;
    tmp_ptr->input_json_buffer_size = 0;
    tmp_ptr->etc_unit_is_stderr = MPP_C_FALSE;
    tmp_ptr->request_multiply = 0;
    tmp_ptr->record_timing_data = MPP_C_FALSE;
    tmp_ptr->sync_all_clocks = MPP_C_FALSE;

    /*Set the initialization flag to false.*/
    tmp_ptr->is_initialized = MPP_C_FALSE;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Set the necessary data that will be shared between all contexts.*/
void mpp_c_shared_set_shared_data(mpp_c_shared_t * const * const self,
                                  MPI_Comm const local_comm)
{
    /*Local variables*/
    mpp_c_shared_t *tmp_ptr = NULL;  /*Pointer to the mpp_c_shared_t object.*/
    char *tmp_char_ptr = NULL;       /*Reusable character pointer.*/
    uint32_t *tmp_rank_list = NULL;  /*Rank list for the world pelist.*/
    int tmp_rank_list_size = 0;      /*Size of the rank list for the world pelist.*/
    int my_rank = 0;                 /*The MPI rank of the current process in the inputted communicator.*/
    uint32_t root_rank = 0;          /*Rank id of the root of the world pelist.*/
    FILE *tmp_file_ptr = NULL;       /*Reusable file pointer.*/
    size_t tmp_file_len = 0;         /*Length of the namelist file in bytes.*/
    int32_t input_nml_parse_err = 0; /*Namelist parsing error code.*/
    struct stat status;              /*File status.*/
    int ierr = 0;                    /*MPI error code.*/
    int ioerr = 0;                   /*I/O error code.*/
    int i = 0;                       /*Loop variable.*/

    /*Set the local pointer to the mpp_c_shared_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_shared_t object has been initialized.*/
    mpp_c_shared_check_init_state(tmp_ptr,"MPP_C_SHARED_RESET");

    /*Make sure that a valid MPI communicator has been passed in.*/
    if (local_comm == MPI_COMM_NULL)
    {
        throw_internal_error("MPP_C_SHARED_SET_SHARED_DATA","the inputted"
                             " MPI communicator is equal to MPI_COMM_NULL.");
    }

    /*Initialize the "world" pelist.*/
    safemalloc((void **) &(tmp_ptr->world_pelist),sizeof(mpp_c_pelist_t));
    mpp_c_pelist_init(&(tmp_ptr->world_pelist));

    /*Set the "world" pelist.*/
    tmp_char_ptr = "world pelist";
    ierr = MPI_Comm_size(local_comm,&tmp_rank_list_size);
    check_MPI_error_code(ierr,"MPI_Comm_size","MPP_C_SHARED_SET_SHARED_DATA");
    safemalloc((void **) &tmp_rank_list,
               sizeof(uint32_t)*((size_t)tmp_rank_list_size));
    for (i=0;i<tmp_rank_list_size;i++)
    {
        tmp_rank_list[i] = (uint32_t)i;
    }
    mpp_c_pelist_set_pelist(&(tmp_ptr->world_pelist),tmp_char_ptr,
                            strlen(tmp_char_ptr),tmp_rank_list,
                            (size_t)tmp_rank_list_size,local_comm);
    tmp_char_ptr = NULL;
    safefree((void **) &tmp_rank_list);

    /*Get the size of the input.nml file and broadcast it to all other ranks
      in the world pelist.*/
    ierr = MPI_Comm_rank(local_comm,&my_rank);
    check_MPI_error_code(ierr,"MPI_Comm_rank","MPP_C_SHARED_SET_SHARED_DATA");
    root_rank = mpp_c_pelist_get_root_rank(tmp_ptr->world_pelist);
    if ((uint32_t)my_rank == root_rank)
    {
        tmp_char_ptr = "input.nml";
        stat(tmp_char_ptr,&status);
        tmp_file_ptr = fopen(tmp_char_ptr,"r");
        tmp_file_len = (size_t)status.st_size+1;
    }
    ierr = MPI_Bcast((void *) &tmp_file_len,1,MPI_UINT64_T,(int)root_rank,
                     local_comm);
    check_MPI_error_code(ierr,"MPI_Bcast","MPP_C_SHARED_SET_SHARED_DATA");

    /*Make sure that the input.nml file is a valid size.*/
    if (tmp_file_len == 0 || tmp_file_len > MPP_C_NAMELIST_BUFFER_MAX_SIZE)
    {
        throw_internal_error("MPP_C_SHARED_SET_SHARED_DATA","the input.nml"
                             " file is either empty or larger than"
                             " MPP_C_NAMELIST_BUFFER_MAX_SIZE bytes.");
    }

    /*Allocate the input_nml buffer and set its size.*/
    safemalloc((void **) &(tmp_ptr->input_nml_buffer),
               sizeof(char)*tmp_file_len);
    tmp_ptr->input_nml_buffer_size = tmp_file_len;

    /*Read in the namelist buffer and broadcast it to all other ranks in the
      world pelist.*/
    if ((uint32_t)my_rank == root_rank)
    {
        for (i=0;i<(int64_t)tmp_file_len;i++)
        {
            tmp_ptr->input_nml_buffer[i] = getc(tmp_file_ptr);
        }
        tmp_ptr->input_nml_buffer[tmp_file_len-1] = '\0';

        /*Close the input.nml file.*/
        ioerr = fclose(tmp_file_ptr);
        if (ioerr != 0)
        {
            throw_internal_error("MPP_C_SHARED_SET_SHARED_DATA",
                                 "the input.nml file did not close properly.");
        }
        tmp_file_ptr = NULL;
    }
    ierr = MPI_Bcast((void *) (tmp_ptr->input_nml_buffer),tmp_file_len,
                     MPI_CHAR,(int)root_rank,local_comm);
    check_MPI_error_code(ierr,"MPI_Bcast","MPP_C_SHARED_SET_SHARED_DATA");

    /*Parse the input_nml buffer and set the namelist parameters.*/
    mpp_c_parse_nml(tmp_file_len,tmp_ptr->input_nml_buffer,
                    &(tmp_ptr->etc_unit_is_stderr),
                    &(tmp_ptr->request_multiply),
                    &(tmp_ptr->record_timing_data),
                    &(tmp_ptr->sync_all_clocks),
                    &(input_nml_parse_err));
    if (input_nml_parse_err != MPP_C_NML_PARSE_SUCCESS)
    {
        throw_internal_error("MPP_C_SHARED_SET_SHARED_DATA",
                             "error parsing the input.nml fortran namelist.");
    }


    if ((uint32_t)my_rank == root_rank)
    {
        tmp_char_ptr = "input.json";
        stat(tmp_char_ptr,&status);
        tmp_file_ptr = fopen(tmp_char_ptr,"r");
        tmp_file_len = (size_t)status.st_size+1;
    }
    ierr = MPI_Bcast((void *) &tmp_file_len,1,MPI_UINT64_T,(int)root_rank,
                     local_comm);
    check_MPI_error_code(ierr,"MPI_Bcast","MPP_C_SHARED_SET_SHARED_DATA");

    /*Make sure that the input.nml file is a valid size.*/
    if (tmp_file_len == 0 || tmp_file_len > MPP_C_NAMELIST_BUFFER_MAX_SIZE)
    {
        throw_internal_error("MPP_C_SHARED_SET_SHARED_DATA","the input.json"
                             " file is either empty or larger than"
                             " MPP_C_NAMELIST_BUFFER_MAX_SIZE bytes.");
    }

    /*Allocate the input_json buffer and set its size.*/
    safemalloc((void **) &(tmp_ptr->input_json_buffer),
               sizeof(char)*tmp_file_len);
    tmp_ptr->input_json_buffer_size = tmp_file_len;

    /*Read in the namelist buffer and broadcast it to all other ranks in the
      world pelist.*/
    if ((uint32_t)my_rank == root_rank)
    {
        for (i=0;i<(int64_t)tmp_file_len;i++)
        {
            tmp_ptr->input_json_buffer[i] = getc(tmp_file_ptr);
        }
        tmp_ptr->input_json_buffer[tmp_file_len-1] = '\0';

        /*Close the input.json file.*/
        ioerr = fclose(tmp_file_ptr);
        if (ioerr != 0)
        {
            throw_internal_error("MPP_C_SHARED_SET_SHARED_DATA",
                                 "the input.json file did not close properly.");
        }
        tmp_file_ptr = NULL;
    }
    ierr = MPI_Bcast((void *) (tmp_ptr->input_json_buffer),tmp_file_len,
                     MPI_CHAR,(int)root_rank,local_comm);
    check_MPI_error_code(ierr,"MPI_Bcast","MPP_C_SHARED_SET_SHARED_DATA");

   /* Initialize the output json */
   init_json_c ();

 struct jsonvariable v[4];
 v[0].json       = tmp_ptr->input_json_buffer;
 v[0].nml_name   = "mpp_nml";
 v[0].var_name   = "etc_unit_is_stderr";
 v[0].vartype    = LOG_VAR;
 v[0].var        = &tmp_ptr->etc_unit_is_stderr;
 v[0].asize      = SCALAR_SIZE;

 v[1].json       = tmp_ptr->input_json_buffer;
 v[1].nml_name   = "mpp_nml";
 v[1].var_name   = "request_multiply";
 v[1].vartype    = INTEGER_VAR;
 v[1].var        = &tmp_ptr->request_multiply;
 v[1].asize      = SCALAR_SIZE;

 v[2].json       = tmp_ptr->input_json_buffer;
 v[2].nml_name   = "mpp_nml";
 v[2].var_name   = "mpp_record_timing_data";
 v[2].vartype    = LOG_VAR;
 v[2].var        = &tmp_ptr->record_timing_data;
 v[2].asize      = SCALAR_SIZE;

 v[3].json       = tmp_ptr->input_json_buffer;
 v[3].nml_name   = "mpp_nml";
 v[3].var_name   = "sync_all_clocks";
 v[3].vartype    = LOG_VAR;
 v[3].var        = &tmp_ptr->sync_all_clocks;
 v[3].asize      = SCALAR_SIZE;



  /*Parse the input_json buffer and set the namelist parameters.*/
    cargs_json( v, sizeof(v)/sizeof(v[0]) );

    /*Mark the mpp_c_shared_t object as set.*/
    tmp_ptr->is_set = MPP_C_TRUE;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that tells whether or not an mpp_c_shared_t object has been
  initialized.*/
mpp_c_flag_t mpp_c_shared_is_init(mpp_c_shared_t const * const self)
{
    /*Local variables*/
    mpp_c_flag_t init_flag = 0; /*Initialization flag.*/

    init_flag = MPP_C_FALSE;
    if (self != NULL)
    {
        if (self->is_initialized)
        {
            init_flag = MPP_C_TRUE;
        }
    }

    return init_flag;
}

/*---------------------------------------------------------------------------*/
/*Function that throws a FATAL error if the inputted mpp_c_shared_t object
  is not initialized.*/
void mpp_c_shared_check_init_state(mpp_c_shared_t const * const self,
                                   char *routine_name)
{
    if (!mpp_c_shared_is_init(self))
    {
        throw_internal_error(routine_name,"you must first initialize the"
                             " mpp_c_shared_t object.");
    }

    return;
}
/*---------------------------------------------------------------------------*/
/*Function that tells whether or not an mpp_c_shared_t object has been
  set.*/
mpp_c_flag_t mpp_c_shared_is_set(mpp_c_shared_t const * const self)
{
    /*Make sure that the mpp_c_shared_t object has been initialized.*/
    mpp_c_shared_check_init_state(self,"MPP_C_SHARED_IS_SET");

    return self->is_set;
}

/*---------------------------------------------------------------------------*/
/*Function that throws a FATAL error if the inputted mpp_c_shared_t object
  is not set.*/
void mpp_c_shared_check_is_set(mpp_c_shared_t const * const self,
                               char *routine_name)
{
    if (!mpp_c_shared_is_set(self))
    {
        throw_internal_error(routine_name,"you must first set the"
                             " mpp_c_shared_t object.");
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the "world" pelist for an
  mpp_c_shared_t object.*/
mpp_c_pelist_t *mpp_c_shared_get_world_pelist(mpp_c_shared_t const * const self)
{
    /*Make sure that the mpp_c_shared_t object has been set.*/
    mpp_c_shared_check_is_set(self,"MPP_C_SHARED_GET_WORLD_PELIST");

    return self->world_pelist;
}

/*---------------------------------------------------------------------------*/
/*Function that returns a pointer to the input_nml buffer for an
  mpp_c_shared_t object.*/
char *mpp_c_shared_get_input_nml_buffer(mpp_c_shared_t const * const self)
{
    /*Make sure that the mpp_c_shared_t object has been set.*/
    mpp_c_shared_check_is_set(self,"MPP_C_SHARED_GET_INPUT_NML_BUFFER");

    return self->input_nml_buffer;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the size of the input_nml buffer in bytes for an
  mpp_c_shared_t object.*/
size_t mpp_c_shared_get_input_nml_buffer_size(mpp_c_shared_t const * const self)
{
    /*Make sure that the mpp_c_shared_t object has been set.*/
    mpp_c_shared_check_is_set(self,"MPP_C_SHARED_GET_INPUT_NML_BUFFER_SIZE");

    return self->input_nml_buffer_size;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the etc_file_is_stderr namelist parameter for an
  mpp_c_shared_t object.*/
mpp_c_flag_t mpp_c_shared_get_etc_unit_is_stderr(mpp_c_shared_t const * const self)
{
    /*Make sure that the mpp_c_shared_t object has been set.*/
    mpp_c_shared_check_is_set(self,"MPP_C_SHARED_GET_ETC_UNIT_IS_STDERR");

    return self->etc_unit_is_stderr;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the request_multiply namelist parameter for an
  mpp_c_shared_t object.*/
size_t mpp_c_shared_get_request_multiply(mpp_c_shared_t const * const self)
{
    /*Make sure that the mpp_c_shared_t object has been set.*/
    mpp_c_shared_check_is_set(self,"MPP_C_SHARED_GET_REQUEST_MULTIPLY");

    return self->request_multiply;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the record_timing_data namelist parameter for an
  mpp_c_shared_t object.*/
mpp_c_flag_t mpp_c_shared_get_record_timing_data(mpp_c_shared_t const * const self)
{
    /*Make sure that the mpp_c_shared_t object has been set.*/
    mpp_c_shared_check_is_set(self,"MPP_C_SHARED_GET_RECORD_TIMING_DATA");

    return self->record_timing_data;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the sync_all_clocks namelist parameter for an
  mpp_c_shared_t object.*/
mpp_c_flag_t mpp_c_shared_get_sync_all_clocks(mpp_c_shared_t const * const self)
{
    /*Make sure that the mpp_c_shared_t object has been set.*/
    mpp_c_shared_check_is_set(self,"MPP_C_SHARED_GET_SYNC_ALL_CLOCKS");

    return self->sync_all_clocks;
}

/*---------------------------------------------------------------------------*/

