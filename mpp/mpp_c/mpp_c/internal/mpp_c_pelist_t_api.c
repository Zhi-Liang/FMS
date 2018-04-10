#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "helper_functions.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"
#include "mpp_c_pelist_t_api.h"
#include "mpp_c_typedefs.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Initialize an mpp_c_pelist_t object.*/
void mpp_c_pelist_init(mpp_c_pelist_t * const * const self)
{
    /*Local variables*/
    mpp_c_pelist_t *tmp_ptr = NULL; /*Pointer to the mpp_c_pelist_t object.*/

    /*Set the local pointer to the mpp_c_pelist_t object.*/
    tmp_ptr = *self;

    /*Make sure that the inputted mpp_c_pelist_t pointer is not null.*/
    if (tmp_ptr == NULL)
    {
        throw_internal_error("MPP_C_PELIST_INIT","inputted mpp_c_pelist_t"
                             " pointer is null.  First malloc space.");
    }

    /*Set default values.*/
    tmp_ptr->is_set = MPP_C_FALSE;
    snprintf(tmp_ptr->name,MPP_C_MAX_PELIST_NAME_LENGTH,"%c",'\0');
    tmp_ptr->rank_list = NULL;
    tmp_ptr->rank_list_size = 0;
    tmp_ptr->root_rank = 0;
    tmp_ptr->comm_id = MPI_COMM_NULL;
    tmp_ptr->group_id = MPI_GROUP_NULL;

    /*Set the initialization flag to true.*/
    tmp_ptr->is_initialized = MPP_C_TRUE;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Release memory and reset default values for an mpp_c_pelist_t object.*/
void mpp_c_pelist_reset(mpp_c_pelist_t * const * const self)
{
    /*Local variables*/
    mpp_c_pelist_t *tmp_ptr = NULL;       /*Pointer to the mpp_c_pelist_t object.*/
    int ierr = 0;                         /*MPI error code.*/
    MPI_Group tmp_group = MPI_GROUP_NULL; /*MPI group id.*/

    /*Set the local pointer to the mpp_c_pelist_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_pelist_t object has been initialized.*/
    mpp_c_pelist_check_init_state(tmp_ptr,"MPP_C_PELIST_RESET");

    /*Free MPI communicators and groups and reset default values.*/
    if (mpp_c_pelist_is_set(tmp_ptr))
    {
        if (mpp_c_pelist_get_rank_list(tmp_ptr) != NULL)
        {
            safefree((void **) &(tmp_ptr->rank_list));
            tmp_ptr->rank_list = NULL;
        }
        if (mpp_c_pelist_get_comm_id(tmp_ptr) != MPI_COMM_NULL &&
            mpp_c_pelist_get_comm_id(tmp_ptr) != MPI_COMM_WORLD)
        {
            ierr = MPI_Comm_free(&(tmp_ptr->comm_id));
            check_MPI_error_code(ierr,"MPI_Comm_free","MPP_C_PELIST_RESET");
        }
        ierr = MPI_Comm_group(MPI_COMM_WORLD,&tmp_group);
        check_MPI_error_code(ierr,"MPI_Comm_group","MPP_C_PELIST_RESET");
        if (mpp_c_pelist_get_group_id(tmp_ptr) != MPI_GROUP_NULL &&
            mpp_c_pelist_get_group_id(tmp_ptr) != tmp_group)
        {
            ierr = MPI_Group_free(&(tmp_ptr->group_id));
            check_MPI_error_code(ierr,"MPI_Group_free","MPP_C_PELIST_RESET");
        }
    }
    tmp_ptr->is_set = MPP_C_FALSE;
    snprintf(tmp_ptr->name,MPP_C_MAX_PELIST_NAME_LENGTH,"%c",'\0');
    tmp_ptr->rank_list_size = 0;
    tmp_ptr->root_rank = 0;
    tmp_ptr->comm_id = MPI_COMM_NULL;
    tmp_ptr->group_id = MPI_GROUP_NULL;

    /*Set the initialization flag to false.*/
    tmp_ptr->is_initialized = MPP_C_FALSE;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Create a pelist for an mpp_c_pelist_t object.*/
void mpp_c_pelist_set_pelist(mpp_c_pelist_t * const * const self,
                             char const *name,
                             size_t const name_len,
                             uint32_t const *rank_array,
                             size_t const num_ranks,
                             MPI_Comm const comm_id)
{
    /*Local variables*/
    mpp_c_pelist_t *tmp_ptr = NULL;       /*Pointer to the mpp_c_pelist_t object.*/
    int tmp_num_ranks = 0;                /*Total number of ranks in the inputted communicator.*/
    int tmp_rank = 0;                     /*Rank id for the process in the inputted communicator.*/
    MPI_Group tmp_group = MPI_GROUP_NULL; /*MPI group id for inputted communicator.*/
    int ierr = 0;                         /*MPI error code.*/
    unsigned int i = 0;                   /*Loop variable.*/

    /*Set the local pointer to the mpp_c_pelist_t object.*/
    tmp_ptr = *self;

    /*Make sure that the mpp_c_pelist_t object has been initialized.*/
    mpp_c_pelist_check_init_state(tmp_ptr,"MPP_C_PELIST_SET_PELIST");

    /*Check inputted string.*/
    check_string_input(name,name_len,"MPP_C_PELIST_SET_PELIST");

    /*Check inputted array.*/
    check_array_input((void *)rank_array,num_ranks,"MPP_C_PELIST_SET_PELIST");

    /*Check inputted communicator id.*/
    check_comm_id_input(comm_id,"MPP_C_PELIST_SET_PELIST");

    /*Set the name for the pelist.  The name will be truncated if it exceeds
      MPP_C_MAX_PELIST_NAME_LENGTH characters.*/
    if (name_len > MPP_C_MAX_PELIST_NAME_LENGTH)
    {
        print_internal_message("MPP_C_PELIST_SET_PELIST","inputted name"
                               " exceeds MPP_C_MAX_PELIST_NAME_LENGTH"
                               " characters and will be truncated.");
    }
    snprintf(tmp_ptr->name,MPP_C_MAX_PELIST_NAME_LENGTH,"%s",name);

    /*Allocate and set the array of rank ids. The inputted rank array must
      be monotonically increasing, include the current processes rank id in
      the inputted communicator, and not contain any rank ids that are less
      than 0 or greater than the total number of rank ids in the inputted
      communicator minus 1.*/
    ierr = MPI_Comm_size(comm_id,&tmp_num_ranks);
    check_MPI_error_code(ierr,"MPI_Comm_size","MPP_C_PELIST_SET_PELIST");
    if (num_ranks > (unsigned int)tmp_num_ranks)
    {
        throw_internal_error("MPP_C_PELIST_SET_PELIST","size of the inputted"
                             " rank array cannot be greater than the total"
                             " number of ranks the total number of ranks in"
                             " the world pelist.");
    }
    ierr = MPI_Comm_rank(comm_id,&tmp_rank);
    check_MPI_error_code(ierr,"MPI_Comm_rank","MPP_C_PELIST_SET_PELIST");
    if (num_ranks == 1 && tmp_num_ranks > 1)
    {
        if (rank_array[0] != (unsigned int)tmp_rank)
        {
            throw_internal_error("MPP_C_PELIST_SET_PELIST","if more than one"
                                 " rank exists, the only allowed pelist of"
                                 " size one must contain the current rank's"
                                 " rank id in the wold pelist.");
        }
    }
    for (i=0;i<num_ranks;i++)
    {
        if (rank_array[i] >= (unsigned int)tmp_num_ranks)
        {
            throw_internal_error("MPP_C_PELIST_SET_PELIST","invalid rank id in"
                                 " inputted rank array.");
        }
        if (i > 0)
        {
            if (rank_array[i] <= rank_array[i-1])
            {
                throw_internal_error("MPP_C_PELIST_SET_PELIST","the inputted"
                                     " rank array is not monotonically"
                                     " increasing.");
            }
        }
    }
    tmp_ptr->rank_list_size = num_ranks;
    safemalloc((void **) &(tmp_ptr->rank_list),sizeof(uint32_t)*num_ranks);
    memcpy((void *)(tmp_ptr->rank_list),(void *)rank_array,
           sizeof(uint32_t)*num_ranks);
    tmp_ptr->root_rank = rank_array[0];

    /*Create a new MPI group and associated MPI communicator.*/
    ierr = MPI_Comm_group(comm_id,&tmp_group);
    check_MPI_error_code(ierr,"MPI_Comm_group","MPP_C_PELIST_SET_PELIST");

    ierr = MPI_Group_incl(tmp_group,num_ranks,(int32_t *)rank_array,
                          &(tmp_ptr->group_id));
    check_MPI_error_code(ierr,"MPI_Group_incl","MPP_C_PELIST_SET_PELIST");
    if (comm_id == MPI_COMM_WORLD)
    {
        tmp_ptr->comm_id = comm_id;
    }
    else
    {
        ierr = MPI_Comm_create(comm_id,tmp_ptr->group_id,
                               &(tmp_ptr->comm_id));
        check_MPI_error_code(ierr,"MPI_Comm_create",
                             "MPP_C_PELIST_SET_PELIST");
    }

    /*Set the is_set flag to true.*/
    tmp_ptr->is_set = MPP_C_TRUE;

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that sets the root rank id for an mpp_c_pelist_t object.*/
void mpp_c_pelist_set_root_rank(mpp_c_pelist_t * const * const self,
                                uint32_t const root_rank)
{
    /*Local variables*/
    mpp_c_pelist_t *tmp_ptr = NULL; /*Pointer to the mpp_c_pelist_t object.*/
    uint32_t *rank_list = NULL;     /*Rank list.*/
    size_t rank_list_size = 0;      /*Rank list size.*/

    /*Set the local pointer to the mpp_c_pelist_t object.*/
    tmp_ptr = *self;

    /*Make sure the inputted rank id exists in the object's rank list.*/
    rank_list = mpp_c_pelist_get_rank_list(tmp_ptr);
    rank_list_size = mpp_c_pelist_get_rank_list_size(tmp_ptr);
    if (is_int_in_int_array(root_rank,rank_list,rank_list_size))
    {
        tmp_ptr->root_rank = root_rank;
    }
    else
    {
        throw_internal_error("MPP_C_PELIST_SET_ROOT_RANK","the inputted rank"
                             " id does not exist in the object's rank list.");
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that tells whether or not an mpp_c_pelist_t object has been
  initialized.*/
mpp_c_flag_t mpp_c_pelist_is_init(mpp_c_pelist_t const * const self)
{
    /*Local variables*/
    mpp_c_flag_t init_flag = -1; /*Initialization flag.*/

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
/*Function that throws a FATAL error if the inputted mpp_c_pelist_t object
  is not initialized.*/
void mpp_c_pelist_check_init_state(mpp_c_pelist_t const * const self,
                                   char *routine_name)
{
    if (!mpp_c_pelist_is_init(self))
    {
        throw_internal_error(routine_name,"you must first initialize the"
                             " mpp_c_pelist_t object.");
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that tells whether or not a pelist has been set for an
   mpp_c_pelist_t object.*/
mpp_c_flag_t mpp_c_pelist_is_set(mpp_c_pelist_t const * const self)
{
    /*Make sure that the mpp_c_pelist_t object has been initialized.*/
    mpp_c_pelist_check_init_state(self,"MPP_C_PELIST_IS_SET");

    return self->is_set;
}

/*---------------------------------------------------------------------------*/
/*Function that throws a FATAL error if the inputted mpp_c_pelist_t object
  is not set.*/
void mpp_c_pelist_check_is_set(mpp_c_pelist_t const * const self,
                               char *routine_name)
{
    if (!mpp_c_pelist_is_set(self))
    {
        throw_internal_error(routine_name,"you must first set the"
                             " mpp_c_pelist_t object.");
    }

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the name of a pelist for an mpp_pelist_type object.*/
char *mpp_c_pelist_get_name(mpp_c_pelist_t const * const self)
{
    /*Make sure that the mpp_c_pelist_t object has been set.*/
    mpp_c_pelist_check_is_set(self,"MPP_C_PELIST_GET_NAME");

    return self->name;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the rank list for an mpp_c_pelist_t object.*/
uint32_t *mpp_c_pelist_get_rank_list(mpp_c_pelist_t const * const self)
{
    /*Make sure that the mpp_c_pelist_t object has been set.*/
    mpp_c_pelist_check_is_set(self,"MPP_C_PELIST_GET_RANK_LIST");

    return self->rank_list;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the size of the rank list for an mpp_c_pelist_t
  object.*/
size_t mpp_c_pelist_get_rank_list_size(mpp_c_pelist_t const * const self)
{
    /*Make sure that the mpp_c_pelist_t object has been set.*/
    mpp_c_pelist_check_is_set(self,"MPP_C_PELIST_GET_RANK_LIST_SIZE");

    return self->rank_list_size;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the root rank id for an mpp_c_pelist_t object.*/
uint32_t mpp_c_pelist_get_root_rank(mpp_c_pelist_t const * const self)
{
    /*Make sure that the mpp_c_pelist_t object has been set.*/
    mpp_c_pelist_check_is_set(self,"MPP_C_PELIST_GET_ROOT_RANK");

    return self->root_rank;
}

/*---------------------------------------------------------------------------*/
/*Function that determines if the inputted rank is the root of the
  mpp_c_pelist_t object.*/
mpp_c_flag_t mpp_c_pelist_is_root_rank(mpp_c_pelist_t const * const self,
                                       uint32_t const root_rank)
{
    /*Local variables*/
    mpp_c_flag_t is_root = 0; /*Is root flag.*/

    /*Make sure that the mpp_c_pelist_t object has been set.*/
    mpp_c_pelist_check_is_set(self,"MPP_C_PELIST_IS_ROOT_RANK");

    /*Determine if the rank is the root.*/
    if (root_rank == mpp_c_pelist_get_root_rank(self))
    {
        is_root = MPP_C_TRUE;
    }
    else
    {
        is_root = MPP_C_FALSE;
    }

    return is_root;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the MPI communicator id for an mpp_c_pelist_t
  object.*/
MPI_Comm mpp_c_pelist_get_comm_id(mpp_c_pelist_t const * const self)
{
    /*Make sure that the mpp_c_pelist_t object has been set.*/
    mpp_c_pelist_check_is_set(self,"MPP_C_PELIST_GET_COMM_ID");

    return self->comm_id;
}

/*---------------------------------------------------------------------------*/
/*Function that returns the MPI group id for an mpp_c_pelist_t object.*/
MPI_Group mpp_c_pelist_get_group_id(mpp_c_pelist_t const * const self)
{
    /*Make sure that the mpp_c_pelist_t object has been set.*/
    mpp_c_pelist_check_is_set(self,"MPP_C_PELIST_GET_GROUP_ID");

    return self->group_id;
}

/*---------------------------------------------------------------------------*/
/*Function that deterimines if a rank exists on the rank list for an
  mpp_c_pelist_t object.*/
mpp_c_flag_t mpp_c_pelist_is_rank_on_rank_list(mpp_c_pelist_t const * const self,
                                               uint32_t const rank)
{
    /*Local variables*/
    mpp_c_flag_t is_rank_on_rank_list = 0; /*Flag denoting whether the inputted rank is on the rank list.*/
    uint32_t *tmp_rank_list = NULL;        /*Pointer to a rank list.*/
    size_t tmp_rank_list_size = 0;         /*Rank list size.*/

    tmp_rank_list = mpp_c_pelist_get_rank_list(self);
    tmp_rank_list_size = mpp_c_pelist_get_rank_list_size(self);
    is_rank_on_rank_list = is_int_in_int_array(rank,tmp_rank_list,
                                               tmp_rank_list_size);

    /*Nullify local pointers.*/
    tmp_rank_list = NULL;

    return is_rank_on_rank_list;
}

/*---------------------------------------------------------------------------*/

