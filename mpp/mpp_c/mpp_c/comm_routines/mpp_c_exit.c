#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "helper_functions.h"
#include "mpp_c_context_t_api.h"
#include "mpp_c_exit.h"
#include "mpp_c_internal_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_mpi.h"
#include "mpp_c_shared_t_api.h"
#include "mpp_c_timing_table.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Function that finalizes the mpp_c library.*/
void mpp_c_exit(mpp_c_shared_t **shared,
                mpp_c_context_t **context)
{
    /*Local variables.*/
    mpp_c_shared_t *tmp_shared = NULL;    /*Pointer to the mpp_c_shared_t object.*/
    mpp_c_context_t *tmp_context = NULL;  /*Pointer to the mpp_c_context_t object.*/
    MPI_Comm tmp_comm_id = MPI_COMM_NULL; /*MPI communicator id.*/
    int ierr = 0;                         /*MPI error code.*/

    /*Set the local pointer to the mpp_c_shared_t object.*/
    tmp_shared = *shared;

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_context = *context;

    /*Stop the total runtime timer and print out the timing results.*/
    mpp_c_context_set_cur_pelist(&tmp_context,NULL,0,MPP_C_FALSE);
    mpp_c_context_stop_timer(&tmp_context,
                             mpp_c_context_get_runtime_timer_id(tmp_context));
    mpp_c_output_context_timing_data(&tmp_context);

    /*Store the world pelist communicator id.*/
    tmp_comm_id = mpp_c_context_get_world_pelist_comm_id(tmp_context);

    /*Reset the mpp_c_context_t object.*/
    mpp_c_context_reset(&tmp_context);
    *context = tmp_context;

    /*Reset and free the mpp_c_shared_t object.*/
    mpp_c_shared_reset(&tmp_shared);
    safefree((void **)&tmp_shared);
    *shared = tmp_shared;

    /*If necessary, finalize MPI.*/
    if (tmp_comm_id == MPI_COMM_WORLD)
    {
        ierr = MPI_Finalize();
        check_MPI_error_code(ierr,"MPI_Finalize","MPP_C_EXIT");
    }

    /*Nullify local pointers.*/
    tmp_shared = NULL;
    tmp_context = NULL;

    return;
}

/*---------------------------------------------------------------------------*/

