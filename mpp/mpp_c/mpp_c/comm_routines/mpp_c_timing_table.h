
#ifndef SET_MPP_C_TIMING_TABLE_H_
#define SET_MPP_C_TIMING_TABLE_H_

#include <stdio.h>
#include "mpp_c_context_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations.*/

/*---------------------------------------------------------------------------*/
/**
    Function that outputs timing table for a context.

    \param [in,out] context  Context handle.
*/
void mpp_c_output_context_timing_data(mpp_c_context_t * const * const context);

/*---------------------------------------------------------------------------*/
/**
    Calculate and print timing statistics across all ranks for a context. All
    ranks in a pelist must have called their timers in the same order to get
    correct timing results.

    \param [in,out] context      Context handle.
    \param [in]     logfile_ptr  Logfile FILE pointer.
    \param [in]     etcfile_ptr  Etcfile FILE pointer.
*/
void mpp_c_print_main_timing_table(mpp_c_context_t * const * const context,
                                   FILE *logfile_ptr,
                                   FILE *etcfile_ptr);

/*---------------------------------------------------------------------------*/
/**
    Function that calculate statistics and prints out event summaries for each
    detailed timer in a context.

    \param [in,out] context                 Context handle.
    \param [in]     event_summary_file_ptr  Event summary file FILE pointer.
*/
void mpp_c_print_event_timing_data(mpp_c_context_t * const * const context,
                                   FILE *event_summary_file_ptr);

/*---------------------------------------------------------------------------*/

#endif
