#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "helper_functions.h"
#include "mpp_c_context_t_api.h"
#include "mpp_c_error.h"
#include "mpp_c_macros.h"
#include "mpp_c_max.h"
#include "mpp_c_min.h"
#include "mpp_c_pelist_t_api.h"
#include "mpp_c_sum.h"
#include "mpp_c_sync.h"
#include "mpp_c_timer_t_api.h"
#include "mpp_c_timing_table.h"
#include "mpp_c_typedefs.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Functions.*/

/*---------------------------------------------------------------------------*/
/*Function that outputs timing data for a context.*/
void mpp_c_output_context_timing_data(mpp_c_context_t * const * const context)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL;      /*Pointer to the mpp_c_context_t object.*/
    char *tmp_char_ptr = NULL;            /*Reusable character pointer.*/
    FILE *tmp_logfile_ptr = NULL;         /*Logfile file pointer.*/
    FILE *tmp_etcfile_ptr = NULL;         /*Etcfile file pointer.*/
    FILE *tmp_file_ptr = NULL;            /*Reusable file pointer.*/
    mpp_c_timer_t *tmp_timer_list = NULL; /*Pointer to a timer list.*/
    mpp_c_timer_t *tmp_timer_ptr = NULL;  /*Pointer to a timer.*/
    char *error_mesg = NULL;              /*Error message pointer.*/
    size_t num_timers = 0;                /*Current number of existing timers.*/
    mpp_c_flag_t detail_flag = 0;         /*Flag telling if any timers are detailed.*/
    int ioerr = 0;                        /*I/O error code.*/
    unsigned int i = 0;                   /*Loop variable.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *context;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,
                                   "MPP_C_OUTPUT_CONTEXT_TIMING_DATA");

    /*Point to the list of timers*/
    tmp_timer_list = mpp_c_context_get_timer_list(tmp_ptr);

    /*Get the total number of timers that exist on this rank.*/
    num_timers = mpp_c_context_get_cur_num_timers(tmp_ptr);

    /*Determine if any timers are detailed.*/
    detail_flag = MPP_C_FALSE;
    for (i=1;i<num_timers+1;i++)
    {
        tmp_timer_ptr = &(tmp_timer_list[i]);
        if (mpp_c_timer_is_detailed(tmp_timer_ptr))
        {
            detail_flag = MPP_C_TRUE;
            break;
        }
    }

    /*Open the necessary log and etcfiles.*/
    tmp_char_ptr = mpp_c_context_get_logfile_name(tmp_ptr);
    tmp_logfile_ptr = fopen(tmp_char_ptr,"a");
    tmp_char_ptr = NULL;
    if (mpp_c_context_get_etcfile_ptr(tmp_ptr) != stderr)
    {
        tmp_char_ptr = mpp_c_context_get_etcfile_name(tmp_ptr);
        tmp_etcfile_ptr = fopen(tmp_char_ptr,"a");
        tmp_char_ptr = NULL;
    }
    else
    {
        tmp_etcfile_ptr = stderr;
    }

    /*Stop recording timing data.  This is needed because at this stage all
      timers should have been stopped.*/
    mpp_c_context_set_record_timing_data(&tmp_ptr,MPP_C_FALSE);

    /*Print out timing table title and headings.*/
    if (mpp_c_context_is_world_pelist_root_rank(tmp_ptr))
    {
        fprintf(stdout,"Tabulating mpp_c_timer statistics across");
        fprintf(stdout," %lu ranks ...\n",
                mpp_c_context_get_world_pelist_rank_list_size(tmp_ptr));
        if (detail_flag)
        {
            fprintf(stdout,"   ... see mpp_c_timer.out.###### for");
            fprintf(stdout," details on individual ranks.\n\n");
        }
    }
    mpp_c_sync(NULL,0,&tmp_ptr);

    /*Output the main timing table.*/
    mpp_c_print_main_timing_table(&tmp_ptr,tmp_logfile_ptr,tmp_etcfile_ptr);
    mpp_c_sync(NULL,0,&tmp_ptr);

    /*If necessary, create a file to store event timings for each detailed
      timer, and output the event summaries.*/
    if (detail_flag)
    {
        /*Open a file where the event summaries will be printed.*/
        safemalloc((void **) &tmp_char_ptr,
                   sizeof(char)*MPP_C_EVENT_SUMMARY_FILE_NAME_LENGTH);
        snprintf(tmp_char_ptr,MPP_C_EVENT_SUMMARY_FILE_NAME_LENGTH,
                 "mpp_c_timer.out.%06u",mpp_c_context_get_world_rank(tmp_ptr));
        tmp_file_ptr = fopen(tmp_char_ptr,"w+");
        mpp_c_sync(NULL,0,&tmp_ptr);

        /*Output the event summary data.*/
        mpp_c_print_event_timing_data(&tmp_ptr,tmp_file_ptr);
        mpp_c_sync(NULL,0,&tmp_ptr);

        /*Close the event summary file.*/
        ioerr = fclose(tmp_file_ptr);
        if (ioerr != 0)
        {
            error_mesg = "MPP_C_OUTPUT_CONTEXT_TIMING_DATA: the event summary"
                         " file did not close properly";
            mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
        }
        tmp_file_ptr = NULL;
        safefree((void **) &tmp_char_ptr);
    }

    /*Close the necessary log and etcfiles.*/
    ioerr = fclose(tmp_logfile_ptr);
    if (ioerr != 0)
    {
        error_mesg = "MPP_C_PRINT_TIMING_TABLE: the logfile did not close"
                     " properly.";
        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
    }
    tmp_logfile_ptr = NULL;
    if (tmp_etcfile_ptr != stderr)
    {
        ioerr = fclose(tmp_etcfile_ptr);
        if (ioerr != 0)
        {
            error_mesg = "MPP_C_PRINT_TIMING_TABLE: the etcfile did not close"
                         " properly.";
            mpp_c_error(FATAL,error_mesg,strlen(error_mesg),tmp_ptr);
        }
        tmp_etcfile_ptr = NULL;
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_char_ptr = NULL;
    tmp_logfile_ptr = NULL;
    tmp_etcfile_ptr = NULL;
    tmp_timer_list = NULL;
    tmp_timer_ptr = NULL;
    tmp_file_ptr = NULL;
    error_mesg = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Calculate and print timing statistics across all ranks for a context. All
  ranks in a pelist must have called their timers in the same order to get
  correct timing results.*/
void mpp_c_print_main_timing_table(mpp_c_context_t * const * const context,
                                   FILE *logfile_ptr,
                                   FILE *etcfile_ptr)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL;      /*Pointer to the mpp_c_context_t object.*/
    mpp_c_timer_t *tmp_timer_list = NULL; /*Pointer to a timer list.*/
    size_t num_timers = 0;                /*Current number of existing timers.*/
    mpp_c_timer_t *tmp_timer_ptr = NULL;  /*Pointer to a timer.*/
    mpp_c_pelist_t *tmp_pelist = NULL;    /*Pointer to a pelist.*/
    uint32_t *tmp_rank_list = NULL;       /*Pointer to a rank list.*/
    size_t tmp_rank_list_size = 0;        /*Temporary rank list size.*/
    char *tmp_char_ptr = NULL;            /*Reusable character pointer.*/
    int32_t tmp_id = 0;                   /*Temporary timer ID variable.*/
    double total_runtime = 0.0;           /*Total runtime.*/
    uint32_t my_rank = 0;                 /*MPI world pelist rank id.*/
    double tmp_timing = 0.0;              /*Temporary timing variable.*/
    double tmp_min = 0.0;                 /*Minimum timing for a timer.*/
    double tmp_max = 0.0;                 /*Maximum timing for a timer.*/
    double tmp_avg = 0.0;                 /*Average timing for a timer.*/
    double tmp_std = 0.0;                 /*Standard deviation of timing for a timer.*/
    void *tmp_data_ptr = NULL;            /*Pointer to data.*/
    char *space32 = NULL;                 /*Spacing for the table.*/
    char *space10 = NULL;                 /*Spacing for the table.*/
    char *space4 = NULL;                  /*Spacing for the table.*/
    char *space_ptr = NULL;               /*Pointer used for table spacing.*/
    unsigned int i = 0;                   /*Loop variable.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *context;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,
                                   "MPP_C_PRINT_MAIN_TIMING_TABLE");

    /*Point to the list of timers*/
    tmp_timer_list = mpp_c_context_get_timer_list(tmp_ptr);

    /*Get the total number of timers that exist on this rank.*/
    num_timers = mpp_c_context_get_cur_num_timers(tmp_ptr);

    /*Write out table headings.*/
    space32 = "                                ";
    space10 = "          ";
    space4 = "    ";
    if (mpp_c_context_is_world_pelist_root_rank(tmp_ptr))
    {
        fprintf(stdout,"%s%stmin%stmax%stavg%ststd%stfrac%sgrain pemin"
                "  pemax\n",space32,space10,space10,space10,space10,
                space10,space4);
        fprintf(logfile_ptr,"%s     time\n",space32);
    }
    fprintf(etcfile_ptr,"%s     time\n",space32);

    /*Calculate statistics and write out the table.*/
    my_rank = mpp_c_context_get_world_rank(tmp_ptr);
    tmp_id = mpp_c_context_get_runtime_timer_id(tmp_ptr);
    total_runtime = mpp_c_timer_get_total_time(&(tmp_timer_list[tmp_id]));

    /*Prevent any possible divide by zero.*/
    if (total_runtime == 0.0)
    {
        total_runtime = 1.0;
    }

    for (i=1;i<num_timers+1;i++)
    {
        /*Set necessary pointers.*/
        tmp_timer_ptr = &(tmp_timer_list[i]);
        tmp_pelist = mpp_c_timer_get_pelist(tmp_timer_ptr);
        tmp_rank_list = mpp_c_pelist_get_rank_list(tmp_pelist);
        tmp_rank_list_size = mpp_c_pelist_get_rank_list_size(tmp_pelist);

        /*Make sure that timer's name match across each pelist.*/
        tmp_char_ptr = mpp_c_timer_get_name(tmp_timer_ptr);

        /*Calculate timing statistics.*/
        tmp_timing = mpp_c_timer_get_total_time(tmp_timer_ptr);
        tmp_min = tmp_timing;
        tmp_data_ptr = (void *)&tmp_min;
        mpp_c_min(&tmp_data_ptr,1,MPP_REAL64,(int32_t *)tmp_rank_list,
                  tmp_rank_list_size,&tmp_ptr);
        tmp_max = tmp_timing;
        tmp_data_ptr = (void *)&tmp_max;
        mpp_c_max(&tmp_data_ptr,1,MPP_REAL64,(int32_t *)tmp_rank_list,
                  tmp_rank_list_size,&tmp_ptr);
        tmp_avg = tmp_timing;
        tmp_data_ptr = (void *)&tmp_avg;
        mpp_c_sum(&tmp_data_ptr,1,MPP_REAL64,(int32_t *)tmp_rank_list,
                  tmp_rank_list_size,&tmp_ptr);
        tmp_avg = tmp_avg/((double)tmp_rank_list_size);
        tmp_std = (tmp_timing-tmp_avg)*(tmp_timing-tmp_avg);
        tmp_data_ptr = (void *)&tmp_std;
        mpp_c_sum(&tmp_data_ptr,1,MPP_REAL64,(int32_t *)tmp_rank_list,
                  tmp_rank_list_size,&tmp_ptr);
        tmp_std = sqrt(tmp_std/((double)tmp_rank_list_size));

        /*Print out the timing table.*/
        space_ptr = &space32[strlen(mpp_c_timer_get_name(tmp_timer_ptr))];
        if (mpp_c_pelist_is_root_rank(tmp_pelist,my_rank))
        {
            fprintf(stdout,"%s%s%s%013.6f %013.6f %013.6f %013.6f %07.6f"
                    " %02d%s%06d %06d\n",
                    mpp_c_timer_get_name(tmp_timer_ptr),
                    space_ptr,space10,
                    tmp_min,tmp_max,tmp_avg,tmp_std,
                    tmp_avg/total_runtime,
                    mpp_c_timer_get_grain(tmp_timer_ptr),space4,
                    tmp_rank_list[0],tmp_rank_list[tmp_rank_list_size-1]);
            fprintf(logfile_ptr,"%s%s     %013.6f\n",
                    mpp_c_timer_get_name(tmp_timer_ptr),space_ptr,tmp_timing);
        }
        else
        {
            fprintf(etcfile_ptr,"%s%s     %013.6f\n",
                    mpp_c_timer_get_name(tmp_timer_ptr),space_ptr,tmp_timing);
        }
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_char_ptr = NULL;
    tmp_timer_list = NULL;
    tmp_timer_ptr = NULL;
    tmp_pelist = NULL;
    tmp_rank_list = NULL;
    tmp_data_ptr = NULL;
    space32 = NULL;
    space10 = NULL;
    space4 = NULL;
    space_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/
/*Function that calculate statistics and prints out event summaries for each
  detailed timer in a context.*/
void mpp_c_print_event_timing_data(mpp_c_context_t * const * const context,
                                   FILE *event_summary_file_ptr)
{
    /*Local variables*/
    mpp_c_context_t *tmp_ptr = NULL;                                /*Pointer to the mpp_c_context_t object.*/
    mpp_c_timer_t *tmp_timer_list = NULL;                           /*Pointer to a timer list.*/
    mpp_c_timer_t *tmp_timer_ptr = NULL;                            /*Pointer to a timer.*/
    mpp_c_pelist_t *tmp_pelist = NULL;                              /*Pointer to a pelist.*/
    uint32_t *tmp_rank_list = NULL;                                 /*Pointer to a rank list.*/
    mpp_c_event_t *tmp_event_list = NULL;                           /*Pointer to an event list.*/
    mpp_c_event_t *tmp_event_ptr = NULL;                            /*Pointer to an event.*/
    char *error_mesg = NULL;                                        /*Error message pointer.*/
    void *tmp_data_ptr = NULL;                                      /*Pointer to data.*/
    size_t num_timers = 0;                                          /*Current number of existing timers.*/
    size_t tmp_rank_list_size = 0;                                  /*Temporary rank list size.*/
    size_t num_events = 0;                                          /*Current number of existing events for a timer.*/
    size_t num_bytes = 0;                                           /*Temporary byte size.*/
    char *event_names[MPP_C_NUM_EVENT_TYPES];                       /*Event names array.*/
    size_t event_counts[MPP_C_NUM_EVENT_TYPES];                     /*Event counter array.*/
    size_t event_bytes[MPP_C_NUM_EVENT_TYPES];                      /*Event total bytes array.*/
    double event_timing[MPP_C_NUM_EVENT_TYPES];                     /*Event total timings array.*/
    size_t event_bin_counts[MPP_C_NUM_EVENT_TYPES][MPP_C_MAX_BINS]; /*Event counts per bin array.*/
    size_t event_bin_bytes[MPP_C_NUM_EVENT_TYPES][MPP_C_MAX_BINS];  /*Event data sums per bin array.*/
    double event_bin_timing[MPP_C_NUM_EVENT_TYPES][MPP_C_MAX_BINS]; /*Event timings per bin array.*/
    uint32_t event_type_index = 0;                                  /*Event type index.*/
    uint32_t event_bin_index = 0;                                   /*Event bin index.*/
    size_t bin_upper_limit = 0;                                     /*Temporary bin upper limit.*/
    double tmp_timing = 0.0;                                        /*Temporary timing variable.*/
    double time_sum = 0.0;                                          /*Temporary sum for timings.*/
    char *byte_unit = NULL;                                         /*Units for byte outputs.*/
    double avg_size = 0.0;                                          /*Average size variable.*/
    double eff_bw = 0.0;                                            /*Effective bandwidth variable.*/
    char byte_label[16];                                            /*Label for event summary.*/
    double tmp_min = 0.0;                                           /*Minimum timing for a timer.*/
    double tmp_max = 0.0;                                           /*Maximum timing for a timer.*/
    double tmp_avg = 0.0;                                           /*Average timing for a timer.*/
    double tmp_std = 0.0;                                           /*Standard deviation of timing for a timer.*/
    size_t btmp_min = 0;                                            /*Minimum data communicated for a timer.*/
    size_t btmp_max = 0;                                            /*Maximum data communicated for a timer.*/
    double btmp_avg = 0.0;                                          /*Average data communicated for a timer.*/
    double btmp_std = 0.0;                                          /*Standard deviation of data communicated for a timer.*/
    double bw = 0.0;                                                /*Bandwidth variable.*/
    char *space32 = NULL;                                           /*Spacing for the table.*/
    char *space10 = NULL;                                           /*Spacing for the table.*/
    char *space4 = NULL;                                            /*Spacing for the table.*/
    char *space_ptr = NULL;                                         /*Pointer used for table spacing.*/
    unsigned int i = 0;                                             /*Loop variable.*/
    unsigned int j = 0;                                             /*Loop variable.*/
    unsigned int k = 0;                                             /*Loop variable.*/
    unsigned int m = 0;                                             /*Loop variable.*/

    /*Set the local pointer to the mpp_c_context_t object.*/
    tmp_ptr = *context;

    /*Make sure that the mpp_c_context_t object has been initialized.*/
    mpp_c_context_check_init_state(tmp_ptr,
                                   "MPP_C_PRINT_EVENT_TIMING_DATA");

    /*Point to the list of timers*/
    tmp_timer_list = mpp_c_context_get_timer_list(tmp_ptr);

    /*Get the total number of timers that exist on this rank.*/
    num_timers = mpp_c_context_get_cur_num_timers(tmp_ptr);

    /*Print out table headings for detailed timing data.*/
    space32 = "                                ";
    space10 = "          ";
    space4 = "    ";
    if (mpp_c_context_is_world_pelist_root_rank(tmp_ptr))
    {
        fprintf(stdout,"\n");
        fprintf(stdout,"%s%stype     count  tmin%stmax%stavg%ststd%smmin%s"
                       "mmax%smavg%smstd%smavg/tavg\n",
                space32,space10,space10,space10,space10,space10,
                space10,space10,space10,space10);
    }

    /*Set temporary arrays to store event timing summary data for the
      local rank.*/
    event_names[0] = "Send        ";
    event_names[1] = "Recv        ";
    event_names[2] = "Bcast       ";
    event_names[3] = "Reduce      ";
    event_names[4] = "Wait        ";
    event_names[5] = "Alltoall    ";
    event_names[6] = "Start_timer ";
    event_names[7] = "Stop_timer  ";

    /*Calculate and print out event summaries for each detailed timer
      on the local rank.*/
    for (i=1;i<num_timers+1;i++)
    {
        /*Point the the appropriate timer.*/
        tmp_timer_ptr = &(tmp_timer_list[i]);

        /*Initialize arrays used to accumulate event data for each timer.*/
        for (j=0;j<MPP_C_NUM_EVENT_TYPES;j++)
        {
            event_counts[j] = 0;
            event_bytes[j] = 0;
            event_timing[j] = 0;
            for (k=0;k<MPP_C_MAX_BINS;k++)
            {
                event_bin_counts[j][k] = 0;
                event_bin_bytes[j][k] = 0;
                event_bin_timing[j][k] = 0;
            }
        }

        /*Only look at "detailed" (i.e. event logging) timers.*/
        if (mpp_c_timer_is_detailed(tmp_timer_ptr))
        {
            /*Print a heading for the  timer.*/
            fprintf(event_summary_file_ptr,"----------------------------"
                    "----------------------------------------------\n");
            fprintf(event_summary_file_ptr,"%s: Communication Data"
                    " for rank %06u\n\n",
                    mpp_c_timer_get_name(tmp_timer_ptr),
                    mpp_c_context_get_world_rank(tmp_ptr));

            /*Set necessary pointers.*/
            tmp_pelist = mpp_c_timer_get_pelist(tmp_timer_ptr);
            tmp_rank_list = mpp_c_pelist_get_rank_list(tmp_pelist);
            tmp_rank_list_size = mpp_c_pelist_get_rank_list_size(tmp_pelist);
            tmp_event_list = mpp_c_timer_get_event_list(tmp_timer_ptr);

            /*Loop over the events.*/
            num_events = mpp_c_timer_get_cur_num_events(tmp_timer_ptr);

            for (j=0;j<num_events;j++)
            {
                /*Point to the appropriate event.*/
                tmp_event_ptr = &(tmp_event_list[j]);

                /*Get the event type.*/
                switch (mpp_c_event_get_event_type(tmp_event_ptr))
                {
                    case EVENT_SEND:
                        event_type_index = 0;
                        break;
                    case EVENT_RECV:
                        event_type_index = 1;
                        break;
                    case EVENT_BCAST:
                        event_type_index = 2;
                        break;
                    case EVENT_REDUCE:
                        event_type_index = 3;
                        break;
                    case EVENT_WAIT:
                        event_type_index = 4;
                        break;
                    case EVENT_ALLTOALL:
                        event_type_index = 5;
                        break;
                    case EVENT_START_TIMER:
                        event_type_index = 6;
                        break;
                    case EVENT_STOP_TIMER:
                        event_type_index = 7;
                        break;
                    default:
                        error_mesg = "MPP_C_PRINT_EVENT_TIMING_DATA:"
                                     " unsupported event type.";
                        mpp_c_error(FATAL,error_mesg,strlen(error_mesg),
                                    tmp_ptr);
                }

                /*Get the number of bytes communicated for the event.*/
                num_bytes = mpp_c_event_get_num_bytes(tmp_event_ptr);

                /*Print the event and its details to the event summary file.*/
                fprintf(event_summary_file_ptr,"%s %s %013.6f %013.6f"
                        " %09lu %06d\n",
                        mpp_c_event_get_name(tmp_event_ptr),
                        event_names[event_type_index],
                        mpp_c_event_get_start_time(tmp_event_ptr),
                        mpp_c_event_get_end_time(tmp_event_ptr),
                        num_bytes,
                        mpp_c_event_get_timer_id(tmp_event_ptr));

                /*Find the correct bin for the event.*/
                event_bin_index = 0;
                bin_upper_limit = 8;
                while (num_bytes > bin_upper_limit &&
                       event_bin_index+1 < MPP_C_MAX_BINS)
                {
                    event_bin_index++;
                    bin_upper_limit = bin_upper_limit*2;
                }

                /*Accumulate and store the data for the event types.*/
                tmp_timing = mpp_c_event_get_end_time(tmp_event_ptr) -
                             mpp_c_event_get_start_time(tmp_event_ptr);
                (event_counts[event_type_index])++;
                (event_bin_counts[event_type_index][event_bin_index])++;
                event_bytes[event_type_index] += num_bytes;
                event_bin_bytes[event_type_index][event_bin_index] +=
                                                                    num_bytes;
                event_timing[event_type_index] += tmp_timing;
                event_bin_timing[event_type_index][event_bin_index] +=
                                                                    tmp_timing;
            }

            /*Write out the totals for each event type.*/
            time_sum = 0.0;
            for (j=0;j<MPP_C_NUM_EVENT_TYPES;j++)
            {
                if (event_counts[j] != 0)
                {
                    time_sum += event_timing[j];
                    fprintf(event_summary_file_ptr,"\n%s  Events:\n",
                            event_names[j]);
                    fprintf(event_summary_file_ptr,"Total Data:  "
                            "%013.6f MB\n",
                            ((double)event_bytes[j])/(1024*1024));
                    fprintf(event_summary_file_ptr,"Total Time:  "
                            "%013.6f secs\n",event_timing[j]);
                    fprintf(event_summary_file_ptr,"Total Calls: "
                            "%08lu\n\n",event_counts[j]);

                    /*Write out totals for each bin.*/
                    fprintf(event_summary_file_ptr,"%sBin%s"
                            "Counts%sAvg_Size%sEff_B/W (MB/S)\n\n",
                            space4,space10,space10,space10);
                    for (k=0;k<MPP_C_MAX_BINS;k++)
                    {
                        if (event_bin_counts[j][k] != 0)
                        {
                            byte_unit = " B";
                            bin_upper_limit = 8;
                            avg_size = ((double)event_bin_bytes[j][k])/
                                       ((double)event_bin_counts[j][k]);
                            eff_bw = ((double)event_bin_bytes[j][k])/
                                     (((double)1024*1024)*
                                     event_bin_timing[j][k]);
                            for (m=0;m<k;m++)
                            {
                                bin_upper_limit = bin_upper_limit*2;
                            }
                            if (k > 7 && k < 18)
                            {
                                bin_upper_limit = bin_upper_limit/1024;
                                avg_size = avg_size/1024;
                                byte_unit = "KB";
                            }
                            else if (k >= 18)
                            {
                                bin_upper_limit = bin_upper_limit/
                                                  (1024*1024);
                                avg_size = avg_size/(1024*1024);
                                byte_unit = "MB";
                            }
                            if (k == 0)
                            {
                                snprintf(byte_label,16,"0000 - %04lu"
                                         " %s",bin_upper_limit,byte_unit);
                            }
                            else if (k == MPP_C_MAX_BINS-1)
                            {
                                snprintf(byte_label,16,">%04lu %s      ",
                                         bin_upper_limit/2,byte_unit);
                            }
                            else
                            {
                                snprintf(byte_label,16,"%04lu - %04lu"
                                         " %s",bin_upper_limit/2,
                                         bin_upper_limit,byte_unit);
                            }
                            byte_label[15] = '\0';
                            fprintf(event_summary_file_ptr,
                                    "%s   %08lu        "
                                    "%013.6f     %013.6f\n\n",
                                    byte_label,
                                    event_bin_counts[j][k],
                                    avg_size,eff_bw);
                        }
                    }
                }

                /*Calculate statistics across ranks.*/
                tmp_data_ptr = (void *) &(event_counts[j]);
                mpp_c_max(&tmp_data_ptr,1,MPP_UINT64,
                         (int32_t *)tmp_rank_list,tmp_rank_list_size,&tmp_ptr);

                if (event_counts[j] > 0)
                {
                    tmp_min = event_timing[j];
                    tmp_data_ptr = (void *)&tmp_min;
                    mpp_c_min(&tmp_data_ptr,1,MPP_REAL64,
                              (int32_t *)tmp_rank_list,tmp_rank_list_size,
                              &tmp_ptr);
                    tmp_max = event_timing[j];
                    tmp_data_ptr = (void *)&tmp_max;
                    mpp_c_max(&tmp_data_ptr,1,MPP_REAL64,
                              (int32_t *)tmp_rank_list,tmp_rank_list_size,
                              &tmp_ptr);
                    tmp_avg = event_timing[j];
                    tmp_data_ptr = (void *)&tmp_avg;
                    mpp_c_sum(&tmp_data_ptr,1,MPP_REAL64,
                              (int32_t *)tmp_rank_list,tmp_rank_list_size,
                              &tmp_ptr);
                    tmp_avg = tmp_avg/((double)tmp_rank_list_size);
                    tmp_std = (event_timing[j]-tmp_avg)*
                              (event_timing[j]-tmp_avg);
                    tmp_data_ptr = (void *)&tmp_std;
                    mpp_c_sum(&tmp_data_ptr,1,MPP_REAL64,
                              (int32_t *)tmp_rank_list,tmp_rank_list_size,
                              &tmp_ptr);
                    tmp_std = sqrt(tmp_std/((double)tmp_rank_list_size));
                    btmp_min = event_bytes[j];
                    tmp_data_ptr = (void *)&btmp_min;
                    mpp_c_min(&tmp_data_ptr,1,MPP_UINT64,
                              (int32_t *)tmp_rank_list,tmp_rank_list_size,
                              &tmp_ptr);
                    btmp_max = event_bytes[j];
                    tmp_data_ptr = (void *)&btmp_max;
                    mpp_c_max(&tmp_data_ptr,1,MPP_UINT64,
                              (int32_t *)tmp_rank_list,tmp_rank_list_size,
                              &tmp_ptr);
                    btmp_avg = (double)event_bytes[j];
                    tmp_data_ptr = (void *)&btmp_avg;
                    mpp_c_sum(&tmp_data_ptr,1,MPP_REAL64,
                              (int32_t *)tmp_rank_list,tmp_rank_list_size,
                              &tmp_ptr);
                    btmp_avg = btmp_avg/((double)tmp_rank_list_size);
                    btmp_std = ((double)event_bytes[j]-btmp_avg)*
                               ((double)event_bytes[j]-btmp_avg);
                    tmp_data_ptr = (void *)&btmp_std;
                    mpp_c_sum(&tmp_data_ptr,1,MPP_REAL64,
                              (int32_t *)tmp_rank_list,tmp_rank_list_size,
                               &tmp_ptr);
                    btmp_std = sqrt(btmp_std/((double)tmp_rank_list_size));
                    space_ptr = &space32[strlen(mpp_c_timer_get_name(
                                     tmp_timer_ptr))];

                    if (mpp_c_pelist_is_root_rank(tmp_pelist,
                        mpp_c_context_get_world_rank(tmp_ptr)))
                    {
                        /*Prevent a divide by zero.*/
                        if (tmp_avg != 0.0)
                        {
                            bw = btmp_avg/tmp_avg;
                        }
                        else
                        {
                            bw = 0.0;
                        }

                        /*Print out the results.*/
                        fprintf(stdout,"%s%s%s%s %06lu %013.6f %013.6f %013.6f"
                                " %013.6f %09lu %09lu %013.6f %013.6f"
                                " %013.6f\n",
                                mpp_c_timer_get_name(tmp_timer_ptr),
                                space_ptr,space10,event_names[j],
                                event_counts[j],tmp_min,tmp_max,tmp_avg,
                                tmp_std,btmp_min,btmp_max,btmp_avg,btmp_std,
                                bw);
                    }
                    fprintf(event_summary_file_ptr,"Total communication time"
                            " spent for %s: %013.6f secs\n\n",
                            mpp_c_timer_get_name(tmp_timer_ptr),
                            time_sum);
                }
            }
        }
    }

    /*Nullify local pointers.*/
    tmp_ptr = NULL;
    tmp_timer_list = NULL;
    for (i=0;i<MPP_C_NUM_EVENT_TYPES;i++)
    {
        event_names[i] = NULL;
    }
    tmp_timer_ptr = NULL;
    tmp_pelist = NULL;
    tmp_rank_list = NULL;
    tmp_event_list = NULL;
    tmp_event_ptr = NULL;
    tmp_data_ptr = NULL;
    error_mesg = NULL;
    byte_unit = NULL;
    space32 = NULL;
    space10 = NULL;
    space4 = NULL;
    space_ptr = NULL;

    return;
}

/*---------------------------------------------------------------------------*/

