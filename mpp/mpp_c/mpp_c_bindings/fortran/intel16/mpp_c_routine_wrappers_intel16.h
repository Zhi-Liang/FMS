#ifdef _INTEL16_FBINDINGS

/*Header file for mpp_c_util_gnu methods.*/
#ifndef SET_MPP_C_ROUTINE_WRAPPERS_INTEL16_H_
#define SET_MPP_C_ROUTINE_WRAPPERS_INTEL16_H_

#include <stdint.h>
#include <stdlib.h>
#include "ISO_Fortran_binding.h"
#include "mpp_c_context_t_api.h"
#include "mpp_c_shared_t_api.h"

/*Function declarations*/

void mpp_c_init_wrap(mpp_c_shared_t **,
                     mpp_c_context_t **,
                     int const);

void mpp_c_exit_wrap(mpp_c_shared_t **,
                     mpp_c_context_t **);

int32_t mpp_c_pe(mpp_c_context_t const * const);

size_t mpp_c_npes(mpp_c_context_t const * const);

int32_t mpp_c_root_pe(mpp_c_context_t const * const);

void mpp_c_set_root_pe(mpp_c_context_t * const * const,
                       int32_t const);

void mpp_c_declare_pelist(mpp_c_context_t * const * const,
                          char const *,
                          size_t const,
                          int32_t const *,
                          size_t const);

void mpp_c_set_current_pelist(mpp_c_context_t * const * const,
                              int32_t const *,
                              size_t const,
                              int8_t const);

size_t mpp_c_get_current_pelist_name_len(mpp_c_context_t const * const);

char *mpp_c_get_current_pelist_name(mpp_c_context_t const * const);

int mpp_c_get_current_pelist_comm_id(mpp_c_context_t const * const);

int32_t *mpp_c_get_current_pelist(mpp_c_context_t const * const);

void mpp_c_clock_set_grain(mpp_c_context_t * const * const,
                           int32_t const);

int32_t mpp_c_clock_id(mpp_c_context_t * const * const,
                       char const *,
                       size_t const,
                       int32_t const,
                       int8_t const,
                       int8_t const);

void mpp_c_clock_begin(mpp_c_context_t * const * const,
                       int32_t const);

void mpp_c_clock_end(mpp_c_context_t * const * const,
                     int32_t const);

void mpp_c_record_time_start(mpp_c_context_t * const * const);

void mpp_c_record_time_end(mpp_c_context_t * const * const);

void mpp_c_read_input_nml(mpp_c_context_t **);

size_t mpp_c_get_namelist_buffer_len(mpp_c_context_t const * const);

char *mpp_c_get_namelist_buffer(mpp_c_context_t const * const);

size_t mpp_c_get_input_nml_buffer_size(mpp_c_context_t const * const);

char *mpp_c_get_input_nml_buffer(mpp_c_context_t const * const);

void mpp_c_error_wrap(int32_t const,
                      char const *,
                      size_t const,
                      mpp_c_context_t const * const);

void mpp_c_sync_wrap(int32_t const *,
                     size_t const,
                     mpp_c_context_t * const * const);

void mpp_c_sync_self_wrap(int32_t const,
                          int *,
                          size_t const,
                          size_t const *,
                          int32_t const *,
                          mpp_c_context_t * const * const);

void mpp_c_send_wrap(CFI_cdesc_t *,
                     size_t const,
                     int32_t const,
                     int32_t const,
                     int32_t const,
                     int *,
                     mpp_c_context_t * const * const);

void mpp_c_recv_wrap(CFI_cdesc_t *,
                     size_t const,
                     int32_t const,
                     int32_t const,
                     int32_t const,
                     int *,
                     int32_t const,
                     mpp_c_context_t * const * const);

void mpp_c_broadcast_wrap(CFI_cdesc_t *,
                          size_t const,
                          int32_t,
                          int32_t const,
                          int32_t const *,
                          size_t const,
                          mpp_c_context_t * const * const);

void mpp_c_sum_wrap(CFI_cdesc_t *,
                    size_t const,
                    int32_t const,
                    int32_t const *,
                    size_t const,
                    mpp_c_context_t * const * const);

void mpp_c_max_wrap(CFI_cdesc_t *,
                    size_t const,
                    int32_t const,
                    int32_t const *,
                    size_t const,
                    mpp_c_context_t * const * const);

void mpp_c_min_wrap(CFI_cdesc_t *,
                   size_t const,
                   int32_t const,
                   int32_t const *,
                   size_t const,
                   mpp_c_context_t * const * const);

int64_t mpp_c_chksum_wrap(CFI_cdesc_t *,
                          size_t const,
                          size_t const,
                          int8_t const,
                          CFI_cdesc_t *,
                          int32_t const *,
                          size_t const,
                          mpp_c_context_t * const * const);

void mpp_c_gather_wrap(CFI_cdesc_t *,
                       size_t const,
                       CFI_cdesc_t *,
                       size_t const,
                       int32_t const,
                       int32_t *,
                       size_t const,
                       mpp_c_context_t * const * const);

void mpp_c_gatherv_wrap(CFI_cdesc_t *,
                        size_t const,
                        CFI_cdesc_t *,
                        size_t const,
                        size_t const *,
                        size_t const,
                        int32_t const,
                        int32_t *,
                        size_t const,
                        mpp_c_context_t * const * const);

void mpp_c_gather_pelist_wrap(CFI_cdesc_t *,
                              size_t const,
                              size_t const,
                              size_t const,
                              int32_t const,
                              CFI_cdesc_t *,
                              int32_t const,
                              int32_t const,
                              int32_t const,
                              int32_t const,
                              int32_t const,
                              int32_t const,
                              int32_t const *,
                              size_t const,
                              int32_t const,
                              int32_t const,
                              int32_t const,
                              int32_t const,
                              mpp_c_context_t * const * const);

void mpp_c_scatter_pelist_wrap(CFI_cdesc_t *,
                               size_t const,
                               size_t const,
                               size_t const,
                               int32_t const,
                               CFI_cdesc_t *,
                               int32_t const,
                               int32_t const,
                               int32_t const,
                               int32_t const,
                               int32_t const,
                               int32_t const,
                               int32_t const *,
                               size_t const,
                               int32_t const,
                               int32_t const,
                               int32_t const,
                               int32_t const,
                               mpp_c_context_t * const * const);

void mpp_c_alltoall_wrap(CFI_cdesc_t *,
                         size_t const,
                         CFI_cdesc_t *,
                         size_t const,
                         size_t const,
                         size_t const,
                         int32_t const,
                         int32_t const *,
                         size_t const,
                         mpp_c_context_t * const * const);

void mpp_c_alltoallv_wrap(CFI_cdesc_t *,
                          int32_t *,
                          int32_t *,
                          CFI_cdesc_t *,
                          int32_t *,
                          int32_t *,
                          int32_t const,
                          int32_t const *,
                          size_t const,
                          mpp_c_context_t * const * const);

#endif
#endif
