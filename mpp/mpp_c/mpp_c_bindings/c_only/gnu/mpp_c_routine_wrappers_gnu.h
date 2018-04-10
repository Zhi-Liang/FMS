#ifdef _C_ONLY_GNU

/*Header file for mpp_c_util_gnu methods.*/
#ifndef SET_MPP_C_ROUTINE_WRAPPERS_GNU_H_
#define SET_MPP_C_ROUTINE_WRAPPERS_GNU_H_

#include <stdint.h>
#include <stdlib.h>
#include "mpp_c_context_t_api.h"
#include "mpp_c_shared_t_api.h"

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*Function declarations*/

void mpp_init(mpp_c_shared_t **,
              mpp_c_context_t **,
              int const);

void mpp_exit(mpp_c_shared_t **,
              mpp_c_context_t **);

int32_t mpp_pe(mpp_c_context_t const * const);

size_t mpp_npes(mpp_c_context_t const * const);

int32_t mpp_root_pe(mpp_c_context_t const * const);

void mpp_set_root_pe(mpp_c_context_t * const * const,
                     int32_t const);

void mpp_declare_pelist(mpp_c_context_t * const * const,
                        char const *,
                        size_t const,
                        int32_t const *,
                        size_t const);

void mpp_set_current_pelist(mpp_c_context_t * const * const,
                            int32_t const *,
                            size_t const,
                            int8_t const);

size_t mpp_get_current_pelist_name_len(mpp_c_context_t const * const);

char *mpp_get_current_pelist_name(mpp_c_context_t const * const);

int mpp_get_current_pelist_comm_id(mpp_c_context_t const * const);

int mpp_get_current_pelist_group_id(mpp_c_context_t const * const context);

int32_t *mpp_get_current_pelist(mpp_c_context_t const * const);

void mpp_clock_set_grain(mpp_c_context_t * const * const,
                         int32_t const);

int32_t mpp_clock_id(mpp_c_context_t * const * const,
                     char const *,
                     size_t const,
                     int32_t const,
                     int8_t const,
                     int8_t const);

void mpp_clock_begin(mpp_c_context_t * const * const,
                     int32_t const);

void mpp_clock_end(mpp_c_context_t * const * const,
                   int32_t const);

void mpp_record_time_start(mpp_c_context_t * const * const);

void mpp_record_time_end(mpp_c_context_t * const * const);

void read_input_nml(mpp_c_context_t **);

size_t mpp_get_namelist_buffer_len(mpp_c_context_t const * const);

char *mpp_get_namelist_buffer(mpp_c_context_t const * const);

void mpp_error(int32_t const,
               char const *,
               size_t const,
               mpp_c_context_t const * const);

void mpp_sync(int32_t const *,
              size_t const,
              mpp_c_context_t * const * const);

void mpp_sync_self(int32_t const,
                   int *,
                   size_t const,
                   size_t const *,
                   int32_t const *,
                   mpp_c_context_t * const * const);

void mpp_send(void **,
              size_t const,
              int32_t const,
              int32_t const,
              int32_t const,
              int *,
              mpp_c_context_t * const * const);

void mpp_recv(void **,
              size_t const,
              int32_t const,
              int32_t const,
              int32_t const,
              int *,
              int32_t const,
              mpp_c_context_t * const * const);

void mpp_broadcast(void **,
                   size_t const,
                   int32_t,
                   int32_t const,
                   int32_t const *,
                   size_t const,
                   mpp_c_context_t * const * const);

void mpp_sum(void **,
             size_t const,
             int32_t const,
             int32_t const *,
             size_t const,
             mpp_c_context_t * const * const);

void mpp_max(void **,
             size_t const,
             int32_t const,
             int32_t const *,
             size_t const,
             mpp_c_context_t * const * const);

void mpp_min(void **,
             size_t const,
             int32_t const,
             int32_t const *,
             size_t const,
             mpp_c_context_t * const * const);

int64_t mpp_chksum(void *,
                   size_t const,
                   size_t const,
                   void const *,
                   int32_t const *,
                   size_t const,
                   mpp_c_context_t * const * const);

void mpp_gather(void *,
                size_t const,
                void **,
                size_t const,
                int32_t const,
                int32_t *,
                size_t const,
                mpp_c_context_t * const * const);

void mpp_gatherv(void *,
                 size_t const,
                 void **,
                 size_t const,
                 size_t const *,
                 size_t const,
                 int32_t const,
                 int32_t *,
                 size_t const,
                 mpp_c_context_t * const * const);

void mpp_gather_pelist(void **,
                       size_t const,
                       size_t const,
                       size_t const,
                       int32_t const,
                       void **,
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

void mpp_scatter_pelist(void **,
                        size_t const,
                        size_t const,
                        size_t const,
                        int32_t const,
                        void **,
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

void mpp_alltoall(void *,
                  size_t const,
                  void **,
                  size_t const,
                  size_t const,
                  size_t const,
                  int32_t const,
                  int32_t const *,
                  size_t const,
                  mpp_c_context_t * const * const);

void mpp_alltoallv(void *,
                   int32_t *,
                   int32_t *,
                   void **,
                   int32_t *,
                   int32_t *,
                   int32_t const,
                   int32_t const *,
                   size_t const,
                   mpp_c_context_t * const * const);

#endif
#endif
