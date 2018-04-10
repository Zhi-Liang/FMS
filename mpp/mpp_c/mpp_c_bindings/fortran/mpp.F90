module mpp_mod
  use cjson_objects_mod, only : cjsonObject_init
  use cjson_wrapper_mod, only : JSON_ARGS
 USE ISO_C_BINDING
  use iso_c_binding, only: c_ptr,c_null_ptr

  implicit none
  private

 !-----------------------------------------------------------------------------
 !Public parameters
  public :: INT_KIND
  public :: LONG_KIND
  public :: FLOAT_KIND
  public :: DOUBLE_KIND
  public :: FATAL
  public :: WARNING
  public :: NOTE
  public :: EVENT_SEND
  public :: EVENT_RECV
  public :: MPP_INT32
  public :: MPP_UINT32
  public :: MPP_INT64
  public :: MPP_UINT64
  public :: MPP_REAL32
  public :: MPP_REAL64
  public :: MPP_CHAR

 !-----------------------------------------------------------------------------
 !Public module variables (move to fms?)
  public :: input_nml_file
  public :: json
  public :: cjson
  public :: COMM_TAG_1
  public :: COMM_TAG_2
  public :: COMM_TAG_3
  public :: COMM_TAG_4
  public :: COMM_TAG_5
  public :: COMM_TAG_6
  public :: COMM_TAG_7
  public :: COMM_TAG_8
  public :: COMM_TAG_9
  public :: COMM_TAG_10
  public :: COMM_TAG_11
  public :: COMM_TAG_12
  public :: COMM_TAG_13
  public :: COMM_TAG_14
  public :: COMM_TAG_15
  public :: COMM_TAG_16
  public :: COMM_TAG_17
  public :: COMM_TAG_18
  public :: COMM_TAG_19
  public :: COMM_TAG_20
  public :: NULL_PE
  public :: MPP_CLOCK_SYNC
  public :: MPP_CLOCK_DETAILED
  public :: CLOCK_ROUTINE
  public :: ALL_PES
  public :: MPP_FILL_DOUBLE
  public :: MPP_FILL_INT
  public :: CLOCK_COMPONENT
  public :: CLOCK_SUBCOMPONENT
  public :: CLOCK_MODULE_DRIVER
  public :: CLOCK_MODULE
  public :: CLOCK_LOOP
  public :: CLOCK_INFRA
  public :: MPP_DEBUG
  public :: INPUT_STR_LENGTH
  public :: MPP_NULL_CLOCK_ID

 !-----------------------------------------------------------------------------
 !Public interfaces
  public :: stdout
  public :: stderr
  public :: stdlog
  public :: mpp_pe
  public :: mpp_npes
  public :: mpp_root_pe
  public :: mpp_set_root_pe
  public :: mpp_declare_pelist
  public :: mpp_get_current_pelist
  public :: mpp_set_current_pelist
  public :: mpp_get_current_pelist_name
  public :: mpp_clock_set_grain
  public :: mpp_clock_id
  public :: mpp_clock_begin
  public :: mpp_clock_end
  public :: mpp_record_time_start
  public :: mpp_record_time_end
  public :: read_input_nml

  public :: mpp_init
  public :: mpp_exit
  public :: mpp_error
  public :: mpp_sync
  public :: mpp_sync_self
  public :: mpp_send
  public :: mpp_recv
  public :: mpp_broadcast
  public :: mpp_sum
  public :: mpp_max
  public :: mpp_min
  public :: mpp_chksum
  public :: mpp_gather
  public :: mpp_scatter
  public :: mpp_alltoall

 !Routines that can be moved to fms (hopefully)
  public :: lowercase
  public :: uppercase
  public :: get_unit
  public :: read_ascii_file
  public :: get_ascii_file_num_lines

 !Deprecated routines
  public :: stdin
  public :: mpp_error_state
  public :: mpp_set_warn_level
  public :: mpp_set_stack_size
  public :: mpp_node
  public :: mpp_malloc
  public :: mpp_gsm_malloc
  public :: mpp_gsm_free

  interface mpp_error
      module procedure mpp_error_scalar
      module procedure mpp_error_1D
      module procedure mpp_error_scalar_1D
      module procedure mpp_error_1D_scalar
      module procedure mpp_error_mesg
      module procedure mpp_error_no_args
  end interface mpp_error

  interface mpp_send
#ifdef _INTEL16_FBINDINGS
!     module procedure mpp_send_i32s
      module procedure mpp_send_i32
!     module procedure mpp_send_i64s
      module procedure mpp_send_i64
!     module procedure mpp_send_r32s
      module procedure mpp_send_r32
!     module procedure mpp_send_r64s
      module procedure mpp_send_r64
!     module procedure mpp_send_l32s
      module procedure mpp_send_l32
!     module procedure mpp_send_l64s
      module procedure mpp_send_l64
#elif defined(_GNU_FBINDINGS)
      module procedure mpp_send_scalar
      module procedure mpp_send_1D
      module procedure mpp_send_2D
      module procedure mpp_send_3D
      module procedure mpp_send_4D
      module procedure mpp_send_5D
#else
      module procedure mpp_send_i32s
      module procedure mpp_send_i32_1D
      module procedure mpp_send_i32_2D
      module procedure mpp_send_i32_3D
      module procedure mpp_send_i32_4D
      module procedure mpp_send_i32_5D
      module procedure mpp_send_i64s
      module procedure mpp_send_i64_1D
      module procedure mpp_send_i64_2D
      module procedure mpp_send_i64_3D
      module procedure mpp_send_i64_4D
      module procedure mpp_send_i64_5D
      module procedure mpp_send_r32s
      module procedure mpp_send_r32_1D
      module procedure mpp_send_r32_2D
      module procedure mpp_send_r32_3D
      module procedure mpp_send_r32_4D
      module procedure mpp_send_r32_5D
      module procedure mpp_send_r64s
      module procedure mpp_send_r64_1D
      module procedure mpp_send_r64_2D
      module procedure mpp_send_r64_3D
      module procedure mpp_send_r64_4D
      module procedure mpp_send_r64_5D
      module procedure mpp_send_l32s
      module procedure mpp_send_l32_1D
      module procedure mpp_send_l32_2D
      module procedure mpp_send_l32_3D
      module procedure mpp_send_l32_4D
      module procedure mpp_send_l32_5D
      module procedure mpp_send_l64s
      module procedure mpp_send_l64_1D
      module procedure mpp_send_l64_2D
      module procedure mpp_send_l64_3D
      module procedure mpp_send_l64_4D
      module procedure mpp_send_l64_5D
#endif
  end interface mpp_send

  interface mpp_recv
#ifdef _INTEL16_FBINDINGS
!     module procedure mpp_recv_i32s
      module procedure mpp_recv_i32
!     module procedure mpp_recv_i64s
      module procedure mpp_recv_i64
!     module procedure mpp_recv_r32s
      module procedure mpp_recv_r32
!     module procedure mpp_recv_r64s
      module procedure mpp_recv_r64
!     module procedure mpp_recv_l32s
      module procedure mpp_recv_l32
!     module procedure mpp_recv_l64s
      module procedure mpp_recv_l64
#elif defined(_GNU_FBINDINGS)
      module procedure mpp_recv_scalar
      module procedure mpp_recv_1D
      module procedure mpp_recv_2D
      module procedure mpp_recv_3D
      module procedure mpp_recv_4D
      module procedure mpp_recv_5D
#else
      module procedure mpp_recv_i32s
      module procedure mpp_recv_i32_1D
      module procedure mpp_recv_i32_2D
      module procedure mpp_recv_i32_3D
      module procedure mpp_recv_i32_4D
      module procedure mpp_recv_i32_5D
      module procedure mpp_recv_i64s
      module procedure mpp_recv_i64_1D
      module procedure mpp_recv_i64_2D
      module procedure mpp_recv_i64_3D
      module procedure mpp_recv_i64_4D
      module procedure mpp_recv_i64_5D
      module procedure mpp_recv_r32s
      module procedure mpp_recv_r32_1D
      module procedure mpp_recv_r32_2D
      module procedure mpp_recv_r32_3D
      module procedure mpp_recv_r32_4D
      module procedure mpp_recv_r32_5D
      module procedure mpp_recv_r64s
      module procedure mpp_recv_r64_1D
      module procedure mpp_recv_r64_2D
      module procedure mpp_recv_r64_3D
      module procedure mpp_recv_r64_4D
      module procedure mpp_recv_r64_5D
      module procedure mpp_recv_l32s
      module procedure mpp_recv_l32_1D
      module procedure mpp_recv_l32_2D
      module procedure mpp_recv_l32_3D
      module procedure mpp_recv_l32_4D
      module procedure mpp_recv_l32_5D
      module procedure mpp_recv_l64s
      module procedure mpp_recv_l64_1D
      module procedure mpp_recv_l64_2D
      module procedure mpp_recv_l64_3D
      module procedure mpp_recv_l64_4D
      module procedure mpp_recv_l64_5D
#endif
  end interface mpp_recv

  interface mpp_broadcast
#ifdef _INTEL16_FBINDINGS
      module procedure mpp_broadcast_char
      module procedure mpp_broadcast_i32s
      module procedure mpp_broadcast_i32
      module procedure mpp_broadcast_i64s
      module procedure mpp_broadcast_i64
      module procedure mpp_broadcast_r32s
      module procedure mpp_broadcast_r32
      module procedure mpp_broadcast_r64s
      module procedure mpp_broadcast_r64
      module procedure mpp_broadcast_l32s
      module procedure mpp_broadcast_l32
      module procedure mpp_broadcast_l64s
      module procedure mpp_broadcast_l64
#elif defined(_GNU_FBINDINGS)
      module procedure mpp_broadcast_scalar
      module procedure mpp_broadcast_1D
      module procedure mpp_broadcast_2D
      module procedure mpp_broadcast_3D
      module procedure mpp_broadcast_4D
      module procedure mpp_broadcast_5D
#else
      module procedure mpp_broadcast_char
      module procedure mpp_broadcast_i32s
      module procedure mpp_broadcast_i32_0D
      module procedure mpp_broadcast_i32_1D
      module procedure mpp_broadcast_i32_2D
      module procedure mpp_broadcast_i32_3D
      module procedure mpp_broadcast_i32_4D
      module procedure mpp_broadcast_i32_5D
      module procedure mpp_broadcast_i64s
      module procedure mpp_broadcast_i64_0D
      module procedure mpp_broadcast_i64_1D
      module procedure mpp_broadcast_i64_2D
      module procedure mpp_broadcast_i64_3D
      module procedure mpp_broadcast_i64_4D
      module procedure mpp_broadcast_i64_5D
      module procedure mpp_broadcast_r32s
      module procedure mpp_broadcast_r32_0D
      module procedure mpp_broadcast_r32_1D
      module procedure mpp_broadcast_r32_2D
      module procedure mpp_broadcast_r32_3D
      module procedure mpp_broadcast_r32_4D
      module procedure mpp_broadcast_r32_5D
      module procedure mpp_broadcast_r64s
      module procedure mpp_broadcast_r64_0D
      module procedure mpp_broadcast_r64_1D
      module procedure mpp_broadcast_r64_2D
      module procedure mpp_broadcast_r64_3D
      module procedure mpp_broadcast_r64_4D
      module procedure mpp_broadcast_r64_5D
      module procedure mpp_broadcast_l32s
      module procedure mpp_broadcast_l32_0D
      module procedure mpp_broadcast_l32_1D
      module procedure mpp_broadcast_l32_2D
      module procedure mpp_broadcast_l32_3D
      module procedure mpp_broadcast_l32_4D
      module procedure mpp_broadcast_l32_5D
      module procedure mpp_broadcast_l64s
      module procedure mpp_broadcast_l64_0D
      module procedure mpp_broadcast_l64_1D
      module procedure mpp_broadcast_l64_2D
      module procedure mpp_broadcast_l64_3D
      module procedure mpp_broadcast_l64_4D
      module procedure mpp_broadcast_l64_5D
#endif
  end interface mpp_broadcast

  interface mpp_sum
#ifdef _INTEL16_FBINDINGS
      module procedure mpp_sum_i32s
      module procedure mpp_sum_i32
      module procedure mpp_sum_i64s
      module procedure mpp_sum_i64
      module procedure mpp_sum_r32s
      module procedure mpp_sum_r32
      module procedure mpp_sum_r64s
      module procedure mpp_sum_r64
#elif defined(_GNU_FBINDINGS)
      module procedure mpp_sum_scalar
      module procedure mpp_sum_1D
      module procedure mpp_sum_2D
      module procedure mpp_sum_3D
      module procedure mpp_sum_4D
      module procedure mpp_sum_5D
#else
      module procedure mpp_sum_i32s
      module procedure mpp_sum_i32_1D
      module procedure mpp_sum_i32_2D
      module procedure mpp_sum_i32_3D
      module procedure mpp_sum_i32_4D
      module procedure mpp_sum_i32_5D
      module procedure mpp_sum_i64s
      module procedure mpp_sum_i64_1D
      module procedure mpp_sum_i64_2D
      module procedure mpp_sum_i64_3D
      module procedure mpp_sum_i64_4D
      module procedure mpp_sum_i64_5D
      module procedure mpp_sum_r32s
      module procedure mpp_sum_r32_1D
      module procedure mpp_sum_r32_2D
      module procedure mpp_sum_r32_3D
      module procedure mpp_sum_r32_4D
      module procedure mpp_sum_r32_5D
      module procedure mpp_sum_r64s
      module procedure mpp_sum_r64_1D
      module procedure mpp_sum_r64_2D
      module procedure mpp_sum_r64_3D
      module procedure mpp_sum_r64_4D
      module procedure mpp_sum_r64_5D
#endif
  end interface mpp_sum

  interface mpp_max
#ifdef _INTEL16_FBINDINGS
      module procedure mpp_max_i32s
      module procedure mpp_max_i64s
      module procedure mpp_max_r32s
      module procedure mpp_max_r64s
#elif defined(_GNU_FBINDINGS)
      module procedure mpp_max_scalar
#else
      module procedure mpp_max_i32s
      module procedure mpp_max_i64s
      module procedure mpp_max_r32s
      module procedure mpp_max_r64s
#endif
  end interface mpp_max

  interface mpp_min
#ifdef _INTEL16_FBINDINGS
      module procedure mpp_min_i32s
      module procedure mpp_min_i64s
      module procedure mpp_min_r32s
      module procedure mpp_min_r64s
#elif defined(_GNU_FBINDINGS)
      module procedure mpp_min_scalar
#else
      module procedure mpp_min_i32s
      module procedure mpp_min_i64s
      module procedure mpp_min_r32s
      module procedure mpp_min_r64s
#endif
  end interface mpp_min

  interface mpp_chksum
#ifdef _INTEL16_FBINDINGS
      module procedure mpp_chksum_i32s
      module procedure mpp_chksum_i32_1D
      module procedure mpp_chksum_i32_2D
      module procedure mpp_chksum_i32_3D
      module procedure mpp_chksum_i32_4D
      module procedure mpp_chksum_i32_5D
      module procedure mpp_chksum_i64s
      module procedure mpp_chksum_i64_1D
      module procedure mpp_chksum_i64_2D
      module procedure mpp_chksum_i64_3D
      module procedure mpp_chksum_i64_4D
      module procedure mpp_chksum_i64_5D
      module procedure mpp_chksum_r32s
      module procedure mpp_chksum_r32_1D
      module procedure mpp_chksum_r32_2D
      module procedure mpp_chksum_r32_3D
      module procedure mpp_chksum_r32_4D
      module procedure mpp_chksum_r32_5D
      module procedure mpp_chksum_r64s
      module procedure mpp_chksum_r64_1D
      module procedure mpp_chksum_r64_2D
      module procedure mpp_chksum_r64_3D
      module procedure mpp_chksum_r64_4D
      module procedure mpp_chksum_r64_5D
#elif defined(_GNU_FBINDINGS)
      module procedure mpp_chksum_scalar
      module procedure mpp_chksum_1D
      module procedure mpp_chksum_2D
      module procedure mpp_chksum_3D
      module procedure mpp_chksum_4D
      module procedure mpp_chksum_5D
#else
      module procedure mpp_chksum_i32s
      module procedure mpp_chksum_i32_1D
      module procedure mpp_chksum_i32_2D
      module procedure mpp_chksum_i32_3D
      module procedure mpp_chksum_i32_4D
      module procedure mpp_chksum_i32_5D
      module procedure mpp_chksum_i64s
      module procedure mpp_chksum_i64_1D
      module procedure mpp_chksum_i64_2D
      module procedure mpp_chksum_i64_3D
      module procedure mpp_chksum_i64_4D
      module procedure mpp_chksum_i64_5D
      module procedure mpp_chksum_r32s
      module procedure mpp_chksum_r32_1D
      module procedure mpp_chksum_r32_2D
      module procedure mpp_chksum_r32_3D
      module procedure mpp_chksum_r32_4D
      module procedure mpp_chksum_r32_5D
      module procedure mpp_chksum_r64s
      module procedure mpp_chksum_r64_1D
      module procedure mpp_chksum_r64_2D
      module procedure mpp_chksum_r64_3D
      module procedure mpp_chksum_r64_4D
      module procedure mpp_chksum_r64_5D
#endif
  end interface mpp_chksum

  interface mpp_gather
#ifdef _INTEL16_FBINDINGS
      module procedure mpp_gather_i32_1D
      module procedure mpp_gather_r32_1D
      module procedure mpp_gather_r64_1D
      module procedure mpp_gather_l32_1D
      module procedure mpp_gatherv_i32_1D
      module procedure mpp_gatherv_r32_1D
      module procedure mpp_gatherv_r64_1D
      module procedure mpp_gatherv_l32_1D
      module procedure mpp_gather_pelist_i32_2D
      module procedure mpp_gather_pelist_i32_3D
      module procedure mpp_gather_pelist_r32_2D
      module procedure mpp_gather_pelist_r32_3D
      module procedure mpp_gather_pelist_r64_2D
      module procedure mpp_gather_pelist_r64_3D
      module procedure mpp_gather_pelist_l32_2D
      module procedure mpp_gather_pelist_l32_3D
#elif defined(_GNU_FBINDINGS)
      module procedure mpp_gather_1D
      module procedure mpp_gatherv_1D
      module procedure mpp_gather_pelist_2D
      module procedure mpp_gather_pelist_3D
#else
      module procedure mpp_gather_i32_1D
      module procedure mpp_gather_r32_1D
      module procedure mpp_gather_r64_1D
      module procedure mpp_gather_l32_1D
      module procedure mpp_gatherv_i32_1D
      module procedure mpp_gatherv_r32_1D
      module procedure mpp_gatherv_r64_1D
      module procedure mpp_gatherv_l32_1D
      module procedure mpp_gather_pelist_i32_2D
      module procedure mpp_gather_pelist_i32_3D
      module procedure mpp_gather_pelist_r32_2D
      module procedure mpp_gather_pelist_r32_3D
      module procedure mpp_gather_pelist_r64_2D
      module procedure mpp_gather_pelist_r64_3D
      module procedure mpp_gather_pelist_l32_2D
      module procedure mpp_gather_pelist_l32_3D
#endif
  end interface

  interface mpp_scatter
#ifdef _INTEL16_FBINDINGS
      module procedure mpp_scatter_pelist_i32_2D
      module procedure mpp_scatter_pelist_i32_3D
      module procedure mpp_scatter_pelist_r32_2D
      module procedure mpp_scatter_pelist_r32_3D
      module procedure mpp_scatter_pelist_r64_2D
      module procedure mpp_scatter_pelist_r64_3D
#elif defined(_GNU_FBINDINGS)
      module procedure mpp_scatter_pelist_2D
      module procedure mpp_scatter_pelist_3D
#else
      module procedure mpp_scatter_pelist_i32_2D
      module procedure mpp_scatter_pelist_i32_3D
      module procedure mpp_scatter_pelist_r32_2D
      module procedure mpp_scatter_pelist_r32_3D
      module procedure mpp_scatter_pelist_r64_2D
      module procedure mpp_scatter_pelist_r64_3D
#endif
  end interface

  interface mpp_alltoall
#ifdef _INTEL16_FBINDINGS
      module procedure mpp_alltoall_i32_1D
      module procedure mpp_alltoall_i64_1D
      module procedure mpp_alltoall_r32_1D
      module procedure mpp_alltoall_r64_1D
      module procedure mpp_alltoallv_i32_1D
      module procedure mpp_alltoallv_i64_1D
      module procedure mpp_alltoallv_r32_1D
      module procedure mpp_alltoallv_r64_1D
#elif defined(_GNU_FBINDINGS)
      module procedure mpp_alltoall_1D
      module procedure mpp_alltoallv_1D
#else
      module procedure mpp_alltoall_i32_1D
      module procedure mpp_alltoall_i64_1D
      module procedure mpp_alltoall_r32_1D
      module procedure mpp_alltoall_r64_1D
      module procedure mpp_alltoallv_i32_1D
      module procedure mpp_alltoallv_i64_1D
      module procedure mpp_alltoallv_r32_1D
      module procedure mpp_alltoallv_r64_1D
#endif
  end interface

 !-----------------------------------------------------------------------------
 !Private interfaces

  interface array_to_char
      module procedure arg_to_string_scalar
      module procedure arg_to_string_1D
  end interface array_to_char

 !c bindings
#include "mpp_util_c_binding.inc"
#ifdef _INTEL16_FBINDINGS
#include "mpp_error_c_binding_intel16.inc"
#include "mpp_sync_c_binding_intel16.inc"
#include "mpp_send_c_binding_intel16.inc"
#include "mpp_recv_c_binding_intel16.inc"
#include "mpp_broadcast_c_binding_intel16.inc"
#include "mpp_sum_c_binding_intel16.inc"
#include "mpp_max_c_binding_intel16.inc"
#include "mpp_min_c_binding_intel16.inc"
#include "mpp_chksum_c_binding_intel16.inc"
#include "mpp_gather_c_binding_intel16.inc"
#include "mpp_gatherv_c_binding_intel16.inc"
#include "mpp_gather_pelist_c_binding_intel16.inc"
#include "mpp_scatter_pelist_c_binding_intel16.inc"
#include "mpp_alltoall_c_binding_intel16.inc"
#include "mpp_alltoallv_c_binding_intel16.inc"
#elif _GNU_FBINDINGS
#include "mpp_error_c_binding_gnu.inc"
#include "mpp_sync_c_binding_gnu.inc"
#include "mpp_send_c_binding_gnu.inc"
#include "mpp_recv_c_binding_gnu.inc"
#include "mpp_broadcast_c_binding_gnu.inc"
#include "mpp_sum_c_binding_gnu.inc"
#include "mpp_max_c_binding_gnu.inc"
#include "mpp_min_c_binding_gnu.inc"
#include "mpp_chksum_c_binding_gnu.inc"
#include "mpp_gather_c_binding_gnu.inc"
#include "mpp_gatherv_c_binding_gnu.inc"
#include "mpp_gather_pelist_c_binding_gnu.inc"
#include "mpp_scatter_pelist_c_binding_gnu.inc"
#include "mpp_alltoall_c_binding_gnu.inc"
#include "mpp_alltoallv_c_binding_gnu.inc"
#else
#include "mpp_error_c_binding_basic.inc"
#include "mpp_sync_c_binding_basic.inc"
#include "mpp_send_c_binding_basic.inc"
#include "mpp_recv_c_binding_basic.inc"
#include "mpp_broadcast_c_binding_basic.inc"
#include "mpp_sum_c_binding_basic.inc"
#include "mpp_max_c_binding_basic.inc"
#include "mpp_min_c_binding_basic.inc"
#include "mpp_chksum_c_binding_basic.inc"
#include "mpp_gather_c_binding_basic.inc"
#include "mpp_gatherv_c_binding_basic.inc"
#include "mpp_gather_pelist_c_binding_basic.inc"
#include "mpp_scatter_pelist_c_binding_basic.inc"
#include "mpp_alltoall_c_binding_basic.inc"
#include "mpp_alltoallv_c_binding_basic.inc"
#endif

 !-----------------------------------------------------------------------------
 !Public module parameters
  integer(4),parameter        :: INT_KIND = 4
  integer(INT_KIND),parameter :: LONG_KIND = 8
  integer(INT_KIND),parameter :: FLOAT_KIND = 4
  integer(INT_KIND),parameter :: DOUBLE_KIND = 8
  integer(INT_KIND),parameter :: FATAL = 0
  integer(INT_KIND),parameter :: WARNING = 1
  integer(INT_KIND),parameter :: NOTE = 2
  integer(INT_KIND),parameter :: EVENT_SEND = 4
  integer(INT_KIND),parameter :: EVENT_RECV = 3
  integer(INT_KIND),parameter :: MPP_INT32                    = 10000
  integer(INT_KIND),parameter :: MPP_UINT32                   = 10001
  integer(INT_KIND),parameter :: MPP_INT64                    = 10002
  integer(INT_KIND),parameter :: MPP_UINT64                   = 10003
  integer(INT_KIND),parameter :: MPP_REAL32                   = 10004
  integer(INT_KIND),parameter :: MPP_REAL64                   = 10005
  integer(INT_KIND),parameter :: MPP_CHAR                     = 10006

 !-----------------------------------------------------------------------------
 !Public module variables.  This should be changed.
  character(len=256),dimension(:),allocatable,target :: input_nml_file
  character(len=:),allocatable                       :: json
  type (C_PTR)                                       :: cjson=C_NULL_PTR
 !-----------------------------------------------------------------------------
 !Private module variables
  type(c_ptr)                 :: mpp_shared                   = c_null_ptr
  type(c_ptr)                 :: mpp_context                  = c_null_ptr
  integer(INT_KIND),parameter :: MPP_C_NULL_RANK_LIST         = -1
  integer(INT_KIND),parameter :: MPP_C_NULL_REQUEST           = -1
  integer(INT_KIND),parameter :: MPP_C_NULL_MSG_SIZE          = -1
  integer(INT_KIND),parameter :: MPP_C_NULL_MSG_TYPE          = -1


! integer(INT_KIND),parameter :: MPP_C_NULL_PELIST            = -1
! integer(INT_KIND),parameter :: MPP_C_NULL_PELIST_SIZE       = -1
! integer(INT_KIND),parameter :: MPP_C_NULL_REQUEST           = -1
! integer(INT_KIND),parameter :: MPP_C_NULL_MESG_SIZE         = -1
! integer(INT_KIND),parameter :: MPP_C_NULL_MESG_TYPE         = -1
  integer(INT_KIND),parameter :: MPP_C_MAX_PELIST_NAME_LENGTH = 32
  integer(INT_KIND),parameter :: MPP_C_MAX_TIMER_NAME_LEN     = 48


  integer(INT_KIND),parameter :: MPP_C_RUNTIME_TIMER          = 0
  integer(INT_KIND),parameter :: MPP_C_COMPONENT_TIMER        = 1
  integer(INT_KIND),parameter :: MPP_C_SUBCOMPONENT_TIMER     = 2
  integer(INT_KIND),parameter :: MPP_C_MODULE_DRIVER_TIMER    = 3
  integer(INT_KIND),parameter :: MPP_C_MODULE_TIMER           = 4
  integer(INT_KIND),parameter :: MPP_C_ROUTINE_TIMER          = 5
  integer(INT_KIND),parameter :: MPP_C_LOOP_TIMER             = 6
  integer(INT_KIND),parameter :: MPP_C_INFRASTRUCTURE_TIMER   = 7

  integer(INT_KIND),parameter :: MPP_C_TRUE                   = 1
  integer(INT_KIND),parameter :: MPP_C_FALSE                  = 0
! integer(INT_KIND),parameter :: MPP_C_WORLD_ROOT             = 0
! integer(INT_KIND),parameter :: MPP_STDOUT_FILE_UNIT         = 6
! integer(INT_KIND),parameter :: MPP_STDERR_FILE_UNIT         = 5
  integer(INT_KIND),parameter :: DEFAULT_TAG                  = 1

  integer(INT_KIND),parameter :: MPP_C_LOGFILE_UNIT           = 8
  integer(INT_KIND),parameter :: MPP_C_ETCFILE_UNIT           = 9

 !-----------------------------------------------------------------------------
 !Private module variables that might possibly be moved to fms.
  logical(INT_KIND)           :: read_ascii_file_on  = .false.
  integer(INT_KIND),parameter :: COMM_TAG_1          = 1
  integer(INT_KIND),parameter :: COMM_TAG_2          = 2
  integer(INT_KIND),parameter :: COMM_TAG_3          = 3
  integer(INT_KIND),parameter :: COMM_TAG_4          = 4
  integer(INT_KIND),parameter :: COMM_TAG_5          = 5
  integer(INT_KIND),parameter :: COMM_TAG_6          = 6
  integer(INT_KIND),parameter :: COMM_TAG_7          = 7
  integer(INT_KIND),parameter :: COMM_TAG_8          = 8
  integer(INT_KIND),parameter :: COMM_TAG_9          = 9
  integer(INT_KIND),parameter :: COMM_TAG_10         = 10
  integer(INT_KIND),parameter :: COMM_TAG_11         = 11
  integer(INT_KIND),parameter :: COMM_TAG_12         = 12
  integer(INT_KIND),parameter :: COMM_TAG_13         = 13
  integer(INT_KIND),parameter :: COMM_TAG_14         = 14
  integer(INT_KIND),parameter :: COMM_TAG_15         = 15
  integer(INT_KIND),parameter :: COMM_TAG_16         = 16
  integer(INT_KIND),parameter :: COMM_TAG_17         = 17
  integer(INT_KIND),parameter :: COMM_TAG_18         = 18
  integer(INT_KIND),parameter :: COMM_TAG_19         = 19
  integer(INT_KIND),parameter :: COMM_TAG_20         = 20
  integer(INT_KIND),parameter :: NULL_PE             = -3
  integer(INT_KIND),parameter :: MPP_CLOCK_SYNC      = 1
  integer(INT_KIND),parameter :: MPP_CLOCK_DETAILED  = 2
  integer(INT_KIND),parameter :: ALL_PES             = -1
  real(DOUBLE_KIND),parameter :: MPP_FILL_DOUBLE     = 9.9692099683868690e+36
  integer(INT_KIND),parameter :: MPP_FILL_INT        = -2147483647
  integer(INT_KIND),parameter :: CLOCK_ROUTINE       = MPP_C_ROUTINE_TIMER
  integer(INT_KIND),parameter :: CLOCK_COMPONENT     = MPP_C_COMPONENT_TIMER
  integer(INT_KIND),parameter :: CLOCK_SUBCOMPONENT  = MPP_C_SUBCOMPONENT_TIMER
  integer(INT_KIND),parameter :: CLOCK_MODULE_DRIVER = MPP_C_MODULE_DRIVER_TIMER
  integer(INT_KIND),parameter :: CLOCK_MODULE        = MPP_C_MODULE_TIMER
  integer(INT_KIND),parameter :: CLOCK_LOOP          = MPP_C_LOOP_TIMER
  integer(INT_KIND),parameter :: CLOCK_INFRA         = MPP_C_INFRASTRUCTURE_TIMER
  integer(INT_KIND),parameter :: MPP_DEBUG           = 2
  integer(INT_KIND),parameter :: INPUT_STR_LENGTH    = 256
  integer(INT_KIND),parameter :: MPP_NULL_CLOCK_ID   = -1

contains

#include "mpp_possible_fms_routines.inc"
#include "mpp_deprecated_routines.inc"
#include "mpp_util.inc"
#include "mpp_util_private.inc"

#ifdef _INTEL16_FBINDINGS
#include "mpp_error_intel16.inc"
#include "mpp_sync_intel16.inc"
#include "mpp_send_intel16.inc"
#include "mpp_recv_intel16.inc"
#include "mpp_broadcast_intel16.inc"
#include "mpp_sum_intel16.inc"
#include "mpp_max_intel16.inc"
#include "mpp_min_intel16.inc"
#include "mpp_chksum_intel16.inc"
#include "mpp_gather_intel16.inc"
#include "mpp_gatherv_intel16.inc"
#include "mpp_gather_pelist_intel16.inc"
#include "mpp_scatter_pelist_intel16.inc"
#include "mpp_alltoall_intel16.inc"
#include "mpp_alltoallv_intel16.inc"
#elif _GNU_FBINDINGS
#include "mpp_error_gnu.inc"
#include "mpp_sync_gnu.inc"
#include "mpp_send_gnu.inc"
#include "mpp_recv_gnu.inc"
#include "mpp_broadcast_gnu.inc"
#include "mpp_sum_gnu.inc"
#include "mpp_max_gnu.inc"
#include "mpp_min_gnu.inc"
#include "mpp_chksum_gnu.inc"
#include "mpp_gather_gnu.inc"
#include "mpp_gatherv_gnu.inc"
#include "mpp_gather_pelist_gnu.inc"
#include "mpp_scatter_pelist_gnu.inc"
#include "mpp_alltoall_gnu.inc"
#include "mpp_alltoallv_gnu.inc"
#else
#include "mpp_error_basic.inc"
#include "mpp_sync_basic.inc"
#include "mpp_send_factory_basic.inc"
#include "mpp_recv_factory_basic.inc"
#include "mpp_broadcast_factory_basic.inc"
#include "mpp_sum_factory_basic.inc"
#include "mpp_max_factory_basic.inc"
#include "mpp_min_factory_basic.inc"
#include "mpp_chksum_factory_basic.inc"
#include "mpp_gather_factory_basic.inc"
#include "mpp_gatherv_factory_basic.inc"
#include "mpp_gather_pelist_factory_basic.inc"
#include "mpp_scatter_pelist_factory_basic.inc"
#include "mpp_alltoall_factory_basic.inc"
#include "mpp_alltoallv_factory_basic.inc"
#endif

end module mpp_mod
