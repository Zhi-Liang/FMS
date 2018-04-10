 !*****************************************************************************
 !Public facing mpp_send routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#if !defined(_GNU_FBINDINGS) && !defined(_INTEL16_FBINDINGS)

#if defined(_S_NAME)
#undef _S_NAME
#endif
#if defined(_C_TYPE)
#undef _C_TYPE
#endif
#if defined(_MPP_TYPE)
#undef _MPP_TYPE
#endif
#if defined(_MPP_PARAM)
#undef _MPP_PARAM
#endif
#if defined(_A_NAME)
#undef _A_NAME
#endif
#if defined(_DIMENSION)
#undef _DIMENSION
#endif
#if defined(_A_SUB)
#undef _A_SUB
#endif

#define _C_TYPE
#define _MPP_TYPE integer(c_int32_t)
#define _MPP_PARAM MPP_INT32
#define _S_NAME mpp_send_i32s
#define _A_SUB mpp_send_c_style_i32a
#include "mpp_send_scalar_template_basic.inc"
#define _A_NAME mpp_send_i32_1D
#define _DIMENSION dimension(:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_i32_2D
#define _DIMENSION dimension(:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_i32_3D
#define _DIMENSION dimension(:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_i32_4D
#define _DIMENSION dimension(:,:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_i32_5D
#define _DIMENSION dimension(:,:,:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#undef _C_TYPE
#undef _MPP_TYPE
#undef _MPP_PARAM
#undef _S_NAME
#undef _A_SUB

#define _C_TYPE ,c_int64_t
#define _MPP_TYPE integer(c_int64_t)
#define _MPP_PARAM MPP_INT64
#define _S_NAME mpp_send_i64s
#define _A_SUB mpp_send_c_style_i64a
#include "mpp_send_scalar_template_basic.inc"
#define _A_NAME mpp_send_i64_1D
#define _DIMENSION dimension(:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_i64_2D
#define _DIMENSION dimension(:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_i64_3D
#define _DIMENSION dimension(:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_i64_4D
#define _DIMENSION dimension(:,:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_i64_5D
#define _DIMENSION dimension(:,:,:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#undef _C_TYPE
#undef _MPP_TYPE
#undef _MPP_PARAM
#undef _S_NAME
#undef _A_SUB

#define _C_TYPE ,c_float
#define _MPP_TYPE real(c_float)
#define _MPP_PARAM MPP_REAL32
#define _S_NAME mpp_send_r32s
#define _A_SUB mpp_send_c_style_r32a
#include "mpp_send_scalar_template_basic.inc"
#define _A_NAME mpp_send_r32_1D
#define _DIMENSION dimension(:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_r32_2D
#define _DIMENSION dimension(:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_r32_3D
#define _DIMENSION dimension(:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_r32_4D
#define _DIMENSION dimension(:,:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_r32_5D
#define _DIMENSION dimension(:,:,:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#undef _C_TYPE
#undef _MPP_TYPE
#undef _MPP_PARAM
#undef _S_NAME
#undef _A_SUB

#define _C_TYPE ,c_double
#define _MPP_TYPE real(c_double)
#define _MPP_PARAM MPP_REAL64
#define _S_NAME mpp_send_r64s
#define _A_SUB mpp_send_c_style_r64a
#include "mpp_send_scalar_template_basic.inc"
#define _A_NAME mpp_send_r64_1D
#define _DIMENSION dimension(:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_r64_2D
#define _DIMENSION dimension(:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_r64_3D
#define _DIMENSION dimension(:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_r64_4D
#define _DIMENSION dimension(:,:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_r64_5D
#define _DIMENSION dimension(:,:,:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#undef _C_TYPE
#undef _MPP_TYPE
#undef _MPP_PARAM
#undef _S_NAME
#undef _A_SUB

#define _C_TYPE
#define _MPP_TYPE logical(INT_KIND)
#define _MPP_PARAM MPP_INT32
#define _S_NAME mpp_send_l32s
#define _A_SUB mpp_send_c_style_l32a
#include "mpp_send_scalar_template_basic.inc"
#define _A_NAME mpp_send_l32_1D
#define _DIMENSION dimension(:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_l32_2D
#define _DIMENSION dimension(:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_l32_3D
#define _DIMENSION dimension(:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_l32_4D
#define _DIMENSION dimension(:,:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_l32_5D
#define _DIMENSION dimension(:,:,:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#undef _C_TYPE
#undef _MPP_TYPE
#undef _MPP_PARAM
#undef _S_NAME
#undef _A_SUB

#define _C_TYPE
#define _MPP_TYPE logical(LONG_KIND)
#define _MPP_PARAM MPP_INT64
#define _S_NAME mpp_send_l64s
#define _A_SUB mpp_send_c_style_l64a
#include "mpp_send_scalar_template_basic.inc"
#define _A_NAME mpp_send_l64_1D
#define _DIMENSION dimension(:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_l64_2D
#define _DIMENSION dimension(:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_l64_3D
#define _DIMENSION dimension(:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_l64_4D
#define _DIMENSION dimension(:,:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_send_l64_5D
#define _DIMENSION dimension(:,:,:,:,:)
#include "mpp_send_array_template_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#undef _C_TYPE
#undef _MPP_TYPE
#undef _MPP_PARAM
#undef _S_NAME
#undef _A_SUB

#include "mpp_send_basic.inc"
#endif

 !*****************************************************************************
