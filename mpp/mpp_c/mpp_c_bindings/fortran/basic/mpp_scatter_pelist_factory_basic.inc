 !*****************************************************************************
 !Public facing mpp_scatter_pelist routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#if !defined(_GNU_FBINDINGS) && !defined(_INTEL16_FBINDINGS)

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
#define _A_SUB mpp_scatter_pelist_c_style_i32a
#define _A_NAME mpp_scatter_pelist_i32_2D
#define _DIMENSION dimension(:,:)
#include "mpp_scatter_pelist_array_template_2D_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_scatter_pelist_i32_3D
#define _DIMENSION dimension(:,:,:)
#include "mpp_scatter_pelist_array_template_3D_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#undef _C_TYPE
#undef _MPP_TYPE
#undef _MPP_PARAM
#undef _A_SUB

#define _C_TYPE ,c_float
#define _MPP_TYPE real(c_float)
#define _MPP_PARAM MPP_REAL32
#define _A_SUB mpp_scatter_pelist_c_style_r32a
#define _A_NAME mpp_scatter_pelist_r32_2D
#define _DIMENSION dimension(:,:)
#include "mpp_scatter_pelist_array_template_2D_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_scatter_pelist_r32_3D
#define _DIMENSION dimension(:,:,:)
#include "mpp_scatter_pelist_array_template_3D_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#undef _C_TYPE
#undef _MPP_TYPE
#undef _MPP_PARAM
#undef _A_SUB

#define _C_TYPE ,c_double
#define _MPP_TYPE real(c_double)
#define _MPP_PARAM MPP_REAL64
#define _A_SUB mpp_scatter_pelist_c_style_r64a
#define _A_NAME mpp_scatter_pelist_r64_2D
#define _DIMENSION dimension(:,:)
#include "mpp_scatter_pelist_array_template_2D_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#define _A_NAME mpp_scatter_pelist_r64_3D
#define _DIMENSION dimension(:,:,:)
#include "mpp_scatter_pelist_array_template_3D_basic.inc"
#undef _A_NAME
#undef _DIMENSION
#undef _C_TYPE
#undef _MPP_TYPE
#undef _MPP_PARAM
#undef _A_SUB

#include "mpp_scatter_pelist_basic.inc"
#endif

 !*****************************************************************************
