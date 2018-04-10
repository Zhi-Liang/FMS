 !*****************************************************************************
 !Public facing mpp_max routines.

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

#define _C_TYPE
#define _MPP_TYPE integer(c_int32_t)
#define _MPP_PARAM MPP_INT32
#define _S_NAME mpp_max_i32s
#include "mpp_max_scalar_template_basic.inc"
#undef _C_TYPE
#undef _MPP_TYPE
#undef _MPP_PARAM
#undef _S_NAME

#define _C_TYPE ,c_int64_t
#define _MPP_TYPE integer(c_int64_t)
#define _MPP_PARAM MPP_INT64
#define _S_NAME mpp_max_i64s
#include "mpp_max_scalar_template_basic.inc"
#undef _C_TYPE
#undef _MPP_TYPE
#undef _MPP_PARAM
#undef _S_NAME

#define _C_TYPE ,c_float
#define _MPP_TYPE real(c_float)
#define _MPP_PARAM MPP_REAL32
#define _S_NAME mpp_max_r32s
#include "mpp_max_scalar_template_basic.inc"
#undef _C_TYPE
#undef _MPP_TYPE
#undef _MPP_PARAM
#undef _S_NAME

#define _C_TYPE ,c_double
#define _MPP_TYPE real(c_double)
#define _MPP_PARAM MPP_REAL64
#define _S_NAME mpp_max_r64s
#include "mpp_max_scalar_template_basic.inc"
#undef _C_TYPE
#undef _MPP_TYPE
#undef _MPP_PARAM
#undef _S_NAME

#include "mpp_max_basic.inc"
#endif

 !*****************************************************************************
