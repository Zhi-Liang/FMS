 !*****************************************************************************
 !Public facing mpp_sum routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _INTEL16_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer scalar data.
  subroutine mpp_sum_i32s(sum_data,pelist)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(c_int32_t),intent(inout)                    :: sum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_sum_c_style(sum_data,1,MPP_INT32,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_sum_c_style(sum_data,1,MPP_INT32,1,null_pelist)
    endif

  end subroutine mpp_sum_i32s

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer data.
  subroutine mpp_sum_i32(sum_data,sum_len,pelist)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(c_int32_t),dimension(..),intent(inout)      :: sum_data
    integer(INT_KIND),intent(in)                        :: sum_len
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_sum_c_style(sum_data,sum_len,MPP_INT32,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_sum_c_style(sum_data,sum_len,MPP_INT32,1,null_pelist)
    endif

  end subroutine mpp_sum_i32

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit integer scalar data.
  subroutine mpp_sum_i64s(sum_data,pelist)
    use iso_c_binding, only: c_int64_t,c_int32_t

   !<Inputs/Outputs
    integer(c_int64_t),intent(inout)                    :: sum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_sum_c_style(sum_data,1,MPP_INT64,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_sum_c_style(sum_data,1,MPP_INT64,1,null_pelist)
    endif

  end subroutine mpp_sum_i64s

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit integer data.
  subroutine mpp_sum_i64(sum_data,sum_len,pelist)
    use iso_c_binding, only: c_int64_t,c_int32_t

   !<Inputs/Outputs
    integer(c_int64_t),dimension(..),intent(inout)      :: sum_data
    integer(INT_KIND),intent(in)                        :: sum_len
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_sum_c_style(sum_data,sum_len,MPP_INT64,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_sum_c_style(sum_data,sum_len,MPP_INT64,1,null_pelist)
    endif

  end subroutine mpp_sum_i64

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real scalar data.
  subroutine mpp_sum_r32s(sum_data,pelist)
    use iso_c_binding, only: c_float,c_int32_t

   !<Inputs/Outputs
    real(c_float),intent(inout)                         :: sum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_sum_c_style(sum_data,1,MPP_REAL32,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_sum_c_style(sum_data,1,MPP_REAL32,1,null_pelist)
    endif

  end subroutine mpp_sum_r32s

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real data.
  subroutine mpp_sum_r32(sum_data,sum_len,pelist)
    use iso_c_binding, only: c_float,c_int32_t

   !<Inputs/Outputs
    real(c_float),dimension(..),intent(inout)           :: sum_data
    integer(INT_KIND),intent(in)                        :: sum_len
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_sum_c_style(sum_data,sum_len,MPP_REAL32,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_sum_c_style(sum_data,sum_len,MPP_REAL32,1,null_pelist)
    endif

  end subroutine mpp_sum_r32

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real scalar data.
  subroutine mpp_sum_r64s(sum_data,pelist)
    use iso_c_binding, only: c_double,c_int32_t

   !<Inputs/Outputs
    real(c_double),intent(inout)                        :: sum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_sum_c_style(sum_data,1,MPP_REAL64,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_sum_c_style(sum_data,1,MPP_REAL64,1,null_pelist)
    endif

  end subroutine mpp_sum_r64s

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real data.
  subroutine mpp_sum_r64(sum_data,sum_len,pelist)
    use iso_c_binding, only: c_double,c_int32_t

   !<Inputs/Outputs
    real(c_double),dimension(..),intent(inout)          :: sum_data
    integer(INT_KIND),intent(in)                        :: sum_len
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_sum_c_style(sum_data,sum_len,MPP_REAL64,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_sum_c_style(sum_data,sum_len,MPP_REAL64,1,null_pelist)
    endif

  end subroutine mpp_sum_r64

 !-----------------------------------------------------------------------------
 !>Sum the data across ranks on a pelist.
  subroutine mpp_sum_c_style(sum_data,sum_len,mpp_type,n,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    type(*),dimension(..),intent(inout)        :: sum_data
    integer(INT_KIND),intent(in)               :: sum_len
    integer(c_int32_t),intent(in)              :: mpp_type
    integer(INT_KIND),intent(in)               :: n
    integer(c_int32_t),dimension(n),intent(in) :: pelist

    call iso_c_mpp_sum(sum_data,int(sum_len,kind=c_size_t),mpp_type, &
                       pelist,int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_sum_c_style

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
