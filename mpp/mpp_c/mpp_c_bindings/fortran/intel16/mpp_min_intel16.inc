 !*****************************************************************************
 !Public facing mpp_min routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _INTEL16_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer scalar data.
  subroutine mpp_min_i32s(min_data,pelist)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(c_int32_t),intent(inout)                    :: min_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_min_c_style(min_data,1,MPP_INT32,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_min_c_style(min_data,1,MPP_INT32,1,null_pelist)

    endif

  end subroutine mpp_min_i32s

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit integer scalar data.
  subroutine mpp_min_i64s(min_data,pelist)
    use iso_c_binding, only: c_int64_t,c_int32_t

   !<Inputs/Outputs
    integer(c_int64_t),intent(inout)                    :: min_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_min_c_style(min_data,1,MPP_INT64,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_min_c_style(min_data,1,MPP_INT64,1,null_pelist)

    endif

  end subroutine mpp_min_i64s

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real scalar data.
  subroutine mpp_min_r32s(min_data,pelist)
    use iso_c_binding, only: c_float,c_int32_t

   !<Inputs/Outputs
    real(c_float),intent(inout)                         :: min_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_min_c_style(min_data,1,MPP_REAL32,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_min_c_style(min_data,1,MPP_REAL32,1,null_pelist)

    endif

  end subroutine mpp_min_r32s

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real scalar data.
  subroutine mpp_min_r64s(min_data,pelist)
    use iso_c_binding, only: c_double,c_int32_t

   !<Inputs/Outputs
    real(c_double),intent(inout)                        :: min_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_min_c_style(min_data,1,MPP_REAL64,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_min_c_style(min_data,1,MPP_REAL64,1,null_pelist)

    endif

  end subroutine mpp_min_r64s

 !-----------------------------------------------------------------------------
 !>Get the minimum value of the data across ranks on a pelist.
  subroutine mpp_min_c_style(min_data,min_len,mpp_type,n,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    type(*),dimension(..)                             :: min_data
    integer(INT_KIND),intent(in)                      :: min_len
    integer(c_int32_t),intent(in)                     :: mpp_type
    integer(INT_KIND),intent(in)                      :: n
    integer(c_int32_t),dimension(n),target,intent(in) :: pelist

    call iso_c_mpp_min(min_data,int(min_len,kind=c_size_t),mpp_type,pelist, &
                       int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_min_c_style

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
