 !*****************************************************************************
 !Public facing mpp_min routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _GNU_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for scalar data.
  subroutine mpp_min_scalar(min_data,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    class(*),intent(inout)                             :: min_data
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t)              :: mpp_type
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Set the mpp_type value.
    select type (min_data)
    type is (integer(INT_KIND))
        mpp_type = MPP_INT32
    type is (integer(LONG_KIND))
        mpp_type = MPP_INT64
    type is (real(FLOAT_KIND))
        mpp_type = MPP_REAL32
    type is (real(DOUBLE_KIND))
        mpp_type = MPP_REAL64
    class default
        call mpp_error(FATAL,'MPP_MIN_SCALAR: unsupported type.')
    end select

    if (present(pelist)) then
        call mpp_min_c_style_scalar(min_data,1,mpp_type,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_min_c_style_scalar(min_data,1,mpp_type,1,null_pelist)

    endif

  end subroutine mpp_min_scalar

 !-----------------------------------------------------------------------------
 !>Get the minimum value of the data across ranks on a pelist.
  subroutine mpp_min_c_style_scalar(min_data,min_len,mpp_type,n,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    type(*)                                          :: min_data
    integer(INT_KIND),intent(in)                     :: min_len
    integer(c_int32_t),intent(in)                    :: mpp_type
    integer(INT_KIND),intent(in)                     :: n
    integer(INT_KIND),dimension(n),target,intent(in) :: pelist

    call iso_c_mpp_min_scalar(min_data,int(min_len,kind=c_size_t),mpp_type,pelist, &
                              int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_min_c_style_scalar

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
