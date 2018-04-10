 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_sum_c_style_i32a(sum_data,sum_len,mpp_type,n,pelist)
    use iso_c_binding, only: c_int32_t,c_loc

   !<Inputs/Outputs
    integer(c_int32_t),dimension(*),target,intent(inout) :: sum_data
    integer(INT_KIND),intent(in)                         :: sum_len
    integer(c_int32_t),intent(in)                        :: mpp_type
    integer(INT_KIND),intent(in)                         :: n
    integer(c_int32_t),dimension(n),intent(in)           :: pelist

    call mpp_sum_c_style(c_loc(sum_data),sum_len,mpp_type,n,pelist)

  end subroutine mpp_sum_c_style_i32a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_sum_c_style_i64a(sum_data,sum_len,mpp_type,n,pelist)
    use iso_c_binding, only: c_int64_t,c_int32_t,c_loc

   !<Inputs/Outputs
    integer(c_int64_t),dimension(*),target,intent(inout) :: sum_data
    integer(INT_KIND),intent(in)                         :: sum_len
    integer(c_int32_t),intent(in)                        :: mpp_type
    integer(INT_KIND),intent(in)                         :: n
    integer(c_int32_t),dimension(n),intent(in)           :: pelist

    call mpp_sum_c_style(c_loc(sum_data),sum_len,mpp_type,n,pelist)

  end subroutine mpp_sum_c_style_i64a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_sum_c_style_r32a(sum_data,sum_len,mpp_type,n,pelist)
    use iso_c_binding, only: c_float,c_int32_t,c_loc

   !<Inputs/Outputs
    real(c_float),dimension(*),target,intent(inout) :: sum_data
    integer(INT_KIND),intent(in)                    :: sum_len
    integer(c_int32_t),intent(in)                   :: mpp_type
    integer(INT_KIND),intent(in)                    :: n
    integer(c_int32_t),dimension(n),intent(in)      :: pelist

    call mpp_sum_c_style(c_loc(sum_data),sum_len,mpp_type,n,pelist)

  end subroutine mpp_sum_c_style_r32a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_sum_c_style_r64a(sum_data,sum_len,mpp_type,n,pelist)
    use iso_c_binding, only: c_double,c_int32_t,c_loc

   !<Inputs/Outputs
    real(c_double),dimension(*),target,intent(inout) :: sum_data
    integer(INT_KIND),intent(in)                     :: sum_len
    integer(c_int32_t),intent(in)                    :: mpp_type
    integer(INT_KIND),intent(in)                     :: n
    integer(c_int32_t),dimension(n),intent(in)       :: pelist

    call mpp_sum_c_style(c_loc(sum_data),sum_len,mpp_type,n,pelist)

  end subroutine mpp_sum_c_style_r64a

 !-----------------------------------------------------------------------------
 !>Sum the data across ranks on a pelist.
  subroutine mpp_sum_c_style(sum_data,sum_len,mpp_type,n,pelist)
    use iso_c_binding, only: c_ptr,c_int32_t,c_size_t

   !<Inputs/Outputs
    type(c_ptr)                                :: sum_data
    integer(INT_KIND),intent(in)               :: sum_len
    integer(c_int32_t),intent(in)              :: mpp_type
    integer(INT_KIND),intent(in)               :: n
    integer(c_int32_t),dimension(n),intent(in) :: pelist

    call iso_c_mpp_sum(sum_data,int(sum_len,kind=c_size_t),mpp_type, &
                       pelist,int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_sum_c_style

 !-----------------------------------------------------------------------------

