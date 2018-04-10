 !-----------------------------------------------------------------------------
 !>Get the minimum value of the data across ranks on a pelist.
  subroutine mpp_min_c_style(min_data,min_len,mpp_type,n,pelist)
    use iso_c_binding, only: c_ptr,c_int32_t,c_size_t

   !<Inputs/Outputs
    type(c_ptr)                                       :: min_data
    integer(INT_KIND),intent(in)                      :: min_len
    integer(c_int32_t),intent(in)                     :: mpp_type
    integer(INT_KIND),intent(in)                      :: n
    integer(c_int32_t),dimension(n),target,intent(in) :: pelist

    call iso_c_mpp_min(min_data,int(min_len,kind=c_size_t),mpp_type,pelist, &
                       int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_min_c_style

 !-----------------------------------------------------------------------------

