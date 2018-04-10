 !-----------------------------------------------------------------------------
 !>Get the maximum value of the data across ranks on a pelist.
  subroutine mpp_max_c_style(max_data,max_len,mpp_type,n,pelist)
    use iso_c_binding, only: c_ptr,c_int32_t,c_size_t

   !<Inputs/Outputs
    type(c_ptr)                                       :: max_data
    integer(INT_KIND),intent(in)                      :: max_len
    integer(c_int32_t),intent(in)                     :: mpp_type
    integer(INT_KIND),intent(in)                      :: n
    integer(c_int32_t),dimension(n),target,intent(in) :: pelist

    call iso_c_mpp_max(max_data,int(max_len,kind=c_size_t),mpp_type,pelist, &
                       int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_max_c_style

 !-----------------------------------------------------------------------------

