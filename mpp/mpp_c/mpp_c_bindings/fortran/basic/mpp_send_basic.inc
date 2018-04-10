 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_send_c_style_i32a(put_data,put_len,to_pe,mpp_type,comm_tag, &
                                   request)
    use iso_c_binding, only: c_int32_t,c_loc

   !<Inputs/Outputs
    integer(c_int32_t),dimension(*),target,intent(inout) :: put_data
    integer(INT_KIND),intent(in)                         :: put_len
    integer(c_int32_t),intent(in)                        :: to_pe
    integer(c_int32_t),intent(in)                        :: mpp_type
    integer(c_int32_t),intent(in)                        :: comm_tag
    integer(INT_KIND),intent(inout),optional             :: request

    call mpp_send_c_style(c_loc(put_data),put_len,to_pe,mpp_type,comm_tag, &
                          request)

  end subroutine mpp_send_c_style_i32a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_send_c_style_i64a(put_data,put_len,to_pe,mpp_type,comm_tag, &
                                   request)
    use iso_c_binding, only: c_int64_t,c_int32_t,c_loc

   !<Inputs/Outputs
    integer(c_int64_t),dimension(*),target,intent(inout) :: put_data
    integer(INT_KIND),intent(in)                         :: put_len
    integer(c_int32_t),intent(in)                        :: to_pe
    integer(c_int32_t),intent(in)                        :: mpp_type
    integer(c_int32_t),intent(in)                        :: comm_tag
    integer(INT_KIND),intent(inout),optional             :: request

    call mpp_send_c_style(c_loc(put_data),put_len,to_pe,mpp_type,comm_tag, &
                          request)

  end subroutine mpp_send_c_style_i64a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_send_c_style_r32a(put_data,put_len,to_pe,mpp_type,comm_tag, &
                                   request)
    use iso_c_binding, only: c_float,c_int32_t,c_loc

   !<Inputs/Outputs
    real(c_float),dimension(*),target,intent(inout) :: put_data
    integer(INT_KIND),intent(in)                    :: put_len
    integer(c_int32_t),intent(in)                   :: to_pe
    integer(c_int32_t),intent(in)                   :: mpp_type
    integer(c_int32_t),intent(in)                   :: comm_tag
    integer(INT_KIND),intent(inout),optional        :: request

    call mpp_send_c_style(c_loc(put_data),put_len,to_pe,mpp_type,comm_tag, &
                          request)

  end subroutine mpp_send_c_style_r32a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_send_c_style_r64a(put_data,put_len,to_pe,mpp_type,comm_tag, &
                                   request)
    use iso_c_binding, only: c_double,c_int32_t,c_loc

   !<Inputs/Outputs
    real(c_double),dimension(*),target,intent(inout) :: put_data
    integer(INT_KIND),intent(in)                     :: put_len
    integer(c_int32_t),intent(in)                    :: to_pe
    integer(c_int32_t),intent(in)                    :: mpp_type
    integer(c_int32_t),intent(in)                    :: comm_tag
    integer(INT_KIND),intent(inout),optional         :: request

    call mpp_send_c_style(c_loc(put_data),put_len,to_pe,mpp_type,comm_tag, &
                          request)

  end subroutine mpp_send_c_style_r64a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_send_c_style_l32a(put_data,put_len,to_pe,mpp_type,comm_tag, &
                                   request)
    use iso_c_binding, only: c_double,c_int32_t,c_loc

   !<Inputs/Outputs
    logical(INT_KIND),dimension(*),target,intent(inout) :: put_data
    integer(INT_KIND),intent(in)                        :: put_len
    integer(c_int32_t),intent(in)                       :: to_pe
    integer(c_int32_t),intent(in)                       :: mpp_type
    integer(c_int32_t),intent(in)                       :: comm_tag
    integer(INT_KIND),intent(inout),optional            :: request

    call mpp_send_c_style(c_loc(put_data),put_len,to_pe,mpp_type,comm_tag, &
                          request)

  end subroutine mpp_send_c_style_l32a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_send_c_style_l64a(put_data,put_len,to_pe,mpp_type,comm_tag, &
                                   request)
    use iso_c_binding, only: c_double,c_int32_t,c_loc

   !<Inputs/Outputs
    logical(LONG_KIND),dimension(*),target,intent(inout) :: put_data
    integer(INT_KIND),intent(in)                         :: put_len
    integer(c_int32_t),intent(in)                        :: to_pe
    integer(c_int32_t),intent(in)                        :: mpp_type
    integer(c_int32_t),intent(in)                        :: comm_tag
    integer(INT_KIND),intent(inout),optional             :: request

    call mpp_send_c_style(c_loc(put_data),put_len,to_pe,mpp_type,comm_tag, &
                          request)

  end subroutine mpp_send_c_style_l64a

 !-----------------------------------------------------------------------------
 !>Calls the mpp_c_send routine.
  subroutine mpp_send_c_style(put_data,put_len,to_pe,mpp_type,comm_tag, &
                              request)
    use iso_c_binding, only: c_ptr,c_int32_t,c_int,c_size_t,c_null_ptr

   !<Inputs/Outputs
    type(c_ptr)                              :: put_data
    integer(INT_KIND),intent(in)             :: put_len
    integer(c_int32_t),intent(in)            :: to_pe
    integer(c_int32_t),intent(in)            :: mpp_type
    integer(c_int32_t),intent(in)            :: comm_tag
    integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
    integer(c_int) :: null_request

    if (present(request)) then
        call iso_c_mpp_send(put_data,int(put_len,kind=c_size_t), &
                            to_pe,mpp_type,comm_tag,request, &
                            mpp_context)
    else
        null_request = int(MPP_C_NULL_REQUEST,kind=c_int)
        call iso_c_mpp_send(put_data,int(put_len,kind=c_size_t), &
                            to_pe,mpp_type,comm_tag,null_request,mpp_context)
    endif

  end subroutine mpp_send_c_style

 !-----------------------------------------------------------------------------

