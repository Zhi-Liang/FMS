 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_recv_c_style_i32a(get_data,get_len,from_pe,mpp_type,comm_tag, &
                                   comm_block,request)
    use iso_c_binding, only: c_int32_t,c_loc

   !<Inputs/Outputs
    integer(c_int32_t),dimension(*),target,intent(inout) :: get_data
    integer(INT_KIND),intent(in)                         :: get_len
    integer(INT_KIND),intent(in)                         :: from_pe
    integer(c_int32_t),intent(in)                        :: mpp_type
    integer(c_int32_t),intent(in)                        :: comm_tag
    integer(c_int32_t),intent(in)                        :: comm_block
    integer(INT_KIND),intent(inout),optional             :: request

    call mpp_recv_c_style(c_loc(get_data),get_len,from_pe,mpp_type,comm_tag, &
                          comm_block,request)

  end subroutine mpp_recv_c_style_i32a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_recv_c_style_i64a(get_data,get_len,from_pe,mpp_type,comm_tag, &
                                   comm_block,request)
    use iso_c_binding, only: c_int64_t,c_int32_t,c_loc

   !<Inputs/Outputs
    integer(c_int64_t),dimension(*),target,intent(inout) :: get_data
    integer(INT_KIND),intent(in)                         :: get_len
    integer(INT_KIND),intent(in)                         :: from_pe
    integer(c_int32_t),intent(in)                        :: mpp_type
    integer(c_int32_t),intent(in)                        :: comm_tag
    integer(c_int32_t),intent(in)                        :: comm_block
    integer(INT_KIND),intent(inout),optional             :: request

    call mpp_recv_c_style(c_loc(get_data),get_len,from_pe,mpp_type,comm_tag, &
                          comm_block,request)

  end subroutine mpp_recv_c_style_i64a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_recv_c_style_r32a(get_data,get_len,from_pe,mpp_type,comm_tag, &
                                   comm_block,request)
    use iso_c_binding, only: c_float,c_int32_t,c_loc

   !<Inputs/Outputs
    real(c_float),dimension(*),target,intent(inout) :: get_data
    integer(INT_KIND),intent(in)                    :: get_len
    integer(INT_KIND),intent(in)                    :: from_pe
    integer(c_int32_t),intent(in)                   :: mpp_type
    integer(c_int32_t),intent(in)                   :: comm_tag
    integer(c_int32_t),intent(in)                   :: comm_block
    integer(INT_KIND),intent(inout),optional        :: request

    call mpp_recv_c_style(c_loc(get_data),get_len,from_pe,mpp_type,comm_tag, &
                          comm_block,request)

  end subroutine mpp_recv_c_style_r32a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_recv_c_style_r64a(get_data,get_len,from_pe,mpp_type,comm_tag, &
                                   comm_block,request)
    use iso_c_binding, only: c_double,c_int32_t,c_loc

   !<Inputs/Outputs
    real(c_double),dimension(*),target,intent(inout) :: get_data
    integer(INT_KIND),intent(in)                     :: get_len
    integer(INT_KIND),intent(in)                     :: from_pe
    integer(c_int32_t),intent(in)                    :: mpp_type
    integer(c_int32_t),intent(in)                    :: comm_tag
    integer(c_int32_t),intent(in)                    :: comm_block
    integer(INT_KIND),intent(inout),optional         :: request

    call mpp_recv_c_style(c_loc(get_data),get_len,from_pe,mpp_type,comm_tag, &
                          comm_block,request)

  end subroutine mpp_recv_c_style_r64a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_recv_c_style_l32a(get_data,get_len,from_pe,mpp_type,comm_tag, &
                                   comm_block,request)
    use iso_c_binding, only: c_int32_t,c_loc

   !<Inputs/Outputs
    logical(INT_KIND),dimension(*),target,intent(inout) :: get_data
    integer(INT_KIND),intent(in)                        :: get_len
    integer(INT_KIND),intent(in)                        :: from_pe
    integer(c_int32_t),intent(in)                       :: mpp_type
    integer(c_int32_t),intent(in)                       :: comm_tag
    integer(c_int32_t),intent(in)                       :: comm_block
    integer(INT_KIND),intent(inout),optional            :: request

    call mpp_recv_c_style(c_loc(get_data),get_len,from_pe,mpp_type,comm_tag, &
                          comm_block,request)

  end subroutine mpp_recv_c_style_l32a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_recv_c_style_l64a(get_data,get_len,from_pe,mpp_type,comm_tag, &
                                   comm_block,request)
    use iso_c_binding, only: c_int32_t,c_loc

   !<Inputs/Outputs
    logical(LONG_KIND),dimension(*),target,intent(inout) :: get_data
    integer(INT_KIND),intent(in)                         :: get_len
    integer(INT_KIND),intent(in)                         :: from_pe
    integer(c_int32_t),intent(in)                        :: mpp_type
    integer(c_int32_t),intent(in)                        :: comm_tag
    integer(c_int32_t),intent(in)                        :: comm_block
    integer(INT_KIND),intent(inout),optional             :: request

    call mpp_recv_c_style(c_loc(get_data),get_len,from_pe,mpp_type,comm_tag, &
                          comm_block,request)

  end subroutine mpp_recv_c_style_l64a

 !-----------------------------------------------------------------------------
 !>Receive data from another rank on the world pelist.
  subroutine mpp_recv_c_style(get_data,get_len,from_pe,mpp_type,comm_tag, &
                              comm_block,request)
    use iso_c_binding, only: c_ptr,c_int32_t,c_int,c_size_t

   !<Inputs/Outputs
    type(c_ptr)                              :: get_data
    integer(INT_KIND),intent(in)             :: get_len
    integer(INT_KIND),intent(in)             :: from_pe
    integer(c_int32_t),intent(in)            :: mpp_type
    integer(c_int32_t),intent(in)            :: comm_tag
    integer(c_int32_t),intent(in)            :: comm_block
    integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
    integer(c_int) :: null_request

    if (present(request)) then
        call iso_c_mpp_recv(get_data,int(get_len,kind=c_size_t), &
                            int(from_pe,kind=c_int32_t),mpp_type, &
                            comm_tag,request, &
                            comm_block,mpp_context)
    else
        null_request = MPP_C_NULL_REQUEST
        call iso_c_mpp_recv(get_data,int(get_len,kind=c_size_t), &
                            int(from_pe,kind=c_int32_t),mpp_type, &
                            comm_tag,null_request,comm_block,mpp_context)
    endif

  end subroutine mpp_recv_c_style

 !-----------------------------------------------------------------------------

