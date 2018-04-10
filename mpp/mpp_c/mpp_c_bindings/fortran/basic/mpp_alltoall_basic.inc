 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_alltoall_c_style_i32a(send_data,send_data_size,recv_data, &
                                       recv_data_size,send_len,recv_len, &
                                       mpp_type,n,pelist)
    use iso_c_binding, only: c_int32_t,c_loc

   !<Inputs/Outputs
    integer(c_int32_t),dimension(*),target,intent(in)    :: send_data
    integer(INT_KIND),intent(in)                         :: send_data_size
    integer(c_int32_t),dimension(*),target,intent(inout) :: recv_data
    integer(INT_KIND),intent(in)                         :: recv_data_size
    integer(INT_KIND)                                    :: send_len
    integer(INT_KIND)                                    :: recv_len
    integer(c_int32_t),intent(in)                        :: mpp_type
    integer(INT_KIND),intent(in)                         :: n
    integer(c_int32_t),dimension(n),intent(in)           :: pelist

    call mpp_alltoall_c_style(c_loc(send_data),send_data_size, &
                              c_loc(recv_data),recv_data_size, &
                              send_len,recv_len,mpp_type,n,pelist)

  end subroutine mpp_alltoall_c_style_i32a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_alltoall_c_style_i64a(send_data,send_data_size,recv_data, &
                                       recv_data_size,send_len,recv_len, &
                                       mpp_type,n,pelist)
    use iso_c_binding, only: c_int64_t,c_int32_t,c_loc

   !<Inputs/Outputs
    integer(c_int64_t),dimension(*),target,intent(in)    :: send_data
    integer(INT_KIND),intent(in)                         :: send_data_size
    integer(c_int64_t),dimension(*),target,intent(inout) :: recv_data
    integer(INT_KIND),intent(in)                         :: recv_data_size
    integer(INT_KIND)                                    :: send_len
    integer(INT_KIND)                                    :: recv_len
    integer(c_int32_t),intent(in)                        :: mpp_type
    integer(INT_KIND),intent(in)                         :: n
    integer(c_int32_t),dimension(n),intent(in)           :: pelist

    call mpp_alltoall_c_style(c_loc(send_data),send_data_size, &
                              c_loc(recv_data),recv_data_size, &
                              send_len,recv_len,mpp_type,n,pelist)

  end subroutine mpp_alltoall_c_style_i64a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_alltoall_c_style_r32a(send_data,send_data_size,recv_data, &
                                       recv_data_size,send_len,recv_len, &
                                       mpp_type,n,pelist)
    use iso_c_binding, only: c_float,c_int32_t,c_loc

   !<Inputs/Outputs
    real(c_float),dimension(*),target,intent(in)    :: send_data
    integer(INT_KIND),intent(in)                    :: send_data_size
    real(c_float),dimension(*),target,intent(inout) :: recv_data
    integer(INT_KIND),intent(in)                    :: recv_data_size
    integer(INT_KIND)                               :: send_len
    integer(INT_KIND)                               :: recv_len
    integer(c_int32_t),intent(in)                   :: mpp_type
    integer(INT_KIND),intent(in)                    :: n
    integer(c_int32_t),dimension(n),intent(in)      :: pelist

    call mpp_alltoall_c_style(c_loc(send_data),send_data_size, &
                              c_loc(recv_data),recv_data_size, &
                              send_len,recv_len,mpp_type,n,pelist)

  end subroutine mpp_alltoall_c_style_r32a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_alltoall_c_style_r64a(send_data,send_data_size,recv_data, &
                                       recv_data_size,send_len,recv_len, &
                                       mpp_type,n,pelist)
    use iso_c_binding, only: c_double,c_int32_t,c_loc

   !<Inputs/Outputs
    real(c_double),dimension(*),target,intent(in)    :: send_data
    integer(INT_KIND),intent(in)                     :: send_data_size
    real(c_double),dimension(*),target,intent(inout) :: recv_data
    integer(INT_KIND),intent(in)                     :: recv_data_size
    integer(INT_KIND)                                :: send_len
    integer(INT_KIND)                                :: recv_len
    integer(c_int32_t),intent(in)                    :: mpp_type
    integer(INT_KIND),intent(in)                     :: n
    integer(c_int32_t),dimension(n),intent(in)       :: pelist

    call mpp_alltoall_c_style(c_loc(send_data),send_data_size, &
                              c_loc(recv_data),recv_data_size, &
                              send_len,recv_len,mpp_type,n,pelist)

  end subroutine mpp_alltoall_c_style_r64a

 !-----------------------------------------------------------------------------
 !>Perform alltoall communication with all ranks on the pelist.
  subroutine mpp_alltoall_c_style(send_data,send_data_size,recv_data, &
                                  recv_data_size,send_len,recv_len, &
                                  mpp_type,n,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    type(c_ptr),intent(in)                     :: send_data
    integer(INT_KIND),intent(in)               :: send_data_size
    type(c_ptr)                                :: recv_data
    integer(INT_KIND),intent(in)               :: recv_data_size
    integer(INT_KIND)                          :: send_len
    integer(INT_KIND)                          :: recv_len
    integer(c_int32_t),intent(in)              :: mpp_type
    integer(INT_KIND),intent(in)               :: n
    integer(c_int32_t),dimension(n),intent(in) :: pelist

    call iso_c_mpp_alltoall(send_data,int(send_data_size,kind=c_size_t), &
                            recv_data,int(recv_data_size,kind=c_size_t), &
                            int(send_len,kind=c_size_t), &
                            int(recv_len,kind=c_size_t),mpp_type, &
                            pelist,int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_alltoall_c_style

 !-----------------------------------------------------------------------------

