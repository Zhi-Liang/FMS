 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_alltoallv_c_style_i32a(send_data,sln,send_len,sdn,send_displace, &
                                        recv_data,rln,recv_len,rdn,recv_displace, &
                                        mpp_type,n,pelist)
    use iso_c_binding, only: c_int32_t,c_loc

   !<Inputs/Outputs
    integer(c_int32_t),dimension(*),target,intent(in)    :: send_data
    integer(INT_KIND),intent(in)                         :: sln
    integer(c_int32_t),dimension(sln),intent(in)         :: send_len
    integer(INT_KIND),intent(in)                         :: sdn
    integer(c_int32_t),dimension(sdn),intent(in)         :: send_displace
    integer(c_int32_t),dimension(*),target,intent(inout) :: recv_data
    integer(INT_KIND),intent(in)                         :: rln
    integer(c_int32_t),dimension(rln),intent(in)         :: recv_len
    integer(INT_KIND),intent(in)                         :: rdn
    integer(c_int32_t),dimension(rdn),intent(in)         :: recv_displace
    integer(c_int32_t),intent(in)                        :: mpp_type
    integer(INT_KIND),intent(in)                         :: n
    integer(c_int32_t),dimension(n),intent(in)           :: pelist

    call mpp_alltoallv_c_style(c_loc(send_data),sln,send_len,sdn,send_displace, &
                               c_loc(recv_data),rln,recv_len,rdn,recv_displace, &
                               mpp_type,n,pelist)

  end subroutine mpp_alltoallv_c_style_i32a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_alltoallv_c_style_i64a(send_data,sln,send_len,sdn,send_displace, &
                                        recv_data,rln,recv_len,rdn,recv_displace, &
                                        mpp_type,n,pelist)
    use iso_c_binding, only: c_int64_t,c_int32_t,c_loc

   !<Inputs/Outputs
    integer(c_int64_t),dimension(*),target,intent(in)    :: send_data
    integer(INT_KIND),intent(in)                         :: sln
    integer(c_int32_t),dimension(sln),intent(in)         :: send_len
    integer(INT_KIND),intent(in)                         :: sdn
    integer(c_int32_t),dimension(sdn),intent(in)         :: send_displace
    integer(c_int64_t),dimension(*),target,intent(inout) :: recv_data
    integer(INT_KIND),intent(in)                         :: rln
    integer(c_int32_t),dimension(rln),intent(in)         :: recv_len
    integer(INT_KIND),intent(in)                         :: rdn
    integer(c_int32_t),dimension(rdn),intent(in)         :: recv_displace
    integer(c_int32_t),intent(in)                        :: mpp_type
    integer(INT_KIND),intent(in)                         :: n
    integer(c_int32_t),dimension(n),intent(in)           :: pelist

    call mpp_alltoallv_c_style(c_loc(send_data),sln,send_len,sdn,send_displace, &
                               c_loc(recv_data),rln,recv_len,rdn,recv_displace, &
                               mpp_type,n,pelist)

  end subroutine mpp_alltoallv_c_style_i64a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_alltoallv_c_style_r32a(send_data,sln,send_len,sdn,send_displace, &
                                        recv_data,rln,recv_len,rdn,recv_displace, &
                                        mpp_type,n,pelist)
    use iso_c_binding, only: c_float,c_int32_t,c_loc

   !<Inputs/Outputs
    real(c_float),dimension(*),target,intent(in)    :: send_data
    integer(INT_KIND),intent(in)                    :: sln
    integer(c_int32_t),dimension(sln),intent(in)    :: send_len
    integer(INT_KIND),intent(in)                    :: sdn
    integer(c_int32_t),dimension(sdn),intent(in)    :: send_displace
    real(c_float),dimension(*),target,intent(inout) :: recv_data
    integer(INT_KIND),intent(in)                    :: rln
    integer(c_int32_t),dimension(rln),intent(in)    :: recv_len
    integer(INT_KIND),intent(in)                    :: rdn
    integer(c_int32_t),dimension(rdn),intent(in)    :: recv_displace
    integer(c_int32_t),intent(in)                   :: mpp_type
    integer(INT_KIND),intent(in)                    :: n
    integer(c_int32_t),dimension(n),intent(in)      :: pelist

    call mpp_alltoallv_c_style(c_loc(send_data),sln,send_len,sdn,send_displace, &
                               c_loc(recv_data),rln,recv_len,rdn,recv_displace, &
                               mpp_type,n,pelist)

  end subroutine mpp_alltoallv_c_style_r32a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_alltoallv_c_style_r64a(send_data,sln,send_len,sdn,send_displace, &
                                        recv_data,rln,recv_len,rdn,recv_displace, &
                                        mpp_type,n,pelist)
    use iso_c_binding, only: c_double,c_int32_t,c_loc

   !<Inputs/Outputs
    real(c_double),dimension(*),target,intent(in)    :: send_data
    integer(INT_KIND),intent(in)                     :: sln
    integer(c_int32_t),dimension(sln),intent(in)     :: send_len
    integer(INT_KIND),intent(in)                     :: sdn
    integer(c_int32_t),dimension(sdn),intent(in)     :: send_displace
    real(c_double),dimension(*),target,intent(inout) :: recv_data
    integer(INT_KIND),intent(in)                     :: rln
    integer(c_int32_t),dimension(rln),intent(in)     :: recv_len
    integer(INT_KIND),intent(in)                     :: rdn
    integer(c_int32_t),dimension(rdn),intent(in)     :: recv_displace
    integer(c_int32_t),intent(in)                    :: mpp_type
    integer(INT_KIND),intent(in)                     :: n
    integer(c_int32_t),dimension(n),intent(in)       :: pelist

    call mpp_alltoallv_c_style(c_loc(send_data),sln,send_len,sdn,send_displace, &
                               c_loc(recv_data),rln,recv_len,rdn,recv_displace, &
                               mpp_type,n,pelist)

  end subroutine mpp_alltoallv_c_style_r64a

 !-----------------------------------------------------------------------------
 !>Perform variable alltoall communication with all ranks on the pelist.
  subroutine mpp_alltoallv_c_style(send_data,sln,send_len,sdn,send_displace, &
                                   recv_data,rln,recv_len,rdn,recv_displace, &
                                   mpp_type,n,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    type(c_ptr),intent(in)                       :: send_data
    integer(INT_KIND),intent(in)                 :: sln
    integer(c_int32_t),dimension(sln),intent(in) :: send_len
    integer(INT_KIND),intent(in)                 :: sdn
    integer(c_int32_t),dimension(sdn),intent(in) :: send_displace
    type(c_ptr)                                  :: recv_data
    integer(INT_KIND),intent(in)                 :: rln
    integer(c_int32_t),dimension(rln),intent(in) :: recv_len
    integer(INT_KIND),intent(in)                 :: rdn
    integer(c_int32_t),dimension(rdn),intent(in) :: recv_displace
    integer(c_int32_t),intent(in)                :: mpp_type
    integer(INT_KIND),intent(in)                 :: n
    integer(c_int32_t),dimension(n),intent(in)   :: pelist

    call iso_c_mpp_alltoallv(send_data,send_len,send_displace,recv_data, &
                             recv_len,recv_displace,mpp_type,pelist, &
                             int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_alltoallv_c_style

 !-----------------------------------------------------------------------------

