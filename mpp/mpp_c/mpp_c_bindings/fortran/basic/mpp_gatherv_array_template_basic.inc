 !-----------------------------------------------------------------------------
 !>mpp_gatherv template for array data.
  subroutine _A_NAME(send_data,send_len,recv_data,recv_len_array, &
                     pelist)
    use iso_c_binding, only: c_int32_t,c_size_t _C_TYPE

   !<Inputs/Outputs
    _MPP_TYPE,_DIMENSION,intent(in)                     :: send_data
    integer(INT_KIND),intent(in)                        :: send_len
    _MPP_TYPE,_DIMENSION,intent(inout)                  :: recv_data
    integer(INT_KIND),dimension(:),intent(in)           :: recv_len_array
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1)            :: null_pelist
    integer(c_size_t),dimension(:),allocatable :: tmp_recv_len_array
    integer(INT_KIND)                          :: i

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(send_data)) then
!       call mpp_error(FATAL,'_A_NAME: data is not contiguous.')
!   endif
!   if (.not. is_contiguous(recv_data)) then
!       call mpp_error(FATAL,'_A_NAME: data is not contiguous.')
!   endif

   !<Convert the recv_len_array to the correct type.
    allocate(tmp_recv_len_array(size(recv_len_array)))
    do i = 1,size(recv_len_array)
        tmp_recv_len_array(i) = int(recv_len_array(i),kind=c_size_t)
    enddo

    if (present(pelist)) then
        call _A_SUB(send_data,send_len,recv_data, &
                    size(recv_data),size(tmp_recv_len_array), &
                    tmp_recv_len_array,_MPP_PARAM, &
                    size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call _A_SUB(send_data,send_len,recv_data, &
                    size(recv_data),size(tmp_recv_len_array), &
                    tmp_recv_len_array,_MPP_PARAM, &
                    1,null_pelist)
    endif

   !<Deallocate local allocatables.
    deallocate(tmp_recv_len_array)

  end subroutine _A_NAME

 !-----------------------------------------------------------------------------
