subroutine mpp_ascii_fread_i4_1d(file_index, &
                                 num_vals, &
                                 data_buf, &
                                 delimiter, &
                                 val_width)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: file_index
    integer(kind=c_int),intent(in) :: num_vals
    integer(kind=c_int),dimension(:),intent(inout) :: data_buf
    character(len=1),intent(in) :: delimiter
    integer(kind=c_int),intent(in) :: val_width

    call openmp_thread_trap()
    call mpp_ascii_fread_i4_1d_(context, &
                                file_index, &
                                num_vals, &
                                int(size(data_buf,1),kind=c_int), &
                                data_buf, &
                                delimiter, &
                                val_width)

end subroutine mpp_ascii_fread_i4_1d

subroutine mpp_ascii_fread_i4_1d_(context, &
                                  file_index, &
                                  num_vals, &
                                  ds1, &
                                  data_buf, &
                                  delimiter, &
                                  val_width)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    integer(kind=c_int),intent(in) :: num_vals
    integer(kind=c_int),intent(in) :: ds1
    integer(kind=c_int),dimension(ds1),target,intent(inout) :: data_buf
    character(kind=c_char,len=1),intent(in) :: delimiter
    integer(kind=c_int),intent(in) :: val_width

    !Local variables
    integer(kind=c_int) :: f

    f = check_file_index(context, &
                         file_index)

    if (context%files(file_index)%is_reader) then
        call mpp_io_c_fread_regular_file(f, &
                                         num_vals, &
                                         int(MPP_IO_C_INT,kind=c_int), &
                                         val_width, &
                                         delimiter, &
                                         c_loc(data_buf), &
                                         .false._c_bool)
    endif

    call mpp_broadcast(data_buf, &
                       num_vals, &
                       context%files(file_index)%rank_list_root, &
                       context%files(file_index)%rank_list)

end subroutine mpp_ascii_fread_i4_1d_












subroutine mpp_ascii_fread_r8_3d(file_index, &
                                 num_vals, &
                                 data_buf, &
                                 delimiter, &
                                 val_width)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: file_index
    integer(kind=c_int),intent(in) :: num_vals
    real(kind=c_double),dimension(:,:,:),intent(inout) :: data_buf
    character(len=1),intent(in) :: delimiter
    integer(kind=c_int),intent(in) :: val_width

    call openmp_thread_trap()
    call mpp_ascii_fread_r8_3d_(context, &
                                file_index, &
                                num_vals, &
                                int(size(data_buf,1),kind=c_int), &
                                int(size(data_buf,2),kind=c_int), &
                                int(size(data_buf,3),kind=c_int), &
                                data_buf, &
                                delimiter, &
                                val_width)

end subroutine mpp_ascii_fread_r8_3d

subroutine mpp_ascii_fread_r8_3d_(context, &
                                  file_index, &
                                  num_vals, &
                                  ds1, &
                                  ds2, &
                                  ds3, &
                                  data_buf, &
                                  delimiter, &
                                  val_width)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    integer(kind=c_int),intent(in) :: num_vals
    integer(kind=c_int),intent(in) :: ds1
    integer(kind=c_int),intent(in) :: ds2
    integer(kind=c_int),intent(in) :: ds3
    real(kind=c_double),dimension(ds1,ds2,ds3),target,intent(inout) :: data_buf
    character(kind=c_char,len=1),intent(in) :: delimiter
    integer(kind=c_int),intent(in) :: val_width

    !Local variables
    integer(kind=c_int) :: f

    f = check_file_index(context, &
                         file_index)

    if (context%files(file_index)%is_reader) then
        call mpp_io_c_fread_regular_file(f, &
                                         num_vals, &
                                         int(MPP_IO_C_DOUBLE,kind=c_int), &
                                         val_width, &
                                         delimiter, &
                                         c_loc(data_buf), &
                                         .false._c_bool)
    endif

    call mpp_broadcast(data_buf, &
                       num_vals, &
                       context%files(file_index)%rank_list_root, &
                       context%files(file_index)%rank_list)

end subroutine mpp_ascii_fread_r8_3d_

subroutine mpp_ascii_fread_r8_5d(file_index, &
                                 num_vals, &
                                 data_buf, &
                                 delimiter, &
                                 val_width)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: file_index
    integer(kind=c_int),intent(in) :: num_vals
    real(kind=c_double),dimension(:,:,:,:,:),intent(inout) :: data_buf
    character(len=1),intent(in) :: delimiter
    integer(kind=c_int),intent(in) :: val_width

    call openmp_thread_trap()
    call mpp_ascii_fread_r8_5d_(context, &
                                file_index, &
                                num_vals, &
                                int(size(data_buf,1),kind=c_int), &
                                int(size(data_buf,2),kind=c_int), &
                                int(size(data_buf,3),kind=c_int), &
                                int(size(data_buf,4),kind=c_int), &
                                int(size(data_buf,5),kind=c_int), &
                                data_buf, &
                                delimiter, &
                                val_width)

end subroutine mpp_ascii_fread_r8_5d

subroutine mpp_ascii_fread_r8_5d_(context, &
                                  file_index, &
                                  num_vals, &
                                  ds1, &
                                  ds2, &
                                  ds3, &
                                  ds4, &
                                  ds5, &
                                  data_buf, &
                                  delimiter, &
                                  val_width)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    integer(kind=c_int),intent(in) :: num_vals
    integer(kind=c_int),intent(in) :: ds1
    integer(kind=c_int),intent(in) :: ds2
    integer(kind=c_int),intent(in) :: ds3
    integer(kind=c_int),intent(in) :: ds4
    integer(kind=c_int),intent(in) :: ds5
    real(kind=c_double),dimension(ds1,ds2,ds3,ds4,ds5),target,intent(inout) :: data_buf
    character(kind=c_char,len=1),intent(in) :: delimiter
    integer(kind=c_int),intent(in) :: val_width

    !Local variables
    integer(kind=c_int) :: f

    f = check_file_index(context, &
                         file_index)

    if (context%files(file_index)%is_reader) then
        call mpp_io_c_fread_regular_file(f, &
                                         num_vals, &
                                         int(MPP_IO_C_DOUBLE,kind=c_int), &
                                         val_width, &
                                         delimiter, &
                                         c_loc(data_buf), &
                                         .false._c_bool)
    endif

    call mpp_broadcast(data_buf, &
                       num_vals, &
                       context%files(file_index)%rank_list_root, &
                       context%files(file_index)%rank_list)

end subroutine mpp_ascii_fread_r8_5d_















!
!subroutine mpp_ascii_fread_a(file_index, &
!                             data_buf)
!    implicit none
!
!    !Inputs/outputs
!    integer(kind=c_int),intent(in) :: file_index
!    character(len=*),dimension(:),intent(inout) :: data
!
!end subroutine mpp_ascii_fread_a




subroutine mpp_read_distributed_ascii_a1d(unit, &
                                          fmt, &
                                          ssize, &
                                          data, &
                                          iostat)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    character(*),intent(in) :: fmt
    integer(kind=c_int),intent(in) :: ssize
    character(len=*),dimension(:),intent(inout) :: data
    integer(kind=c_int),intent(out) :: iostat

    call mpp_error(FATAL, &
                   "mpp_read_distribruted_ascii_a1d is not currently supported.")

end subroutine mpp_read_distributed_ascii_a1d

subroutine mpp_read_distributed_ascii_i1d(unit, &
                                          fmt, &
                                          ssize, &
                                          data, &
                                          iostat)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    character(*),intent(in) :: fmt
    integer(kind=c_int),intent(in) :: ssize
    integer(kind=c_int),dimension(:),intent(inout) :: data
    integer(kind=c_int),intent(out) :: iostat

    call mpp_error(FATAL, &
                   "mpp_read_distribruted_ascii_i1d is not currently supported.")

end subroutine mpp_read_distributed_ascii_i1d

subroutine mpp_read_distributed_ascii_r1d(unit, &
                                          fmt, &
                                          ssize, &
                                          data, &
                                          iostat)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    character(*),intent(in) :: fmt
    integer(kind=c_int),intent(in) :: ssize
    real(kind=c_double),dimension(:),intent(inout) :: data
    integer(kind=c_int),intent(out) :: iostat

    call mpp_error(FATAL, &
                   "mpp_read_distribruted_ascii_r1d is not currently supported.")

end subroutine mpp_read_distributed_ascii_r1d
