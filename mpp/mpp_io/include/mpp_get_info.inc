!Get the number of dimensions, variables, and global attributes in a netcdf
!file.
subroutine mpp_get_info(unit, &
                        ndim, &
                        nvar, &
                        natt, &
                        ntime)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    integer(kind=c_int),intent(out) :: ndim
    integer(kind=c_int),intent(out) :: nvar
    integer(kind=c_int),intent(out) :: natt
    integer(kind=c_int),intent(out) :: ntime

    call openmp_thread_trap()
    call mpp_get_info_(context, &
                       unit, &
                       ndim, &
                       nvar, &
                       natt, &
                       ntime)

end subroutine mpp_get_info

subroutine mpp_get_info_(context, &
                         file_index, &
                         num_dims, &
                         num_vars, &
                         num_global_atts, &
                         unlim_dim_length)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    integer(kind=c_int),intent(out) :: num_dims
    integer(kind=c_int),intent(out) :: num_vars
    integer(kind=c_int),intent(out) :: num_global_atts
    integer(kind=c_int),intent(out) :: unlim_dim_length

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: i
    integer(kind=c_int) :: dim_index
    integer(kind=c_int) :: var_index
    type(c_ptr) :: ptr
    integer(kind=c_int) :: tot_num_vars

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = get_c_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    num_dims = -1
    num_vars = -1
    num_global_atts = -1
    unlim_dim_length = -1

    if (allocated(context%files(file_index)%dim_indices)) then
        num_dims = size(context%files(file_index)%dim_indices)
        do i = 1,num_dims
            dim_index = context%files(file_index)%dim_indices(i)
            if (mpp_io_c_get_netcdf_dimension_is_unlimited(f, &
                                                           dim_index)) then
                unlim_dim_length = mpp_io_c_get_netcdf_dimension_current_level(f, &
                                                                               dim_index)
                exit
            endif
        enddo
    endif

    if (allocated(context%files(file_index)%vars)) then
        tot_num_vars = size(context%files(file_index)%vars)

        !Apparently variables that hold dimension values do not count,
        !so subtract them away.
        ptr = c_null_ptr
        num_vars = 0
        do i = 1,tot_num_vars
            var_index = context%files(file_index)%vars(i)%var_index

            !Get the name of the variable.
            ptr = mpp_io_c_get_netcdf_variable_name(f, &
                                                    var_index)

            !Look for a matching dimension.
            dim_index = mpp_io_c_get_netcdf_dimension_index(f, &
                                                            ptr)
            ptr = c_null_ptr

            if (dim_index .eq. MPP_IO_C_INDEX_NOT_FOUND) then
                num_vars = num_vars + 1
            endif
        enddo
    endif

    if (allocated(context%files(file_index)%global_att_indices)) then
        num_global_atts = size(context%files(file_index)%global_att_indices)
    endif

end subroutine mpp_get_info_
