subroutine mpp_get_tavg_info(unit, &
                             field, &
                             fields, &
                             tstamp, &
                             tstart, &
                             tend, &
                             tavg)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    type(fieldtype),dimension(:),intent(in) :: fields
    real(kind=c_double),dimension(:),intent(inout) :: tstamp
    real(kind=c_double),dimension(:),intent(inout) :: tstart
    real(kind=c_double),dimension(:),intent(inout) :: tend
    real(kind=c_double),dimension(:),intent(inout) :: tavg

    call openmp_thread_trap()
    call mpp_get_tavg_info_(context, &
                            unit, &
                            field, &
                            fields, &
                            tstamp, &
                            tstart, &
                            tend, &
                            tavg)

end subroutine mpp_get_tavg_info

subroutine mpp_get_tavg_info_(context, &
                              file_index, &
                              field, &
                              fields, &
                              tstamp, &
                              tstart, &
                              tend, &
                              tavg)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(fieldtype),intent(in) :: field
    type(fieldtype),dimension(:),intent(in) :: fields
    real(kind=c_double),dimension(:),intent(inout) :: tstamp
    real(kind=c_double),dimension(:),intent(inout) :: tstart
    real(kind=c_double),dimension(:),intent(inout) :: tend
    real(kind=c_double),dimension(:),intent(inout) :: tavg

    !Local variables
    logical(kind=c_bool) :: tavg_info_exists
    integer(kind=c_int) :: n
    integer(kind=c_int) :: m

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    call check_file_index(context, &
                          file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    !Check array sizes.
    if (size(tstamp,1) .ne. size(tstart,1)) then
        call mpp_error(FATAL, &
                       "mpp_get_tavg_info_: inputted tstart and tstamp" &
                           //" arrays must be the same size.")
    endif

    if ((size(tstart,1) .ne. size(tend,1)) .or. (size(tstart,1) .ne. &
            size(tavg,1))) then
        call mpp_error(FATAL, &
                       "mpp_get_tavg_info_: the inputted tstart array must" &
                           //" be the same size as the inputted tend and" &
                           //" tavg arrays.")
    endif

    !Set initial values.
    tavg = -1.0
    tstart = tstamp
    tend = tstamp
    tavg_info_exists = .false._c_bool

    !Look for the "time_vag_info" attribute in the inputted array of
    !variables.
    do n = 1,field%natt
        if (field%Att(n)%type .eq. MPP_IO_C_NC_CHAR) then
            if (field%Att(n)%name(1:13) == "time_avg_info") then
                tavg_info_exists = .true._c_bool
                exit
            endif
        endif
    enddo

    !Read in the data.
    if (tavg_info_exists) then
        do n = 1,size(fields)
            if (trim(fields(n)%name) == "average_T1") then
                do m = 1,size(tstart)
                    call mpp_read(file_index, &
                                  fields(n),&
                                  tstart(m), &
                                  m)
                enddo
            elseif (trim(fields(n)%name) == "average_T2") then
                do m = 1,size(tend)
                    call mpp_read(file_index, &
                                  fields(n), &
                                  tend(m), &
                                  m)
                enddo
            elseif (trim(fields(n)%name) == "average_DT") then
                do m = 1,size(tavg)
                    call mpp_read(file_index, &
                                  fields(n), &
                                  tavg(m), &
                                  m)
                enddo
            endif
        enddo
    endif

end subroutine mpp_get_tavg_info_
