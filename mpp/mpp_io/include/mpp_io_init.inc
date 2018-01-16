!Initialize the mpp_io module.
subroutine mpp_io_init(flags, &
                       maxunit)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in),optional :: flags
    integer(kind=c_int),intent(in),optional :: maxunit

    !Local variables
    integer(kind=c_int) :: min_nml_funit
    integer(kind=c_int) :: max_nml_funit
    integer(kind=c_int) :: funit
    logical :: is_open
    integer(kind=c_int) :: io_status
    integer(kind=c_int32_t) :: stdout_unit
    integer(kind=c_int32_t) :: stdlog_unit
    integer(kind=c_int) :: max_num_netcdf_files
    integer(kind=c_int) :: max_num_regular_files
    integer(kind=c_int) :: max_num_hdf5_files
    logical(kind=c_bool) :: debug_flag
    logical(kind=c_bool) :: verbose_flag

    !Do not allow this routine to be executed in a threaded region.
    call openmp_thread_trap()

    !Return if the module has been previously initialized.
    if (module_is_initialized) then
        call mpp_error(NOTE, &
                       "mpp_io_init: mpp_io has already been initialized.")
        return
    endif

    !Initialize mpp.
    call mpp_init(flags)

    !Read in the namelist values.
#ifdef INTERNAL_FILE_NML
    read(input_nml_file, &
         mpp_io_nml, &
         iostat=io_status)
#else
    min_nml_funit = 10
    max_nml_funit = 100
    do funit = min_nml_funit,max_nml_funit
         if (funit .ne. output_unit .and. funit .ne. input_unit .and. &
                 funit .ne. error_unit) then
             inquire(funit, &
                     opened=is_open)
             if (.not. is_open) then
                 exit
             endif
         endif
    enddo
    if (funit .eq. max_nml_funit + 1) then
        call mpp_error(FATAL, &
                       "mpp_io_init: could not find an available fortran" &
                           //" file unit to use for the input.nml file read.")
    endif
    open(funit, &
         file='input.nml')
    read(funit, &
         mpp_io_nml, &
         iostat=io_status)
    close(funit)
#endif

    !Make sure that the namelist read was succesful.
    if (io_status .gt. 0) then
        call mpp_error(FATAL, &
                       "mpp_io_init: an error occurred while reading the" &
                           //" mpp_io namelist from the input.nml file.")
    endif

    !Write the mpp_io namelist to stdout and the logfile.
    stdout_unit = stdout()
    stdlog_unit = stdlog()
    write(stdout_unit, &
          mpp_io_nml)
    write(stdlog_unit, &
          mpp_io_nml)

    !Set flag values.
    if (present(maxunit)) then
        max_num_netcdf_files = maxunit
        max_num_regular_files = maxunit
        max_num_hdf5_files = maxunit
    else
        max_num_netcdf_files = MPP_IO_MAX_NUM_NETCDF_FILES
        max_num_regular_files = MPP_IO_MAX_NUM_REGULAR_FILES
        max_num_hdf5_files = MPP_IO_MAX_NUM_HDF5_FILES
    endif
    debug_flag = .false._c_bool
    verbose_flag = .false._c_bool
    if (present(flags)) then
        if (flags .eq. MPP_IO_DEBUG) then
            debug_flag = .true._c_bool
            verbose_flag = .true._c_bool
        elseif (flags .eq. MPP_IO_VERBOSE) then
            verbose_flag = .true._c_bool
        endif
    endif

    !Initialize the mpp_io_c library.
    call mpp_io_c_init(max_num_netcdf_files, &
                       max_num_regular_files, &
                       max_num_hdf5_files, &
                       debug_flag, &
                       verbose_flag, &
                       header_buffer_val, &
                       shuffle, &
                       deflate, &
                       deflate_level)

    !Initialize the mpp_domains module.
    call mpp_domains_init(flags)

    !Initialize the context for the mpp_io module.  Currently, only one
    !context is defined.  This can be extended in the future.
    call mpp_io_context_init(context, &
                             max_num_netcdf_files + max_num_regular_files &
                                 + max_num_hdf5_files)

    !Set the module state as initialized.
    module_is_initialized = .true._c_bool

    !Legacy behavior, to support backwards compatibility.
    if (present(maxunit)) then
        maxunits = maxunit
    endif

    default_att%type = -1
    default_att%len = -1
    default_att%name = "noname"
    default_att%catt = "none"

    default_axis%name = "noname"
!   default_axis%name_bounds = ""
    default_axis%units = "nounits"
    default_axis%longname = "noname"
    default_axis%cartesian = "none"
    default_axis%compressed = "unspecified"
    default_axis%calendar = "unspecified"
    default_axis%sense = 0
    default_axis%len = -1
    default_axis%domain => null()
    default_axis%id = -1
    default_axis%did = -1
    default_axis%type = -1
    default_axis%natt = -1
!   default_axis%shift = 0

    default_field%name = "noname"
    default_field%units = "nounits"
    default_field%longname = "noname"
!   default_field%standard_name = ""
    default_field%min = -huge(1._4)
    default_field%max =  huge(1._4)
    default_field%missing = MPP_FILL_DOUBLE
    default_field%fill = MPP_FILL_DOUBLE
    default_field%scale = 1.0
    default_field%add = 0.0
    default_field%pack = 1
    default_field%checksum = 0
    default_field%time_axis_index = -1
    default_field%id = -1
    default_field%type = -1
    default_field%natt = -1
    default_field%ndim = -1
!   default_field%position = 0

end subroutine mpp_io_init
