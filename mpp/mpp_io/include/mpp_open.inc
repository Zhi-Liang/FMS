!Open a file.
subroutine mpp_open(unit, &
                    file, &
                    action, &
                    form, &
                    access, &
                    threading, &
                    fileset, &
                    iospec, &
                    nohdrs, &
                    recl, &
                    iostat, &
                    is_root_pe, &
                    domain, &
                    domain_ug)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(out) :: unit
    character(len=*),intent(in) :: file
    integer(kind=c_int),intent(in),optional :: action
    integer(kind=c_int),intent(in),optional :: form
    integer(kind=c_int),intent(in),optional :: access
    integer(kind=c_int),intent(in),optional :: threading
    integer(kind=c_int),intent(in),optional :: fileset
    character(len=*),intent(in),optional :: iospec
    logical,intent(in),optional :: nohdrs
    integer(kind=c_int),intent(in),optional :: recl
    integer(kind=c_int),intent(out),optional :: iostat
    logical,intent(in),optional :: is_root_pe
    type(domain2D),intent(in),optional :: domain
    type(domainUG),intent(in),optional :: domain_ug

    call openmp_thread_trap()
    call mpp_open_(context, &
                   unit, &
                   file, &
                   action, &
                   form, &
                   is_root_pe, &
                   threading, &
                   fileset, &
                   domain, &
                   domain_ug)

end subroutine mpp_open

subroutine mpp_open_(context, &
                     file_index, &
                     file_name, &
                     file_action, &
                     file_format, &
                     is_root_rank, &
                     threading, &
                     fileset, &
                     domain, &
                     domain_ug)
    implicit none

    !Input/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(out) :: file_index
    character(len=*),intent(in) :: file_name
    integer(kind=c_int),intent(in),optional :: file_action
    integer(kind=c_int),intent(in),optional :: file_format
    logical,intent(in),optional :: is_root_rank
    integer(kind=c_int),intent(in),optional :: threading
    integer(kind=c_int),intent(in),optional :: fileset
    type(domain2D),intent(in),optional :: domain
    type(domainUG),intent(in),optional :: domain_ug

    !Local variables
    integer(kind=c_int) :: action_flag
    integer(kind=c_int) :: form_flag
    integer(kind=c_int) :: thread_flag
    integer(kind=c_int) :: fileset_flag
    integer(kind=c_int) :: fname_length
    character(kind=c_char,len=1),dimension(:),allocatable :: c_fname
    integer(kind=c_int) :: i
    logical(kind=c_bool) :: am_root
    integer(kind=c_int) :: pelist_root
    logical(kind=c_bool) :: is_reader
    logical(kind=c_bool) :: is_writer
    integer(kind=c_int) :: pelist_size

    type(domain2d) :: domain_
    type(domain2d) :: io_domain
    type(domainUG) :: domain_ug_
    type(domainUG) :: io_domain_ug

    integer(kind=c_int) :: nc_pos
    character(len=MPP_IO_MAX_FILENAME_LEN) :: fname_full
    integer(kind=c_int),dimension(2) :: io_layout
    integer(kind=c_int),dimension(1) :: io_domain_tile_id
    character(len=MPP_IO_MAX_FILENAME_LEN) :: fname_full_buffer
    character(len=MPP_IO_MAX_FILENAME_LEN) :: fname_full_buffer2
    logical :: fexists
    logical(kind=c_bool) :: data_in_multi_files
    logical(kind=c_bool) :: comm_required
    logical(kind=c_bool) :: is_io_domain_tile_root
    logical(kind=c_bool) :: multi_io_domain_tiles
    integer(kind=c_int) :: fname_suffix
    integer(kind=c_int) :: num_io_domain_tiles

    !Make sure that the module has been initialized.
    !file index was passed in.
    call check_is_init()

    !Determine which action should be taken on the file.  The default value
    !is write only.  Invalid values are checked on the c side.
    if (present(file_action)) then
        action_flag = file_action
    else
        action_flag = MPP_WRONLY
    endif

    !Determine the file format.  The default is an ascii file.
    if (present(file_format)) then
        if (file_format .ne. MPP_ASCII .and. file_format .ne. MPP_NETCDF) then
            call mpp_error(FATAL, &
                           "mpp_open_: the only currently supported file" &
                               //" formats are MPP_ASCII and MPP_NETCDF.")
        endif
        form_flag = file_format
    else
        form_flag = MPP_ASCII
    endif

    !Determine the "threading" for the file.  The default is single rank I/O.
    if (present(threading)) then
        if (threading .ne. MPP_SINGLE .and. threading .ne. MPP_MULTI) then
            call mpp_error(FATAL, &
                           "mpp_open_: the only currently supported" &
                               //" threading values are MPP_SINGLE and" &
                               //" MPP_MULTI.")
        endif
        thread_flag = threading
    else
        thread_flag = MPP_SINGLE
    endif

    !Determine the "fileset" for the file.  The default is that the file
    !is assumed to be part of a file set.  If single rank I/O will be
    !performed, then the file cannot be part of a file set.
    if (thread_flag .eq. MPP_SINGLE) then
        fileset_flag = MPP_SINGLE
    else
        if (present(fileset)) then
            if (fileset .ne. MPP_SINGLE .and. fileset .ne. MPP_MULTI) then
                call mpp_error(FATAL, &
                               "mpp_open_: the only currently supported" &
                                   //" fileset values are MPP_SINGLE and" &
                                   //" MPP_MULTI.")
            endif
            fileset_flag = fileset
        else
            fileset_flag = MPP_MULTI
        endif
    endif

    !Make sure that the inputted file name is not too long.
    fname_length = len_trim(file_name)
    if (fname_length .gt. MPP_IO_MAX_FILENAME_LEN-10) then
        call mpp_error(FATAL, &
                       "mpp_open_: the inputted filename (" &
                           //trim(file_name)//") is too long.")
    endif

    !Get an index in the context's files array where the data about the file
    !will be stored.
    file_index = 0
    do i = 1,context%cur_num_files
        if (.not. context%files(i)%in_use) then
            file_index = i
            exit
        endif
    enddo
    if (file_index .eq. 0) then
        context%cur_num_files = context%cur_num_files + 1
        if (context%cur_num_files .gt. size(context%files)) then
            call mpp_error(FATAL, &
                           "mpp_open_: the number of open files exceeds" &
                               //" the maximum number allowed.")
        endif
        file_index = context%cur_num_files
    endif
    context%files(file_index)%in_use = .true._c_bool

    if (form_flag .eq. MPP_ASCII) then

        !Allowed cases:
        !    1.) threading = single, fileset = single.
        !
        !        For this case only the root rank is allowed to read
        !        from/write to a file.  Here the data is not spread across
        !        multiple files.  If more than one rank exists on the current
        !        pelist, then communication is required.
        !
        !    2.) threading = single, fileset = multiple.
        !
        !        This case is not actually allowed to run.  It instead
        !        runs the threading = single, fileset = single case.

        !Throw an error if any non supported cases are detected.
        if (thread_flag .ne. MPP_SINGLE .or. fileset_flag .ne. MPP_SINGLE) then
            call mpp_error(FATAL, &
                           "mpp_open_: only single rank ascii reads/writes" &
                               //" are currently supported.")
        endif

        !Determine the I/O root.
        if (mpp_npes() .gt. 1) then

            !If the is_root_rank flag is passed in, use that.  If not, the
            !I/O root is the root rank of the current pelist.
            am_root = .false._c_bool
            if (present(is_root_rank)) then
                am_root = is_root_rank
            else
                if (mpp_pe() .eq. mpp_root_pe()) then
                    am_root = .true._c_bool
                endif
            endif

            !Let all ranks know who the root rank is.  Also check that only one
            !rank is the root.
            if (am_root) then
                pelist_root = mpp_pe()
            else
                pelist_root = -1
            endif
            call mpp_max(pelist_root)
            if (pelist_root .eq. -1) then
                call mpp_error(FATAL, &
                               "mpp_open_: there must be at least one rank" &
                                   //" designated as root to perform the I/O.")
            else
                if (am_root .and. pelist_root .ne. mpp_pe()) then
                    call mpp_error(FATAL, &
                                   "mpp_open_: only one rank is allowed to be" &
                                       //" the I/O root.")
                endif
            endif

            !Set the communication required flag to true.
            comm_required = .true._c_bool

        else
            am_root = .true._c_bool
            comm_required = .false._c_bool
        endif

        !Set the is reader and is writer flags.
        is_reader = .false._c_bool
        is_writer = .false._c_bool
        if (am_root) then
            if (action_flag .eq. MPP_RDONLY .or. action_flag .eq. &
                    MPP_APPEND) then
                is_reader = .true._c_bool
            endif
            if (action_flag .eq. MPP_WRONLY .or. action_flag .eq. &
                    MPP_OVERWR .or. action_flag .eq. MPP_APPEND) then
                is_writer = .true._c_bool
            endif
        endif

        if (is_reader .or. is_writer) then

            !Copy the file name into a buffer that is interoperable with c.
            call f_string_to_c(file_name, &
                               c_fname)

            !Open the file.
            context%files(file_index)%c_file_index = mpp_io_c_open_regular_file(c_fname, &
                                                                                action_flag)
            deallocate(c_fname)
        endif

        if (comm_required) then

            !Store the pelist necessary for communication.
            pelist_size = mpp_npes()
            allocate(context%files(file_index)%rank_list(pelist_size))
            call mpp_get_current_pelist(context%files(file_index)%rank_list)
            context%files(file_index)%rank_list_root = pelist_root
        endif

        !Store the is_reader and is_writer flags for the file in the context.
        context%files(file_index)%is_reader = is_reader
        context%files(file_index)%is_writer = is_writer

        !Store the file format flag.
        context%files(file_index)%file_type = form_flag

        !Mark the data as not spread amongst multiple files.
        context%files(file_index)%data_in_multi_files = .false._c_bool

        !Mark whether communication is required for this file.
        context%files(file_index)%comm_required = comm_required

    elseif (form_flag .eq. MPP_NETCDF) then

        !Throw a fatal error if both a structured and unstructured mpp domain
        !are passed in.
        if (present(domain) .and. present(domain_ug)) then
            call mpp_error(FATAL, &
                           "mpp_open_: domain and domain_ug cannot both be" &
                               //" present in the same mpp_open_ call.")
        endif

        !If present, store a pointer to the inputted domains.
        domain_ = NULL_DOMAIN2D
        io_domain = NULL_DOMAIN2D
        domain_ug_ = NULL_DOMAINUG
        io_domain_ug = NULL_DOMAINUG
        if (present(domain)) then
            domain_ = domain
            if (mpp_domain_is_defined(domain)) then
                io_domain = mpp_get_io_domain(domain)
            endif
        elseif (present(domain_ug)) then
            domain_ug_ = domain_ug
            if (mpp_domainUG_is_defined(domain_ug)) then
                io_domain_ug = mpp_get_UG_io_domain(domain_ug)
            endif
        endif

        if (mpp_domain_is_defined(io_domain) .or. &
            mpp_domainUG_is_defined(io_domain_ug)) then

            !Handle cases that involve an I/O domain.

            if (mpp_domain_is_defined(io_domain)) then
                pelist_root = mpp_get_domain_tile_root_pe(io_domain)
                io_layout = mpp_get_io_domain_layout(domain_)
                num_io_domain_tiles = io_layout(1)*io_layout(2)

            else
                call mpp_get_UG_domain_tile_pe_inf(io_domain_ug, &
                                                   root_pe=pelist_root)
                num_io_domain_tiles = mpp_get_io_domain_UG_layout(domain_ug_)

            endif

            !Determine if the current rank is an I/O domain tile root.
            if (mpp_pe() .eq. pelist_root) then
                is_io_domain_tile_root = .true._c_bool
            else
                is_io_domain_tile_root = .false._c_bool
            endif

            !Determine whether the domain contains more than one I/O domain
            !tile.
            if (num_io_domain_tiles .gt. 1) then
                multi_io_domain_tiles = .true._c_bool
            else
                multi_io_domain_tiles = .false._c_bool
            endif

            if (action_flag .eq. MPP_RDONLY) then

                !No writing allowed.
                is_writer = .false._c_bool

                if (multi_io_domain_tiles) then

                    !All ranks read if there are more than one I/O domain
                    !tiles.  No communication required.
                    is_reader = .true._c_bool
                    comm_required = .false._c_bool
                else

                    !Only the I/O domain tile root can read when there
                    !is only one I/O domain tile.  Communication is
                    !required.
                    if (is_io_domain_tile_root) then
                        is_reader = .true._c_bool
                    else
                        is_reader = .false._c_bool
                    endif
                    comm_required = .true._c_bool
                endif

            elseif (action_flag .eq. MPP_WRONLY .or. action_flag .eq. &
                        MPP_OVERWR) then

                !No reading allowed.
                is_reader = .false._c_bool

                !Only the I/O domain tile root can write.  Communication
                !is required.
                if (is_io_domain_tile_root) then
                    is_writer = .true._c_bool
                else
                    is_writer = .false._c_bool
                endif
                comm_required = .true._c_bool

            else

                !Only the I/O domain tile root can append (both read
                !and write.)  Communication is required.
                if (is_io_domain_tile_root) then
                    is_reader = .true._c_bool
                    is_writer = .true._c_bool
                else
                    is_reader = .false._c_bool
                    is_writer = .false._c_bool
                endif
                comm_required = .true._c_bool

            endif

            !Determine if the domain data will be spread over more than
            !one file.
            data_in_multi_files = multi_io_domain_tiles

        else

            !Handle cases that are independent of an I/O domain.

            !Initialize flags.
            is_reader = .false._c_bool
            is_writer = .false._c_bool
            am_root = .false._c_bool
            pelist_root = -1

            if (thread_flag .eq. MPP_SINGLE) then

                !Only the root rank (see below) is allowed to read from or
                !write to the file.

                !Determine the I/O root.
                if (mpp_npes() .gt. 1) then

                    !If the is_root_rank flag is passed in, use that.  If not, the
                    !I/O root is the root rank of the current pelist.
                    am_root = .false._c_bool
                    if (present(is_root_rank)) then
                        am_root = is_root_rank
                    else
                        if (mpp_pe() .eq. mpp_root_pe()) then
                            am_root = .true._c_bool
                        endif
                    endif

                    !Let all ranks know who the root rank is.  Also check that only one
                    !rank is the root.
                    if (am_root) then
                        pelist_root = mpp_pe()
                    else
                        pelist_root = -1
                    endif
                    call mpp_max(pelist_root)
                    if (pelist_root .eq. -1) then
                        call mpp_error(FATAL, &
                                       "mpp_open_: there must be at least one rank" &
                                           //" designated as root to perform the I/O.")
                    else
                        if (am_root .and. pelist_root .ne. mpp_pe()) then
                            call mpp_error(FATAL, &
                                           "mpp_open_: only one rank is allowed to be" &
                                               //" the I/O root.")
                        endif
                    endif

                    !Set the communication required flag to true.
                    comm_required = .true._c_bool

                else
                    am_root = .true._c_bool
                    comm_required = .false._c_bool
                endif

                !Mark the data as not spread across multiple files.
                data_in_multi_files = .false._c_bool

            elseif (fileset_flag .eq. MPP_SINGLE) then

                !All ranks will act on the same file.  No communication is
                !required.

                !Only reading is allowed in this case.
                if (action_flag .ne. MPP_RDONLY) then
                    call mpp_error(FATAL, &
                                   "mpp_open_: having multiple ranks write" &
                                       //" to the same netcdf file is not" &
                                       //" currently supported.")
                endif
                is_reader = .true._c_bool
                data_in_multi_files = .false._c_bool
                comm_required = .false._c_bool

            else

                !All ranks will act on their own file.  No communication is
                !required.
                am_root = .true._c_bool
                if (mpp_npes() .gt. 1) then
                    data_in_multi_files = .true._c_bool
                else
                    data_in_multi_files = .false._c_bool
                endif
                comm_required = .false._c_bool

            endif

            !Set the value of the is reader and is writer flags.
            if (am_root) then
                if (action_flag .eq. MPP_RDONLY .or. action_flag .eq. &
                        MPP_APPEND) then
                    is_reader = .true._c_bool
                endif
                if (action_flag .eq. MPP_WRONLY .or. action_flag .eq. &
                        MPP_OVERWR .or. action_flag .eq. MPP_APPEND) then
                    is_writer = .true._c_bool
                endif
            endif
        endif

        if (is_reader .or. is_writer) then

            !If the inputted file is a netCDF file that does not end in ".nc"
            !or ".nc.xxxx", then ".nc" is appended onto the end of the file
            !name.
            nc_pos = index(file_name, &
                           ".nc.")
            if (file_name(fname_length-2:fname_length) .ne. ".nc" .and. &
                    nc_pos .le. 0) then
                fname_full = trim(file_name)//".nc"
            else
                fname_full = trim(file_name)
            endif

            if (mpp_domain_is_defined(io_domain) .or. &
                mpp_domainUG_is_defined(io_domain_ug)) then

                !Modify file names by appending unique identifiers, if
                !there will be more than one file.

                if (num_io_domain_tiles .gt. 1) then

                    !I/O domain tile ids are used as unique identifiers.
                    if (mpp_domain_is_defined(io_domain)) then
                        io_domain_tile_id = mpp_get_tile_id(io_domain)
                        fname_suffix = io_domain_tile_id(1)
                    else
                        fname_suffix = mpp_get_UG_domain_tile_id(io_domain_ug)
                    endif

                    !Check the that the file suffix is valid.
                    if (fname_suffix .ge. 10000) then
                        call mpp_error(FATAL, &
                                       "mpp_open_: the I/O domain tile id should" &
                                           //" be less than 10,000.")
                    endif

                    !Append the four digit I/O domain tile id to the end of the
                    !filename.
                    write(fname_full_buffer,'(a,i4.4)') trim(fname_full)//".", &
                                                        fname_suffix

                    !If the file that is being opened will be read, check that it
                    !exists.  If the there is no filename which matches with a
                    !four digit tile id, then try a six digit tile id.
                    if (action_flag .eq. MPP_RDONLY .or. action_flag .eq. &
                            MPP_APPEND) then

                        inquire(file=trim(fname_full_buffer), &
                                exist=fexists)

                        if (.not. fexists) then
                            write(fname_full_buffer2,'(a,i6.6)') trim(fname_full)//".", &
                                                                 fname_suffix

                            inquire(file=trim(fname_full_buffer2), &
                                    exist=fexists)

                            if (.not. fexists) then
                                call mpp_error(FATAL, &
                                               "mpp_open_: neither "//trim(fname_full_buffer) &
                                                   //" nor "//trim(fname_full_buffer2) &
                                                   //" exist.")
                            endif

                            fname_full_buffer = trim(fname_full_buffer2)
                        endif
                    endif
                    fname_full = fname_full_buffer
                endif

            elseif (data_in_multi_files) then

                !Get the full file name for a file that is not associated
                !with an I/O domain, but is part of a file set (case 4 above).
                if (mpp_npes() .gt. 10000) then
                    write (fname_full,'(a,i6.6)') trim(fname_full)//".", &
                                                  mpp_pe() - mpp_root_pe()
                else
                    write (fname_full,'(a,i4.4)') trim(fname_full)//".", &
                                                  mpp_pe() - mpp_root_pe()
                endif
            endif

            !Copy the file name into a buffer that is interoperable with c.
            call f_string_to_c(fname_full, &
                               c_fname)

            !Open the file.
            context%files(file_index)%c_file_index = mpp_io_c_open_netcdf_file(c_fname, &
                                                                               action_flag, &
                                                                               MPP_IO_C_MAX_GLOBAL_ATTS_PER_FILE, &
                                                                               MPP_IO_C_MAX_DIMS_PER_FILE, &
                                                                               MPP_IO_C_MAX_VARS_PER_FILE, &
                                                                               MPP_IO_C_MAX_ATTS_PER_VAR, &
                                                                               MPP_IO_C_MAX_DIMS_PER_VAR, &
                                                                               MPP_IO_C_MAX_METADATA_BYTES_PER_FILE)
            deallocate(c_fname)

#ifdef RLMDEBUG
            if (mpp_pe() .eq. 0) then
                write(error_unit,*) "opened file "//trim(fname_full)//" as unit", &
                                    context%files(file_index)%c_file_index
            endif
#endif

        endif

        !If communication is necessary (cases 1, 2, and 6 above), then
        !broadcast the file index to the ranks that do not participate in
        !the I/O.  Store the I/O domain pointers and pelists needed for the
        !communication.
        if (mpp_domain_is_defined(io_domain)) then

            if (comm_required) then
                pelist_size = mpp_get_domain_npes(io_domain)
                allocate(context%files(file_index)%rank_list(pelist_size))
                call mpp_get_pelist(io_domain, &
                                    context%files(file_index)%rank_list)
                context%files(file_index)%rank_list_root = pelist_root
            endif
            context%files(file_index)%io_domain = mpp_get_io_domain(domain_)

        elseif (mpp_domainUG_is_defined(io_domain_ug)) then

            if (comm_required) then
                pelist_size = mpp_get_UG_domain_npes(io_domain_ug)
                allocate(context%files(file_index)%rank_list(pelist_size))
                call mpp_get_UG_domain_pelist(io_domain_ug, &
                                              context%files(file_index)%rank_list)
                context%files(file_index)%rank_list_root = pelist_root
            endif
            context%files(file_index)%io_domain_ug = mpp_get_UG_io_domain(domain_ug_)

        elseif (thread_flag .eq. MPP_SINGLE) then

            if (comm_required) then
                pelist_size = mpp_npes()
                allocate(context%files(file_index)%rank_list(pelist_size))
                call mpp_get_current_pelist(context%files(file_index)%rank_list)
                context%files(file_index)%rank_list_root = pelist_root
            endif

        endif

        !If present, store a copy of the input domain.
        if (mpp_domain_is_defined(domain_)) then
            context%files(file_index)%domain = domain_
        elseif(mpp_domainUG_is_defined(domain_ug_)) then
            context%files(file_index)%domain_ug = domain_ug_
        endif

        !Store the is_reader and is_writer flags for the file in the context.
        context%files(file_index)%is_reader = is_reader
        context%files(file_index)%is_writer = is_writer

        !Store the file format flag.
        context%files(file_index)%file_type = form_flag

        !Store the flag telling if the data is spread across multiple files.
        context%files(file_index)%data_in_multi_files = data_in_multi_files

        !Store the flag telling whether communication is required for the
        !file.
        context%files(file_index)%comm_required = comm_required

        !Store the number of ranks on the current pelist when the file
        !was opened.
        context%files(file_index)%num_ranks_at_init = mpp_npes()

        !If necessary, read in the file's metadata.
        if (context%files(file_index)%is_reader) then
            call mpp_read_meta(file_index, &
                               .false.)
        endif
    endif

end subroutine mpp_open_
