 !*****************************************************************************
 !Public facing mpp utility routines.

 !NOTE: The variables mpp_shared and mpp_context are private module c_ptr
 !      variables, which is needed by the c routines.

 !-----------------------------------------------------------------------------
 !>Initialize mpp.
  subroutine mpp_init(localcomm)
    use iso_c_binding, only: c_int,c_associated,c_null_ptr

   !<Inputs/Outputs
    integer(INT_KIND),intent(in),optional :: localcomm

   !<Local variables
    character(len=64) :: tmp_file_name
    logical(INT_KIND) :: is_opened

    if (c_associated(mpp_shared)) then
!       call mpp_error(NOTE,"MPP_INIT: mpp is already initialized.")
        return
    endif

    if (c_associated(mpp_context)) then
!       call mpp_error(NOTE,'MPP_INIT: mpp is already initialized.')
        return
    endif

   !<Initialize mpp.
    if (present(localcomm)) then
        call iso_c_mpp_init(mpp_shared,mpp_context,int(localcomm,kind=c_int))
    else
        call iso_c_mpp_init(mpp_shared,mpp_context,0)
    endif
   !> Initialize the json
    CALL cjsonObject_init("input.json",json,cjson) 
   !<Copy the internal namelist buffer into the input_nml_file buffer.
    call update_input_nml_file_buffer(c_null_ptr)

   !<Open the logfile and etcfile.
    inquire(unit=MPP_C_LOGFILE_UNIT,opened=is_opened)
    if (is_opened) then
        call mpp_error(FATAL,'MPP_INIT: unit 8 is already in use. Pick a' &
                             //' different one for the logfile.')
    else
        write(tmp_file_name,'(a,i6.6,a)') 'logfile.',mpp_pe(),'.out'
        open(unit=MPP_C_LOGFILE_UNIT,status='unknown', &
             file=trim(tmp_file_name),position='append')
    endif
    inquire(unit=MPP_C_ETCFILE_UNIT,opened=is_opened)
    if (is_opened) then
        call mpp_error(FATAL,'MPP_INIT: unit 9 is already in use. Pick a' &
                             //' different one for the etcfile.')
    else
        write(tmp_file_name,'(a,i6.6,a)') '._mpp_c.nonrootpe.msgs.',mpp_pe()
        open(unit=MPP_C_ETCFILE_UNIT,status='unknown', &
             file=trim(tmp_file_name),position='append')
    endif

  end subroutine mpp_init

 !-----------------------------------------------------------------------------
 !>Finalize mpp.
  subroutine mpp_exit()

    close(MPP_C_LOGFILE_UNIT)
    close(MPP_C_ETCFILE_UNIT)

    call iso_c_mpp_exit(mpp_shared,mpp_context)

  end subroutine mpp_exit

 !-----------------------------------------------------------------------------
 !>Function that returns the file unit for stdout.
  function stdout() result(stdout_file_unit)
    use,intrinsic :: iso_fortran_env, only: output_unit

   !<Inputs/Outputs
    integer(INT_KIND) :: stdout_file_unit

    if (mpp_pe() .eq. mpp_root_pe()) then
        stdout_file_unit = output_unit
    else
        stdout_file_unit = stdlog()
    endif

  end function stdout

 !-----------------------------------------------------------------------------
 !>Function that returns the file unit for stderr.
  function stderr() result(stderr_file_unit)
    use,intrinsic :: iso_fortran_env, only: error_unit

   !<Inputs/Outputs
    integer(INT_KIND) :: stderr_file_unit

    stderr_file_unit = error_unit

  end function stderr

 !-----------------------------------------------------------------------------
 !>Function that returns the file unit for the log file.
  function stdlog() result(stdlog_file_unit)

   !<Inputs/Outputs
    integer(INT_KIND) :: stdlog_file_unit

    if (mpp_pe() .eq. mpp_root_pe()) then
        stdlog_file_unit = MPP_C_LOGFILE_UNIT
    else
        stdlog_file_unit = MPP_C_ETCFILE_UNIT
    endif

  end function stdlog

 !-----------------------------------------------------------------------------
 !>Function that returns the rank id for a process in MPI_COMM_WORLD.
  function mpp_pe() result(my_world_rank)

   !<Inputs/Outputs
    integer(INT_KIND) :: my_world_rank

    my_world_rank = int(iso_c_mpp_pe(mpp_context),kind=INT_KIND)

  end function mpp_pe

 !-----------------------------------------------------------------------------
 !>Function that returns the total number of ranks in the current pelist.
  function mpp_npes() result(num_ranks)

   !<Inputs/Outputs
    integer(INT_KIND) :: num_ranks

    num_ranks = int(iso_c_mpp_npes(mpp_context),kind=INT_KIND)

  end function mpp_npes

 !-----------------------------------------------------------------------------
 !>Function that returns the root rank id in the current pelist.
  function mpp_root_pe() result(root_rank)

   !<Inputs/Outputs
    integer(INT_KIND) :: root_rank

    root_rank = int(iso_c_mpp_root_pe(mpp_context),kind=INT_KIND)

  end function mpp_root_pe

 !-----------------------------------------------------------------------------
 !>Subroutine that sets the root rank id for the current pelist.
  subroutine mpp_set_root_pe(root_rank_id)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in) :: root_rank_id

    call iso_c_mpp_set_root_pe(mpp_context,int(root_rank_id,kind=c_int32_t))

  end subroutine mpp_set_root_pe

 !-----------------------------------------------------------------------------
 !>Subroutine that creates a new pelist.
  subroutine mpp_declare_pelist(pelist,name)

   !<Inputs/Outputs
    integer(INT_KIND),dimension(:),intent(in) :: pelist
    character(len=*),intent(in),optional      :: name

    if (size(pelist) .eq. 0) then
!       call mpp_error(NOTE,'MPP_DECLARE_PELIST: pelist with 0 size: ' &
!                           //trim(name))
        return
    endif

    if (present(name)) then
        call mpp_declare_pelist_c_style(size(pelist),pelist,len_trim(name), &
                                        trim(name))
    else
        call mpp_declare_pelist_c_style(size(pelist),pelist,13, &
                                        'Unamed_pelist')

    endif

  end subroutine mpp_declare_pelist

  subroutine mpp_declare_pelist_c_style(n,pelist,m,pname)
    use iso_c_binding, only: c_char,c_size_t,c_null_char

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)              :: n
    integer(INT_KIND),dimension(n),intent(in) :: pelist
    integer(INT_KIND),intent(in)              :: m
    character(len=m),intent(in)               :: pname

   !<Local variables
    character(kind=c_char),dimension(MPP_C_MAX_PELIST_NAME_LENGTH) :: tmp_name
    integer(c_size_t)                                              :: tmp_len
    integer(LONG_KIND)                                             :: i

    do i = 1,MPP_C_MAX_PELIST_NAME_LENGTH
        tmp_name(i) = c_null_char
    enddo
    if (m .ge. MPP_C_MAX_PELIST_NAME_LENGTH) then
        tmp_len = int(MPP_C_MAX_PELIST_NAME_LENGTH-1,kind=c_size_t)
    else
        tmp_len = int(m,kind=c_size_t)
    endif
    do i = 1,tmp_len
        tmp_name(i) = pname(i:i)
    enddo

    call iso_c_mpp_declare_pelist(mpp_context,tmp_name,tmp_len, &
                                  pelist,int(n,kind=c_size_t))

  end subroutine mpp_declare_pelist_c_style

 !-----------------------------------------------------------------------------
 !>Subroutine that returns the current pelist.
  subroutine mpp_get_current_pelist(pelist,name,commID)
    use iso_c_binding, only: c_ptr,c_int32_t,c_null_ptr,c_f_pointer

   !<Inputs/Ouputs
    integer(INT_KIND),dimension(:),intent(out) :: pelist
    character(len=*),intent(out),optional      :: name
    integer(INT_KIND),intent(out),optional     :: commID

   !<Local variables
    integer(INT_KIND)                       :: current_pelist_size
    type(c_ptr)                             :: tmp_c_ptr
    integer(c_int32_t),dimension(:),pointer :: tmp_f_ptr
    integer(INT_KIND)                       :: i

   !<Initialize local pointers.
    tmp_c_ptr = c_null_ptr
    tmp_f_ptr => NULL()

   !<Get the size of the current pelist.
    current_pelist_size = mpp_npes()

   !<Make sure that the outgoing pelist array size matches the current pelist
   !!size.
    if (size(pelist) .ne. current_pelist_size) then
        call mpp_error(FATAL,'MPP_GET_CURRENT_PELIST: Outgoing pelist array' &
                             //' is not the same size as the current pelist')
    endif

   !<Get the c pointer to the current pelist, and copy it into fortran.
    tmp_c_ptr = iso_c_mpp_get_current_pelist(mpp_context)
    call c_f_pointer(tmp_c_ptr,tmp_f_ptr,[current_pelist_size])
    do i = 1,current_pelist_size
        pelist(i) = tmp_f_ptr(i)
    enddo

   !<If necessary, get the name and communicator id for the current pelist.
    if (present(name)) then
        name = mpp_get_current_pelist_name()
    endif
    if (present(commID)) then
        commID = int(iso_c_mpp_get_current_pelist_comm_id(mpp_context), &
                     kind=INT_KIND)
    endif

   !<Nullify local pointers.
    tmp_c_ptr = c_null_ptr
    tmp_f_ptr => NULL()

  end subroutine mpp_get_current_pelist

 !-----------------------------------------------------------------------------
 !>Subroutine that sets the current pelist to be equal to inputted pelist.
  subroutine mpp_set_current_pelist(pelist,no_sync)
    use iso_c_binding, only: c_int8_t,c_int32_t,c_size_t

   !<Inputs/Outputs
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist
    logical(INT_KIND),intent(in),optional              :: no_sync

   !<Local variables
    integer(c_int8_t)               :: no_sync_flag
    integer(c_int32_t),dimension(1) :: null_pelist

    no_sync_flag = MPP_C_FALSE
    if (present(no_sync)) then
        if (no_sync) then
            no_sync_flag = MPP_C_TRUE
        endif
    endif

    if (present(pelist)) then
        call mpp_set_current_pelist_c_style(size(pelist),pelist, &
                                            no_sync_flag)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call iso_c_mpp_set_current_pelist(mpp_context,null_pelist, &
                                          int(1,kind=c_size_t),no_sync_flag)
    endif

  end subroutine mpp_set_current_pelist

  subroutine mpp_set_current_pelist_c_style(n,pelist,no_sync_flag)
    use iso_c_binding, only: c_int8_t,c_size_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)              :: n
    integer(INT_KIND),dimension(n),intent(in) :: pelist
    integer(c_int8_t),intent(in)              :: no_sync_flag

    call iso_c_mpp_set_current_pelist(mpp_context,pelist, &
                                      int(n,kind=c_size_t),no_sync_flag)

  end subroutine mpp_set_current_pelist_c_style

 !-----------------------------------------------------------------------------
 !>Function that returns the name of the current pelist.
  function mpp_get_current_pelist_name() result(current_pelist_name)
    use iso_c_binding, only: c_size_t,c_ptr,c_char,c_null_ptr,c_f_pointer

   !<Inputs/Outputs
    character(len=MPP_C_MAX_PELIST_NAME_LENGTH) :: current_pelist_name

   !<Local variables
    integer(c_size_t)                                 :: current_pelist_name_len
    type(c_ptr)                                       :: tmp_c_ptr
    character(kind=c_char,len=1),dimension(:),pointer :: tmp_f_ptr
    integer(c_size_t)                                 :: i

   !<Initialize the outgoing name and local pointers.
    do i = 1,len(current_pelist_name)
        current_pelist_name(i:i) = ' '
    enddo
    tmp_c_ptr = c_null_ptr
    tmp_f_ptr => NULL()

   !<Get the length of the current pelist name.
    current_pelist_name_len = iso_c_mpp_get_current_pelist_name_len( &
                                                                   mpp_context)

   !<Get the c pointer to the current pelist name, and copy it into fortran.
    tmp_c_ptr = iso_c_mpp_get_current_pelist_name(mpp_context)
    call c_f_pointer(tmp_c_ptr,tmp_f_ptr,[current_pelist_name_len])
    do i = 1,current_pelist_name_len
        current_pelist_name(i:i) = tmp_f_ptr(i)
    enddo

   !<Nullify local pointers.
    tmp_c_ptr = c_null_ptr
    tmp_f_ptr => NULL()

  end function mpp_get_current_pelist_name

 !-----------------------------------------------------------------------------
 !>Function that sets the granularity for the timers.
  subroutine mpp_clock_set_grain(grain)

   !<Inputs/Outputs
    integer(INT_KIND),intent(in) :: grain

    call iso_c_mpp_clock_set_grain(mpp_context,grain)

  end subroutine mpp_clock_set_grain
!
 !-----------------------------------------------------------------------------
 !>Function that creates a new timer and returns its id.
  function mpp_clock_id(name,flags,grain) result(clock_id)
    use iso_c_binding, only: c_char,c_size_t,c_int32_t,c_int8_t,c_null_char

   !<Inputs/Output
    integer(INT_KIND)                     :: clock_id
    character(len=*),intent(in)           :: name
    integer(INT_KIND),intent(in),optional :: flags
    integer(INT_KIND),intent(in),optional :: grain

   !<Local variables
    character(kind=c_char),dimension(MPP_C_MAX_TIMER_NAME_LEN) :: tmp_name
    integer(c_size_t)                                          :: tmp_len
    integer(c_int32_t)                                         :: tmp_grain
    integer(c_int8_t)                                          :: sync_flag
    integer(c_int8_t)                                          :: detail_flag
    integer(LONG_KIND)                                         :: i

    do i = 1,MPP_C_MAX_TIMER_NAME_LEN
        tmp_name(i) = c_null_char
    enddo
    if (len(name) .ge. MPP_C_MAX_TIMER_NAME_LEN) then
        tmp_len = int(MPP_C_MAX_TIMER_NAME_LEN-1,kind=c_size_t)
    else
        tmp_len = int(len(name),kind=c_size_t)
    endif
    do i = 1,tmp_len
        tmp_name(i) = name(i:i)
    enddo

    if (present(grain)) then
        tmp_grain = grain
    else
        tmp_grain = MPP_C_RUNTIME_TIMER
    endif

    sync_flag = MPP_C_FALSE
    detail_flag = MPP_C_FALSE
    if (present(flags)) then
        if (BTEST(flags,0)) then
            sync_flag = MPP_C_TRUE
        endif
        if (BTEST(flags,1)) then
            detail_flag = MPP_C_TRUE
        endif
    endif

    clock_id = int(iso_c_mpp_clock_id(mpp_context,tmp_name,tmp_len, &
                                      tmp_grain,sync_flag,detail_flag), &
                                      kind=INT_KIND)

  end function mpp_clock_id

 !-----------------------------------------------------------------------------
 !>Subroutine that starts a timer.
  subroutine mpp_clock_begin(timer_id)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in) :: timer_id

    if (timer_id .eq. -1) then
        return
    endif

    call iso_c_mpp_clock_begin(mpp_context,int(timer_id,kind=c_int32_t))

  end subroutine mpp_clock_begin
!
 !-----------------------------------------------------------------------------
 !>Subroutine that stops a timer.
  subroutine mpp_clock_end(timer_id)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in) :: timer_id

    if (timer_id .eq. -1) then
        return
    endif

    call iso_c_mpp_clock_end(mpp_context,int(timer_id,kind=c_int32_t))

  end subroutine mpp_clock_end
!
 !-----------------------------------------------------------------------------
 !>Subroutine that sets a flag that allows timers.
  subroutine mpp_record_time_start()

    call iso_c_mpp_record_time_start(mpp_context)

  end subroutine mpp_record_time_start
!
 !-----------------------------------------------------------------------------
 !>Subroutine that sets a flag that does not allows timers.
  subroutine mpp_record_time_end()

    call iso_c_mpp_record_time_end(mpp_context)

  end subroutine mpp_record_time_end

 !-----------------------------------------------------------------------------
 !>Subroutine that reads in a .nml file and stores it in an internal character
 !!buffer.
  subroutine read_input_nml()
    use iso_c_binding, only: c_ptr,c_null_ptr

   !<Local variables
    type(c_ptr) :: tmp_c_ptr

   !<Read the .nml file into an internal buffer.
    call iso_c_read_input_nml(mpp_context)

   !<Initialize local pointer.
    tmp_c_ptr = c_null_ptr

   !<Get the pointer to namelist buffer.
    tmp_c_ptr = iso_c_mpp_get_namelist_buffer(mpp_context)

   !<Copy the internal namelist buffer into the input_nml_file buffer.
    call update_input_nml_file_buffer(tmp_c_ptr)

   !<Nullify local pointer.
    tmp_c_ptr = c_null_ptr

  end subroutine read_input_nml

 !-----------------------------------------------------------------------------
 !>Subroutine that copies in the internal namelist buffer into the
 !!input_nml_file buffer.
  subroutine update_input_nml_file_buffer(nml_ptr)
    use iso_c_binding, only: c_ptr,c_null_ptr,c_size_t,c_char,c_f_pointer, &
                             c_associated,c_new_line

   !<Inputs/Outputs
    type(c_ptr),intent(in) :: nml_ptr

   !<Local variables
    integer(c_size_t)                                 :: nml_buffer_len
    type(c_ptr)                                       :: tmp_c_ptr
    character(kind=c_char,len=1),dimension(:),pointer :: tmp_f_ptr
    integer(INT_KIND)                                 :: nline_counter
    integer(INT_KIND)                                 :: spot_counter
    character(len=256),pointer                        :: tmp_str
    integer(c_size_t)                                 :: i

   !<Initialize local pointers.
    tmp_c_ptr = c_null_ptr
    tmp_f_ptr => NULL()

   !<Use the inputted nml_ptr, unless it is null.  In that case, use the
   !!input_nml buffer shared by all contexts.
    if (c_associated(nml_ptr)) then
        nml_buffer_len = iso_c_mpp_get_namelist_buffer_len(mpp_context)
        tmp_c_ptr = nml_ptr
    else
        nml_buffer_len = iso_c_mpp_get_input_nml_buffer_size(mpp_context)
        tmp_c_ptr = iso_c_mpp_get_input_nml_buffer(mpp_context)
    endif

   !<Copy the c pointer to a fortran pointer.
    call c_f_pointer(tmp_c_ptr,tmp_f_ptr,[nml_buffer_len])

   !<Count the number of newline characters.
    nline_counter = 0
    do i = 1,nml_buffer_len
        if (tmp_f_ptr(i) .eq. c_new_line) then
            nline_counter = nline_counter + 1
        endif
    enddo
    nline_counter = nline_counter + 1

   !<Allocate and initialize the input_nml_file buffer.
    if (allocated(input_nml_file)) then
        deallocate(input_nml_file)
    endif
    allocate(input_nml_file(nline_counter))
    do i = 1,nline_counter
        input_nml_file = ''
    enddo

   !<Copy the namelist buffer into the input_nml_file buffer.
    nline_counter = 1
    spot_counter = 1
    tmp_str => input_nml_file(nline_counter)
    do i = 1,nml_buffer_len
        tmp_str(spot_counter:spot_counter) = tmp_f_ptr(i)
        spot_counter = spot_counter + 1
        if (spot_counter .gt. 256) then
            call mpp_error(FATAL,'READ_INPUT_NML: input_nml_file needs a' &
                                 //' longer length.')
        endif
        if (tmp_f_ptr(i) .eq. c_new_line) then
            nline_counter = nline_counter + 1
            tmp_str => input_nml_file(nline_counter)
            spot_counter = 1
        endif
    enddo

   !<Nullify local pointers.
    tmp_c_ptr = c_null_ptr
    tmp_f_ptr => NULL()
    tmp_str => NULL()

  end subroutine update_input_nml_file_buffer

 !-----------------------------------------------------------------------------

 !*****************************************************************************
