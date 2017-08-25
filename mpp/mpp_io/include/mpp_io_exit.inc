!Finalize the mpp_io fortran module.
subroutine mpp_io_exit(string)
    implicit none

    !Inputs/outputs
    character(len=*),optional :: string

    !Local variables
    logical(kind=c_bool) :: do_sync

    !Do not allow this routine to be executed in a threaded region.
    call openmp_thread_trap()

    !Make sure that the module has been previously initialized.
    if (.not. module_is_initialized) then
        call mpp_error(FATAL, &
                       "mpp_io_exit: you must first call mpp_io_init.")
    endif

    !Set flag values.
    do_sync = .true._c_bool
    if (present(string)) then
        if (lowercase(trim(string)) .eq. "nosync") then
            do_sync = .false._c_bool
        endif
    endif

    !Synchronize ranks.
    if (do_sync) then
        call mpp_sync()
    endif

    !Destroy the mpp_io context.
    call mpp_io_context_destroy(context)

    !Set the module state as uninitialized.
    module_is_initialized = .false._c_bool

    !Finalize the mpp_io_c library.
    call mpp_io_c_finalize()

end subroutine mpp_io_exit
