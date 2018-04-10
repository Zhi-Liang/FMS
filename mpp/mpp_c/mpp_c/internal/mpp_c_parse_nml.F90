module mpp_c_parse_nml_mod
  implicit none
  private

#include "mpp_c_macros.h"

  public :: mpp_c_parse_nml

  integer(4),parameter        :: INT_KIND                   = 4
  integer(INT_KIND),parameter :: LONG_KIND                  = 8
  integer(INT_KIND),parameter :: MPP_C_TRUE_I4              = int(MPP_C_TRUE, &
                                                                  kind=INT_KIND)
  integer(INT_KIND),parameter :: MPP_C_FALSE_I4             = int(MPP_C_FALSE, &
                                                                  kind=INT_KIND)
  integer(INT_KIND),parameter :: MPP_C_NML_PARSE_SUCCESS_I4 = int(MPP_C_NML_PARSE_SUCCESS, &
                                                                  kind=INT_KIND)
  integer(INT_KIND),parameter :: MPP_C_NML_PARSE_FAILURE_I4 = int(MPP_C_NML_PARSE_FAILURE, &
                                                                  kind=INT_KIND)

 !<Namelist is defined and defaults are set here.
  logical(INT_KIND) :: etc_unit_is_stderr     = .false.
  integer(INT_KIND) :: request_multiply       = 20
  logical(INT_KIND) :: mpp_record_timing_data = .true.
  logical(INT_KIND) :: sync_all_clocks        = .false.

  namelist /mpp_nml/ etc_unit_is_stderr,request_multiply, &
                     mpp_record_timing_data,sync_all_clocks

contains

 !-----------------------------------------------------------------------------
 !>Subroutine that parses a namelist contained in a buffer.
  subroutine mpp_c_parse_nml(buffer_len,buffer,etc_stderr_flag, &
                             request_factor,timers_allowed,sync_timers_flag, &
                             nml_parse_err) &
    bind(C,name='mpp_c_parse_nml')
    use iso_c_binding, only: c_size_t,c_char,c_int8_t,c_int32_t

   !<Inputs/Outputs
    integer(c_size_t),value,intent(in)                      :: buffer_len
    character(kind=c_char),dimension(buffer_len),intent(in) :: buffer
    integer(c_int8_t),intent(out)                           :: etc_stderr_flag
    integer(c_size_t),intent(out)                           :: request_factor
    integer(c_int8_t),intent(out)                           :: timers_allowed
    integer(c_int8_t),intent(out)                           :: sync_timers_flag
    integer(c_int32_t),intent(out)                          :: nml_parse_err

   !<Local variables
    integer(INT_KIND)         :: io_status
    character(len=buffer_len) :: tmp_internal_file
    integer(LONG_KIND)        :: f_buff_len
    integer(LONG_KIND)        :: i

   !<Copy the buffer.  The internal file read does not like the inputted
   !!one-dimensional buffer array of with character(len=1) elements.
    f_buff_len = int(buffer_len,kind=LONG_KIND)
    do i = 1,f_buff_len
        tmp_internal_file(i:i) = buffer(i)
    enddo

   !<Parse the namelist.
    read(tmp_internal_file,mpp_nml,iostat=io_status)

    if (io_status .eq. 0 .or. is_iostat_end(io_status)) then

       !<This branch covers two cases:
       !!    Case 1: The namelist exists in the buffer.  Here io_status should
       !!            be 0 per the fortran standard.
       !!    Case 2: The namelist does not exist in the buffer, so the read
       !!            reaches the end of the internal file.  Return the default
       !!            values.
        nml_parse_err = int(MPP_C_NML_PARSE_SUCCESS_I4,kind=c_int32_t)
        if (etc_unit_is_stderr) then
            etc_stderr_flag = int(MPP_C_TRUE_I4,kind=c_int8_t)
        else
            etc_stderr_flag = int(MPP_C_FALSE_I4,kind=c_int8_t)
        endif
        request_factor = int(request_multiply,kind=c_size_t)
        if (mpp_record_timing_data) then
            timers_allowed = int(MPP_C_TRUE_I4,kind=c_int8_t)
        else
            timers_allowed = int(MPP_C_FALSE_I4,kind=c_int8_t)
        endif
        if (sync_all_clocks) then
            sync_timers_flag = int(MPP_C_TRUE_I4,kind=c_int8_t)
        else
            sync_timers_flag = int(MPP_C_FALSE_I4,kind=c_int8_t)
        endif

    else

       !<This branch returns a MPP_C_NML_PARSE_FAILURE value if an error
       !!occurs during the Fortran read.  Negative values are returned.
        nml_parse_err = int(MPP_C_NML_PARSE_FAILURE_I4,kind=c_int32_t)
        etc_stderr_flag = int(MPP_C_FALSE_I4,kind=c_int8_t)
        request_factor = int(0,kind=c_size_t)
        timers_allowed = int(MPP_C_FALSE_I4,kind=c_int8_t)
        sync_timers_flag = int(MPP_C_FALSE_I4,kind=c_int8_t)
    endif

  end subroutine mpp_c_parse_nml

 !>----------------------------------------------------------------------------

end module mpp_c_parse_nml_mod
