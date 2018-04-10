module cjson_error_mod

use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, &
                                          stdout=>output_unit, &
                                          stderr=>error_unit
implicit none
 include 'mpif.h'

 integer  :: FATAL=0 , WARNING=1 , NOTE=2

 contains

subroutine cjson_error_mesg (routine,message,level,wtype)
 character(*)  :: routine     !< The name of the calling routine
 character(*)  :: message     !< The error message
 integer       :: level       !< The severity level of the message
                              !! 0=FATAL , 1=WARNING . 2=NOTE
 integer,optional :: wtype    !< The level of warning.  < 0  is lower level warning
 integer       :: rank,ierr,master=0
 character*10  :: rk
 logical       :: is_mpi_on   !> \param is_mpi_on Used to determine if MPI has been initialized
 is_mpi_on = .false. !> Set the MPI check to false at the beginning
 call MPI_INITIALIZED (is_mpi_on,ierr) !> Check if MPI is initialized

IF (is_mpi_on) then
 call MPI_COMM_RANK (MPI_COMM_WORLD, rank, ierr)
 
 if     (level == NOTE   ) then !> NOTE is written only if the master processor 0 encounters the trigger
     if(rank == 0) write (stdout,'(a)')"NOTE from PE 0: "//trim(routine)//": "//trim(message)
 elseif (level == WARNING .AND. present(wtype) .AND. wtype < 0) then
     if(rank == 0) write (stdout,'(a)')"WARNING from PE "//trim(rk)//": "//trim(routine)//": "//trim(message)
 elseif (level == WARNING) then !> WARNING is written on all processors that encounter the warning
     write(rk,'(I0)') rank
     write (stdout,'(a)')"WARNING from PE "//trim(rk)//": "//trim(routine)//": "//trim(message)
 elseif (level == FATAL  ) then !> FATAL prints message and kills program
     write(rk,'(I0)') rank
     write (stdout,'(a)')"FATAL from PE "//trim(rk)//": "//trim(routine)//": "//trim(message)
     call MPI_ABORT(MPI_COMM_WORLD,999,ierr)
 else
     write(rk,'(I0)') rank
     write (stdout,'(a)')"ERROR LEVEL UNKNOWN from PE"//trim(rk)//": "//trim(routine)//": "//trim(message)
     call MPI_ABORT(MPI_COMM_WORLD,999,ierr)
 endif
ELSE !> If MPI is not initialized, no processor number is given in message
 if     (level == NOTE   ) then
      write (stdout,'(a)')"NOTE (mpi not initialized): "//trim(routine)//": "//trim(message)
 elseif (level == WARNING) then
     write (stdout,'(a)')"WARNING (mpi not initialized): "//trim(routine)//": "//trim(message)
 elseif (level == FATAL  ) then
     write (stdout,'(a)')"FATAL (mpi not initialized): "//trim(routine)//": "//trim(message)
     call ABORT
 else
     write (stdout,'(a)')"ERROR LEVEL UNKNOWN (mpi not initialized): "//trim(routine)//": "//trim(message)
     call ABORT
 endif
ENDIF
end subroutine cjson_error_mesg

!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date August 2016
!! This function takes a string and makes all of the letters lower case.  Can be used to compare   
!! strings where case shouldn't matter.  This is useful for JSONs where the variable name should 
!! not be case sensative
function strcase (string) result (lower)

 character (*), intent(in),target       :: string           !< The input string
 character (len=len(string)),target     :: lower            !< The input string in lower case
 character (len=1)                      :: s_in             !> \param s_in A pointer to a single character in the input string
 character (len=1),pointer              :: s_out => null () !> \param s_out A pointer to a single character in the output string
 integer                                :: i                !> \param i Used for looping

 do i = 1,len(string)
  s_in = string (i:i) ; s_out => lower(i:i) !> Use a single character pointer for each character
  !> Convert any upper case letters to lower case
  if (s_in == "A") s_in="a"
  if (s_in == "B") s_in="b"
  if (s_in == "C") s_in="c"
  if (s_in == "D") s_in="d"
  if (s_in == "E") s_in="e"
  if (s_in == "F") s_in="f"
  if (s_in == "G") s_in="g"
  if (s_in == "H") s_in="h"
  if (s_in == "I") s_in="i"
  if (s_in == "J") s_in="j"
  if (s_in == "K") s_in="k"
  if (s_in == "L") s_in="l"
  if (s_in == "M") s_in="m"
  if (s_in == "N") s_in="n"
  if (s_in == "O") s_in="o"
  if (s_in == "P") s_in="p"
  if (s_in == "Q") s_in="q"
  if (s_in == "R") s_in="r"
  if (s_in == "S") s_in="s"
  if (s_in == "T") s_in="t"
  if (s_in == "U") s_in="u"
  if (s_in == "V") s_in="v"
  if (s_in == "W") s_in="w"
  if (s_in == "X") s_in="x"
  if (s_in == "Y") s_in="y"
  if (s_in == "Z") s_in="z"
  s_out = s_in !> Store the character in s_in in s_out
 enddo

end function strcase

end module cjson_error_mod
