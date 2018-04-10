module cjson_utils_mod

use cjson_error_mod,    only: cjson_error_mesg,FATAL,WARNING,NOTE
use iso_c_binding

implicit none

 interface
        integer function array_length (json, nml_name , var_name)bind(C, name="array_length")
           use iso_c_binding
           character(kind=c_char) :: json     !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable
        end function
 end interface

contains


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
  s_out = s_in !> Store the character s_in in s_out
 enddo

end function strcase
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date September 2016
!! Goes through the characters of a strings and checks to see if they are all numbers.  Returns true if each
!! character is a number.  If a character is not a number, the checking ends, and the function returns false.
logical function string_is_num (string)
 character(*),intent(in)        :: string !< The input string
 integer                        :: i 
  
 do i=1,len(string)
   if ( string(i:i) == "0" .or. &
        string(i:i) == "1" .or. &
        string(i:i) == "2" .or. &
        string(i:i) == "3" .or. &
        string(i:i) == "4" .or. &
        string(i:i) == "5" .or. &
        string(i:i) == "6" .or. &
        string(i:i) == "7" .or. &
        string(i:i) == "8" .or. &
        string(i:i) == "9" ) then
     string_is_num = .true. 
   elseif (string(i:i) == "-" .AND. i==1) then
     string_is_num = .true. 
   else
     string_is_num = .false.
     exit
   endif
 enddo 
end function string_is_num
!> @author Tom Robinson thomas.robinson@noaa.gov
!> @date September 2016
!! Checks to see if the size of a variable (arsize) is the same as the size if the json array.
!! It dumps warnings if they are not the same size
subroutine cjson_array_length (json,nmlname,varname,arsize,json_array_size)
 use iso_c_binding
        integer,intent(in)              :: arsize       !< The size of the variable array
        character(*),intent(in)         :: varname      !< The name of the variable      
        character(*),intent(in)         :: nmlname      !< The name of the array containing the variable
        character(*),intent(in)         :: json         !< A string holding the json
        integer                         :: json_array_size !< The size of the JSON array
        character*10                    :: as,js
 json_array_size = array_length (json//c_null_char, &
        trim(nmlname)//c_null_char , trim(varname)//c_null_char)
 if (arsize .EQ. json_array_size ) then !> If the JSON array and variable are the same size, great!
 elseif (arsize < json_array_size ) then !> Print a warning if the variable array is smaller than the JSON array
        Write(js,'(I0)') json_array_size
        Write(as,'(I0)') arsize
         call cjson_error_mesg("cjson_array_length",&
        "The size of "//nmlname//"{"//varname//"} is "//trim(js)//" but the actual size of "//varname//&
        " is "//trim(as)//". "//"The parts of the JSON array after "//trim(as)//" will be ignored.",&
        WARNING)
 elseif (arsize > json_array_size ) then !> Print a warning if the variable array is smaller than the JSON array
        Write(js,'(I0)') json_array_size
        Write(as,'(I0)') arsize
         call cjson_error_mesg("cjson_array_length",&
        "The size of "//nmlname//"{"//varname//"} is "//trim(js)//" but the actual size of "//varname//&
        " is "//trim(as)//". "//"Only the first "//trim(js)//" in "//nmlname//"{"//varname//"} used.",&
        NOTE)
 endif
end subroutine cjson_array_length
!> @author Tom Robinson thomas.robinson@noaa.gov
!> @date September 2016
!! Checks to make sure that an array from a JSON lies within a variable arrays range
subroutine bounds_check (lowbound,upbound,varname,single,lba,uba,com1,com2,col1,col2,lowb2,upb2)
 integer, intent(in)            :: lowbound     !< The lower bound of the array
 integer, intent(in)            :: upbound      !< The upper bound of the array
 integer, intent(in), optional  :: single       !< The element number in an array
 integer, intent(in), optional  :: lba          !< The lower bound of an array in the JSON
 integer, intent(in), optional  :: uba          !< The upper bound of an array in the JSON
 integer, intent(in), optional  :: lowb2        !< The lower bound of the second dimension of an array in the JSON
 integer, intent(in), optional  :: upb2         !< The upper bound of the second dimension of an array in the JSON
 integer, intent(in), optional  :: com1         !< THe number before the  comma
 integer, intent(in), optional  :: com2         !< The number after the comma
 integer, intent(in), optional  :: col1         !< The lower bound of a 2D array
 integer, intent(in), optional  :: col2         !< The upper bound of a 2D array
 character (*), intent(in)      :: varname      !< The name of the variable

 character(len=:),allocatable   :: errormes     !> \param errormes The error message
 logical                        :: isError      !> \param isError True if there is an error
 character*10                   :: n,n1,n2,b1,b2,b12,b22
 isError = .false.
! If none of the optional aruments are given
 if (.not.present(single) .AND. .not.present(lba)  .AND. .not.present(uba)  .AND.                       &
     .not.present(com1)   .AND. .not.present(com2) .AND. .not.present(col1) .AND..not.present(col2)) then
        errormes = "You must give bounds to check for "//varname
        isError=.true.
! If the second dimension of a 2D array is left not passed
 elseif (present(com1) .and. (.not.present(lowb2) .or. .not.present(upb2)) ) then
        errormes = "You must give bounds for the second dimension of "//varname
        isError=.true.
! For a single value in a 1D array
 elseif (present(single) .AND. .not.present(lba) .AND. .not.present(uba)) then
        if (lowbound <= single .AND. single <= upbound) then
                return
        else
                Write(n ,'(I0)') single
                Write(b1,'(I0)') lowbound
                Write(b2,'(I0)') upbound
                errormes = varname//"("//trim(n)//") in the JSON is outside of the range of "//&
                 varname//"("//trim(b1)//":"//trim(b2)//")."
                isError=.true.
        endif
! For a single value in a 2D array
 elseif (present(com1) .AND. present(com2)) then
        if (lowbound <= com1 .AND. com1 <= upbound .AND. &
            lowb2    <= com2 .AND. com2 <= upb2) then
                return
        else
                Write(n1,'(I0)') com1
                Write(n2,'(I0)') com2
                Write(b1,'(I0)') lowbound
                Write(b2,'(I0)') upbound
                Write(b12,'(I0)') lowb2
                Write(b22,'(I0)') upb2
                errormes = varname//"("//trim(n1)//","//trim(n2)//") in the JSON is outside of the range of "//&
                 varname//"("//trim(b1)//":"//trim(b2)//","//trim(b12)//":"//trim(b22)//")."
                isError=.true. 
        endif
! For a range in a 1D array
 elseif (present(lba) .AND. present(uba)) then
        if (lowbound <= lba .AND. uba <= upbound) then
                return
        else
                Write(n1,'(I0)') lba
                Write(n2,'(I0)') uba
                Write(b1,'(I0)') lowbound
                Write(b2,'(I0)') upbound
                errormes = varname//"("//trim(n1)//":"//trim(n2)//") in the JSON is outside of the range of "//&
                 varname//"("//trim(b1)//":"//trim(b2)//")."
                isError=.true.
        endif
! If only one of the bounds is given for a 1D array
 elseif (present(lba) .or. present(uba)) then
        errormes = "You must give both bounds to check "//varname
        isError=.true.
 endif

 if(isError) call cjson_error_mesg("cjson_error:bounds_check",errormes,FATAL)
end subroutine bounds_check



end module cjson_utils_mod
