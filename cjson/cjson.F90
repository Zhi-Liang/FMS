module cjson_mod
use iso_c_binding
use cjson_error_mod,    only: cjson_error_mesg,FATAL,WARNING,NOTE
use cjson_utils_mod,    only: string_is_num,cjson_array_length,strcase,bounds_check
use cjson_array_string_mod, only: string_2d
implicit none

include 'mpif.h'

 INTERFACE
!> Parses the JSON
         type(C_PTR) function parseJson (json) result (cjson) bind(C, name="parseJson")
           use iso_c_binding
           character(kind=c_char) :: json     !< The json string
         end function
!> Gets an item that is a JSON object and stores it as a C-pointer
         type(C_PTR) function cJSON_GetObjectItem (cjson,nml_name) result (nmlptr) bind(C, name="cJSON_GetObjectItem")
           use iso_c_binding
           type (c_ptr),intent(in),value :: cjson !< The parsed json
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
         end function
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> Returns the value of an integer scalar
         integer function integer_val (cjson, json, nml_name , var_name) bind(C, name="integer_val")
           use iso_c_binding
           type (c_ptr),intent(in),value :: cjson !< The parsed json
           character(kind=c_char) :: json     !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable
         end function
!> Returns the value of a real*8 scalar, can be used for real*4
         real*8    function real_val (cjson, json, nml_name , var_name) bind(C, name="real_val")
           use iso_c_binding
           type (c_ptr),intent(in),value :: cjson !< The parsed json
           character(kind=c_char) :: json     !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable
         end function
!> Returns the value of a string scalar as a C-pointer
        function string_val (cjson, json, nml_name , var_name) result (svalue) bind(C, name="string_val")
           use iso_c_binding
           type (c_ptr),intent(in),value :: cjson !< The parsed json
           character(kind=c_char) :: json     !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable
           type (c_ptr):: svalue  !< The value of the variable (pointer)
         end function
!> Returns the value of an integer in an array
         integer function integer_array (cjson, json, nml_name , var_name, arind) bind(C, name="integer_array")
           use iso_c_binding
           type (c_ptr),intent(in),value :: cjson !< The parsed json
           character(kind=c_char) :: json     !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable array
           integer  (kind=c_int), VALUE :: arind !< The index of the array that is being inquiried
         end function
!> Returns the value of a real*8 in an array, can be used for real*4
         real*8 function real_array (cjson, json, nml_name , var_name, arind) bind(C, name="real_array")
           use iso_c_binding
           type (c_ptr),intent(in),value :: cjson !< The parsed json
           character(kind=c_char) :: json     !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable
           integer  (kind=c_int), VALUE :: arind !< The index of the array that is being inquiried
         end function
!> Returns the value of a string in an array as a C-pointer
         function string_array (cjson, json, nml_name , var_name, arind) result (s)  bind(C, name="string_array")
           use iso_c_binding
           type (c_ptr),intent(in),value :: cjson !< The parsed json
           character(kind=c_char) :: json     !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable
           integer  (kind=c_int), VALUE :: arind !< The index of the array that is being inquiried
           type (c_ptr):: s  !< The value of the variable (pointer)
         end function

!> For user use
         subroutine json_check_for_variable ( json,  nml_name, var_name, nmlexist, varexist) & 
                                          bind (C,name="json_check_for_variable")
           use iso_c_binding
           character(kind=c_char) :: json     !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable
           integer  , intent (inout):: nmlexist !< Greater than 0 if the namelist exists
           integer  , intent (inout):: varexist !< Greater than 0 if the variable exists
         end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine C_free (cptr) bind(C, name="free") !> C-free function
           use iso_c_binding
           type (c_ptr),intent(in),value :: cptr
         end subroutine
        
 END INTERFACE
!> JSON_VALUE is called by cjson_args_mod and is used for sclars, 1D arrays, and 2D arrays
 INTERFACE JSON_VALUE
        module procedure json_scalar
        module procedure json_array
        module procedure json_2d
 END INTERFACE

 CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> @author Tom Robinson 2016 thomas.robinson@noaa.gov
!! Subroutine to open and read the json file.  The json file is stored in an allocatable 
!! string to be used later for parsing and getting the values of variables.
subroutine jsoninit (json_file,json,json_length)
use iso_c_binding
 character (*),intent(in)                    :: json_file   !< The name of the json file
 character (len=:),allocatable,intent(out)   :: json        !< String containing the contents of the json file
 integer ,intent(out)                        :: json_length !< The length of the json string
 integer                                     :: i           !> \param i for iostat
 integer                                     :: iunit       !> \param iunit Unit number of file to open
 character (len=:),allocatable               :: get_len
 character*100                          :: error  !> \param error The error message
 integer                                :: rank   !> \param rank The number processor
 integer                                :: ierr   !> \param ierr The error code coming from MPI
 logical                                :: is_mpi_on !> \param is_mpi_on true if MPI is already initialized
 logical                                :: isopened  !> \param isopened Used to check for an available file unit number
 
 call MPI_INITIALIZED(is_mpi_on, ierr)
IF (is_mpi_on) then
     call MPI_COMM_RANK (MPI_COMM_WORLD, rank, ierr)
     !> Open the json file
     if (rank == 0) then
      do iunit = 29, 700
         inquire (iunit,OPENED=isopened)
         if (.not. isopened) exit
      enddo
          !> Allocate the very large temporary character string
          allocate (character (len=1000000) :: get_len)
          open (29,file=trim(json_file),status="old",access="stream")
           !> Get the length of the json file to pass to the C routines for allocating the string when reading
           read (29,iostat=i) get_len  
           inquire (unit=29,pos=json_length) !> Find the endpoint in the json file using inquire
          close (29) !> Close the json file
          deallocate (get_len) !> Deallocate the temporary string because it is not neccessary
     endif 
     call MPI_BCAST(json_length,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
     if (allocated(json) ) then !> If json already exists, overwrite the old one
        CALL cjson_error_mesg('cjson_fortran_mod', "JSON - The json had been previously initialized.  Overwriting previous initialization.",NOTE)
        deallocate(json)
     endif

     allocate (character (len=json_length) :: json) !> Allocate the json string with the proper length

     if (rank == 0) then
      do iunit = 29, 700
         inquire (iunit,OPENED=isopened)
         if (.not. isopened) exit
      enddo
          open (iunit,file=trim(json_file),status="old",access="stream")
           read (iunit,iostat=i) json  !> Read the json string
          close (iunit) !> Close the json file
!
     endif
     call MPI_BCAST (json,json_length,MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
ELSE
      do iunit = 29, 700
         inquire (iunit,OPENED=isopened)
         if (.not. isopened) exit
      enddo
     !> Allocate the very large temporary character string
     allocate (character (len=1000000) :: get_len)
     open (iunit,file=trim(json_file),status="old",access="stream")
      !> Get the length of the json file to pass to the C routines for allocating the string when reading
      read (iunit,iostat=i) get_len
      inquire (unit=iunit,pos=json_length) !> Find the endpoint in the json file using inquire
     close (iunit) !> Close the json file
     deallocate (get_len) !> Deallocate the temporary string because it is not neccessary
     if (allocated(json) ) then !> If json already exists, overwrite the old one
        CALL cjson_error_mesg('cjson_fortran_mod', "JSON - The json had been previously initialized.  Overwriting previous initialization.",NOTE)
        deallocate(json)
     endif

     allocate (character (len=json_length) :: json) !> Allocate the json string with the proper length
     open (iunit,file=trim(json_file),status="old",access="stream")
      read (iunit,iostat=i) json  !> Read the json string
     close (iunit) !> Close the json file
ENDIF

end subroutine jsoninit
!> \author Tom Robinson thomas.robinson@noaa.gov 
!> \date 2016
!> The init subroutine opens the json file, then parses it using the C function parseJson.  The 
!! parsed json file is saved as a c pointer
subroutine cjsonObject_init(json_file,json,cjson)
 character (*),intent(in)			:: json_file 	!< The name of the json file
 character (len=:), allocatable, intent(inout)  :: json		!< String containing the contents of the json file
 integer 			                :: json_length 	!< The length of the json string
 type (C_PTR), intent(inout)                    :: cjson    !< The json string
 CALL jsoninit (json_file,json,json_length)
       cjson = parseJson (trim(json)//c_null_char)
        
end subroutine cjsonObject_init

!> \author Tom Robinson thomas.robinson@noaa.gov
!> \date September 2016
!! JSON_SCALAR_VALUE extracts the value of varname in the JSON array nmlname and stores it in nmlval.
!! The JSON set up must look like this:
!! \code
!! {
!!  "nmlname": [
!!              {"varname" : VALUE}
!!             ]
!! }
!! \endcode
!! where VALUE is any legal JSON SCALAR value.  varname must be key within an object in the array nmlname.
!! nmlname must be within the top level json object.  
subroutine json_scalar_value (cjson,json,nmlname,varname,nmlval,st_size)
use iso_c_binding
        class (*),target,intent(inout)  :: nmlval       !< The variable
        class (*),pointer               :: var          !> \param var A pointer to the variable
        character(*),intent(in)         :: varname      !< The name of the variable      
        character(*),intent(in)         :: nmlname      !< The name of the array containing the variable
        character(*),intent(in)         :: json         !< A string holding the json
        type(C_PTR),intent(in)          :: cjson        !< A C pointer containing the parsed json
        integer,optional,intent(in)     :: st_size      !< The length of a string

        var => nmlval
        select type (var) !> Select the intrinsic type of the namelist variable
   type is (integer)
       !> If scalar integer, using the integer function integer_val to obtain the variable value
       var = integer_val(cjson,json//C_NULL_CHAR,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR)
   type is (real(kind=4))
     !> If scalar real, using the real function real_val to obtain the variable value
       var = real_val(cjson,json//C_NULL_CHAR,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR)
   type is (real(kind=8))
     !> If scalar real, using the real function real_val to obtain the variable value
       var = real_val(cjson,json//C_NULL_CHAR,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR)
   type is (logical)
     !> If scalar logical, using the integer function integer_val to obtain the variable value
       var = integer_val(cjson,json//C_NULL_CHAR,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR)
   type is (character(*))
     !> When the variable is a string, it must have the st_size passed with it.
        if (.not. present (st_size)) call cjson_error_mesg("JSON_SCALAR_VALUE",&
                nmlname//'{'//varname//"} is a string, but the argument ST_SIZE"//&
                " was not included.",FATAL)
     !> Strings are handled by their own subroutine to work with all compilers
        call json_scalar_value_string (cjson,json,nmlname,varname,var,st_size)


 end select

end subroutine json_scalar_value
!> \author Tom Robinson thomas.robinson@noaa.gov
!> \date September 2016
!! json_scalar_value_string handles string scalars
subroutine json_scalar_value_string (cjson,json,nmlname,varname,var,st_size)
        character(*),intent(inout)              :: var          !> \param var A pointer to the variable
        character(*),intent(in)                 :: varname      !< The name of the variable      
        character(*),intent(in)                 :: nmlname      !< The name of the array containing the variable
        character(*),intent(in)                 :: json         !< A string holding the json
        type(C_PTR),intent(in)                  :: cjson        !< A C pointer containing the parsed json
        integer,intent(in)                      :: st_size      !< The length of a string
        type (C_PTR)                                            :: C_st = C_NULL_PTR !> \param C_st Stores the string as a C-pointer
        character(len=1,kind=c_char),pointer,dimension(:)       :: F_st => null() !> \param F_st The string after it's been converted from a C string array
        character(len=:),allocatable                            :: data_string !> \param data_string Used to copy the string array to a typical Fortran string

        integer                         :: i                    !> \param i Used for looping

        allocate (character(len=st_size) :: data_string)  !> Allocate data_string to be the size st_size
        do i=1,st_size
         data_string(i:i)=" " !> Initialize data_string
        enddo
!> Get the string array
        C_st = string_val(cjson,json//C_NULL_CHAR,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR)
!> Convert the string array C-pointer to a fortran pointer with the size of st_size
        call c_f_pointer ( C_St,F_St , [st_size] ) !> Convert the string to a Fortran pointer
        do i=1,st_size
                if (F_St(i) == c_null_char) exit!> End the loop on the null character
                data_string(i:i) = f_st(i)      !> Put the individual characters into the data string
        enddo
        var(1:st_size) = data_string(1:st_size)  !> Put the value of the string into var
!> Clean up the memory
        deallocate (data_string)
        nullify (F_st)
        CALL C_free (C_St)
        C_St = C_NULL_PTR

end subroutine json_scalar_value_string
!> \author Tom Robinson thomas.robinson@noaa.gov
!> \date September 2016
!! JSON_ARRAY_VALUE extracts the value of varname in the JSON array nmlname and stores it in nmlval.
!! The JSON set up must look like this:
!! \code
!! {
!!  "nmlname": [
!!              {"varname" : [VALUE]}
!!             ]
!! }
!! \endcode
!! where VALUE is any legal JSON ARRAY.  varname must be key within an object in the array nmlname.
!! nmlname must be within the top level json object. 
subroutine json_array_value (cjson,json,nmlname,varname,nmlval,ARIND,Cind,lowB,st_size)
use iso_c_binding
        integer                         :: lowb         !< The lower bound of the variable array
        class (*),target,intent(inout)  :: nmlval(lowb:)!< The variable
        class (*),pointer               :: var   (:)    !> \param var A pointer to the variable
        character(*),intent(in)         :: varname      !< The name of the variable      
        character(*),intent(in)         :: nmlname      !< The name of the array containing the variable
        character(*),intent(in)         :: json         !< A string holding the json
        type(C_PTR),intent(in)          :: cjson        !< A C pointer containing the parsed json
        integer,optional,intent(in)     :: st_size      !< The length of a string
        integer,intent(in)              :: ARIND        !< The Fortran array index for the single value 
        integer,intent(in)              :: Cind         !< The C index of the value in the array
        real(kind=8),pointer            :: r8 (:) => null()
        real(kind=4),pointer            :: r4 (:) => null()

        var => nmlval

 select type (var) !> Select the intrinsic type of the namelist variable
   type is (integer)
       !> If scalar integer, using the integer function integer_array to obtain the variable value
        var(ARIND) = integer_array(cjson,json,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,Cind)
   type is (real(kind=4))
     !> If scalar real, using the real function real_array to obtain the variable value
       r4 => var
        r4(ARIND) = real_array(cjson,json,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,Cind)
       nullify (r4)
   type is (real(kind=8))
     !> If scalar real, using the real function real_array to obtain the variable value
       r8 => var
        r8(ARIND) = real_array(cjson,json,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,Cind)
       nullify (r8)
   type is (logical)
     !> If scalar logical, using the integer function integer_array to obtain the variable value
        var(ARIND) = integer_array(cjson,json,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,Cind)
   type is (character(*))
        if (.not. present (st_size)) call cjson_error_mesg("JSON_ARRAY_VALUE",&
                nmlname//'{'//varname//"} is a string, but the argument ST_SIZE"//&
                " was not included.",FATAL)
        call json_array_value_string (cjson,json,nmlname,varname,var(arind),Cind,st_size)

 end select
end subroutine json_array_value
!> \author Tom Robinson thomas.robinson@noaa.gov
!> \date September 2016
!! json_array_value_string handles string arrays
subroutine json_array_value_string (cjson,json,nmlname,varname,var,Cind,st_size)
use iso_c_binding

        character(*),intent(inout)      :: var
        character(*),intent(in)         :: varname      !< The name of the variable      
        character(*),intent(in)         :: nmlname      !< The name of the array containing the variable
        character(*),intent(in)         :: json         !< A string holding the json
        type(C_PTR),intent(in)          :: cjson        !< A C pointer containing the parsed json
        integer,intent(in)              :: st_size      !< The length of a string
        integer,intent(in)              :: Cind         !< The C index of the value in the array
        type (C_PTR)                                            :: C_st = C_NULL_PTR
        character(len=1,kind=c_char),pointer,dimension(:)       :: F_st => null()
        character(len=:),allocatable                            :: data_string
        integer                         :: i


      allocate (character(len=st_size) :: data_string) !> Allocate the data string
      do i=1,st_size
       data_string(i:i)=" " !> Initialize the data string to be empty
      enddo
      C_st = string_array(cjson,json//C_NULL_CHAR,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,Cind) !> Get the string
      call c_f_pointer ( C_St,F_St , [st_size] ) !> Convert the string to a Fortran pointer
      do i=1,st_size !> Loop through the sring characters
              if (F_St(i) == c_null_char) exit  !> End the loop on the null character
              data_string(i:i) = f_st(i) !> Put the individual characters into the data string
      enddo
!      var(ARIND)(1:st_size) = data_string(1:st_size) !> Put the value of the string into var
        var(1:st_size) = data_string(1:st_size) !> Put the value of the string into var

      deallocate (data_string)
      nullify (F_st)
      CALL C_free (C_St)
      C_St = C_NULL_PTR
end subroutine json_array_value_string
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!> \date September 2016
!> JSON_SCALAR is a wrapper for scalar variables.  It checks to see if a variable is a 
!! string.  If it is a string, it passes it to the string handler, if not, it passes it 
!! to json_scalar_val.
subroutine json_scalar (cjson,json,nmlname,varname,nmlval,st_size)
use iso_c_binding
        class (*),target,intent(inout)  :: nmlval       !< The variable
        class (*),pointer               :: var          !> \param var A pointer to the variable
        character(*),intent(in)         :: varname      !< The name of the variable      
        character(*),intent(in)         :: nmlname      !< The name of the array containing the variable
        character(*),intent(in)         :: json         !< A string holding the json
        type(C_PTR),intent(in)          :: cjson        !< A C pointer containing the parsed json
        integer,optional,intent(in)     :: st_size      !< The length of a string

        logical                         :: isString     !> \param isString True if the variable is a character(*)


 var => nmlval
 isString = .false.
   select type (var)
        type is (character(*))
         isString = .true.
   end select
 if (isString) then
        call json_scalar_value (cjson,json,nmlname,varname,nmlval,st_size)
 else
        call json_scalar_value (cjson,json,nmlname,varname,nmlval,st_size)
 endif

 nullify (var)
end subroutine json_scalar
!> \author Tom Robinson thomas.robinson@noaa.gov
!> \date September 2016
!! JSON_ARRAY is the top level array parser.  It searches the JSON array nmlname for the 
!! varname string.  It finds any instances of varname and overwrites values in order of 
!! appearance.  It also searches for parts of an array, and extracts those values as well.
!! \verbatim
!! This routine will find: 
!! array
!! array (i)
!! array (i1:i2)
!! array (i,j)
!! \endverbatim
subroutine json_array (cjson,json,nmlname,varname,nmlval,lowb,st_size,jlowb,idimsize)
use iso_c_binding
        integer,intent(in)              :: lowb         !< The lower bound of the array
        class (*),target,intent(inout)  :: nmlval(lowb:)!< The variable
        class (*),pointer               :: var    (:)   !> \param var A pointer to the variable
        character(*),intent(in)         :: varname      !< The name of the variable      
        character(*),intent(in)         :: nmlname      !< The name of the array containing the variable
        character(*),intent(in)         :: json         !< A string holding the json
        type(C_PTR),intent(in)          :: cjson        !< A C pointer containing the parsed json
        integer,optional,intent(in)     :: st_size      !< The length of a string
        integer,optional,intent(in)     :: jlowb        !< The lower bound of the second dimension for a 2D array
        integer,optional,intent(in)     :: idimsize     !< The size of the first dimension in a 2D array      
        
        integer                         :: nmllen       !> \param nmllen The length of the nmlname string 
        integer                         :: varlen       !> \param varlen The length of the varname string
        integer                         :: nmlloc       !> \param nmlloc The location of the namelist in the json file
        logical                         :: onenml       !> \param onenml Makes sure the namelist is only listed once
        logical                         :: nmlbracket   !> \param nmlbracket Inside the nml array
        logical                         :: varbracket   !> \param varbracket Inside a variable array

        integer                         :: js           !> \param js The size of the array in the JSON

        character(len=:),allocatable    :: varcheck     !> \param varcheck The part of the array call in ()s
        character(len=:),allocatable    :: jvname       !> \param jvname varname in the JSON file to match case 
        character(len=:),allocatable    :: jnname       !> \param jnname nmlname in the JSON file to match case 
        logical                         :: colon        !> \param colon Set to true if a : is found in varcheck
        logical                         :: comma        !> \param comma Set to true if a , is found in varcheck
        integer                         :: n1,com1,com2,col1,col2,pos
        logical                         :: secondcolon  !> \param secondcolon Are there two colons?
        integer                         :: colloc1,colloc2,comloc

        integer                         :: i,k,ic,ii,jj,begin,ending

 var => nmlval

 nmllen=len(trim (nmlname))
 varlen=len(trim (varname))
 allocate (character(len=varlen):: jvname)
 allocate (character(len=nmllen):: jnname)
 onenml=.false.
 nmlbracket=.false.
 varbracket=.false.
 colon = .false.
 comma = .false.
 secondcolon = .false.

   nmlloop: do k=1,len(json)-(nmllen+1) !> Loop through the json
        IF (strcase(json(k:k+nmllen+1)) == '"'//strcase(trim(nmlname))//'"') then !> Find the NMLname
                jnname = json(k+1:k+nmllen)
            if (onenml) then
                call cjson_error_mesg("json_array",&
                   nmlname//" appears more than once.",fatal)
            else                
                onenml = .true.
                nmlloc = k+nmllen+1
            endif    
        ENDIF
   ENDDO nmlloop
 if (.not. onenml) call cjson_error_mesg("json_array",&
                   nmlname//" is not in the json file.",fatal)

 varloop: do k = nmlloc,len(json)-(len(varname)+2) !> Now search for varname
        if     (json(k:k) == "[" .and. .not. nmlbracket) then !> Begin searching the namelist array
                nmlbracket = .true.
        elseif (json(k:k) == "[" .and. nmlbracket) then !> Inside of a variable array - signal not to search 
                varbracket = .true.
        elseif (json(k:k) == "]" .and. varbracket) then !> Ending the variable array - search again
                varbracket = .false.
        elseif (json(k:k) == "]" .and. .not. varbracket) then !> The end of the namelist - exit
                exit
        elseif (strcase(json( k:k+len(varname) )) == '"'//strcase(trim (varname)) .AND. & 
                .not.varbracket .AND. nmlbracket) then !> When the variable array name is found
                jvname=json(k+1:k+len(varname))
                ! !!!!!!!!!!!!!!!!!!!!!!!!!!!  NORMAL  !!!!!!!!!!!!!!!!!!!!! !
                if (json( k+len(varname)+1 : k+len(varname)+1 ) == '"') then !> When its just a normal array
                       CALL cjson_array_length (json,nmlname,varname,size(nmlval),js)
                        begin=lowb
                        if (js == size(var) .OR. js>size(var) ) then
                            ending = ubound(var,dim=1)
                        elseif (js < size(var)) then 
                            ending = lowb+js-1
                        endif 
                        ic=0
                        do i=begin,ending !> Loop through the array and get the values
                                call json_array_value (cjson, json, jnname, &
                                            jvname , var, i, ic, lowb, st_size)
                                ic=ic+1
                        enddo
                !!!!!!!!!!!!!!!!!!!!!!!!!! PART OF AN ARRAY !!!!!!!!!!!!!!!!!!!!!!!!
                elseif (json( k+len(varname)+1 : k+len(varname)+1 ) == '(') then !> When its part of an array
                        do i=k+len(varname)+2,len(json)
                                if (json(i-1:i)==')"') then 
                                 varcheck = json(k+len(varname)+2:i-2) !> Find whats in the parenthesis
                                 exit
                                elseif (json(i:i)=='}') then !> This shouldnt happen, so call a Fatal
                                  varcheck = json(k+len(varname)+2:i-2)
                                   call cjson_error_mesg("json_array",&
                                    'SYNTAX ERROR - "'//nmlname//'":[{"'//varname//varcheck// &
                                     "}] is missing a )",fatal)
                                endif
                        enddo
                        if (i>=len(json))call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//"}] is missing a )",fatal)
                        varcheckloop: do i=1,len(varcheck) !> Loop through varcheck to see if there are colons or commas
                                if (varcheck(i:i) == ",") then
                                  comma=.true. ; n1 = i ; comloc = i
                                elseif (varcheck(i:i) == ":" .AND. colon)then
                                  secondcolon=.true. ; colloc2 = i
                                elseif (varcheck(i:i) == ":")then
                                  colon=.true. ; n1 = i ; colloc1 = i
                                
                                
                                endif
                        enddo varcheckloop
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                if (.not.colon .AND. .not.comma) then !> For a single value in a 1D array
                                    if(.not. string_is_num(varcheck)) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//varcheck//&
                                          " is not a number",FATAL)
                                    endif
                                    read(varcheck,'(I8)')n1 !> Convert the string to an integer
                                    call bounds_check (lbound(var,dim=1),ubound(var,dim=1),varname,single=n1)
                                    call json_scalar_value (cjson,json,nmlname,&
                                        varname//'('//varcheck//')',var(n1),st_size) !> Get the single value
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                elseif (colon .AND. .not.comma) then !> For an array range
                                    if(.not. string_is_num( varcheck(1:n1-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(1:n1-1)//" is not a number",FATAL)
                                    endif
                                    if(.not. string_is_num( varcheck(n1+1:len(varcheck)) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(n1+1:len(varcheck))//" is not a number",FATAL)
                                    endif
                                    read(varcheck(1:n1-1),'(I8)')col1 !> Convert the range strings to integers
                                    read(varcheck(n1+1:len(varcheck)),'(I8)')col2
                                    call bounds_check (lbound(var,dim=1),ubound(var,dim=1),varname,lba=col1,uba=col2)

                                    if (col2<col1) then
                                        call cjson_error_mesg ("json_array", "The array limits in "//&
                                        nmlname//'[{'//varname//'('//varcheck//")} are descending.",FATAL)
                                    endif
                       CALL cjson_array_length (json,nmlname,varname//'('//varcheck//")",col2-col1+1,js)
                        begin=col1
                        if (js >= col2-col1+1) then
                            ending = col2
                        elseif (js < size(var)) then 
                            ending = col1+js-1
                        endif
                                     ic=0
                                    do i=begin,ending
                                        call json_array_value (cjson, json, nmlname, &
                                             varname//'('//varcheck//')', &
                                             var, i, ic, lowb, st_size)  
                                        ic=ic+1
                                    enddo
                                   colon=.false.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                elseif (.not.colon .AND. comma) then !> For a single value in a 2d Array
                                    if (.not.present(jlowb) .or. .not.present(idimsize)) then
                                        call cjson_error_mesg("json_array",&
                                        "The required arguments for a 2D array to calculate the position that "//&
                                        varname//varcheck//" should go were not passed.",FATAL)
                                    else !> If the requirements to calculate pos are available
                                    if(.not. string_is_num( varcheck(1:n1-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(1:n1-1)//" is not a number",FATAL)
                                    endif
                                    if(.not. string_is_num( varcheck(n1+1:len(varcheck)) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(n1+1:len(varcheck))//" is not a number",FATAL)
                                    endif
                                    read(varcheck(1:n1-1),'(I8)')com1 !> Convert the range strings to integers
                                    read(varcheck(n1+1:len(varcheck)),'(I8)')com2
                                    call bounds_check (lbound(var,dim=1),lowb+idimsize-1,varname,&
                                        com1=com1,com2=com2,lowb2=jlowb,upb2=(size(var)/idimsize)+jlowb-1)
                                    !> Calculate the position using 
                                    !! \code
                                    !! pos = IdimSize(j-jlowb)+i
                                    !! \endcode
                                    pos = ((com2-jlowb)*idimsize)+com1 !> Calculate the position using 
                                    call json_scalar_value (cjson,json,jnname,&
                                        jvname//'('//varcheck//')',&
                                        var(pos),st_size) !> Get the single value
                                   comma=.false.
                                   endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                elseif (colon .and. comma) then !> FOr a range of values in a 2D array
                                  if (.not.present(jlowb) .or. .not.present(idimsize)) then
                                   call cjson_error_mesg("json_array",&
                                   "The required arguments for a 2D array to calculate the position that "//&
                                   varname//varcheck//" should go were not passed.",FATAL)
                                  endif 
                                  if (secondcolon) then

                                    if(.not. string_is_num( varcheck(1:colloc1-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(1:colloc1-1)//" is not a number",FATAL)
                                     elseif (.not. string_is_num( varcheck(colloc1+1:comloc-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(colloc1+1:comloc-1)//" is not a number",FATAL)
                                     elseif (.not. string_is_num( varcheck(comloc+1:colloc2-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(comloc+1:colloc2-1)//" is not a number",FATAL)
                                     elseif (.not. string_is_num( varcheck(colloc2+1:len(varcheck)) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(colloc2+1:len(varcheck))//" is not a number",FATAL)
                                    endif !> String to number checking
                                    read(varcheck        (1:colloc1-1),'(I8)')    col1
                                    read(varcheck(colloc1+1:comloc-1),'(I8)')     com1 !> Convert the range strings to integers
                                    read(varcheck (comloc+1:colloc2-1),'(I8)')    com2
                                    read(varcheck(colloc2+1:len(varcheck)),'(I8)')col2 !> Convert the range strings to integers
                                    call bounds_check (lbound(var,dim=1),lowb+idimsize-1,varname,lba=col1,uba=com1)
                                    call bounds_check (lowbound=jlowb,upbound=(size(var)/idimsize)+jlowb-1,&
                                         varname=varname,lba=com2,uba=col2)
                                    if (col2<com2 .OR. com1<col1) then
                                        call cjson_error_mesg ("json_array", "The array limits in "//&
                                        nmlname//'[{'//varname//'('//varcheck//")} are descending.",FATAL)
                                    endif
                             CALL cjson_array_length (json,nmlname,varname//'('//varcheck//")",(col2-com2+1)+(com1-col1+1),js)
                                    if (js .NE. (col2-com2+1)+(com1-col1+1)) CALL cjson_error_mesg("json_array",&
                                    "The 2D array "//nmlname//'[{'//varname//'('//varcheck//")} does not match the size of the"//&
                                    " array in the JSON.",FATAL)
                                     ic=0
                                    do jj=com2,col2
                                    do ii=col1,com1
                                        pos=((jj-jlowb)*idimsize)+ii
                                        call json_array_value (cjson, json, nmlname, &
                                             varname//'('//varcheck//')', &
                                             var, pos, ic, lowb, st_size)  
                                        ic=ic+1
                                    enddo
                                    enddo
                                        
                                  elseif (comloc < colloc1) then !> When the array is in dim=2 of the variable
                                    if(.not. string_is_num( varcheck(1:comloc-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(1:comloc-1)//" is not a number",FATAL)
                                     elseif (.not. string_is_num( varcheck(comloc+1:colloc1-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(comloc+1:colloc1-1)//" is not a number",FATAL)
                                     elseif (.not. string_is_num( varcheck(colloc1+1:len(varcheck)) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(colloc1+1:len(varcheck))//" is not a number",FATAL)
                                    endif !> String to number checking
                                    read(varcheck(1:comloc-1),'(I8)')com1
                                    read(varcheck(comloc+1:colloc1-1),'(I8)')col1 !> Convert the range strings to integers
                                    read(varcheck(colloc1+1:len(varcheck)),'(I8)')col2
                                     call bounds_check (lbound(var,dim=1),lowb+idimsize-1,varname,single=com1)
                                     call bounds_check (lowbound=jlowb,upbound=(size(var)/idimsize)+jlowb-1,&
                                         varname=varname,lba=col1,uba=col2)
                                    if (col2<col1) then
                                        call cjson_error_mesg ("json_array", "The array limits in "//&
                                        nmlname//'[{'//varname//'('//varcheck//")} are descending.",FATAL)
                                    endif
                       CALL cjson_array_length (json,nmlname,varname//'('//varcheck//")",col2-col1+1,js)
                        begin=col1
                        if (js >= col2-col1+1) then
                            ending = col2
                        elseif (js < size(var)) then 
                            ending = col1+js-1
                        endif
                                     ic=0
                                    do i=begin,ending

                                        pos=((i-jlowb)*idimsize)+com1
                                        call json_array_value (cjson, json, nmlname, &
                                             varname//'('//varcheck//')', &
                                             var, pos, ic, lowb, st_size)  
                                        ic=ic+1
                                    enddo
                                  elseif (colloc1 < comloc) then !> When the array is in dim=1 of the variable
                                    if(.not. string_is_num( varcheck(1:colloc1-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(1:colloc1-1)//" is not a number",FATAL)
                                     elseif (.not. string_is_num( varcheck(colloc1+1:comloc-1) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(colloc1+1:comloc-1)//" is not a number",FATAL)
                                     elseif (.not. string_is_num( varcheck(comloc+1:len(varcheck)) )) then
                                     call cjson_error_mesg("json_array",&
                                          nmlname//'[{'//varname//'('//varcheck//")}] -> "//&
                                          varcheck(comloc+1:len(varcheck))//" is not a number",FATAL)
                                    endif !> String to number checking
                                    read(varcheck(1:colloc1-1),'(I8)')col1
                                    read(varcheck(colloc1+1:comloc-1),'(I8)')col2 !> Convert the range strings to integers
                                    read(varcheck(comloc+1:len(varcheck)),'(I8)')com2
                                     call bounds_check (lbound(var,dim=1),lowb+idimsize-1,varname,lba=col1,uba=col2)
                                     call bounds_check (lowbound=jlowb,upbound=(size(var)/idimsize)+jlowb-1,&
                                         varname=varname,single=com2)
                                    if (col2<col1) then
                                        call cjson_error_mesg ("json_array", "The array limits in "//&
                                        nmlname//'[{'//varname//'('//varcheck//")}] are descending.",FATAL)
                                    endif
                       CALL cjson_array_length (json,nmlname,varname//'('//varcheck//")",col2-col1+1,js)
                        begin=col1
                        if (js >= col2-col1+1) then
                            ending = col2
                        elseif (js < size(var)) then 
                            ending = col1+js-1
                        endif
                                     ic=0
                                    do i=begin,ending
                                        pos=((com2-jlowb)*idimsize)+i
                                        call json_array_value (cjson, json, nmlname, &
                                             varname//'('//varcheck//')', &
                                             var, pos, ic, lowb, st_size)  
                                        ic=ic+1
                                    enddo
                                  endif
                                colon=.false.
                                comma=.false.
                                secondcolon=.false.
                                endif !comma/colon checking if
                                        
                endif
        endif
 enddo varloop
 !> Memory cleanup
 if (allocated(varcheck)) deallocate(varcheck)
 nullify(var)
        
end subroutine json_array
!> \author Tom Robinson thomas.robinson@noaa.gov
!> \date September 2016
!! JSON_2D converts a 2D array into a 1D array and passes it to json_array.  If the array is a string,
!! it gets passed to a different subroutine that handles 2D strings
subroutine json_2d (cjson,json,nmlname,varname,nmlval,ilowB,jlowB,st_size)
use iso_c_binding
 class (*), target,intent(inout)       	:: nmlval (ilowB:,jlowB:) !< The value of the variable from the namelist/json
 class (*), pointer		      	:: var    (:,:) !> \param var A pointer to the value
 character (*),intent(in)               :: json	        !< String containing the contents of the json file
        type(C_PTR),intent(in)          :: cjson        !< A C pointer containing the parsed json
 character (*),intent(in)           	:: nmlname      !< The name of the namelist that the variable is in
 character (*),intent(in)           	:: varname      !< The name of the variable of interest
 integer, optional,intent(in)           :: st_size      !< Size of a string 
 integer, intent(in)                    :: ilowB, jlowB !< lower bounds of array
 integer                                :: lowB1, lowB2 !< \param lowB1,lowB2 lower bounds of array
 integer                                :: ar_size      !> \param ar_size The size of the array
 integer,allocatable			:: pos (:,:) !> \param pos The index of the 1D array corresponding with the 2D array
 real(kind=8)   , pointer, dimension (:):: rarray	!> \param rarray Real 1D array for conversion 
 real(kind=8)   , pointer, dimension (:):: r8		!> \param ra Real 1D array for conversion with correct low bound
 real(kind=4)   , pointer, dimension (:):: r4rray	!> \param rarray Real 1D array for conversion 
 real(kind=4)   , pointer, dimension (:):: r4		!> \param ra Real 1D array for conversion with correct low bound
 integer, pointer, dimension (:)        :: iarray	!> \param iarray Integer 1D array for conversion
 integer, pointer, dimension (:)        :: ia		!> \param ia Integer 1D array for conversion with correct low bound
 logical, pointer, dimension (:)        :: larray	!> \param larray Logical 1D array for conversion
 logical, pointer, dimension (:)        :: la		!> \param la logical 1D array for conversion with correc

 real(kind=8), pointer, dimension (:,:)	:: re	!> \param re real*8 pointer to var
 real(kind=4), pointer, dimension (:,:)	:: rf	!> \param rf real*4 pointer to var
 integer, pointer, dimension (:,:)	:: ii	!> \param ii integer pointer to var
 logical, pointer, dimension (:,:)	:: ll	!> \param ll logical pointer to var

 var => nmlval !> Point var to the value of the variable from the nml file
!!> Turn the 2D array into a 1D array fur use with the jsonarray subroutine
allocate ( pos(size(nmlval,dim=1),size(nmlval,dim=2)) )
! if (present(ilowb) .and. present(jlowb)) then
        lowb1 = ilowb ; lowb2 = jlowb
! elseif (present(ilowb)) then
!        lowb1 = ilowb ; lowb2 = 1
! elseif (present(jlowb)) then
!        lowb1 = 1 ; lowb2 = jlowb
! else
!        lowb1 = 1 ; lowb2 = 1
! endif
 ar_size = size(nmlval,dim=1)*size(nmlval,dim=2)
 select type (var)

   type is (integer) !> Copy the 2D array into a temporary 1D array
    ii => var !> Use a 2D array pointer
    call c_f_pointer( C_LOC(ii(ilowB,jLowb)), iarray, [ar_size]) !> Point a 1D array pointer to the 2D pointer
    ia(ilowB:) => iarray !> Use the correct lower bound
    call json_array (cjson,json,trim(nmlname),trim(varname),ia,lowB=ilowB,jlowb=jlowb,idimsize=size(var,dim=1))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   type is (real(kind=8)) ! 
    re => var
    call c_f_pointer( C_LOC(re(ilowB,jLowb)), rarray, [ar_size])
    r8(ilowB:) => rarray
    call json_array (cjson,json,trim(nmlname),trim(varname),r8,lowB=ilowB,jlowb=jlowb,idimsize=size(var,dim=1))
   type is (real(kind=4)) ! 
    rf => var
    call c_f_pointer( C_LOC(rf(ilowB,jLowb)), r4rray, [ar_size])
    r4(ilowB:) => r4rray
    call json_array (cjson,json,trim(nmlname),trim(varname),r4,lowB=ilowB,jlowb=jlowb,idimsize=size(var,dim=1))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   type is (logical) !
    ll => var
    call c_f_pointer( C_LOC(ll(ilowB,jLowb)), larray, [ar_size])
    la(ilowB:) => larray
    call json_array (cjson,json,trim(nmlname),trim(varname),la,lowB=ilowB,jlowb=jlowb,idimsize=size(var,dim=1))
   type is (character(*)) !> Handle 2D strings with the routine string_2d 
    if (.not. present (st_size)) call cjson_error_mesg("JSON_SCALAR_VALUE",&
                nmlname//'{'//varname//"} is a string, but the argument ST_SIZE"//&
                " was not included.",FATAL)
    call string_2d (cjson,json,nmlname,varname,var,ilowB,jlowB,st_size)

 end select

 nullify (var)
 if (associated(ii)) then; nullify (ii); nullify (ia); nullify (iarray); endif
 if (associated(re)) then; nullify (re); nullify (rarray); nullify (r8); endif
 if (associated(rf)) then; nullify (rf); nullify (r4rray); nullify (r4); endif
 if (associated(ll)) then; nullify (ll); nullify (la); nullify (larray); endif

end subroutine json_2d

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> @author Tom Robinson thomas.robinson@noaa.gov
!! @date October 2016
!!
!! A user routine to get a single scalar from a json in lieu of using the wrapper
subroutine cjson_single_value_scalar (cjson,json,nmlname,varname,variable,st_size)
use iso_c_binding
        class (*),target,intent(inout)  :: variable     !< The variable
        class (*),pointer               :: var=> null() !> \param var A pointer to the variable
        character(*),intent(in)         :: varname      !< The name of the variable      
        character(*),intent(in)         :: nmlname      !< The name of the array containing the variable
        character(*),intent(in)         :: json         !< A string holding the json
        type(C_PTR),intent(in)          :: cjson        !< A C pointer containing the parsed json
        integer,optional,intent(in)     :: st_size      !< The length of a string
        integer                         :: nmlexist     !> \param nmlexist >1 if nmlname exists on the top-level of the JSON
        integer                         :: varexist     !> \param varexist >1 if the varname exists in the array nmlname

     var => variable !> Point var at the variable
     select type (var)
          type is (character(*)) !> Make sure that the string size is included if the variable is a string
               if (.not. present(st_size) ) then
                call cjson_error_mesg ("cjson_single_value_scalar","The variable "//trim(nmlname)//" {"//trim(varname)// &
                 "} is a string and must be passed with it's size.  Change your call to this routine to: "//NEW_LINE('A')& 
                 //'CALL cjson_single_value_scalar(cjson,json,"'//trim(nmlname)//'","'//trim(varname)//'",'//            &
                 trim(varname)//',st_size=len('//trim(varname) //') )',FATAL)
               endif
     end select
!> Check to see if nmlname is in the top level of the JSON and that 
!! varname is in the array of the JSON
     CALL json_check_for_variable (json,nmlname,varname,nmlexist,varexist) 
!> If the varname exists                     , then varexist ==  1 and nmlname ==  1
!> If the nmlname exists but varname does not, then varexist ==  0 and nmlname ==  1
!> If the nmlname does not exist             , then varexist == -1 and nmlname == -1
     if (varexist > 0) then 
          CALL json_value(cjson,json,nmlname,varname,variable,st_size) !> Get the value from the JSON
     elseif (nmlexist > 0 .AND. varexist <= 0) then !> Print a note that the variable is not listed in nmlname
          call cjson_error_mesg ("cjson_single_value_scalar","The variable "//trim(nmlname)//" {"//trim(varname)// &
            "} is not in the JSON array "//trim(nmlname),NOTE)
     elseif (nmlexist <= 0 ) then !> Print a warning that nmlname is not in the JSON
          call cjson_error_mesg ("cjson_single_value_scalar",trim(nmlname)//"Does not exist in the top level of the JSON"&
            ,WARNING,wtype=-1)
     endif

end subroutine cjson_single_value_scalar

end module cjson_mod
