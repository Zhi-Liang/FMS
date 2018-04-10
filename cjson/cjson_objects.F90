module cjson_objects_mod
!> @ file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date May 2016
!> This module is used to read json values (using cJSON.c) into variables.  It takes the 
!! variable with its intrinsic type (real, integer, logical, character), its name and 
!! namelist name passed as strings. The subroutine determines the intrinsic type and 
!! calls the approrpriate C fuunction listed in the interface.  The C functions are found
!! in getJSONvals.c.  The functions call the approrpriate routines in cJSON to get the 
!! values.  The values are then output as the "nmlvar" (pointer var).
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
use iso_c_binding
use cjson_mod, only: jsoninit
use cjson_error_mod,   only: cjson_error_mesg, FATAL, WARNING, NOTE, strcase   
implicit none
include 'mpif.h'
     interface

         type(C_PTR) function parseJson (json) result (cjson) bind(C, name="parseJson")
           use iso_c_binding
           character(kind=c_char) :: json     !< The json string
         end function

         integer function  cjson_array_length (cjson, nml_name , var_name, attribute) bind(C, name="cjson_array_length")
           use iso_c_binding
           type (C_PTR),intent(in),value  :: cjson    !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable
           character(kind=c_char) :: attribute !< The attribute of the variable object
         end function

         integer function int_jsonscalar (cjson, nml_name , var_name, attribute) bind(C, name="int_jsonscalar")
           use iso_c_binding
           type (C_PTR),intent(in),value  :: cjson    !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable
           character(kind=c_char) :: attribute !< The attribute of the variable object
         end function

         real    function real_jsonscalar (cjson, nml_name , var_name, attribute) bind(C, name="real_jsonscalar")
           use iso_c_binding
           type (C_PTR),intent(in),value  :: cjson    !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable
           character(kind=c_char) :: attribute !< The attribute of the variable object
         end function

         real    function real_jsonarray (cjson, nml_name , var_name, attribute, arind) bind(C, name="real_jsonarray")
           use iso_c_binding
           type (C_PTR),intent(in),value  :: cjson    !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable array
           character(kind=c_char) :: attribute !< The attribute of the variable object
           integer  (kind=c_int), VALUE :: arind !< The index of the array that is being inquiried
         end function

         integer function int_jsonarray (cjson, nml_name , var_name, attribute, arind) bind(C, name="int_jsonarray")
           use iso_c_binding
           type (C_PTR),intent(in),value  :: cjson    !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable array
           character(kind=c_char) :: attribute !< The attribute of the variable object
           integer  (kind=c_int), VALUE :: arind !< The index of the array that is being inquiried
         end function

         function string_jsonscalar (cjson, nml_name , var_name, attribute) result (svalue) bind(C, name="string_jsonscalar")
           use iso_c_binding
           type (C_PTR),intent(in),value  :: cjson    !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable
           character(kind=c_char) :: attribute !< The attribute of the variable object
           type (c_ptr):: svalue  !< The value of the variable (pointer)
         end function    
         
         function string_jsonarray (cjson, nml_name , var_name, attribute, ind) result (svalue) bind(C, name="string_jsonarray")
           use iso_c_binding
           type (C_PTR),intent(in),value  :: cjson    !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable
           character(kind=c_char) :: attribute !< The attribute of the variable object
           integer  (kind=c_int), VALUE :: ind !< The index of the array
           type (c_ptr):: svalue  !< The value of the variable (pointer)
         end function

         integer function existval (cjson, nml_name , var_name, attribute) bind(C, name="existval")
           use iso_c_binding
           type (C_PTR),intent(in),value  :: cjson    !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable
           character(kind=c_char) :: attribute !< The attribute of the variable object
         end function

         integer function convert_int (input_unit, variable) bind(C, name="convert_int")
           use iso_c_binding
           character(kind=c_char) :: input_unit !< The units being checked
           integer  (kind=c_int), VALUE :: variable !< The current value of the variable
         end function

         real    function convert_float  (input_unit, variable) bind(C, name="convert_float")
           use iso_c_binding
           character(kind=c_char) :: input_unit !< The units being checked
           real  (kind=c_float), VALUE :: variable !< The current value of the variable
         end function

         real    function convert_check (input_unit, variable) bind(C, name="convert_check")
           use iso_c_binding
           character(kind=c_char) :: input_unit !< The units being checked
           real  (kind=c_float), VALUE :: variable !< The current value of the variable
         end function

     end interface


 INTERFACE cjsonObject !> This is the user interface
        module procedure cjsonObject_scalar_value
        module procedure cjsonObject_array_value
 END INTERFACE

 CONTAINS

!> \author Tom Robinson thomas.robinson@noaa.gov 2016
!> The init subroutine opens the json file, then parses it using the C function parseJson.  The 
!! parsed json file is saved as a c pointer
subroutine cjsonObject_init(json_file,json,cjson)
 character (*),intent(in)                         :: json_file   !< The name of the json file
 character (len=:), allocatable, intent(inout)    :: json        !< String containing the contents of the json file
 integer                                          :: json_length !< The length of the json string
 type (C_PTR), intent(inout)                      :: cjson       !< The json string

 CALL jsoninit (json_file,json,json_length)
       cjson = parseJson (trim(json)//c_null_char)
        
end subroutine cjsonObject_init

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SCALARS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date May 2016
!! Any json scalar values (integer, real, logical, or character) are accepted and their value is 
!! found.  Strings are handled by the string routine.  The default request will search the json 
!! file for "value" and return that value.  If something other than value is wanted, the optional
!! variable attribute can be used (ie. attribute="units").  If the type is string, the size of the
!! string must be included because some compilers do not pass the string size when using class(*).
subroutine cjsonObject_scalar_value (cjson,nmlval,nmlname,varname,attribute,st_size)
use iso_c_binding
 class (*), target,intent(inout)        :: nmlval      !< The value of the variable from the namelist/json
 class (*), pointer                     :: var         !> \param var A pointer to the value
 type (C_PTR),intent(in)                :: cjson       !< The json string
 character (*),intent(in)               :: nmlname     !< The name of the namelist that the variable is in
 character (*),intent(in)               :: varname     !< The name of the variable of interest
 character (*), optional ,intent(in)    :: attribute   !< If something other than the value is requested
 integer, optional ,intent(in)          :: st_size     !< Size of a string 
 character (len=:),allocatable          :: val         !> \param val a string that is "value"
 character (len=10)                     :: theunits    !> \param theunits a string of the units if they exist
 character (len=:),allocatable          :: units
 integer                                :: u_flag      !> \param u_flag 1 if there are units, 0 if there are not
 real                                   :: convert_flag!< \param convert_flag -1.0 if units are not convertable
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 units="units"
 u_flag= existval( cjson, trim(nmlname)//C_NULL_CHAR , trim(varname)//C_NULL_CHAR, &
                   trim(units)//C_NULL_CHAR )
 if (u_flag == 1 .AND. attribute.ne."fillvalue") then
  call cjsonObject_scalar_string (cjson,theunits,nmlname,varname,len(theunits),units)
  call areUnitsConvert (theunits,nmlname,varname,convert_flag) !> Check if units are convertable
 endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 var => nmlval !> Point var to the value of the variable from the nml file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 CALL getValue (val,attribute)

!> Determine the type of the input
 select type (var) 
   type is (character(*)) !> Strings are handled by the string routine
     CALL cjsonObject_scalar_string (cjson,var,nmlname,varname,st_size,val)
   type is (integer)
       !> If scalar integer, use C function int_jsonscalar to get the value of var
          var=int_jsonscalar(cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                             trim(val)//C_NULL_CHAR)
           if (u_flag == 1 .AND. attribute.ne."fillvalue".and. convert_flag > 0) then
                 var=convert_int (trim(theunits)//C_NULL_CHAR, var)
           endif
   type is (real*4)
     !> If scalar real, use C function real_jsonscalar to get the value of var
          var=real_jsonscalar(cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                             trim(val)//C_NULL_CHAR)
           if (u_flag == 1 .AND. attribute.ne."fillvalue" .and. convert_flag > 0) then
                var=convert_float (trim(theunits)//C_NULL_CHAR, var)
           endif
   type is (real*8)
     !> If scalar real, use C function real_jsonscalar to get the value of var
          var=real_jsonscalar(cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                             trim(val)//C_NULL_CHAR)
           if (u_flag == 1 .AND. attribute.ne."fillvalue" .and. convert_flag > 0) then
                var=convert_float (trim(theunits)//C_NULL_CHAR, var)
           endif
   type is (logical)
     !> If scalar logical, use C function int_jsonscalar to get the value of var
          var=int_jsonscalar(cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                             trim(val)//C_NULL_CHAR)

 end select

end subroutine cjsonObject_scalar_value

!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date May 2016
!! This is the subroutine to handle string scalars in the json file.  Note that VAR has type 
!! character instead of class(*).  The string is found as a C pointer, converted to a fortran 
!! character pointer array, and the array is stored in a 
subroutine cjsonObject_scalar_string (cjson,var,nmlname,varname,st_size,val)
use iso_c_binding
 type (C_PTR),intent(in)                     :: cjson  !< The json string
 character (*),intent(in)                    :: nmlname!< The name of the namelist that the variable is in
 character (*),intent(in)                    :: varname!< The name of the variable of interest
 integer,intent(in)                          :: st_size!< Size of a string
 character (len=st_size),intent(inout)       :: var    !< var The string variable 
 character (len=:),allocatable,intent(in)    :: val    !<  A string that is the name of what is being sought
!! To deal with strings                                                      !!
 character (len=1, kind=c_char), dimension (:), pointer :: ErrChars => null ()!> \param ErrChars gets the individual characters
 type (C_PTR) :: C_String_ptr                                                 !> \param C_String_ptr a C-string pointer
 integer                        :: i            !> \param i Used to get the number of characters of interest in ErrChars

 var="                                       "
!> Use a C pointer to get the string of interest from the C function string_jsonscalar
   C_String_ptr=string_jsonscalar(cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                                  trim(val)//C_NULL_CHAR)
!> Convert the C pointer to a fortran pointer
   call c_f_pointer ( C_String_ptr,ErrChars , [st_size] )
!> C-strings are arrays, so the array of strings is converted into a regular string in Fortran 
    do i=1,st_size 
           if ( ErrChars (i) == c_null_char) exit  !> End the loop on the null character
           var(i:i) = ErrChars (i) !> Put each value of the string array into the correct character location
    enddo
end subroutine cjsonObject_scalar_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ARRAYS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date May 2016
!! Any json array values (integer, real, logical, or character) are accepted and their values are 
!! found.  Strings are handled by the string routine.  The default request will search the json 
!! file for "value" and return that value.  If something other than value is wanted, the optional
!! variable attribute can be used (ie. attribute="units").  If the type is string, the size of the
!! string must be included because some compilers do not pass the string size when using class(*).
subroutine cjsonObject_array_value (cjson,json,nmlval,nmlname,varname,attribute,st_size,lowB)
use iso_c_binding
 class (*), target,intent(inout)        :: nmlval(:)   !< The value of the variable from the namelist/json
 class (*), pointer                     :: var   (:)   !> \param var A pointer to the value
 type (C_PTR),intent(in)                :: cjson       !< The json string C pointer
 character (*),intent(in)               :: json        !< The json string
 character (*),intent(in)               :: nmlname     !< The name of the namelist that the variable is in
 character (*),intent(in)               :: varname     !< The name of the variable of interest
 character (*), optional,intent(in)     :: attribute   !< If something other than the value is requested
 integer, optional,intent(in)           :: st_size     !< Size of a string 
 integer, optional,intent(in)           :: lowB        !< THe lower bound of the array
 integer                                :: ilowB
 character (len=:),allocatable          :: val         !< \param value a string that is "value"
 integer                                :: valueflag   !> \param valueflag Set to 1 if value exists in the json, 0 if not
 integer                                :: ii          !> \param ii Used for indexing the array
 integer                                :: cc          !> \param cc used for indexing the json array in C
 integer                                :: json_array_length    !> \param json_array_length The length of the array in the json file
 integer                                :: ar_size     !> \param ar_size The size of the array variable
 integer                                :: array_loop  !> \param array_loop The ending point of the loop for the json array
 integer,pointer                        :: ip (:)      !> \param ip an integer array pointer
 real*8,pointer                         :: rp (:)      !> \param rp an real*8 array pointer
 real*4,pointer                         :: r4 (:)      !> \param rp an real*4 array pointer
 logical,pointer                        :: lp (:)      !> \param lp an logical array pointer
 character(len=:),pointer               :: sp (:)      !> \param sp an string array pointer
 character(len=100)                     :: string_point!> \param string_point string scalar pointer
 character (len=10)                     :: theunits    !< \param theunits a string of the units if they exist
 character (len=:),allocatable          :: units
 integer                                :: u_flag      !< \param u_flag 1 if there are units, 0 if there are not
 real                                   :: convert_flag!< \param convert_flag -1.0 if units are not convertable
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 units="units"
 u_flag= existval( cjson, trim(nmlname)//C_NULL_CHAR , trim(varname)//C_NULL_CHAR, &
                   trim(units)//C_NULL_CHAR )
 if (u_flag == 1 .AND. strcase(attribute).ne."fillvalue") then
  call cjsonObject_scalar_string (cjson,theunits,nmlname,varname,len(theunits),units)
  call areUnitsConvert (theunits,nmlname,varname,convert_flag) !> Check if units are convertable
 endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 var => nmlval
 cc = 0 !> Initialize CC to 0
if ( present(lowB) ) then !> Get the appropriate lower bound
 ilowB = lowB
else
 ilowB = 1
endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 CALL getValue (val,attribute) !> Get the attribute name as val
IF (strcase(val) == "value") then
   !> Fill the array with the fillvalue
   call fillval_for_array (cjson,nmlval,nmlname,varname,st_size)
   !> Check to see if value exists in the json file
   valueflag = existval (cjson, nmlname//C_NULL_CHAR , varname//C_NULL_CHAR, strcase(val)//C_NULL_CHAR)
 if (valueflag == 1) then
 !> Find the length of the array in the json file and the actual size of the array variable
 json_array_length = cjson_array_length (cjson, nmlname//C_NULL_CHAR , varname//C_NULL_CHAR,&
                                         val//C_NULL_CHAR)
 ar_size = size(var)
        CALL compareJsonActual ( json_array_length,ar_size,trim(varname),trim(nmlname) ) 
!> Get the upper bound of the looping dimension for the array based on the sizes of the variable 
!! and json array.
   if (ar_size >= json_array_length) then
      array_loop = json_array_length - (1-ilowB)
   elseif (ar_size < json_array_length) then
      array_loop = ar_size - (1-ilowB)
   endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 select type (var) 
   type is (character(*)) !> Strings are handled by the string routine
!allocate( character (len=st_size) :: string_point )
     sp(ilowB:) => var
     !> Use the string subroutine to get the string values
     CALL cjsonObject_array_string (cjson,json,sp,nmlname,varname,st_size,val,array_loop,.true.,ilowB)

   type is (integer)
     ip(ilowB:) => var
     !> If scalar integer, use C function int_jsonarray to get the value of var
     do ii = ilowB , array_loop
          ip(ii) = int_jsonarray (cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                             trim(val)//C_NULL_CHAR,CC)
           if (u_flag == 1 .AND. strcase(attribute).ne."fillvalue" .AND. convert_flag > 0) then
                 ip(ii)=convert_int (trim(theunits)//C_NULL_CHAR, ip(ii))
           endif
          CC = CC + 1
     enddo
     !> Check to see if there are any single values of the array in the json
       call cjsonObject_inorder_array (cjson,json,ip,nmlname,varname,st_size=st_size,lowB=ilowB)
!
   type is (real*4)
     r4(ilowB:) => var
     !> If scalar real, use C function real_jsonarray to get the value of var
     do ii = ilowB , array_loop
          r4(ii) = real_jsonarray (cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                             trim(val)//C_NULL_CHAR,CC)
           if (u_flag == 1 .AND. strcase(attribute).ne."fillvalue" .AND. convert_flag > 0) then
                 r4(ii)=convert_float (trim(theunits)//C_NULL_CHAR, r4(ii))
           endif
          CC = CC + 1
     enddo
       call cjsonObject_inorder_array (cjson,json,rp,nmlname,varname,st_size=st_size,lowB=ilowB)
   type is (real*8)
     rp(ilowB:) => var
     !> If scalar real, use C function real_jsonarray to get the value of var
     do ii = ilowB , array_loop
          rp(ii) = real_jsonarray (cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                             trim(val)//C_NULL_CHAR,CC)
           if (u_flag == 1 .AND. strcase(attribute).ne."fillvalue" .AND. convert_flag > 0) then
                 rp(ii)=convert_float (trim(theunits)//C_NULL_CHAR, rp(ii))
           endif
          CC = CC + 1
     enddo
       call cjsonObject_inorder_array (cjson,json,rp,nmlname,varname,st_size=st_size,lowB=ilowB)
!
   type is (logical)
     lp(ilowB:) => var
     !> If scalar logical, use C function int_jsonarray to get the value of var
     do ii = ilowB , array_loop
          lp(ii) = int_jsonarray (cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                             trim(val)//C_NULL_CHAR,CC)
          CC = CC + 1
     enddo
     !> Check to see if there are any single values of the array in the json
       call cjsonObject_inorder_array (cjson,json,lp,nmlname,varname,st_size=st_size,lowB=ilowB)

 end select
 elseif (valueflag == 0) then !> if value is not in the json file, check to see if any single values or partials exist
                                !! and appropriately fill the array
     select type (var) 

   type is (character(*)) !> Strings are handled by the string routine
     sp(ilowB:) => var

        call cjsonObject_inorder_array (cjson,json,nmlval,nmlname,varname,st_size=st_size,lowB=ilowB)

   type is (integer)
     ip(ilowB:) => var
       call cjsonObject_inorder_array (cjson,json,ip,nmlname,varname,st_size=st_size,lowb=ilowB)
   type is (real*4)
     r4(ilowB:) => var
       call cjsonObject_inorder_array (cjson,json,r4,nmlname,varname,st_size=st_size,lowb=ilowB)
   type is (real*8)
     rp(ilowB:) => var
       call cjsonObject_inorder_array (cjson,json,rp,nmlname,varname,st_size=st_size,lowb=ilowB)
   type is (logical)
     lp(ilowB:) => var
       call cjsonObject_inorder_array (cjson,json,lp,nmlname,varname,st_size=st_size,lowb=ilowB)

 end select 
     
 endif
ELSEIF (strcase(val) == "fillvalue") then !> If fillvalue is sent into the array routine, the array is filled with
                                 !! the fillvalue
    select type (var) 
        type is (integer)
                ip(ilowB:) => var !> Point to the array
                !> Get the fillvalue
                CALL cjsonObject_scalar_value (cjson,ip(ilowB),nmlname,varname,attribute=val) 
                var = ip(ilowB) !> Fill the array with the fill value
        type is (real*4)
                r4(ilowB:) => var
                CALL cjsonObject_scalar_value (cjson,r4(ilowB),nmlname,varname,attribute=val)
                var = r4(ilowB)
        type is (real*8)
                rp(ilowB:) => var
                CALL cjsonObject_scalar_value (cjson,rp(ilowB),nmlname,varname,attribute=val)
                var = rp(ilowB)
        type is (logical)
                lp(ilowB:) => var
                CALL cjsonObject_scalar_value (cjson,lp(ilowB),nmlname,varname,attribute=val)
                var = lp(ilowB)
        type is (character(*))
                !> Get the fillvalue string as STRING_POINT
                string_point=""
                CALL cjsonObject_scalar_string (cjson,string_point,nmlname,varname,st_size,val) 
                var = trim(string_point) !> Fill the array with the fill value

    end select
ELSEIF (val(1:1) == "(") then !> If a partial of the array is requested, then call the partial array routine
     if (present(st_size) )then
     else
          call cjsonObject_partial (cjson,nmlval,nmlname,varname,attribute,lowB=ilowB) 
     endif
ELSE !> If it's some other array being requested
      if (existval (cjson, nmlname//C_NULL_CHAR , varname//C_NULL_CHAR, trim(val)//C_NULL_CHAR) == 0) then
        CALL cjson_error_mesg('cjson_object_mod', trim(val)//" Does not exist in "//trim(varname)//" - "//&
                    trim(nmlname),FATAL)
      elseif (existval (cjson, nmlname//C_NULL_CHAR , varname//C_NULL_CHAR, trim(val)//C_NULL_CHAR) == 1) then
        !> Get the JSON and Fortran array lengths
        json_array_length = cjson_array_length (cjson, nmlname//C_NULL_CHAR , varname//C_NULL_CHAR,&
                            val//C_NULL_CHAR)
        ar_size = size(var)
        CALL compareJsonActual ( json_array_length,ar_size,trim(varname),trim(nmlname) ) 
        !> Get the upper bound of the looping dimension for the array based on the sizes of the variable 
        !! and json array.
        if (ar_size >= json_array_length) then
           array_loop = json_array_length - (1-ilowB)
        elseif (ar_size < json_array_length) then
           array_loop = ar_size - (1-ilowB)
        endif

      select type (var) 
   type is (character(*)) !> Strings are handled by the string routine
     sp(ilowB:) => var
     !> Use the string subroutine to get the string values
     CALL cjsonObject_array_string (cjson,json,sp,nmlname,varname,st_size,val,array_loop,.true.,ilowB)
   type is (integer)
     ip(ilowB:) => var
     !> If scalar integer, use C function int_jsonarray to get the value of var
     do ii = ilowB , array_loop
          ip(ii) = int_jsonarray (cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                             trim(val)//C_NULL_CHAR,CC)
           if (u_flag == 1 .AND. strcase(val).ne."fillvalue" .AND. convert_flag > 0) then
                 ip(ii)=convert_int (trim(theunits)//C_NULL_CHAR, ip(ii))
           endif
          CC = CC + 1
     enddo
!
   type is (real*4)
     r4(ilowB:) => var
     !> If scalar real, use C function real_jsonarray to get the value of var
     do ii = ilowB , array_loop
          r4(ii) = real_jsonarray (cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                             trim(val)//C_NULL_CHAR,CC)
           if (u_flag == 1 .AND. strcase(val).ne."fillvalue" .AND. convert_flag > 0) then
                 r4(ii)=convert_float (trim(theunits)//C_NULL_CHAR, r4(ii))
           endif
          CC = CC + 1
     enddo
   type is (real*8)
     rp(ilowB:) => var
     !> If scalar real, use C function real_jsonarray to get the value of var
     do ii = ilowB , array_loop
          rp(ii) = real_jsonarray (cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                             trim(val)//C_NULL_CHAR,CC)
           if (u_flag == 1 .AND. strcase(val).ne."fillvalue" .AND. convert_flag > 0) then
                 rp(ii)=convert_float (trim(theunits)//C_NULL_CHAR, rp(ii))
           endif
          CC = CC + 1
     enddo
!
   type is (logical)
     lp(ilowB:) => var
     !> If scalar logical, use C function int_jsonarray to get the value of var
     do ii = ilowB , array_loop
          lp(ii) = int_jsonarray (cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                             trim(val)//C_NULL_CHAR,CC)
          CC = CC + 1
     enddo
      end select
      endif
ENDIF
end subroutine cjsonObject_array_value
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date June 2016
!! The subroutine processes the keys in an object variable that are partial arrays (not strings)
!! It finds the goup (namelist) name, then finds the variable.  It scans inside the variable 
!! object for any parenthesis.  Once found, it then looks for a : inside of the parenthesis.  
!! The parenthesis string is then sent to a different subroutine to get the value.
subroutine cjsonObject_inorder_array (cjson,json,nmlval,nmlname,varname,st_size,lowB)
use iso_c_binding
 class (*), target,intent(inout)   :: nmlval (:)  !< The value of the variable from the namelist/json
 class (*), pointer                :: var    (:)  !> \param var A pointer to the value
 type (C_PTR),intent(in)           :: cjson       !< The json string C pointer
 character (*),intent(in)          :: json        !< The json string
 character (*),intent(in)          :: nmlname     !< The name of the namelist that the variable is in
 character (*),intent(in)          :: varname     !< The name of the variable of interest
 integer, optional ,intent(in)     :: st_size     !< Size of a string 
 integer,intent(in)                :: lowB        !< THe lower bound of the array
 integer                           :: ii          !> \param ii Used for indexing the array
 logical                           :: level1 ,level2,paren,quote,iscolon
 integer                           :: i,l,j,k
 character (len=:),allocatable     :: thevariable
 level1 = .false.
 level2 = .false.
 paren  = .false.
 quote  = .false.
 iscolon = .false.
 findnml: do i=1,len(json)
 var(lowB:) => nmlval

    if (strcase(trim(nmlname)) == strcase(json(i:i+len(nmlname)-1)) ) then 
      do j=i+len(nmlname)-1,len(json)
           if(json(j:j) == "{" ) then
            l=j
            exit findnml
           endif
      enddo
    endif
  enddo findnml
  if (j>=len(json) ) then
CALL cjson_error_mesg('cjson_object_mod', "JSON - "//trim(nmlname)//" not found.",FATAL)
  endif


 findvars: do i=l+1,len(json)
 if (json(i:i) == "}" .and. .not.level1 .and. .not.level2)then
                 exit findvars
 elseif (json(i:i) == "{" .and. .not.level1) then
            level1=.true.
 elseif (json(i:i) == "{" .and. level1 .and. .not.level2)then
             level2=.true.
 elseif (json(i:i) == "}" .and. level1 .and. .not.level2)then
             level1 = .false.
 elseif (json(i:i) == "}" .and. level1 .and. level2)then
             level2=.false.
 elseif (json(i:i)==":".and. .not.level1 .and. .not.level2 .AND.& 
                   .not.paren  .AND. .not.quote ) then
              do j=i-1,l,-1
                   if (json(j:j) == '"' .and. .not.quote) then
                                 quote = .true.
                                 k=j-1
                   elseif (json(j:j) == '"' .and. quote) then
                                 quote = .false. 
                                 exit
                   elseif (json(j:j) == ")" .and. .not.paren) then
                                 paren = .true.
                   elseif (json(j:j) == "(" .and. paren) then
                                 paren = .false. 
                                 k=j-1
                   elseif (json(j:j) == "(" .and. .not.paren) then
                                 exit
                   elseif ( .not.paren .AND. quote  )then
                                 thevariable = json(j:k)
                       if (strcase(thevariable) == strcase(varname)) then
                           exit findvars
                        endif
                   endif
             enddo
         endif
 enddo findvars
 objectparse: do k=i,len(json)
    if (json(k:k) == "(" ) then
        do j=k,len(json)
                if (json(j:j) == ")" .and. iscolon) then
                      call cjsonObject_partial (cjson,var,nmlname,varname,json(k:j),st_size=st_size,lowB=lowB)
                 iscolon=.false.
                 exit
                elseif (json(j:j) == ")") then
                 read(json(k+1:j-1),*) ii
                 call cjsonObject_single (cjson,var(ii),nmlname,varname,&
                      ubound(var,dim=1),lbound(var,dim=1),ii,st_size=st_size)
                 exit
                elseif (json(j:j) == ":") then
                  iscolon=.true.
                endif            
        enddo   
    elseif (json(k:k) == "}" ) then
     exit objectparse
    endif
 enddo objectparse

end subroutine cjsonObject_inorder_array
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date June 2016
!! The subroutine processes the keys in an object variable that are partial string arrays only.
!! It finds the goup (namelist) name, then finds the variable.  It scans inside the variable 
!! object for any parenthesis.  Once found, it then looks for a : inside of the parenthesis.  
!! The parenthesis string is then sent to a different subroutine to get the value.
subroutine cjsonObject_inorder_string (cjson,json,var,nmlname,varname,st_size,lowB)
use iso_c_binding

 type (C_PTR),intent(in)      :: cjson       !< The json string C pointer
 character (*),intent(in)     :: json        !< The json string
 character (*),intent(in)     :: nmlname     !< The name of the namelist that the variable is in
 character (*),intent(in)     :: varname     !< The name of the variable of interest
 integer,intent(in)           :: st_size     !< Size of a string 
 integer,intent(in)           :: lowB        !< THe lower bound of the array
 character (*),intent(inout)  :: var(lowB:)  !> \param var A pointer to the value
 integer                      :: ii          !> \param ii Used for indexing the array
 logical                      :: level1 ,level2,paren,quote,iscolon
 integer                      :: i,l,j,k
 character (len=:),allocatable:: thevariable
 level1 = .false.
 level2 = .false.
 paren  = .false.
 quote  = .false.
 iscolon = .false.

 findnml: do i=1,len(json)
    if (strcase(trim(nmlname)) == strcase(json(i:i+len(nmlname)-1)) ) then 
      do j=i+len(nmlname)-1,len(json)
           if(json(j:j) == "{" ) then
            l=j
            exit findnml
           endif
      enddo
    endif
  enddo findnml
  if (j>=len(json) ) then
 CALL cjson_error_mesg('cjson_object_mod', "JSON - "//trim(nmlname)//" not found.",FATAL)
  endif


 findvars: do i=l+1,len(json)
 if (json(i:i) == "}" .and. .not.level1 .and. .not.level2)then
                 exit findvars
 elseif (json(i:i) == "{" .and. .not.level1) then
            level1=.true.
 elseif (json(i:i) == "{" .and. level1 .and. .not.level2)then
             level2=.true.
 elseif (json(i:i) == "}" .and. level1 .and. .not.level2)then
             level1 = .false.
 elseif (json(i:i) == "}" .and. level1 .and. level2)then
             level2=.false.
 elseif (json(i:i)==":".and. .not.level1 .and. .not.level2 .AND.& 
                   .not.paren  .AND. .not.quote ) then
              do j=i-1,l,-1
                   if (json(j:j) == '"' .and. .not.quote) then
                                 quote = .true.
                                 k=j-1
                   elseif (json(j:j) == '"' .and. quote) then
                                 quote = .false. 
                                 exit
                   elseif (json(j:j) == ")" .and. .not.paren) then
                                 paren = .true.
                   elseif (json(j:j) == "(" .and. paren) then
                                 paren = .false. 
                                 k=j-1
                   elseif (json(j:j) == "(" .and. .not.paren) then
                                 exit
                   elseif ( .not.paren .AND. quote  )then
                                 thevariable = json(j:k)
                       if (strcase(thevariable) == strcase(varname)) then
                           exit findvars
                        endif
                   endif
             enddo
         endif
 enddo findvars
 objectparse: do k=i,len(json)
    if (json(k:k) == "(" ) then
        do j=k,len(json)
                if (json(j:j) == ")" .and. iscolon) then
                      call cjsonObject_partial (cjson,var,nmlname,varname,json(k:j),st_size=st_size,lowB=lowB)
                 iscolon=.false.
                 exit
                elseif (json(j:j) == ")") then
                 read(json(k+1:j-1),*) ii
                 call cjsonObject_single (cjson,var(ii),nmlname,varname,&
                      ubound(var,dim=1),lbound(var,dim=1),ii,st_size=st_size)
                 exit
                elseif (json(j:j) == ":") then
                  iscolon=.true.
                endif            
        enddo   
    elseif (json(k:k) == "}" ) then
     exit objectparse
    endif
 enddo objectparse

end subroutine cjsonObject_inorder_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date May 2016
!! This subroutine handles string arrays.  Instead of using class(*), it uses character(*) in 
!! order to ensure that a string is being used.  Different compilers seem to handle class(*) 
!! strings in different ways, so this is the simplest solution to that problem.  The string is 
!! passed as a C-pointer, converted to a fortran string pointer array, and then saved as the 
!! string in the correct position in the input array (VAR).
subroutine cjsonObject_array_string (cjson,json,var,nmlname,varname,st_size,val,array_loop,isval,ilowB,begin)
 type (C_PTR),intent(in)                :: cjson       !< The parsed json string C pointer
 character (*),intent(in)               :: json        !< The JSON string
 character (*),intent(in)               :: nmlname     !< The name of the namelist that the variable is in
 character (*),intent(in)               :: varname     !< The name of the variable of interest
 integer,intent(in)                     :: array_loop  !< The size of the array 
 integer                                :: ss          !> \param ss Looping index, the index of the array being checked  
 integer,intent(in)                     :: st_size     !< Size of a string 
 integer,intent(in)                     :: ilowB       !< The lower bound of the array
 integer,optional,intent(in)            :: begin       !< The beginning of the loop
 character (len=st_size),intent(inout)  :: var (ilowB:)!< The string arraay
 character (*) ,intent(in)              :: val         !< val a string that is the array name
 logical,intent(in)                     :: isval       !< If false, then skip to processing single
 integer                                :: cc          !> \param cc used for indexing the json array in C
!! To deal with strings                                                      !!
 character (len=1, kind=c_char), dimension (:), pointer :: ErrChars => null ()!> \param ErrChars gets the individual characters
 type (C_PTR)                                           :: C_String_ptr       !> \param C_String_ptr a C-string pointer
 integer                                :: i           !> \param i Used to get the number of characters of interest in ErrChars
 integer                                :: jj          !> \param jj for looping
 CC = 0

 if ( present(begin) ) then 
   jj = begin
 else
   jj = ilowB
 endif
IF ( isval ) then
do ss = jj , array_loop
       var(ss)=""   
!> Use a C pointer to get the string of interest from the C function string_jsonscalar
   C_String_ptr=string_jsonarray(cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                                  trim(val)//C_NULL_CHAR,CC)
!> Convert the C pointer to a fortran pointer
   call c_f_pointer ( C_String_ptr,ErrChars , [st_size] )
!> C-strings are arrays, so the array of strings is converted into a regular string in Fortran 
    i=1 !> Use i for indexing
    do i=1,st_size
           if ( ErrChars (i) == c_null_char) exit  !> End the loop on the null character
           var(ss)(i:i) = ErrChars (i) !> Put each value of the string array into the correct character location
    enddo
  CC = CC + 1
enddo
ENDIF

!  if ( .not. present(begin) ) then
!       do ss = ilowB , ubound(var,dim=1)
!          CALL cjsonObject_findPartial (cjson,var,nmlname,varname,ubound(var,dim=1),ss,ilowB,st_size=st_size)
!          call cjsonObject_single (cjson,var(ss),nmlname,varname,ss,st_size=st_size)
!       enddo
!  endif
       call cjsonObject_inorder_string (cjson,json,var,nmlname,varname,st_size,ilowB)
end subroutine cjsonObject_array_string
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date May 2016
!! cjsonObject_findPartial is used to find partial arrays in the json object.  It searches for 
!! every possible array combination from (ii:ii) to (ii:end) where ii is the lower bound input to
!! the routine and end is the last index of the array.
subroutine cjsonObject_findPartial (cjson,nmlval,nmlname,varname,ar_size,ii,lowB,st_size)
use iso_c_binding
 class (*), target,intent(inout)   :: nmlval(:)   !< The value of the variable from the namelist/json
 class (*), pointer                :: var (:)     !> \param var A pointer to the value
 type (C_PTR),intent(in)           :: cjson       !< The json string
 character (*),intent(in)          :: nmlname     !< The name of the namelist that the variable is in
 character (*),intent(in)          :: varname     !< The name of the variable of interest
 integer,intent(in)                :: ar_size     !< The size of the array variable
 integer,intent(in)                :: ii          !< The single being checked  
 integer, optional,intent(in)      :: st_size     !< Size of a string 
 integer, optional,intent(in)      :: lowB        !< The lower bound of the array
 character*30                      :: varcheck    !> \ param varcheck The single element number of the array being requested
 integer                           :: isvarcheck  !> \param isvarcheck 1 if there is a single, 
                                                  !! 0 if there isnt
 character*10                      :: begin       !> \param begin The beginning element of the parital array being requested
 character*10                      :: ending      !> \param ending The ending element of the parital array being requested
 integer                           :: l           !> \param l For looping
  var => nmlval

 write (begin,'(I0)') ii !> Convert ii to a string
 varcheck="("//trim(begin)//":)" !> Check for an array with no specified end
 isvarcheck = existval (cjson, nmlname//C_NULL_CHAR , varname//C_NULL_CHAR, trim(varcheck)//C_NULL_CHAR)
if (isvarcheck == 1) then
  call cjsonObject_partial (cjson,nmlval,nmlname,varname,trim(varcheck),st_size,lowB) !> Get the value if it exists
endif
isvarcheck = 0
 do l = ii,ar_size !> Loop from ii to the end of the array
    write (ending,'(I0)') l !> Get the potential ending index
   varcheck="("//trim(begin)//":"//trim(ending)//")"
        !> Check to see if VARCHECK exists in the json
   isvarcheck = existval (cjson, nmlname//C_NULL_CHAR , varname//C_NULL_CHAR, trim(varcheck)//C_NULL_CHAR)
    if (isvarcheck == 1) then
        !> If the VARCHECK exists in the JSON, then get the value(s)
       call cjsonObject_partial (cjson,nmlval,nmlname,varname,trim(varcheck),st_size=st_size,lowB=lowB) 
    endif
   isvarcheck = 0
 enddo

end subroutine cjsonObject_findPartial 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date May 2016
!! This subroutine searches the JSON object to see if a single value for a given index in the JSON
!! exists.  If the single value is included in the JSON, then that value is put in the appropriate
!! location in the array
subroutine cjsonObject_single (cjson,nmlval,nmlname,varname,varmax,varmin,ii,st_size)
use iso_c_binding
 class (*), target,intent(inout)        :: nmlval      !< The value of the variable from the namelist/json
 class (*), pointer                     :: var         !> \param var A pointer to the value
 type (C_PTR),intent(in)                :: cjson       !< The json string
 character (*),intent(in)               :: nmlname     !< The name of the namelist that the variable is in
 character (*),intent(in)               :: varname     !< The name of the variable of interest
 integer,intent(in)                     :: varmax
 integer,intent(in)                     :: varmin
 integer,intent(in)                     :: ii          !< The single being checked 
 integer,optional,intent(in)            :: st_size     !< The length of the string
 character*10                           :: single      !> \ param single The single element number of the array being requested
 character*10                           :: maxs,mins
 integer                                :: issingle    !> \param issingle 1 if there is a single, 
                                                       !! 0 if there isnt
 integer                                :: ip          !> \param ip a integer pointer
 real*4                                 :: r4          !> \param rp a real pointer
 real*8                                 :: rp          !> \param rp a real pointer
 logical                                :: lp          !> \param lp a logical pointer
 character,pointer                      :: sp          !> \param lp a string pointer

  var => nmlval !> point to the scalar value in the array

  write (single,'(I0)') ii !> Convert the integer index to a string
  if (ii > varmax .OR. ii < varmin) then !> Fatal error if the request is outside of the limit of the array
     write (maxs,'(I0)')varmax
     write (mins,'(I0)')varmin
     CALL cjson_error_mesg("cjsonObject_single","JSON - "//trim(nmlname)//"{"// & 
          trim(varname)//"("//trim(single)//")} is outside of the limits of "// & 
          trim(varname)//"("//trim(mins)//":"//trim(maxs)//")",                 & 
          FATAL) 
  endif
  single = "("//trim(single)//")" !> Put the SINGLE string in quotes
  issingle = existval (cjson, nmlname//C_NULL_CHAR , varname//C_NULL_CHAR, trim(single)//C_NULL_CHAR) !> Check to see if SINGLE exists

 IF (issingle == 0) then !> Do nothing if SINGLE does not exist in the json
 ELSEIF (issingle == 1) then !> If SINGLE is in the json, get the scalar value
    select type (var) 
        type is (integer)
                CALL cjsonObject_scalar_value (cjson,ip,nmlname,varname,attribute=trim(single))
                var = ip
        type is (real*4)
                CALL cjsonObject_scalar_value (cjson,r4,nmlname,varname,attribute=trim(single))
                var = r4
        type is (real*8)
                CALL cjsonObject_scalar_value (cjson,rp,nmlname,varname,attribute=trim(single))
                var = rp
        type is (logical)
                CALL cjsonObject_scalar_value (cjson,lp,nmlname,varname,attribute=trim(single))
                var = lp
        type is (character(*))
                sp => var
                CALL cjsonObject_scalar_value (cjson,sp,nmlname,varname,attribute=trim(single),st_size=st_size)

    end select
 ELSE
        CALL cjson_error_mesg('cjson_object_mod', "FATAL: JSON - There was a problem determining if the array "//trim(varname)// &
             " Has a single value call",FATAL)
 ENDIF 

end subroutine cjsonObject_single 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date May 2016
!! Assuming the partial array is included in the JSON file, the partial array is found and inserted
!! into the array.
subroutine cjsonObject_partial (cjson,nmlval,nmlname,varname,varcheck,st_size,lowB)
use iso_c_binding
 class (*), target,intent(inout)   :: nmlval(:)   !< The value of the variable from the namelist/json
 class (*), pointer                :: var   (:)   !> \param var A pointer to the value
 type (C_PTR),intent(in)           :: cjson       !< The json string
 character (*),intent(in)          :: nmlname     !< The name of the namelist that the variable is in
 character (*),intent(in)          :: varname     !< The name of the variable of interest
 character (*),intent(in)          :: varcheck    !< The string containing the limits of the partial array
 integer, optional,intent(in)      :: st_size     !< Size of a string 
 integer, optional,intent(in)      :: lowB        !< THe lower bound of the array
 integer                           :: ilowB
 integer                           :: l           !> \param l Used for indexing
 integer                           :: ii          !> \param ii Used for indexing the array
 integer                           :: cc          !> \param cc used for indexing the json array in C
 integer                           :: begin,ending!> \param begin,ending The beginning and ending positions of the partial array as int
 character (len=:),allocatable     :: stbeg,stend !> \param stbeg,stend The beginning and ending positions of the partial array as string
 integer                           :: json_array_length    !> \param json_array_length The length of the array in the json file
 integer                           :: ar_size     !> \param ar_size The size of the array variable
 integer                           :: array_loop  !> \param array_loop The ending point of the loop for the json array
 integer,pointer                   :: ip (:)      !> \param ip an integer array pointer
 real*4,pointer                    :: r4 (:)      !> \param r4 a real*4 array pointer
 real*8,pointer                    :: rp (:)      !> \param rp a real*8 array pointer
 logical,pointer                   :: lp (:)      !> \param lp a logical array pointer
 character (len=10)                :: theunits    !< \param theunits a string of the units if they exist
 character (len=:),allocatable     :: units
 integer                           :: u_flag      !< \param u_flag 1 if there are units, 0 if there are not
 real                              :: convert_flag!< \param convert_flag -1.0 if units are not convertable
 character*10                      :: mins,maxs
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 units="units"
 u_flag= existval( cjson, trim(nmlname)//C_NULL_CHAR , trim(varname)//C_NULL_CHAR, &
                   trim(units)//C_NULL_CHAR )
 if (u_flag == 1 ) then
  call cjsonObject_scalar_string (cjson,theunits,nmlname,varname,len(theunits),units)
  call areUnitsConvert (theunits,nmlname,varname,convert_flag) !> Check if units are convertable
 endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 cc = 0 !> Initialize CC to 0

if ( present(lowB) ) then !> Get the appropriate lower bound for the full variable array
 ilowB = lowB
else
 ilowB = 1
endif
 var(ilowB:) => nmlval
!> Find the colon (:) in the input string VARCHECK
   do l = 1,len(varcheck)
      if (varcheck(l:l) == ":") exit !> l = location of :
   enddo
!> Get the numbers on either side of the : [ 1="(" ; l=":" ; len(varcheck)=")" ]
     stbeg = varcheck(2:l-1)
     stend = varcheck(l+1:len(varcheck)-1)
   read (stbeg,'(i8)') begin !> Convert the lower number to an integer
!!!!!!!!!!!!!!!!!!!!!
! if (valueflag == 1) then
 !> Find the length of the array in the json file and the actual size of the array variable
 json_array_length = cjson_array_length (cjson, nmlname//C_NULL_CHAR , varname//C_NULL_CHAR,&
                                         varcheck//C_NULL_CHAR)
  if (stend .ne. "") then
   read (stend,'(i8)') ending !> Convert the number to the right of the colon to a string
  else 
   ending = begin + json_array_length - 1 !> If no number is given, use the ending of the json array as the end
  endif

 ar_size = ending - begin + 1 !> The size size of the array request

!> Error check
 if (ending < begin) then
   CALL cjson_error_mesg('cjson_object_mod', "JSON - The request for "//varname//varcheck//" in "//nmlname// &
   "has a higher beginning than end. ("//stbeg//">"//stend//").",FATAL)
 endif
 if (ending < ilowB .OR. begin > ubound(var,dim=1) ) then
     write (mins,'(I0)') lbound(var,dim=1)
     write (maxs,'(I0)') ubound(var,dim=1)
   CALL cjson_error_mesg('cjson_object_mod', "JSON - The request for "//varname//varcheck//" in "//nmlname// &
    " is outside of the limits of "//varname//"("//trim(mins)//":"//trim(maxs)//")."                         &
     ,FATAL)
 endif
 if (begin < ilowb .OR. ending > ubound(var,dim=1) ) then
     write (mins,'(I0)') lbound(var,dim=1)
     write (maxs,'(I0)') ubound(var,dim=1)
   CALL cjson_error_mesg('cjson_object_mod', "JSON - The request for "//varname//varcheck//" in "//nmlname// &
    " is partially outside of the limits of "//varname//"("//trim(mins)//":"//trim(maxs)//&
    ").  Only writing part that is inside of the array",NOTE) 
 endif
 CALL compareJsonActual ( json_array_length,ar_size,trim(varname)//trim(varcheck),trim(nmlname) ) 
!> Find the correct end of the array request 
   if (ar_size >= json_array_length) then
     if (json_array_length + begin - 1 < ubound(var,dim=1) ) then
      array_loop = json_array_length + begin - 1
     else
      array_loop = ubound(var,dim=1)
     endif
   elseif (ar_size < json_array_length) then
      array_loop = ending 
   endif
!!!!!!!!!!!!!!!!!!!!!
 select type (var) 
   type is (character(*)) !> Strings are handled by the string routine
     CALL cjsonObject_partial_string (cjson,var,nmlname,varname,varcheck,begin,array_loop,st_size,lowB)
   type is (integer)
     !> Use a pointer with the correct lower bound ILOWB
     ip(ilowB:) => var
     !> If scalar integer, use C function int_jsonarray to get the value of var
     do ii = begin , array_loop
        if (ii >= ilowB) then
          ip(ii) = int_jsonarray (cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                             trim(varcheck)//C_NULL_CHAR,CC)
           if (u_flag == 1 .and. convert_flag > 0) then
                 ip(ii)=convert_int (trim(theunits)//C_NULL_CHAR, ip(ii))
           endif
        endif
        CC = CC + 1
     enddo
!
   type is (real*4)
     r4(ilowB:) => var
     !> If scalar real, use C function real_jsonarray to get the value of var
     do ii = begin , array_loop
        if (ii >= ilowB) then
          r4(ii) = real_jsonarray (cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                             trim(varcheck)//C_NULL_CHAR,CC)
           if (u_flag == 1 .and. convert_flag > 0) then
                 r4(ii)=convert_float (trim(theunits)//C_NULL_CHAR, r4(ii))
           endif
        endif
        CC = CC + 1
     enddo
   type is (real*8)
     rp(ilowB:) => var
     !> If scalar real, use C function real_jsonarray to get the value of var
     do ii = begin , array_loop
        if (ii >= ilowB) then
          rp(ii) = real_jsonarray (cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                             trim(varcheck)//C_NULL_CHAR,CC)
           if (u_flag == 1 .and. convert_flag > 0) then
                 rp(ii)=convert_float (trim(theunits)//C_NULL_CHAR, rp(ii))
           endif
        endif
        CC = CC + 1
     enddo
!
   type is (logical)
     lp(ilowB:) => var
     !> If scalar logical, use C function int_jsonarray to get the value of var
     do ii = begin , array_loop
        if (ii >= ilowB) then
          lp(ii) = int_jsonarray (cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                             trim(varcheck)//C_NULL_CHAR,CC)
        endif
        CC = CC + 1
     enddo
 end select

 
end subroutine cjsonObject_partial
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date June 2016
!! Assuming the partial string array is included in the JSON file, the partial array is found 
!! and inserted into the array.
subroutine cjsonObject_partial_string (cjson,var,nmlname,varname,val,begin,array_loop,st_size,lowB)
use iso_c_binding


 type (C_PTR),intent(in)                :: cjson       !< The json string
 character (*),intent(in)               :: nmlname     !< The name of the namelist that the variable is in
 character (*),intent(in)               :: varname     !< The name of the variable of interest
 character (*)                          :: val         !< The name of the partial array in the json file
 integer,intent(in)                     :: st_size     !< Size of a string 
 integer,intent(in)                     :: lowB        !< THe lower bound of the array
 character(len=st_size),intent(inout)   :: var(lowB:)  !< The string array
 integer                                :: ss          !> \param l,m Used for indexing
 integer                                :: cc          !> \param cc used for indexing the json array in C
 integer,intent(in)                     :: begin       !< The beginning position of the partial array as int
 integer,intent(in)                     :: array_loop  !< The ending point of the loop for the json array
 character (len=1, kind=c_char), dimension (:), pointer:: ErrChars => null ()!> \param ErrChars gets the individual characters
 type (C_PTR)                                          :: C_String_ptr       !> \param C_String_ptr a C-string pointer
 character (len=st_size)                               :: data_source        !> \param data_source String holding the string value
 integer                                :: i           !> \param i Used to get the number of characters of interest in ErrChars
 integer                                :: jj          !> \param jj for looping
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 cc = 0 !> Initialize CC to 0

   jj = begin

do ss = jj , array_loop
IF (ss >= lowB) then
data_source=" "
!> Use a C pointer to get the string of interest from the C function string_jsonscalar
   C_String_ptr=string_jsonarray(cjson,nmlname//C_NULL_CHAR,varname//C_NULL_CHAR,&
                                  trim(val)//C_NULL_CHAR,CC)
!> Convert the C pointer to a fortran pointer
   call c_f_pointer ( C_String_ptr,ErrChars , [st_size] )
!> C-strings are arrays, so the array of strings is converted into a regular string in Fortran 

    do i=1,st_size
           if ( ErrChars (i) == c_null_char) exit  !> End the loop on the null character
            data_source(i:i) = ErrChars (i)
    enddo
        var(ss)(1:st_size) = data_source(1:i-1)
ENDIF
  CC = CC + 1

enddo

end subroutine cjsonObject_partial_string
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date May 2016
!! Checks to see if there is a fillvalue attribute in the json.  If there is a fillvalue, the 
!! array is initialized with the fillvalue
subroutine fillval_for_array (cjson,nmlval,nmlname,varname,st_size)
use iso_c_binding
 class (*), target,intent(inout)        :: nmlval (:)  !< The value of the variable from the namelist/json
 class (*), pointer                     :: var    (:)  !> \param var A pointer to the value
 type (C_PTR),intent(in)                :: cjson       !< The json string
 character (*),intent(in)               :: nmlname     !< The name of the namelist that the variable is in
 character (*),intent(in)               :: varname     !< The name of the variable of interest
 integer, optional,intent(in)           :: st_size     !< Size of a string 
 character (len=9)                      :: fillvalue = "fillvalue" 
 integer                                :: isfillvalue !> \param isfillvalue 1 if there is a fillvalue, 
                                                       !! 0 if there isnt
 integer                                :: ip          !> \param ip a integer pointer
 real*4                                 :: r4          !> \param r4 a real*4 pointer
 real*8                                 :: rp          !> \param rp a real*8 pointer
 logical                                :: lp          !> \param lp a logical pointer
 character(len=100)                     :: st_fill     !> \param lp a string to hold the fillvalue
 character(len=:),allocatable           :: val

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 var => nmlval
 isfillvalue = existval (cjson, nmlname//C_NULL_CHAR , varname//C_NULL_CHAR, fillvalue//C_NULL_CHAR)
 IF (isfillvalue == 0) then
 ELSEIF (isfillvalue == 1) then
!  CALL cjson_error_mesg('cjson_object_mod', "JSON - A fillvalue was automatically detected for "//trim(varname)//" in "//trim(nmlname) & 
!               //".  Filling the array with the fillvalue.",NOTE)
    select type (var) 
        type is (integer)
                CALL cjsonObject_scalar_value (cjson,ip,nmlname,varname,attribute=fillvalue)
                var = ip
        type is (real*4)
                CALL cjsonObject_scalar_value (cjson,r4,nmlname,varname,attribute=fillvalue)
                var = r4
        type is (real*8)
                CALL cjsonObject_scalar_value (cjson,rp,nmlname,varname,attribute=fillvalue)
                var = rp
        type is (logical)
                CALL cjsonObject_scalar_value (cjson,lp,nmlname,varname,attribute=fillvalue)
                var = lp
        type is (character(*))
                st_fill=""
                val=fillvalue
                CALL cjsonObject_scalar_string (cjson,st_fill,nmlname,varname,st_size,val) 
                CALL stringfill (var,st_fill,st_size)
    end select
 ELSE
        CALL cjson_error_mesg('cjson_object_mod', "JSON - There was a problem determining if the array "//trim(varname)// &
             " Has a fillvalue",FATAL)
 ENDIF

end subroutine fillval_for_array
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date May 2016
subroutine stringfill (var,st_fill,st_size)

 integer                      :: st_size     !< Size of a string 
 character (len=st_size)      :: var    (:)  !> \param var A pointer to the value
 character(len=100)           :: st_fill     !> \param lp a string to hold the fillvalue

      var = trim(st_fill) !> Fill the array with the fill value

end subroutine stringfill 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date May 2016
!! Puts the correct string into VAL.  If no attribute is requested, then the default "value" is 
!! used.
!! \code
!!  if (present (attribute)) then 
!!     val = attribute 
!!  else
!!     val="value" 
!!  endif
!! \endcode
subroutine getValue (val,attribute)
 character (*), optional,intent(in)             :: attribute    !< If something other than the value is requested
 character (len=:),allocatable,intent(out)      :: val          !< \param val a string that is "value"
 character*10                                   :: n1,n2
!> Check if the value or an attribute is being requested
 if (present (attribute)) then 
    val = attribute !> give value the attribute
 else
    val="value" !> If no attribute is requested, the default is value
 endif
end subroutine getValue
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \author Tom Robinson thomas.robinson@noaa.gov
!! \date May 2016
!! Error checking for arrays.  Warnings are written if the Fortran variable array or partial 
!! array size does not match the size of the array in the JSON file.
subroutine compareJsonActual (json_size,actual_size,varname,nmlname)
 integer,intent(in)             :: json_size      !< The size of the array in the JSON file
 integer,intent(in)             :: actual_size    !< The size of the array in Fortran
 character (*),intent(in)       :: nmlname        !< The name of the namelist that the variable is in
 character (*),intent(in)       :: varname        !< The name of the variable of interest
 character (len=:),allocatable  :: errormsg
 character*10                   :: n1,n2
 write (n1,'(I0)')json_size
 write (n2,'(I0)')actual_size
IF (json_size == actual_size) then !> No errors if the arrays are the same size
ELSEIF (json_size < actual_size) then  !> Error if \code (json_size < actual_size) \endcode
errormsg = "The array "//trim(varname)//" in " //trim(nmlname)//" has a size "//trim(n2)//    &
           " which is bigger than the JSON array size of "//trim(n1)//". Will only write "//  &
           trim(n1)//" elements of the array."
        CALL cjson_error_mesg('cjson_object_mod',errormsg,NOTE)
ELSEIF (json_size > actual_size) then !> Error if \code (json_size > actual_size) \endcode
        CALL cjson_error_mesg('cjson_object_mod', " The json array"              &
        //trim(varname)//" in " //trim(nmlname)//" has a size "//trim(n1)//&
        " but the actual size of the array is "//trim(n2)//". " //         &
        "Will only write "//trim(n2)//" elements of the array, and ignore any more.",NOTE)
ENDIF
end subroutine compareJsonActual

subroutine areUnitsConvert (theunits,nmlname,varname,rp)
 character(*),intent(IN)        :: theunits     !< The string containing the units
 character(*),intent(IN)        :: nmlname      !< The name of the namelist that the variable is in
 character(*),intent(IN)        :: varname      !< The name of the variable of interest
 real,intent(OUT)               :: rp           !> \param rp The flag to see if the units are convertable
 rp=100.0
                 rp = convert_check (trim(theunits)//C_NULL_CHAR, rp)
     if (rp == -1.0) then  
 CALL cjson_error_mesg('cjson_object_mod', "The units "//trim(theunits)//" in the json object "//nmlname//&
 "{"//varname//"}"//" is not a convertable unit." ,NOTE)
     elseif (rp==-2.0) then
!        CALL cjson_error_mesg('cjson_object_mod', "The units "//trim(theunits)//" are already MKS",NOTE) 
     endif
end subroutine areUnitsConvert

end module
