module cjson_args_mod
!> @file

!> \author Tom Robinson May 2016 thomas.robinson@noaa.gov
!! This module does the work of the cjson_wrapper module using a flag named whichjson
!! \verbose
!! ______________________________________________________________________
!! |whichjson | Interpretation                                           |
!! |     1    | The variable is not in the JSON file (use default)       |
!! |     2    | The variable is a JSON key in the parent JSON group      |
!! |     3    | The variable is a JSON object in the parent JSON group   |
!! |    100   | The variable is of a Fortran type and is contained in as |
!! |          |     an object of an object.                              |
!! |__________|__________________________________________________________|
!! \endverbose
 use cjson_objects_mod, only: cjsonObject
 use cjson_mod, only: json_value
 use cjson_error_mod,   only: cjson_error_mesg, FATAL, WARNING, NOTE
 use cjson_jsonout_mod, only: cjson_write,OBJ_PTR, OUTPUT_JSON
 use iso_c_binding
 implicit none
 include 'mpif.h'
     interface
         integer function whatlevel (cjson, nml_name , obj_name, var_name, attribute) bind(C, name="whatlevel")
           use iso_c_binding
           type (C_PTR),intent(in),value  :: cjson    !< The json string
           character(kind=c_char) :: nml_name !< The name of the "namelist" that the variable is in
           character(kind=c_char) :: var_name !< the name of the variable
           character(kind=c_char) :: obj_name !< the name of an object           
           character(kind=c_char) :: attribute !< The attribute of the variable object
         end function
         integer function namelist_exist (my_json_string, nml_name) bind (C,name="namelist_exist") 
           use iso_c_binding
           character(kind=c_char) :: nml_name       !< The name of the namelist that the variable is in 
           character(kind=c_char) :: my_json_string !< The string containing the JSON
         end function
     end interface

     interface json_call
        module procedure scalar_json
        module procedure onedim_json
        module procedure twodim_json
     end interface

contains
!> \author Tom Robinson May 2016 thomas.robinson@noaa.gov
!! Determines if the variable is a JSON object or JSON key for scalar variables
subroutine scalar_json(cjson,json,var,nmlname,varname,objname,st_size)
 character (*), intent (in)             :: json         !< String containing the contents of the json file
 type (C_PTR), INTENT (IN)              :: cjson
 class (*),target,INTENT (INOUT)  	:: var          !> \param var A pointer to the value
 class (*),pointer                      :: varpoint
 character (*)           		:: nmlname 	!< The name of the namelist that the variable is in
 character (*)           		:: varname 	!< The name of the variable of interest
 integer, optional  		     	:: st_size 	!< Size of a string 
 character (*), optional                :: objname      !< A string with the fortran type name

 integer                                :: whichjson    !> \param whichjson Determines which json subroutine to call based 
                                                        !! on if the variable is a json key or object 
 integer                                :: nmlflag      !> \param nmlflag Check if namelist exists in the JSON
 nmlflag = namelist_exist (json//C_NULL_CHAR, trim(nmlname)//C_NULL_CHAR)
!> Check that a string carries the sring size
varpoint=>var
IF (nmlflag > 0) then
 select type (varpoint)
        type is ( character(*) )
            CALL check_string (nmlname,varname,objname,st_size)
 end select
!> Find the variable type in the JSON file
 whichjson = find_json_key (cjson,nmlname,varname,objname)
      if        (whichjson==3 .and. present(objname) ) then
        call cjsonObject ( cjson,var,trim(nmlname),trim(varname),attribute=trim(objname),st_size=st_size ) !> Call cjsonObject if it is a JSON object
      elseif    (whichjson==3) then
        call cjsonObject ( cjson,var,trim(nmlname),trim(varname),attribute=objname,st_size=st_size ) !> Call cjsonObject if it is a JSON object
      elseif    (whichjson==2) then        
        call json_value  ( cjson,json, trim(nmlname),trim(varname), var, st_size=st_size ) !> Call json_value if it is a JSON key
      elseif    (whichjson==1) then      !> Print a note that the default will be used.
        !CALL cjson_error_mesg('cjson_args',"JSON - "//trim(varname)//" is not present in the JSON group "//trim(nmlname)//". Using default.",NOTE)
      endif
ENDIF
 CALL cjson_write (var,trim(varname),attribute=objname,st_size=st_size)
end subroutine scalar_json
!> \author Tom Robinson May 2016 thomas.robinson@noaa.gov
!! Determines if the variable is a JSON object or JSON key for 1D array variables
subroutine onedim_json(cjson,json,var,nmlname,varname,ilowb,objname,st_size)
 character (*), intent (in)             :: json         !< String containing the contents of the json file
 type (C_PTR), INTENT (IN)              :: cjson
 class (*),target,INTENT (INOUT)  	:: var (:)    !> \param var A pointer to the value
 class (*),pointer                      :: varpoint(:)
 character (*)           		:: nmlname 	!< The name of the namelist that the variable is in
 character (*)           		:: varname 	!< The name of the variable of interest
 integer, optional  		     	:: st_size 	!< Size of a string 
 integer,optional                       :: ilowB        !< lower bounds of array
 character (*), optional                :: objname      !< A string with the fortran type name


 integer                                :: whichjson    !> \param whichjson Determines which json subroutine to call based 
                                                        !! on if the variable is a json key or object 
 integer                                :: LOWBOUND1
 integer                                :: nmlflag      !> \param nmlflag Check if namelist exists in the JSON
 nmlflag = namelist_exist (json//C_NULL_CHAR, trim(nmlname)//C_NULL_CHAR)

!> Check that a string carries the sring size
varpoint=>var
IF (nmlflag > 0) then
 select type (varpoint)
        type is ( character(*) )
          CALL check_string (nmlname,varname,objname,st_size)
end select

  LOWBOUND1 = lbound(var,dim=1)
      if ( present (ilowb) ) LOWBOUND1 = ilowb
!>
  whichjson = find_json_key (cjson,nmlname,varname,objname)  

      if        (whichjson==3 .and. present(objname) ) then
        call cjsonObject ( cjson,json,var,trim(nmlname),trim(varname),attribute=trim(objname),st_size=st_size ) !> Call cjsonObject if it is a JSON object
      elseif        (whichjson==3) then
        call cjsonObject ( cjson,json,var,trim(nmlname),trim(varname),lowB=LOWBOUND1, st_size=st_size )
      elseif    (whichjson<=2) then   
        call json_value  ( cjson, json, trim(nmlname),trim(varname),var,lowB=LOWBOUND1, st_size=st_size )
      elseif    (whichjson==1) then      
        !CALL cjson_error_mesg('cjson_args',"JSON - "//trim(varname)//" is not present in the JSON group "//trim(nmlname)//". Using default.",NOTE)
      endif
ENDIF
 CALL cjson_write (var,trim(varname),attribute=objname,st_size=st_size)
end subroutine onedim_json
!> \author Tom Robinson May 2016 thomas.robinson@noaa.gov
!! Determines if the variable is a JSON object or JSON key for 2D array variables
subroutine twodim_json(cjson,json,var,nmlname,varname,ilowb,jlowb,objname,st_size)
 character (*), intent (in)             :: json         !< String containing the contents of the json file
 type (C_PTR), INTENT (IN)              :: cjson
 class (*),target,INTENT (INOUT)  	:: var (:,:)    !> \param var A pointer to the value
 class (*),pointer                      :: varpoint(:,:)
 class (*),pointer                      :: oned(:) 
 character (*)           		:: nmlname 	!< The name of the namelist that the variable is in
 character (*)           		:: varname 	!< The name of the variable of interest
 integer, optional  		     	:: st_size 	!< Size of a string 
 integer,optional                       :: ilowB, jlowB !< lower bounds of array
 character (*), optional                :: objname      !< A string with the fortran type name


 integer                                :: whichjson    !> \param whichjson Determines which json subroutine to call based 
                                                        !! on if the variable is a json key or object 
 integer                                :: LOWBOUND1,LOWBOUND2
 integer                                :: ar_size
 integer                                :: nmlflag      !> \param nmlflag Check if namelist exists in the JSON
 nmlflag = namelist_exist (json//C_NULL_CHAR, trim(nmlname)//C_NULL_CHAR)

!> Check that a string carries the sring size
varpoint=>var
IF (nmlflag > 0 ) then
 select type (varpoint)
        type is ( character(*) )
             CALL check_string (nmlname,varname,objname,st_size)
 end select

  LOWBOUND1 = lbound(var,dim=1)
  LOWBOUND2 = lbound(var,dim=2)
      if ( present (ilowb) ) LOWBOUND1 = ilowb
      if ( present (jlowb) ) LOWBOUND2 = jlowb 
!> THIS WILL ONLY WOTK IF whichjson == 2 (KEY)
  whichjson = find_json_key (cjson,nmlname,varname,objname)  
      if        (whichjson==3) then
!        call cjsonObject ( cjson,var2,trim(nmlname),trim(varname) )
CALL cjson_error_mesg('cjson_args',"TODO: 2D functionality as a JSON object",FATAL)
      elseif    (whichjson<=2) then   
        call json_value  (cjson, json, trim(nmlname),trim(varname),var,ilowB=LOWBOUND1,jlowb=LOWBOUND2,&
                           st_size=st_size )
      elseif    (whichjson==1) then      
        !CALL cjson_error_mesg('cjson_args',"JSON - "//trim(varname)//" is not present in the JSON group "//trim(nmlname)//". Using default.",NOTE)
      endif
! ar_size=size(var)
! call c_f_pointer( C_LOC(var(LOWBOUND1,LOWBOUND2)), oned, [ar_size])
ENDIF
 CALL cjson_write (varpoint,trim(varname),attribute=objname,st_size=st_size)
END SUBROUTINE twodim_json
!> \author Tom Robinson May 2016 thomas.robinson@noaa.gov
!! Makes the determination if the variable is a key or an object in the JSON file.  @note If it is  
!! an object, the object MUST contain with "value" or "fillvalue" as one of its keys.
integer function find_json_key (cjson,nmlname,varname,objname)
 type (C_PTR), INTENT (IN)              :: cjson
 character (*) ,intent(in)          	:: nmlname 	!< The name of the namelist that the variable is in
 character (*) ,intent(in)          	:: varname 	!< The name of the variable of interest
 character (*), optional,intent(in)     :: objname      !< A string with the fortran type name

 character (len=:),allocatable          :: obj_name
 character (len=:),allocatable          :: vname
 character (len=:),allocatable          :: attribute
!> Set up to search for "value"
        if (present (objname) ) then
           obj_name = varname
           vname = objname
           attribute="value"
        else 
           obj_name=varname
           vname="value"
           attribute="none"
        endif
!> if "value" is not present, search for "fillvalue" instead
  if (whatlevel(cjson,trim(nmlname)//c_null_char,trim(obj_name)//c_null_char, &
                               trim(vname)//c_null_char,trim(attribute)//c_null_char) == 2) then 
        if (present (objname) ) then
           obj_name = varname
           vname = objname
           attribute="fillvalue"
        else 
           obj_name=varname
           vname="fillvalue"
           attribute="none"
        endif
   endif      
!> Use the C function whatlevel to determine if the variabl is a JSON object or key
     find_json_key = whatlevel(cjson,trim(nmlname)//c_null_char,trim(obj_name)//c_null_char, &
                               trim(vname)//c_null_char,trim(attribute)//c_null_char)  

end function find_json_key
!> \author Tom Robinson May 2016 thomas.robinson@noaa.gov
!! A check for the string size.  If the variable is a string, then it MUST include the string size.
subroutine check_string (nmlname,varname,objname,st_size)

 character (*) ,intent(in)          	:: nmlname 	!< The name of the namelist that the variable is in
 character (*) ,intent(in)          	:: varname 	!< The name of the variable of interest
 character (*), optional,intent(in)     :: objname      !< A string with the fortran type name
 integer,       optional,intent(in)     :: st_size      !< The string size



        if (.not. present(st_size) ) then !> If the string size is not present, give an error
          if (present(objname) ) then
 CALL cjson_error_mesg('cjson_args',"JSON - "//objname//" in the object "//varname//" within the grouping "//nmlname// &
" is a string, but no string size was given when calling JSON_ARGS.  Please specify the size of the string",FATAL)

          else
 CALL cjson_error_mesg('cjson_args',"JSON - "//varname//" within the grouping "//nmlname// &
" is a string, but no string size was given when calling JSON_ARGS.  Please specify the size of the string",FATAL)
stop
          endif
        endif 


end subroutine check_string

end module cjson_args_mod

